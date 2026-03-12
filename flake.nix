{
  description = "wisp - a Lisp-to-WebAssembly compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        # Rust toolchain with WASM target
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
          targets = [ "wasm32-unknown-unknown" ];
        };

        # Build inputs
        buildInputs = with pkgs; [
          openssl
        ] ++ lib.optionals stdenv.isDarwin [
          darwin.apple_sdk.frameworks.Security
          darwin.apple_sdk.frameworks.SystemConfiguration
        ];

        nativeBuildInputs = with pkgs; [
          rustToolchain
          pkg-config
        ];

      in
      {
        devShells.default = pkgs.mkShell {
          inherit buildInputs;

          packages = with pkgs; [
            rustToolchain
            pkg-config

            # Dev tools
            cargo-watch
            cargo-expand

            # WASM tools
            wasmtime
          ];

          RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
          RUST_BACKTRACE = "1";

          shellHook = ''
            echo "Wisp development environment"
            echo "Rust: $(rustc --version)"
            echo ""
            echo "Commands:"
            echo "  cargo build --release     Build wisp compiler"
            echo "  cargo run -- compile X    Compile a .wisp file"
            echo "  cargo test                Run tests"
          '';
        };

        # For nix build, we need to handle the pack dependency
        # This creates a derivation that builds wisp with pack included
        packages.default = let
          # Read pack from the expected location relative to wisp
          packSrc = builtins.path {
            path = ../pack;
            name = "pack-src";
          };

          combinedSrc = pkgs.runCommand "wisp-combined-src" {} ''
            mkdir -p $out
            cp -r ${./.}/. $out/
            chmod -R u+w $out

            # Put pack inside wisp directory
            mkdir -p $out/pack
            cp -r ${packSrc}/. $out/pack/

            # Patch Cargo.toml to use local pack
            ${pkgs.gnused}/bin/sed -i 's|path = "../pack"|path = "./pack"|g' $out/Cargo.toml
          '';
        in pkgs.rustPlatform.buildRustPackage {
          pname = "wisp";
          version = "0.1.0";

          src = combinedSrc;

          cargoLock = {
            lockFile = ./Cargo.lock;
          };

          inherit nativeBuildInputs buildInputs;

          meta = with pkgs.lib; {
            description = "A Lisp-to-WebAssembly compiler";
            license = licenses.mit;
          };
        };

        packages.wisp = self.packages.${system}.default;
      });
}
