{
  description = "wisp - a Lisp-to-WebAssembly compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pack = {
      url = "github:colinrozzi/pack";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, pack }:
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
          pkg-config
          rustToolchain
        ];

      in {
        devShells.default = pkgs.mkShell {
          inherit buildInputs nativeBuildInputs;

          packages = with pkgs; [
            rustToolchain
            pkg-config
            openssl
            wasmtime
          ];

          shellHook = ''
            echo "wisp development environment"
            echo "  cargo build --release     Build wisp compiler"
            echo "  cargo run -- compile X    Compile a .wisp file"
            echo "  cargo test                Run tests"
          '';
        };

        packages.default = let
          combinedSrc = pkgs.runCommand "wisp-combined-src" {} ''
            mkdir -p $out
            cp -r ${./.}/. $out/
            chmod -R u+w $out

            # Put pack as sibling so ../pack paths resolve
            cp -rL ${pack} $out/../pack || true
            # Also put it inside for Cargo git dep override
            mkdir -p $out/pack
            cp -rL ${pack}/. $out/pack/
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
