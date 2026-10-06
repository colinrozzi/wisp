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

        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "wisp";
          version = "0.1.0";

          src = pkgs.lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              pkgs.lib.cleanSourceFilter path type
              && !(builtins.elem (builtins.baseNameOf path)
                [ "target" "compiled" ".direnv" ".jj" ]);
          };

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

          packages.update-theater = pkgs.writeShellScriptBin "update-theater" ''
            set -e
            VERSION="''${1:?Usage: nix run .#update-theater <version> (e.g. v0.3.1)}"

            echo "Updating theater to $VERSION..."

            # Update all Cargo.toml files
            find . -name "Cargo.toml" -not -path "*/target/*" \
              -exec ${pkgs.gnused}/bin/sed -i \
                "s|colinrozzi/theater\.git\", tag = \"[^\"]*\"|colinrozzi/theater.git\", tag = \"$VERSION\"|g" {} \;
            echo "  Updated Cargo.toml files"

            echo ""
            echo "Theater updated to $VERSION. Changes:"
            git diff --stat
          '';
      });
}
