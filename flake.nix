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
      url = "github:colinrozzi/pack/v0.2.0";
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

          packages.update-pack = pkgs.writeShellScriptBin "update-pack" ''
            set -e
            VERSION="''${1:?Usage: nix run .#update-pack <version> (e.g. v0.2.1)}"

            echo "Updating pack to $VERSION..."

            # Update all Cargo.toml files
            find . -name "Cargo.toml" -not -path "*/target/*" \
              -exec ${pkgs.gnused}/bin/sed -i \
                "s|colinrozzi/pack\.git\", tag = \"[^\"]*\"|colinrozzi/pack.git\", tag = \"$VERSION\"|g" {} \;
            echo "  Updated Cargo.toml files"

            # Update flake.nix URL
            ${pkgs.python3}/bin/python3 -c "
import re, sys
with open('flake.nix', 'r') as f:
    content = f.read()
content = re.sub(
    r'(url = \"github:colinrozzi/pack)/[^\"]*',
    r'\1/' + sys.argv[1],
    content,
    count=1
)
with open('flake.nix', 'w') as f:
    f.write(content)
            " "$VERSION"
            echo "  Updated flake.nix"

            # Update flake lock
            nix flake update pack
            echo "  Updated flake.lock"

            echo ""
            echo "Pack updated to $VERSION. Changes:"
            git diff --stat
          '';

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
