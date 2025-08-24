{
  description = "UPLC-CAPE: Comparative Artifact Performance Evaluation for UPLC";

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    # IOHK devx for pre-cached development environments
    devx.url = "github:input-output-hk/devx";

    # Plutus and Cardano tooling
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    plutus = {
      url = "github:IntersectMBO/plutus/800c67d38d01177fd0a36d01aa23ff7fea7bd1ba";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskell-nix,
      devx,
      hackage,
      CHaP,
      plutus,
      iohk-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        # Import nixpkgs with haskell.nix overlays
        pkgs = import nixpkgs {
          inherit system;
          config = haskell-nix.config;
          overlays = [
            iohk-nix.overlays.crypto
            iohk-nix.overlays.cardano-lib
            haskell-nix.overlay
            iohk-nix.overlays.haskell-nix-crypto
            iohk-nix.overlays.haskell-nix-extra
          ];
        };

        # UPLC CLI from Plutus repository (musl build)
        uplcMusl = plutus.packages.${system}.musl64-uplc;
        plcMusl = plutus.packages.${system}.musl64-plc;
        pirMusl = plutus.packages.${system}.musl64-pir;
        plutusMusl = plutus.packages.${system}.musl64-plutus;

        # Development shell extending devx with UPLC-CAPE specific tools
        baseShell = devx.devShells.${system}.ghc96-iog-full.overrideAttrs (old: {
          buildInputs =
            (old.buildInputs or [ ])
            ++ (with pkgs; [
              # Core development tools
              git

              # Formatting tools
              treefmt
              shfmt # Shell script formatter
              nodePackages.prettier # YAML, Markdown, and more

              # Documentation and ADR tools
              nodejs_20 # Required for log4brains

              # ADR command wrapper
              (writeShellScriptBin "adr" ''
                case "$1" in
                  "new"|"n")
                    shift
                    npx log4brains adr new "$@"
                    ;;
                  "preview"|"p")
                    shift
                    npx log4brains preview "$@"
                    ;;
                  "build"|"b")
                    shift
                    npx log4brains build "$@"
                    ;;
                  "init"|"i")
                    shift
                    npx log4brains init "$@"
                    ;;
                  "help"|"h"|"--help"|"-h")
                    echo "ADR (Architecture Decision Records) Tool"
                    echo ""
                    echo "Usage: adr <command> [options]"
                    echo ""
                    echo "Commands:"
                    echo "  new, n        Create a new ADR"
                    echo "  preview, p    Preview ADRs in browser"
                    echo "  build, b      Build static documentation site"
                    echo "  init, i       Initialize log4brains project"
                    echo "  help, h       Show this help message"
                    echo ""
                    echo "Examples:"
                    echo "  adr new \"My Decision Title\""
                    echo "  adr preview"
                    echo "  adr build --out ./docs"
                    echo ""
                    echo "For more options, use: npx log4brains <command> --help"
                    ;;
                  "")
                    echo "Error: No command specified"
                    echo "Run 'adr help' for usage information"
                    exit 1
                    ;;
                  *)
                    echo "Error: Unknown command '$1'"
                    echo "Run 'adr help' for available commands"
                    exit 1
                    ;;
                esac
              '')

              # Essential utilities
              just
              jq
              tree

              # Plotting utilities
              gnuplot
              bc # Calculator for mathematical operations in shell scripts

              # Template rendering
              gomplate

              # Markdown rendering in terminal
              glow

              # Convenience alias for viewing usage documentation
              (writeShellScriptBin "usage" ''
                exec glow USAGE.md
              '')

              # Mermaid CLI for diagram generation
              mermaid-cli

              # JSON Schema validation (required by submission validation script)
              check-jsonschema

              # CAPE project management tool
              (writeShellScriptBin "cape" ''
                # Prefer repo-local cape.sh when available; fallback to store copy
                if [ -x "$PWD/scripts/cape.sh" ]; then
                  exec "$PWD/scripts/cape.sh" --project-root "$PWD" "$@"
                else
                  exec ${./scripts/cape.sh} --project-root "$PWD" "$@"
                fi
              '')

              # --- Additive Plinth / Cardano tooling below (new) ---
              pkg-config
              libsodium
              secp256k1
              libblst
              cabal-install
              haskell.compiler.ghc966
              fourmolu
              haskellPackages.cabal-fmt
              haskellPackages.hlint
              nixfmt-rfc-style
              uplcMusl
              plcMusl
              pirMusl
              plutusMusl
            ]);

          shellHook =
            (old.shellHook or "")
            + ''
              # Install log4brains via npx when needed
              # Create node_modules directory if it doesn't exist
              [ ! -d "node_modules" ] && mkdir -p node_modules

              # Update cabal indexes and build the measure executable
              cabal update
              cabal build exe:measure

              # Display banner using glow for better markdown rendering
              # Resolve repo root so this works when entering the shell from subdirectories
              if command -v git >/dev/null 2>&1; then
                REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
              else
                REPO_ROOT=""
              fi
              if [ -n "$REPO_ROOT" ] && [ -f "$REPO_ROOT/BANNER.md" ]; then
                glow -w0 "$REPO_ROOT/BANNER.md" || true
              else
                # Fallback to the flake's BANNER in the Nix store
                 glow -w0 "${./BANNER.md}" || true
              fi

              # Show environment info
              echo "💻 Development Environment: Full toolchain with devx pre-cached HLS"
              echo ""
            '';
        });
      in
      {
        devShells.default = baseShell;
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
    # Optimize for disk usage in CI environments
    keep-derivations = false;
    keep-outputs = false;
    # Prefer substitutes over building to save disk space
    substitute = true;
    builders-use-substitutes = true;
  };
}
