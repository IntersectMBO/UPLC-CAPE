{
  description = "UPLC-CAPE: Comparative Artifact Performance Evaluation for UPLC";

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

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
      url = "github:IntersectMBO/plutus/ba16ec68d3ba1a53594585deed81cdb3e720e4a3";
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

        # Platform detection
        isDarwin = pkgs.stdenv.hostPlatform.isDarwin;

        # UPLC CLI from Plutus repository (musl build) - only available on non-darwin
        uplcMusl = if isDarwin then null else plutus.packages.${system}.musl64-uplc;
        plcMusl = if isDarwin then null else plutus.packages.${system}.musl64-plc;
        pirMusl = if isDarwin then null else plutus.packages.${system}.musl64-pir;
        plutusMusl = if isDarwin then null else plutus.packages.${system}.musl64-plutus;

        # haskell.nix project for building the Cabal project
        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc966";

          # CHaP repository mapping
          inputMap = {
            "https://chap.intersectmbo.org/" = CHaP;
          };

          # Note: index-state is read from cabal.project automatically
          # Only specify here if you need to override it

          modules = [
            {
              packages.cape.package.buildable = true;
            }
          ];
        };

        # Extract components from the project
        capeLib = project.hsPkgs.cape.components.library;
        measureExe = project.hsPkgs.cape.components.exes.measure;
        plinthSubmissionsExe = project.hsPkgs.cape.components.exes.plinth-submissions;
        capeTests = project.hsPkgs.cape.components.tests.cape-tests;

        # Development shell using haskell.nix shellFor with UPLC-CAPE specific tools
        baseShell = project.shellFor {
          # Include all local packages in the shell
          packages = p: [
            p.cape
          ];

          # Haskell development tools (explicitly specified)
          tools = {
            cabal = "latest";
          };

          # Build tools and dependencies
          buildInputs = with pkgs; [
            # Haskell development tools
            fourmolu
            haskellPackages.cabal-fmt
            haskellPackages.hlint

            # Core development tools
            git

            # Formatting tools
            treefmt
            shfmt # Shell script formatter
            nodePackages.prettier # YAML, Markdown, and more
            nixfmt-rfc-style

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

            # Pretty-print UPLC files in place
            (writeShellScriptBin "pretty-uplc" ''
              set -e

              if [ $# -eq 0 ]; then
                echo "Usage: pretty-uplc <path-to-uplc-file>" >&2
                exit 1
              fi

              uplc_file="$1"

              if [ ! -f "$uplc_file" ]; then
                echo "Error: File not found: $uplc_file" >&2
                exit 1
              fi

              # Create temporary file
              temp_file=$(mktemp) || {
                echo "Error: Failed to create temporary file" >&2
                exit 1
              }

              # Ensure temp file cleanup on exit
              trap 'rm -f "$temp_file"' EXIT

              # Pretty-print to temp file, then move to original location
              if plutus "$uplc_file" -o "$temp_file"; then
                # Ensure file ends with a newline
                if [ -s "$temp_file" ] && [ "$(tail -c 1 "$temp_file" | wc -l)" -eq 0 ]; then
                  echo "" >> "$temp_file"
                fi
                mv "$temp_file" "$uplc_file"
                echo "✓ Pretty-printed: $uplc_file"
              else
                echo "Error: plutus command failed for $uplc_file" >&2
                exit 1
              fi
            '')

            # CAPE project management tool
            (writeShellScriptBin "cape" ''
              # Prefer repo-local cape.sh when available; fallback to store copy
              if [ -x "$PWD/scripts/cape.sh" ]; then
                exec "$PWD/scripts/cape.sh" --project-root "$PWD" "$@"
              else
                exec ${./scripts/cape.sh} --project-root "$PWD" "$@"
              fi
            '')

            # Cardano/Plutus dependencies
            pkg-config
            libsodium
            secp256k1
            libblst
          ];
          # Note: uplcMusl/plcMusl/pirMusl/plutusMusl not included in dev shell
          # as they require building Agda. Use nix build .#packages.* for those.

          # Note: Built executables (measure, plinth-submissions) are NOT added to buildInputs
          # to avoid triggering their build (which includes heavy deps like Agda).
          # Use 'cabal build' and 'cabal run' in the dev shell instead.

          shellHook = ''
            # Install log4brains via npx when needed
            # Create node_modules directory if it doesn't exist
            [ ! -d "node_modules" ] && mkdir -p node_modules

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

            # Synchronize Cabal package index with cabal.project index-state
            # Prevents build failures when index-state is updated (see issue #104)
            echo "📦 Synchronizing Cabal package index..."
            cabal update > /dev/null 2>&1 || true

            # Show environment info
            echo "💻 Development Environment: haskell.nix with binary caches"
            echo "📦 Build with: cabal build exe:measure exe:plinth-submissions"
            echo ""
          '';
        };
      in
      {
        # Expose packages for building
        packages = {
          measure = measureExe;
          plinth-submissions = plinthSubmissionsExe;
          default = measureExe;
        };

        # Expose checks for CI
        checks = {
          cape-tests = capeTests;
        };

        # Development shell
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
