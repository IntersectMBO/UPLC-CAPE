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
      url = "github:IntersectMBO/plutus";
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

        # Plinth project configuration
        plinthProject = pkgs.haskell-nix.cabalProject' {
          name = "uplc-cape-benchmarks";
          compiler-nix-name = "ghc966";
          src = ./plinth;
          inputMap = {
            "https://chap.intersectmbo.org/" = CHaP;
          };
          modules = [
            {
              packages = { };
            }
          ];
        };

        # Plinth development tools
        plinthTools = {
          cabal = plinthProject.tool "cabal" "latest";
          haskell-language-server = plinthProject.tool "haskell-language-server" "latest";
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Core development tools
            git

            # System dependencies for Cardano/Plutus
            pkg-config
            pkgs.libsodium
            pkgs.secp256k1
            pkgs.libblst

            # Plinth/PlutusTx tooling
            plinthTools.cabal
            plinthTools.haskell-language-server

            # GHC compiler (matching the project's GHC version)
            haskell.compiler.ghc966

            # Formatting tools
            treefmt
            shfmt # Shell script formatter
            nodePackages.prettier # YAML, Markdown, and more
            fourmolu # Haskell formatter
            haskellPackages.cabal-fmt # Cabal file formatter
            nixfmt-rfc-style # Nix file formatter

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
                  npx log4brains preview
                  ;;
                "build"|"b")
                  npx log4brains build
                  ;;
                "help"|"h"|*)
                  echo "ADR (Architecture Decision Records) tool"
                  echo ""
                  echo "Usage: adr <command> [args...]"
                  echo ""
                  echo "Commands:"
                  echo "  new, n <title>    Create new ADR"
                  echo "  preview, p        Preview ADRs in browser"
                  echo "  build, b          Build static site"
                  echo "  help, h           Show this help"
                  echo ""
                  echo "Single letter aliases available: adr n, adr p, adr b, adr h"
                  ;;
              esac
            '')

            # CAPE CLI wrapper that ensures the script can find project files
            (writeShellScriptBin "cape" ''
              # This ensures the user can work on their actual project files, not read-only Nix store
              exec ${./scripts/cape.sh} --project-root "$PWD" "$@"
            '')

            # Mermaid CLI for diagram generation
            mermaid-cli

            # JSON Schema validation (required by submission validation script)
            check-jsonschema
          ];

          shellHook = ''
            # Display banner using glow for better markdown rendering
            glow BANNER.md

            # Install log4brains via npx when needed
            # Create node_modules directory if it doesn't exist
            [ ! -d "node_modules" ] && mkdir -p node_modules
          '';
        };
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
