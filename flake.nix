{
  description = "UPLC-CAPE: Comparative Artifact Performance Evaluation for UPLC";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Core development tools
            git
            
            # Documentation and ADR tools
            nodejs_20  # Required for log4brains
            
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
          ];

          shellHook = ''
            echo "🚀 UPLC-CAPE Development Environment"
            echo "📚 ADR (Architecture Decision Records) Tool:"
            echo "  - adr new \"Title\": Create new ADR"
            echo "  - adr preview: Preview ADRs in browser"
            echo "  - adr build: Build static documentation site"
            echo "  - adr help: Show detailed help"
            echo ""
            echo "📖 Documentation: https://github.com/thomvaill/log4brains"
            echo ""
            
            # Install log4brains via npx when needed
            # Create node_modules directory if it doesn't exist
            [ ! -d "node_modules" ] && mkdir -p node_modules
            
            # Check if log4brains is initialized
            if [ ! -f ".log4brains.yml" ]; then
              echo "💡 To initialize log4brains, run: adr init"
            else
              echo "✅ Log4brains is configured and ready to use"
            fi
          '';
        };
      });
}
