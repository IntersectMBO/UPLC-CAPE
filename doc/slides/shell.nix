{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs_20
    nodePackages.pnpm
    git
  ];

  shellHook = ''
    echo "🎨 Slidev presentation environment"
    echo "📦 Node.js: $(node --version)"
    echo "📦 pnpm: $(pnpm --version)"
    echo ""
    echo "Quick start:"
    echo "  pnpm install    # Install dependencies"
    echo "  pnpm dev        # Start dev server"
    echo "  pnpm build      # Build static site"
    echo "  pnpm export     # Export to PDF/PPTX"
  '';
}
