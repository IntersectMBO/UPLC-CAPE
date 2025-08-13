<!-- UPLC-CAPE Development Shell Banner -->
<!-- This file is read by flake.nix to display the shell welcome message -->
<!-- Edit this file to customize the development shell banner -->

# 🚀 UPLC-CAPE Development Shell

💡 Quick Start:

- `usage`: View command cheat sheet
- `cape benchmark list`: List available benchmarks
- `cape submission new`: Create new submission

🛠️ CAPE Project Management Tool:

- `cape benchmark`: List all scenarios
- `cape benchmark <scenario>`: Show scenario details
- `cape submission new <scenario> <language> <version> <handle>`  
  Example: `cape submission new fibonacci Aiken 1.0.8 Unisay`
- `cape submission validate`: Validate submission files against JSON schemas
- `cape submission measure`: Measure UPLC program performance and generate metrics.json
- `cape submission aggregate`: Generate CSV report of benchmark submissions
- `cape submission report <benchmark>`: Generate HTML performance reports with charts
- `cape submission report --all`: Generate HTML reports for all benchmarks

🎨 Documentation & Diagram Tools:

- `glow README`.md: View main documentation
- `usage`: View command cheat sheet (alias for 'glow USAGE.md')
- `adr help`: Architecture Decision Records tool
- `mmdc`: Mermaid CLI for generating diagrams  
  Example: `mmdc -i diagram.mmd -o diagram.png`

🎨 Formatting Tools:

- `treefmt`: Format all files in the project
- `treefmt <file>`: Format specific files
- `shfmt`: Format shell scripts (called by treefmt)
- `prettier`: Format YAML, Markdown, JSON (called by treefmt)
- `fourmolu`: Format Haskell code (called by treefmt)
- `cabal-fmt`: Format Cabal files (called by treefmt)
- `nixfmt`: Format Nix files (called by treefmt)

🔧 Plinth (PlutusTx) Development Environment:

Available tools for Cardano smart contract development:

- GHC compiler (`ghc`) for Haskell/PlutusTx compilation
- Cabal build system with CHaP (Cardano Haskell Packages) integration
- Haskell Language Server for IDE support
- Access to Plutus Core libraries and PlutusTx compiler
- Plutus Core executables: `plutus`, `uplc`, `plc`, `pir` for working with Plutus programs

📝 Getting Started with Plinth:

1. Navigate to the `plinth` directory
2. Use `cabal build` to build the example Fibonacci benchmark
3. Use `cabal run fibonacci-benchmark` to execute
4. Modify `src/Fibonacci.hs` to implement your own benchmarks
5. Follow the patterns in the example for PlutusTx development

💡 The nix shell provides all necessary dependencies for Plinth development, including proper CHaP integration for accessing Cardano ecosystem packages.
