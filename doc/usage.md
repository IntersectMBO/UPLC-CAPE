# USAGE CHEAT SHEET 🚀

Quick reference for UPLC-CAPE commands and workflows.

> 💡 **Tip**: Type `usage` in the UPLC-CAPE shell to view this cheat sheet
> anytime!

## 📊 CAPE Commands

```bash
# BENCHMARKS
cape benchmark list                    # List all benchmarks
cape benchmark fibonacci               # Show benchmark details
cape benchmark new my-benchmark        # Create new benchmark

# SUBMISSIONS
cape submission list                   # List all submissions
cape submission list fibonacci         # Show benchmark submissions
cape submission new fibonacci Aiken 1.0.8 myhandle  # Create submission

# HELP
cape --help                           # Main help
cape benchmark --help                 # Command help
cape submission new --help            # Subcommand help
```

## 📝 Quick Workflow

```bash
# 1. List benchmarks → 2. Create submission → 3. Fill files → 4. Commit
cape benchmark list
cape submission new fibonacci MyCompiler 1.0.0 myhandle
# Edit: submissions/fibonacci/MyCompiler_1.0.0_myhandle/fibonacci.uplc
# Edit: submissions/fibonacci/MyCompiler_1.0.0_myhandle/metrics.json
# Edit: submissions/fibonacci/MyCompiler_1.0.0_myhandle/metadata.json
git add . && git commit -m "Add MyCompiler fibonacci submission"
```

## 🏛️ ADR Commands

```bash
# ARCHITECTURE DECISION RECORDS
adr new "My Decision Title"           # Create ADR
adr preview                           # View in browser
adr build                             # Build static site
adr help                              # Show help

# ALIASES
adr n "Quick ADR"                     # new
adr p                                 # preview
adr b                                 # build
adr h                                 # help
```

## 📁 Key Directories

```bash
scenarios/          # Benchmark definitions
scenarios/TEMPLATE/ # New benchmark template
submissions/        # Performance submissions
submissions/TEMPLATE/ # New submission template
doc/adr/           # Architecture decisions
scripts/           # CAPE CLI tools
```

## 📋 File Templates

```bash
# METRICS (submissions/*/metrics.json)
{"cpu_units": 185916, "memory_units": 592, "script_size_bytes": 1234, "term_size": 45}

# METADATA (submissions/*/metadata.json)
{
  "compiler": {"name": "Aiken", "version": "1.0.8", "commit_hash": "abc123"},
  "compilation_config": {"optimization_level": "O2", "target": "uplc"},
  "contributor": {"name": "myhandle"},
  "submission": {"date": "2025-07-18T10:00:00Z", "source_available": true}
}
```

## 🔧 Development

```bash
nix develop                           # Enter dev environment
direnv allow                          # Auto-enter (recommended)
cape benchmark list                   # Test CLI
glow README.md                        # View docs in terminal
glow USAGE.md                         # View this cheat sheet
```

## 🎨 Code Formatting

```bash
# FORMAT FILES
treefmt                              # Format entire project
treefmt file.sh                      # Format specific file
treefmt README.md                    # Format markdown
treefmt *.yml                        # Format YAML files

# INDIVIDUAL FORMATTERS (called by treefmt)
shfmt script.sh                      # Format shell script
prettier file.md --write            # Format markdown/YAML/JSON

# AUTOMATIC FORMATTING
# Pre-commit hook automatically runs treefmt on staged files
git commit                           # Formats files before commit
```

## ⚙️ Configuration Files

```bash
# EDITOR CONFIGURATION
.editorconfig                        # Editor settings (indentation, etc.)

# FORMATTING CONFIGURATION
treefmt.toml                         # treefmt main configuration
.prettierrc                          # Prettier-specific settings
.prettierignore                      # Files to exclude from Prettier
```

---

💡 **Tip**: All CAPE commands support interactive prompts if you omit arguments.
