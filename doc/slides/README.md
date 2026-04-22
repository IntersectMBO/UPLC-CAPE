# UPLC-CAPE Workshop Presentation

Slidev presentation for the UPLC Conference workshop on UPLC-CAPE (Cardano Application Performance Evaluation).

The source-of-truth is `slides.md`. No checked-in `package.json` / lockfile —
Slidev is invoked directly via `npx` against the latest release so the
presentation can always be rebuilt on demand without carrying a pinned
dependency graph in the repo.

## Quick Start

Requires Node.js 20+ and `npx` (bundled with npm).

```bash
# Preview locally at http://localhost:3030
npx -y @slidev/cli@latest slides.md --open

# Build a static site into ./dist
npx -y @slidev/cli@latest build slides.md

# Export to PDF / PPTX (requires Chromium)
npx -y @slidev/cli@latest export slides.md
npx -y @slidev/cli@latest export slides.md --format pptx
```

## Presenter Mode

1. Run the dev command above.
2. Press `P` to open presenter view with speaker notes, next-slide preview and a timer.
3. Put the presenter window on your laptop and the main slide view on the external display.

## Theme & Styling

- `colors.json` — full color palette (extracted from IO UPLC template).
- `styles/` — CSS overrides and custom properties applied by Slidev.
- `public/` — static assets (backgrounds, logo) referenced from `slides.md`.

Color classes available in markdown:

- `text-primary`, `bg-primary` — #058DC7 (Bright Blue)
- `text-secondary`, `bg-secondary` — #50B432 (Green)
- `text-tertiary`, `bg-tertiary` — #ED561B (Orange)
- `text-accent`, `bg-accent` — #24CBE5 (Cyan)
- `text-warning`, `bg-warning` — #EDEF00 (Yellow)
- `text-success`, `bg-success` — #64E572 (Light Green)

## Presentation Structure

**Duration**: 60 minutes total
- Presentation: 30-35 minutes (Slides 1-15)
- Workshop: 25-30 minutes (Slide 16)

**Slides**:

1. The Multiplication Principle
2. Hard Limits with Real Consequences
3. Concrete Cost Impact
4. Real-World Impact—SundaeSwap
5. Who Benefits—Compiler Authors
6. Who Benefits—dApp Developers
7. Who Benefits—New Developers
8. The Benchmarking Challenge
9. Introducing UPLC-CAPE
10. CAPE Architecture: Core Entities
11. How CAPE Works
12. Key Metrics Explained
13. Available Benchmarks: Two Key Dimensions
14. Creating Submissions: Step-by-Step
15. Live Results & Community
16. Workshop: Hands-on Contribution

## Keyboard Shortcuts

- `Space` / `→` — Next slide
- `←` — Previous slide
- `P` — Presenter mode
- `O` — Slides overview
- `D` — Dark mode toggle
- `F` — Fullscreen
- `?` — Show all shortcuts

## Resources

- Slidev Documentation: <https://sli.dev>
- UPLC-CAPE Repository: <https://github.com/IntersectMBO/UPLC-CAPE>
- Live Results: <https://intersectmbo.github.io/UPLC-CAPE>

## License

Content: Workshop presentation material.
Theme: Extracted from IO UPLC Presentation Template 2025.
