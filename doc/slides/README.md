# UPLC-CAPE Workshop Presentation

Slidev presentation for the UPLC Conference workshop on UPLC-CAPE (Cardano Application Performance Evaluation).

## Quick Start

### Prerequisites

- Nix
- Direnv (optional, for automatic environment loading)

### Setup

1. **Enter the development environment**:

   ```bash
   nix-shell
   ```

   Or with direnv:

   ```bash
   direnv allow
   ```

2. **Install dependencies**:

   ```bash
   pnpm install
   ```

3. **Start the development server**:

   ```bash
   pnpm dev
   ```

   The presentation will open automatically in your browser at <http://localhost:3030>

## Presenter Mode

To use presenter mode with speaker notes:

1. Start the dev server: `pnpm dev`
2. Press `P` to enter presenter mode
3. A new window will open with:
   - Current slide on your laptop screen
   - Next slide preview
   - Speaker notes
   - Timer

Configure your display settings:
- **MacBook Pro screen**: Presenter view (with notes)
- **External display/projector**: Slides view (audience)

## Export Options

### Export to PDF

```bash
pnpm export-pdf
```

Output: `slides-export.pdf`

### Export to PowerPoint

```bash
pnpm export-pptx
```

Output: `slides-export.pptx`

### Build Static Site

```bash
pnpm build
```

Output: `dist/` directory - deploy to any static hosting

## Theme & Styling

The presentation uses the extracted theme from the IO UPLC PowerPoint template:

- **Colors**: See `colors.json` for the complete color palette
- **Fonts**: Arial (headings and body)
- **UnoCSS Config**: `uno.config.ts` - all theme colors available as utility classes
- **Custom Styles**: `styles/theme.css` - CSS custom properties and Slidev overrides

### Using Theme Colors

In markdown:

```markdown
<div class="text-primary">Primary blue text</div>
<div class="bg-secondary text-light">Green background with white text</div>
```

In CSS:

```css
.custom-element {
  color: var(--color-primary);
  background: var(--color-secondary);
}
```

### Available Color Classes

- `text-primary`, `bg-primary` - #058DC7 (Bright Blue)
- `text-secondary`, `bg-secondary` - #50B432 (Green)
- `text-tertiary`, `bg-tertiary` - #ED561B (Orange)
- `text-accent`, `bg-accent` - #24CBE5 (Cyan)
- `text-warning`, `bg-warning` - #EDEF00 (Yellow)
- `text-success`, `bg-success` - #64E572 (Light Green)

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

- `Space` / `→` - Next slide
- `←` - Previous slide
- `P` - Presenter mode
- `O` - Slides overview
- `D` - Dark mode toggle
- `F` - Fullscreen
- `?` - Show all shortcuts

## Media Assets

All media files from the PowerPoint template are available in `public/`:
- 131 images (PNG/JPG)
- Reference in slides: `/image1.png`, `/image2.png`, etc.

## Troubleshooting

### Port already in use

If port 3030 is already in use:

```bash
pnpm dev -- --port 3031
```

### Fonts not loading

Fonts are web-safe (Arial) and should work everywhere. If you see rendering issues, check browser console for errors.

### Export failures

PDF/PPTX export requires Chromium. The Nix environment includes it, but if exports fail:

```bash
# Verify Chromium is available
which chromium
```

## Development

### Editing Slides

Edit `slides.md` - changes hot-reload automatically

### Custom Components

Create Vue components in `components/` directory - they're auto-imported

### Custom Layouts

Create layouts in `layouts/` directory - use with:

```markdown
---
layout: my-custom-layout
---
```

## Resources

- **Slidev Documentation**: <https://sli.dev>
- **UnoCSS Documentation**: <https://unocss.dev>
- **Theme Extraction Guide**: `../EXTRACTION-SUMMARY.md`
- **UPLC-CAPE Repository**: <https://github.com/IntersectMBO/UPLC-CAPE>
- **Live Results**: <https://intersectmbo.github.io/UPLC-CAPE>

## License

Content: Workshop presentation material
Theme: Extracted from IO UPLC Presentation Template 2025
