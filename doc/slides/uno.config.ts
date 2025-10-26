// UnoCSS Configuration for IO UPLC Theme
// Generated from PowerPoint template: IO_UPLC Presentation Template_2025.pptx

import { defineConfig, presetAttributify, presetUno } from 'unocss'

export default defineConfig({
  presets: [
    presetUno(),
    presetAttributify(),
  ],
  theme: {
    colors: {
      // Primary theme colors from PowerPoint
      'primary': '#058DC7',      // Accent 1 - Bright Blue
      'secondary': '#50B432',    // Accent 2 - Green
      'tertiary': '#ED561B',     // Accent 3 - Orange
      'accent': '#24CBE5',       // Accent 5 - Cyan
      'warning': '#EDEF00',      // Accent 4 - Yellow
      'success': '#64E572',      // Accent 6 - Light Green

      // Dark/Light colors
      'dark': '#000000',         // Dark 1
      'dark-alt': '#158158',     // Dark 2 - Teal/Green
      'light': '#FFFFFF',        // Light 1
      'light-alt': '#F3F3F3',    // Light 2 - Light Gray

      // Links
      'link': '#2200CC',         // Hyperlink - Blue-Purple
      'link-visited': '#551A8B', // Followed Hyperlink - Purple

      // Semantic colors (mapped from theme)
      'text': '#000000',
      'background': '#FFFFFF',
      'muted': '#F3F3F3',
      'border': '#158158',
    },
    fontFamily: {
      sans: 'Arial, "Helvetica Neue", Helvetica, sans-serif',
      serif: 'Georgia, "Times New Roman", serif',
      mono: '"Fira Code", Consolas, Monaco, monospace',
    },
  },
})
