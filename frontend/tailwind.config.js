/** @type {import('tailwindcss').Config} */
module.exports = {
  // preflight desactivado: Shiny usa Bootstrap 5 (bslib) y no queremos que el
  // reset de Tailwind rompa los componentes Bootstrap.
  corePlugins: {
    preflight: false
  },
  content: [
    "../app/**/*.R",
    "../app/www/**/*.{html,js}"
  ],
  theme: {
    extend: {
      colors: {
        surface: {
          DEFAULT: "#1e293b",
          light: "#ffffff"
        },
        accent: {
          indigo: "#6366f1",
          cyan: "#0ea5e9",
          emerald: "#10b981",
          rose: "#f43f5e",
          amber: "#f59e0b"
        }
      },
      fontFamily: {
        sans: ["Inter", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"]
      }
    }
  },
  plugins: []
};
