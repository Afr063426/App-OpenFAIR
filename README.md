# Evaluador Cuantitativo de Riesgos — OpenFAIR

Dashboard Shiny para análisis cuantitativo de riesgo (ALE, curvas de excedencia,
efectividad de controles) basado en el paquete `evaluator` (OpenFAIR).
UI moderna: bslib + Tailwind CSS (dark mode elegante, tarjetas KPI con formato
compacto K/M/B).

## Estructura

```
app/            Aplicación Shiny (global.R + ui.R + server.R + helpers)
frontend/       Fuente Tailwind CSS (npm run build -> app/www/css/app.css)
evaluator/      Fork local del paquete evaluator (OpenFAIR)
Dockerfile      Compilación multi-etapa (Node -> Tailwind, R -> Shiny Server)
docker-compose.yml
```

## Ejecución local (sin Docker)

Requisitos: R >= 4.3 y las librerías de `global.R`. Compila los estilos:

```bash
cd frontend && npm install && npm run build && cd ..
```

Lanza la app desde R:

```r
shiny::runApp("app")
```

## Ejecución con Docker

### Opción A — docker compose (recomendada)

```bash
docker compose up --build
# Abrir http://localhost:3838
```

### Opción B — docker build + run

```bash
docker build -t tfm-evaluator .
docker run --rm -p 3838:3838 -v "$(pwd)/app/evaluator_workspace:/srv/shiny-server/evaluator_workspace" tfm-evaluator
# Abrir http://localhost:3838
```

El build multi-etapa:
1. `node:20-alpine` instala dependencias npm y compila Tailwind CSS.
2. `rocker/shiny:4.3.1` instala paquetes R (`bslib`, `plotly`, `DT`, `bsicons`,
   `pscl`, fork local `evaluator`, etc.) y copia la app con el CSS compilado.

El workspace (`survey.xlsx`, resultados) se monta como volumen para persistir
entre reinicios.
