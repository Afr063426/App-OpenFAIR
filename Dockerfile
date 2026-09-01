# =============================================================================
# Dockerfile — Evaluador Cuantitativo de Riesgos (OpenFAIR) con IA local
# opcional (Ollama). Multi-etapa: Node compila Tailwind; R/Shiny ejecuta la app.
#
# Construir:      docker build -t tfm-evaluator .
# Ejecutar (con pregunta interactiva de IA):
#                 docker run -it --rm -p 3838:3838 tfm-evaluator
#
# Capas del build (resumen):
#   1. frontend   -> compila el CSS de Tailwind con Node.js
#   2. runtime    -> Shiny Server + paquetes R + Ollama + la app
# =============================================================================

# -----------------------------------------------------------------------------
# ETAPA 1 — Frontend: compilar Tailwind CSS con Node.js
# -----------------------------------------------------------------------------
# Imagen base mínima de Node 20 sobre Alpine Linux (ligera y reproducible).
# El alias "AS frontend" permite copiar artefactos de esta etapa a la final.
FROM node:20-alpine AS frontend

# Directorio de trabajo dentro de la imagen donde se ejecutan los comandos
# siguientes de esta etapa.
WORKDIR /build/frontend

# COPY <origen> <destino> (solo los manifiestos primero): copia package.json
# al contenedor. Separar los manifiestos del resto del código aprovecha la
# caché de capas de Docker: si package.json no cambia, "npm install" no se
# vuelve a ejecutar en builds posteriores.
COPY frontend/package.json ./

# RUN: ejecuta el instalador de dependencias de Node sin auditoría ni avisos
# de financiación (más rápido en CI y builds locales).
RUN npm install --no-audit --no-fund

# COPY de todo el código fuente del frontend (src/input.css, tailwind.config.js).
# Esta capa se invalida con cualquier cambio en frontend/, pero al estar después
# de npm install, las dependencias ya instaladas se reutilizan de la caché.
COPY frontend/ ./

# RUN: compila Tailwind (preflight desactivado en tailwind.config.js porque
# bslib/Bootstrap ya aporta reset CSS) y escribe el CSS final minificado en
# /build/app/www/css/app.css, listo para ser copiado a la imagen de R.
RUN mkdir -p /build/app/www/css \
    && npx tailwindcss -i ./src/input.css -o /build/app/www/css/app.css --minify

# -----------------------------------------------------------------------------
# ETAPA 2 — Backend: Shiny Server + paquetes R + Ollama + app
# -----------------------------------------------------------------------------
# Imagen base oficial de Shiny Server (rocker/shiny) fijada a R 4.3.1.
# Se fija --platform=linux/amd64 para que la imagen sea idéntica en cualquier
# arquitectura de build (evita sorpresas en Apple Silicon / CI).
FROM --platform=linux/amd64 rocker/shiny:4.3.1

# RUN 1 — Librerías del sistema (paquetes de desarrollo y utilidades):
#   * libcurl/ssl/xml: dependencias de compilación de paquetes R (httr,
#     httr2, xml2) y de conexiones HTTPS.
#   * fontconfig/freetype/png/tiff/jpeg: necesarias para renderizar gráficos
#     y fuentes en R (ggplot2/plotly).
#   * curl: cliente HTTP usado por entrypoint.sh para esperar a Ollama.
#   * procps: provee "pkill"/"ps", usados al pre-cargar el modelo Ollama.
#   * git: requerido por remotes::install_local y por si se instalan
#     dependencias desde GitHub.
# Se limpia la caché de apt (/var/lib/apt/lists/*) en la MISMA capa RUN para
# reducir el tamaño final de la imagen.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    curl \
    procps \
    git \
    && rm -rf /var/lib/apt/lists/*

# RUN 2 — Paquetes de R desde CRAN (repositorio oficial cloud.r-project.org).
#   * shiny/bslib/DT/plotly/ggplot2: stack de la interfaz y los gráficos.
#   * readxl/openxlsx: lectura/escritura del survey.xlsx.
#   * fitdistrplus/pscl: ajuste de distribuciones (TEF/LM).
#   * dplyr/purrr/tidyr/scales: manipulación de datos.
#   * readr/vroom: lectura/escritura de CSV (IMPORTANTE: readr 2.x necesita
#     vroom para cargar; sin vroom la app revienta al conectar el navegador).
#   * httr/httr2/jsonlite: cliente de la API local de Ollama.
#   * future/promises: chat asíncrono (la sesión no se bloquea).
#   * remotes: instalación del paquete local 'evaluator'.
# Un único RUN agrupa todas las instalaciones para minimizar el número de
# capas y aprovechar la caché entre builds.
RUN R -e "install.packages(c( \
    'shiny', \
    'bslib', \
    'readxl', \
    'openxlsx', \
    'fitdistrplus', \
    'dplyr', \
    'purrr', \
    'tidyr', \
    'scales', \
    'ggplot2', \
    'plotly', \
    'DT', \
    'bsicons', \
    'pscl', \
    'readr', \
    'vroom', \
    'httr', \
    'httr2', \
    'jsonlite', \
    'future', \
    'promises', \
    'remotes' \
), repos='https://cloud.r-project.org/')"

# RUN 2b — Pre-cargar el tema Bootswatch 'darkly' para funcionar sin internet
# en runtime. bslib descarga los temas de Bootswatch bajo demanda; si la red no
# está disponible al iniciar Shiny, la app fallaría. Se pre-compila el tema
# durante el build y se guarda en /opt/bslib_cache (ENV BSLIB_CACHE_DIR), que
# bslib consulta antes de intentar descargas. El directorio se asigna a
# 'shiny' porque el servidor Shiny corre con ese usuario.
ENV BSLIB_CACHE_DIR=/opt/bslib_cache
RUN mkdir -p /opt/bslib_cache && chown shiny:shiny /opt/bslib_cache \
    && R -e "sass::sass(bslib::bs_theme(version = 5, preset = 'darkly'))"

# RUN 3 — Instalar el paquete local 'evaluator' (fork de OpenFAIR que incluye
# el soporte de plantillas con 0 escenarios). COPY evaluator a /tmp/evaluator
# y remotes::install_local lo compila e instala en la librería del sistema.
COPY evaluator /tmp/evaluator
RUN R -e "remotes::install_local('/tmp/evaluator', force = TRUE)"

# RUN 4 — Instalar Ollama (binario en /usr/local/bin; sin systemd dentro del
# contenedor, el servidor se lanza manualmente desde entrypoint.sh).
RUN curl -fsSL https://ollama.com/install.sh | sh

# RUN 4b — PRE-CARGAR el modelo llama3.2:1b en la imagen para que los
# contenedores NO descarguen ~1.3 GB en cada arranque. Ollama no corre como
# root, así que se lanza 'ollama serve' como el usuario 'shiny' (su ~/.ollama
# queda dentro de la imagen con el modelo). La descarga es opcional: si falla
# (sin red durante el build), se muestra un aviso y el entrypoint la reintenta
# en el primer arranque.
RUN su -s /bin/bash shiny -c 'nohup ollama serve >/tmp/ollama.log 2>&1 &' \
    && sleep 5 \
    && (su -s /bin/bash shiny -c 'ollama pull llama3.2:1b' || echo "AVISO: no se pudo pre-cargar el modelo en el build") \
    ; pkill -f "ollama serve" || true

# RUN 5 — Copiar la aplicación R (global.R, ui.R, server.R, helpers, www...).
# Primero se limpia el contenido por defecto de /srv/shiny-server (páginas de
# ejemplo de rocker/shiny) para no exponer archivos del proveedor.
# COPY app copia TODO el directorio de la app, incluido evaluator_workspace/
# (con la plantilla vacía en español). El CSS compilado por la etapa frontend
# se copia encima para garantizar que la app use el build de Tailwind.
RUN rm -rf /srv/shiny-server/*
COPY app /srv/shiny-server/
COPY --from=frontend /build/app/www/css/app.css /srv/shiny-server/www/css/app.css

# RUN 6 — Script de entrada interactivo (pregunta si se activa Ollama):
# COPY entrypoint.sh al contenedor y chmod +x para marcarlo como ejecutable.
# El ENTRYPOINT del final lo invoca con la CMD como argumento.
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# RUN 7 — Permisos para el usuario interno 'shiny': todo el árbol de la app
# queda propiedad de shiny para que el servidor pueda leer/escribir el
# workspace (survey.xlsx, resultados) sin errores de permisos.
RUN chown -R shiny:shiny /srv/shiny-server

# RUN 8 — Puerto y arranque:
#   EXPOSE 3838: documenta el puerto de Shiny Server (no abre puertos por sí
#   solo; el mapeo real se hace con -p o ports: en docker-compose).
#   ENTRYPOINT: fija /entrypoint.sh como el proceso que recibe los argumentos
#   de la CMD. El script pregunta por la IA local, configura ENABLE_OLLAMA y
#   hace `exec "$@"` para ejecutar la CMD reemplazando su propio proceso.
#   CMD: comando por defecto = lanzar Shiny Server (puede sobrescribirse con
#   `docker run ... shiny-server --debug`).
EXPOSE 3838
ENTRYPOINT ["/entrypoint.sh"]
CMD ["/usr/bin/shiny-server"]
