# ------------------------------------------------------------------------------
# Imagen base: Rocker Shiny sobre Ubuntu/Debian 
# Usamos --platform=linux/amd64 para garantizar compatibilidad universal
# ------------------------------------------------------------------------------
FROM --platform=linux/amd64 rocker/shiny:4.3.1

# 1. Instalar librerías del Sistema Operativo requeridas por paquetes de R
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# 2. Instalar paquetes de R desde CRAN
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
    'remotes' \
), repos='https://cloud.r-project.org/')"

# 2b. Pre-cargar el tema bootswatch 'darkly' para que la app funcione sin internet en runtime
ENV BSLIB_CACHE_DIR=/opt/bslib_cache
RUN mkdir -p /opt/bslib_cache && chown shiny:shiny /opt/bslib_cache \
    && R -e "sass::sass(bslib::bs_theme(version = 5, preset = 'darkly'))"

# 3. Instalar 'evaluator' desde GitHub
#RUN R -e "remotes::install_github('davidski/evaluator')"

# 3. Instalar 'evaluator' local desde tu repositorio
COPY evaluator /tmp/evaluator
RUN R -e "remotes::install_local('/tmp/evaluator', force = TRUE)"

# 4. Limpiar carpeta por defecto del servidor Shiny y copiar el código de la app
RUN rm -rf /srv/shiny-server/*
COPY app /srv/shiny-server/

# 5. Ajustar permisos para el usuario interno 'shiny'
RUN chown -R shiny:shiny /srv/shiny-server

# 6. Exponer puerto predeterminado de Shiny
EXPOSE 3838

# 7. Comando para iniciar Shiny Server
CMD ["/usr/bin/shiny-server"]
