# =============================================================================
# global.R — Carga de librerías, fuentes y helpers compartidos.
# Se ejecuta automáticamente antes de ui.R / server.R.
# =============================================================================

# Límite de subida de archivos: Shiny permite 5 MB por defecto; para cargar
# históricos de TEF/LM en Excel se sube a 100 MB.
options(shiny.maxRequestSize = 100 * 1024^2)

# -----------------------------------------------------------------------------
# Directorio de la app, resuelto SIN depender del directorio de trabajo:
# se busca el directorio que contiene global.R + ui.R + server.R subiendo desde
# getwd() (y comprobando también ./app en cada nivel). Así la app funciona
# tanto si se lanza desde app/ como desde la raíz del proyecto, y los source()
# relativos siguientes nunca fallan por el CWD.
# -----------------------------------------------------------------------------
app_dir <- {
  d <- normalizePath(getwd(), mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "global.R")) &&
        file.exists(file.path(d, "ui.R")) &&
        file.exists(file.path(d, "server.R"))) {
      break
    }
    if (file.exists(file.path(d, "app", "global.R"))) {
      d <- file.path(d, "app")
      break
    }
    p <- dirname(d)
    if (identical(p, d)) break
    d <- p
  }
  d
}

# Arranque seguro: captura los errores de inicialización en un log de texto
# (shiny_startup_error.txt) para diagnosticar fallos al levantar la app.
startup_log <- function(msg) {
  path <- file.path(getwd(), "shiny_startup_error.txt")
  tryCatch(writeLines(msg, con = path),
           error = function(e) try(writeLines(msg, con = tempfile("shiny_startup_error_"))))
}

tryCatch({
  library(shiny)
  library(bslib)
  library(fitdistrplus)
  library(evaluator)
  library(readxl)
  library(plotly)
  library(ggplot2)
  library(openxlsx)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(scales)
  library(DT)
  source(file.path(app_dir, "fit_dist.R"))
  source(file.path(app_dir, "evaluator_helpers.R"))
  source(file.path(app_dir, "capabilities_module.R"))
}, error = function(e) {
  startup_log(c(sprintf("Startup error at %s", Sys.time()), conditionMessage(e)))
  stop(e)
})

# -----------------------------------------------------------------------------
# Helpers de lógica de negocio (no visuales)
# -----------------------------------------------------------------------------

# Construye el valor que se guarda en survey.xlsx para una dimensión
# (TEF, TC, LM) según su modo de entrada:
#   - Cualitativo  -> se guarda la etiqueta (ej. "Frequent")
#   - PERT         -> "dist:pert|params:min=X,mode=Y,max=Z"
#   - Distribución -> "dist:<nombre>|params:<clave=valor,...>"
build_dimension_value <- function(mode, qual = NULL, dist = NULL, params = NULL,
                                  pert_min = NULL, pert_mode = NULL, pert_max = NULL) {
  if (identical(mode, "Qualitative")) {
    return(qual)
  }
  if (identical(mode, "PERT")) {
    if (is.null(pert_min) || is.null(pert_mode) || is.null(pert_max)) {
      stop("PERT requiere los valores Min, Mode y Max.", call. = FALSE)
    }
    return(sprintf("dist:pert|params:min=%s,mode=%s,max=%s", pert_min, pert_mode, pert_max))
  }
  # Distribution or Fit from file
  return(paste0("dist:", dist, "|params:", params))
}

# Helper de iconos: usa bsicons si está instalado; si no, degrada a NULL
# (el diseño no se rompe, simplemente no aparece el icono).
vb_icon <- function(name) {
  if (requireNamespace("bsicons", quietly = TRUE)) {
    bsicons::bs_icon(name)
  } else {
    NULL
  }
}

# Formato compacto de moneda: $10.5M en lugar de $10,528,977 (evita overflow en KPIs)
fmt_compact_money <- function(x) {
  if (exists("cut_short_scale", where = asNamespace("scales"), inherits = FALSE)) {
    scales::number(x, accuracy = 0.1, scale_cut = scales::cut_short_scale(), prefix = "$")
  } else {
    scales::dollar(x, accuracy = 1)
  }
}

# Convierte una cadena "clave=valor,clave=valor" en una lista nombrada
# (ej. "shape=2,rate=0.5" -> list(shape=2, rate=0.5)).
parse_params_string <- function(s) {
  if (is.null(s) || length(s) == 0 || is.na(s) || !nzchar(trimws(s))) return(list())
  parts <- trimws(unlist(strsplit(s, ",")))
  out <- list()
  for (p in parts) {
    if (nzchar(p)) {
      kv <- strsplit(p, "=")[[1]]
      if (length(kv) == 2) {
        val <- suppressWarnings(as.numeric(trimws(kv[2])))
        out[[trimws(kv[1])]] <- if (is.na(val)) trimws(kv[2]) else val
      }
    }
  }
  out
}

# Genera n muestras de la distribución indicada por su código de la app
# (los códigos coinciden con evaluator: pois, nbinom, gamma, pert, ...).
# Si `params` trae `max` y la distribución no lo usa como cota nativa
# (pert/unif sí), se aplica como límite máximo (pmin) para que la vista
# previa coincida con la simulación.
dist_sample <- function(dist, params, n = 100000) {
  cap <- params[["max"]]
  if (!is.null(cap) && !is.na(cap) && !(dist %in% c("pert", "unif"))) {
    params <- params[names(params) != "max"]
  } else {
    cap <- NULL
  }
  args <- c(list(n = n), params)
  s <- switch(dist,
    pert     = do.call(mc2d::rpert, args),
    lnorm    = do.call(stats::rlnorm, args),
    gamma    = do.call(stats::rgamma, args),
    weibull  = do.call(stats::rweibull, args),
    exp      = do.call(stats::rexp, args),
    norm     = do.call(stats::rnorm, args),
    beta     = do.call(stats::rbeta, args),
    unif     = do.call(stats::runif, args),
    pois     = do.call(stats::rpois, args),
    nbinom   = do.call(stats::rnbinom, args),
    geom     = do.call(stats::rgeom, args),
    zipois   = do.call(get("rzipois", asNamespace("evaluator")), args),
    zinegbin = do.call(get("rzinegbin", asNamespace("evaluator")), args),
    NULL
  )
  if (!is.null(cap) && !is.na(cap) && !is.null(s) && length(s) > 0) {
    s <- pmin(s, cap)
  }
  s
}

# Gráfico de densidad suavizado (ggplot2 -> plotly) con el tema del modo
# claro/oscuro actual. Se usa en "Configuración" para previsualizar TEF/LM.
dist_density_plot <- function(dist, params, style) {
  s <- dist_sample(dist, params)
  if (is.null(s)) return(NULL)
  s <- s[is.finite(s)]
  if (length(s) == 0) return(NULL)
  df <- data.frame(x = s)
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_density(fill = "#38bdf8", color = "#7dd3fc",
                          alpha = 0.35, adjust = 1.2, na.rm = TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      text = ggplot2::element_text(color = style$font),
      axis.text = ggplot2::element_text(color = style$font),
      axis.title = ggplot2::element_text(color = style$font),
      panel.grid.major = ggplot2::element_line(color = style$grid),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Valor", y = "Densidad",
                  title = paste("Distribución", dist))
  plotly::ggplotly(gg)
}

# Estado vacío para los gráficos del dashboard: si aún no hay escenarios
# cargados, se muestra este mensaje en lugar de un lienzo en blanco.
no_scenarios_plot <- function(style) {
  empty_plotly("Esperando ingreso de escenarios...", style)
}

# Tabla de estado vacío (mismo mensaje para los módulos de tablas)
no_scenarios_table <- function() {
  DT::datatable(
    data.frame(Estado = "Esperando ingreso de escenarios..."),
    rownames = FALSE,
    options = list(dom = "t", pageLength = 1, ordering = FALSE, searching = FALSE)
  )
}

# Empty plotly with a centered message (used when no numeric params are set)
empty_plotly <- function(message, style) {
  plotly::plot_ly(x = 0, y = 0, type = "scatter", mode = "markers",
                  marker = list(opacity = 0), hoverinfo = "skip",
                  showlegend = FALSE) |>
    plotly::add_annotations(text = message, showarrow = FALSE,
                            font = list(color = style$font, size = 13)) |>
    plotly::layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) |>
    dash_layout(style)
}

# -----------------------------------------------------------------------------
# Tema y estilo
# -----------------------------------------------------------------------------

# Tema de la app: el preset "darkly" de Bootswatch requiere una descarga
# puntual; si no está disponible (p. ej. sin internet), se cae a un tema
# oscuro autocontenido para que la app siempre arranque.
app_theme <- tryCatch(
  bs_theme(version = 5, preset = "darkly"),
  error = function(e) {
    bs_theme(version = 5,
             bg = "#222222", fg = "#eeeeee",
             primary = "#6366f1", secondary = "#0ea5e9")
  }
)

# Common plotly layout tuned to the current dark/light mode.
# legend_pos: "top" coloca la leyenda horizontal arriba (barras agrupadas),
#             "bottom" debajo (donut/pie) para evitar solapamientos.
# margin_l: margen izquierdo amplio para nombres largos en el eje Y.
dash_layout <- function(p, style, legend_pos = "bottom", margin_l = 60) {
  legend <- if (identical(legend_pos, "top")) {
    list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12,
         font = list(size = 11))
  } else {
    # Leyenda inferior bien separada de la dona (y = -0.2) para no taparla
    list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2,
         font = list(size = 11))
  }
  out <- plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    autosize = TRUE,
    font = list(color = style$font),
    xaxis = list(gridcolor = style$grid, zerolinecolor = style$grid,
                 automargin = TRUE),
    yaxis = list(gridcolor = style$grid, zerolinecolor = style$grid,
                 automargin = TRUE),
    margin = list(l = margin_l, r = 16,
                  t = if (identical(legend_pos, "top")) 60 else 30,
                  b = if (identical(legend_pos, "top")) 24 else 52),
    showlegend = TRUE,
    legend = legend
  )
  plotly::config(out, responsive = TRUE, displaylogo = FALSE)
}

# Paleta de colores para tarjetas KPI (estilo DeepSeek)
kpi_colors <- list(
  indigo  = list(accent = "#3b82f6", bg = "rgba(59,130,246,0.12)",
                 grad = "linear-gradient(90deg,#3b82f6,#60a5fa)"),
  rose    = list(accent = "#f97316", bg = "rgba(249,115,22,0.12)",
                 grad = "linear-gradient(90deg,#f97316,#fb923c)"),
  amber   = list(accent = "#f59e0b", bg = "rgba(245,158,11,0.12)",
                 grad = "linear-gradient(90deg,#f59e0b,#fbbf24)"),
  cyan    = list(accent = "#3b82f6", bg = "rgba(59,130,246,0.12)",
                 grad = "linear-gradient(90deg,#3b82f6,#60a5fa)"),
  emerald = list(accent = "#22c55e", bg = "rgba(34,197,94,0.12)",
                 grad = "linear-gradient(90deg,#22c55e,#4ade80)")
)

# -----------------------------------------------------------------------------
# Diagnóstico de arranque (consola / docker logs): muestra qué workspace usa la
# app y cuántos escenarios lee del survey.xlsx. Si el conteo es NA, el archivo
# es ilegible (placeholder de OneDrive) y el análisis fallará hasta resolverlo.
# -----------------------------------------------------------------------------
invisible({
  ws <- evaluator_workspace()
  n_scen <- count_survey_scenarios(file.path(ws$inputs_dir, "survey.xlsx"))
  message(sprintf("[App] Workspace: %s", ws$base_dir))
  message(sprintf("[App] Escenarios en survey.xlsx: %s",
                  if (is.na(n_scen)) "ILEGIBLE (revisar OneDrive)" else n_scen))
})

# Tarjeta KPI reutilizable (título, output de texto, icono, color).
# Layout 100% flexbox (sin position:absolute) para evitar solapamientos:
#   - kpi-accent: barra de 4px en flujo normal (primera fila)
#   - kpi-head: label + icono en fila (flex, space-between)
#   - kpi-value: número compacto con ellipsis, sin overflow vertical
kpi_card <- function(title, output_id, icon, color = "indigo") {
  cc <- kpi_colors[[color]]
  if (is.null(cc)) cc <- kpi_colors$indigo
  div(class = "kpi-card",
      div(class = "kpi-accent", style = sprintf("background:%s;", cc$grad)),
      div(class = "kpi-content",
          div(class = "kpi-head",
              div(class = "kpi-label", title),
              div(class = "kpi-icon",
                  style = sprintf("color:%s; background:%s;", cc$accent, cc$bg),
                  vb_icon(icon))
          ),
          div(class = "kpi-value", textOutput(output_id, inline = TRUE))
      )
  )
}
