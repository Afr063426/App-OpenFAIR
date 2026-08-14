# Safe startup: capture initialization errors to a startup log
startup_log <- function(msg) {
  path <- file.path(getwd(), "shiny_startup_error.txt")
  tryCatch(writeLines(msg, con = path), error = function(e) try(writeLines(msg, con = tempfile("shiny_startup_error_"))))
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
  source("fit_dist.R")
  source("evaluator_helpers.R")
  source("capabilities_module.R")
}, error = function(e) {
  startup_log(c(sprintf("Startup error at %s", Sys.time()), conditionMessage(e)))
  stop(e)
})

# Build the survey.xlsx value for a dimension (TEF, TC, LM) based on its mode.
# Qualitative -> the label; PERT -> dist:pert|params:min,mode,max;
# Distribution / Fit from file -> dist:<name>|params:<key=value,...>
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

# Icon helper: uses bsicons when installed, otherwise degrades gracefully
# (keeps the app from failing to start on machines without bsicons).
vb_icon <- function(name) {
  if (requireNamespace("bsicons", quietly = TRUE)) {
    bsicons::bs_icon(name)
  } else {
    NULL
  }
}

# Common plotly layout tuned to the current dark/light mode
dash_layout <- function(p, style) {
  plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    font = list(color = style$font),
    xaxis = list(gridcolor = style$grid, zerolinecolor = style$grid),
    yaxis = list(gridcolor = style$grid, zerolinecolor = style$grid)
  )
}

# Parse "k=v,k=v" parameter strings into a named list (e.g. "shape=2,rate=0.5")
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

# Draw n samples from a distribution named by the app's dist codes
dist_sample <- function(dist, params, n = 100000) {
  args <- c(list(n = n), params)
  switch(dist,
    pert     = do.call(mc2d::rpert, args),
    lnorm    = do.call(stats::rlnorm, args),
    gamma    = do.call(stats::rgamma, args),
    weibull  = do.call(stats::rweibull, args),
    exp      = do.call(stats::rexp, args),
    norm     = do.call(stats::rnorm, args),
    pois     = do.call(stats::rpois, args),
    nbinom   = do.call(stats::rnbinom, args),
    geom     = do.call(stats::rgeom, args),
    zipois   = do.call(get("rzipois", asNamespace("evaluator")), args),
    zinegbin = do.call(get("rzinegbin", asNamespace("evaluator")), args),
    NULL
  )
}

# Smooth density plot (ggplot2 -> plotly) themed for the current dark/light mode
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

# Darkly preset needs a one-time bootswatch download; fall back to a
# self-contained dark theme if the download is unavailable (e.g. offline).
app_theme <- tryCatch(
  bs_theme(version = 5, preset = "darkly"),
  error = function(e) {
    bs_theme(version = 5,
             bg = "#222222", fg = "#eeeeee",
             primary = "#6366f1", secondary = "#0ea5e9")
  }
)

ui <- page_sidebar(
  title = "Evaluador Cuantitativo de Riesgos · OpenFAIR",
  theme = app_theme,
  sidebar = sidebar(
    width = 380,
    input_dark_mode(id = "dark_mode"),
    tags$style(HTML("
      [data-bs-theme='dark'] .dataTables_wrapper .dataTables_filter input,
      [data-bs-theme='dark'] .dataTables_wrapper select {
        background-color: #30353b !important; color: #dee2e6 !important; border-color: #495057 !important;
      }
      [data-bs-theme='dark'] table.dataTable { color: #dee2e6 !important; }
      [data-bs-theme='dark'] table.dataTable tbody tr { background-color: transparent !important; }
      [data-bs-theme='dark'] table.dataTable.stripe tbody tr.odd { background-color: rgba(255,255,255,0.04) !important; }
      [data-bs-theme='dark'] table.dataTable.hover tbody tr:hover { background-color: rgba(255,255,255,0.08) !important; }
      .dataTables_wrapper .dt-filter-row select { max-width: 150px; }
      .dataTables_wrapper .dt-filter-row { white-space: nowrap; }
      .dataTables_scrollHead { overflow: visible !important; }
    ")),
    accordion(
      open = "Parámetros del Escenario",
      accordion_panel(
        "Parámetros del Escenario",
        uiOutput("domain_ui"),
        textInput("scenario_description", "Descripción del escenario", value = ""),
        textInput("tcomm", "Threat Community", value = ""),
        selectInput("tef_mode", "TEF mode", choices = c("Qualitative", "Distribution", "Fit from file", "PERT"), selected = "Qualitative"),
        conditionalPanel(
          condition = "input.tef_mode == 'Qualitative'",
          selectInput("tef_cat", "TEF", choices = c("Frequent", "Occasional", "Rare"), selected = "Frequent")
        ),
        conditionalPanel(
          condition = "input.tef_mode == 'Distribution' || input.tef_mode == 'Fit from file'",
          tagList(
            selectInput("tef_dist", "TEF distribution", choices = c("pois", "nbinom", "zipois", "zinegbin"), selected = "pois"),
            textInput("tef_params", "TEF params (e.g. lambda=3 or size=2,mu=3)", value = "")
          )
        ),
        conditionalPanel(
          condition = "input.tef_mode == 'Fit from file'",
          fileInput("tef_hist_file", "Cargar histórico de conteos (TEF)", accept = c('.xlsx', '.xls', '.csv')),
          actionButton("btn_fit_tef", "Ajustar TEF desde histórico", class = "btn-outline-secondary w-100")
        ),
        conditionalPanel(
          condition = "input.tef_mode == 'PERT'",
          tagList(
            numericInput("tef_pert_min", "TEF PERT Min", value = 0),
            numericInput("tef_pert_mode", "TEF PERT Mode", value = 1),
            numericInput("tef_pert_max", "TEF PERT Max", value = 2)
          )
        ),
        selectInput("tc_mode", "TC mode", choices = c("Qualitative", "PERT"), selected = "Qualitative"),
        conditionalPanel(
          condition = "input.tc_mode == 'Qualitative'",
          selectInput("tc", "TC", choices = c("High", "Medium", "Low"), selected = "Medium")
        ),
        conditionalPanel(
          condition = "input.tc_mode == 'PERT'",
          tagList(
            numericInput("tc_pert_min", "TC PERT Min", value = 0),
            numericInput("tc_pert_mode", "TC PERT Mode", value = 0.5),
            numericInput("tc_pert_max", "TC PERT Max", value = 1)
          )
        ),
        selectInput("lm_mode", "LM mode", choices = c("Qualitative", "Distribution", "Fit from file", "PERT"), selected = "Qualitative"),
        conditionalPanel(
          condition = "input.lm_mode == 'Qualitative'",
          selectInput("lm_cat", "LM", choices = c("High", "Medium", "Low"), selected = "Medium")
        ),
        conditionalPanel(
          condition = "input.lm_mode == 'Distribution' || input.lm_mode == 'Fit from file'",
          tagList(
            selectInput("lm_dist", "LM distribution", choices = c("gamma", "lnorm", "weibull"), selected = "gamma"),
            textInput("lm_params", "LM params (e.g. shape=2,rate=0.5)", value = "")
          )
        ),
        conditionalPanel(
          condition = "input.lm_mode == 'Fit from file'",
          fileInput("lm_hist_file", "Cargar histórico de pérdidas (LM)", accept = c('.xlsx', '.xls', '.csv')),
          actionButton("btn_fit_lm", "Ajustar LM desde histórico", class = "btn-outline-secondary w-100")
        ),
        conditionalPanel(
          condition = "input.lm_mode == 'PERT'",
          tagList(
            numericInput("lm_pert_min", "LM PERT Min", value = 0),
            numericInput("lm_pert_mode", "LM PERT Mode", value = 1),
            numericInput("lm_pert_max", "LM PERT Max", value = 2)
          )
        ),
        textInput("scenario_id", "ScenarioID", value = "RS-001")
      ),
      accordion_panel(
        "Capabilities",
        capabilities_ui("capabilities_mod"),
        helpText("Define la efectividad (Beta-PERT) de cada control. Se guardan en %, se convierten a 0-1 para la simulación.")
      ),
      accordion_panel(
        "Acciones",
        numericInput("iterations", "Iteraciones de simulación", value = 1000, min = 100, step = 100),
        fileInput("survey_file", "Cargar encuesta (importar escenarios)", accept = c(".xlsx", ".xls", ".csv")),
        actionButton("btn_add_scenario", "Añadir escenario al survey.xlsx", class = "btn-success w-100"),
        actionButton("btn_run_analysis", "Ejecutar análisis", class = "btn-primary w-100"),
        downloadButton("dl_survey", "Descargar survey.xlsx", class = "w-100")
      )
    )
  ),
  navset_card_tab(
    nav_panel(
      "Dashboard",
      layout_columns(
        value_box(
          title = "ALE Total Mediana",
          value = textOutput("kpi_ale_median"),
          showcase = vb_icon("cash-coin"),
          theme = "primary"
        ),
        value_box(
          title = "ALE Máximo",
          value = textOutput("kpi_ale_max"),
          showcase = vb_icon("fire"),
          theme = "danger"
        ),
        value_box(
          title = "Escenario de Mayor Riesgo",
          value = textOutput("kpi_top_scenario"),
          showcase = vb_icon("exclamation-triangle-fill"),
          theme = "warning"
        ),
        value_box(
          title = "Threat Community Predominante",
          value = textOutput("kpi_top_tcomm"),
          showcase = vb_icon("people-fill"),
          theme = "info"
        )
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(
          full_screen = TRUE,
          card_header("Distribución de ALE por Threat Community"),
          plotlyOutput("tcomm_donut", height = "360px")
        ),
        card(
          full_screen = TRUE,
          card_header("ALE Mediana vs Máximo por Threat Community"),
          plotlyOutput("tcomm_bars", height = "360px")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Frecuencia de Pérdidas por Threat Community"),
        DTOutput("tcomm_table")
      )
    ),
    nav_panel(
      "Configuración",
      card_header("Ajustes de TEF / LM"),
      htmlOutput("tef_summary"),
      plotlyOutput("tef_plot", height = "280px"),
      htmlOutput("lm_summary"),
      plotlyOutput("lm_plot", height = "280px"),
      tags$hr(),
      verbatimTextOutput("analysis_message")
    ),
    nav_panel(
      "Resultados por Escenario",
      card_header("Resumen por Escenario"),
      verbatimTextOutput("analysis_message_scenarios"),
      downloadButton("dl_scenario_csv", "Descargar resumen escenarios (CSV)", class = "btn-outline-secondary mb-3"),
      DTOutput("scenario_summary_table"),
      tags$hr(),
      uiOutput("scenario_selector_ui"),
      plotlyOutput("scenario_scatter_plot", height = "400px")
    ),
    nav_panel(
      "Resultados por Dominio",
      card_header("Resumen por Dominio"),
      downloadButton("dl_domain_csv", "Descargar resumen dominios (CSV)", class = "btn-outline-secondary mb-3"),
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE, card_header("ALE Mediana por Dominio"), plotlyOutput("domain_ale_bar_plot", height = "400px")),
        card(full_screen = TRUE, card_header("Resumen por Dominio"), DTOutput("domain_summary_table"))
      )
    ),
    nav_panel(
      "Curva de Excedencia",
      card_header("Loss Exceedance Curve (LEC)"),
      uiOutput("exceedance_scenario_selector_ui"),
      plotlyOutput("exceedance_plot", height = "480px")
    ),
    nav_panel(
      "Efectividad de Controles",
      layout_columns(
        value_box(title = "ALE Inherente Total (sin controles)", value = textOutput("mit_inherent"), showcase = vb_icon("shield-slash"), theme = "danger"),
        value_box(title = "ALE Residual Total (con controles)", value = textOutput("mit_residual"), showcase = vb_icon("shield-check"), theme = "success"),
        value_box(title = "Reducción de Pérdida", value = textOutput("mit_reduction"), showcase = vb_icon("arrow-down-right-circle"), theme = "primary")
      ),
      actionButton("btn_run_mitigation", "Ejecutar análisis de mitigación", class = "btn-warning w-100 mb-3"),
      card(full_screen = TRUE, card_header("ALE Inherente vs Residual por Escenario (Top 15)"),
           plotlyOutput("mit_bars", height = "450px")),
      layout_columns(
        col_widths = c(5, 7),
        card(full_screen = TRUE, card_header("Ahorro Total por Capability"), plotlyOutput("mit_control_bars", height = "400px")),
        card(full_screen = TRUE, card_header("Detalle por Capability"), DTOutput("mit_control_table"))
      ),
      verbatimTextOutput("mitigation_message")
    )
  )
)

server <- function(input, output, session) {
  analysis_message <- reactiveVal(NULL)
  analysis_results <- reactiveVal(NULL)
  mitigation_results <- reactiveVal(NULL)

  # Colors that follow the dark/light toggle
  plot_style <- reactive({
    dark <- is.null(input$dark_mode) || identical(input$dark_mode, "dark")
    list(font = if (dark) "#dee2e6" else "#212529",
         grid = if (dark) "rgba(255,255,255,0.08)" else "rgba(0,0,0,0.12)")
  })

  output$domain_ui <- renderUI({
    selectInput("domain_id", "Dominio", choices = get_evaluator_domain_choices(), selected = "ISMP")
  })

  # Módulo de configuración de capabilities (controles) con parámetros Beta-PERT
  caps_module <- capabilities_server(
    "capabilities_mod",
    available_choices = reactive(get_evaluator_capabilities())
  )

  output$analysis_message <- renderText({
    if (is.null(analysis_message())) "" else analysis_message()
  })

  # TEF fit from uploaded historical counts
  observeEvent(input$btn_fit_tef, {
    tryCatch({
      req(input$tef_hist_file)
      path <- input$tef_hist_file$datapath
      ext <- tools::file_ext(input$tef_hist_file$name)

      if (tolower(ext) %in% c('xlsx','xls')) {
        df <- readxl::read_excel(path, col_names = FALSE)
      } else {
        df <- read.csv(path, stringsAsFactors = FALSE, header = FALSE)
      }

      numcols_idx <- which(vapply(df, function(col) {
        num_vals <- suppressWarnings(as.numeric(as.character(col)))
        sum(!is.na(num_vals)) / length(col) > 0.5
      }, logical(1)))

      if (length(numcols_idx) == 0) {
        stop('No numeric columns found. Asegúrate de que el archivo contenga números.')
      }

      col_data <- df[[numcols_idx[1]]]
      x <- suppressWarnings(as.numeric(as.character(col_data)))
      x <- x[!is.na(x)]

      if (length(x) < 2) stop('Necesitas al menos 2 valores numéricos.')

      x <- round(x)
      res <- fit_dist(x, type = 'count')
      params <- paste(names(res$best_fit$estimate), round(res$best_fit$estimate, 3), sep = '=', collapse = ', ')
      updateTextInput(session, 'tef_params', value = params)
      updateSelectInput(session, 'tef_dist', selected = res$best_dist)
      analysis_message(sprintf('Ajuste TEF completado: %s con %d observaciones', res$best_dist, length(x)))
    }, error = function(e) {
      analysis_message(paste('Error al ajustar TEF desde histórico:', e$message))
    })
  })

  # LM fit from historical losses
  observeEvent(input$btn_fit_lm, {
    tryCatch({
      req(input$lm_hist_file)
      path <- input$lm_hist_file$datapath
      ext <- tools::file_ext(input$lm_hist_file$name)

      if (tolower(ext) %in% c('xlsx','xls')) {
        df <- readxl::read_excel(path, col_names = FALSE)
      } else {
        df <- read.csv(path, stringsAsFactors = FALSE, header = FALSE)
      }

      numcols_idx <- which(vapply(df, function(col) {
        num_vals <- suppressWarnings(as.numeric(as.character(col)))
        sum(!is.na(num_vals)) / length(col) > 0.5
      }, logical(1)))

      if (length(numcols_idx) == 0) {
        stop('No numeric columns found. Asegúrate de que el archivo contenga números.')
      }

      col_data <- df[[numcols_idx[1]]]
      x <- suppressWarnings(as.numeric(as.character(col_data)))
      x <- x[!is.na(x)]

      if (length(x) < 2) stop('Necesitas al menos 2 valores numéricos.')

      res <- fit_dist(x, type = 'severity')
      params <- paste(names(res$best_fit$estimate), round(res$best_fit$estimate, 3), sep = '=', collapse = ', ')
      updateTextInput(session, 'lm_params', value = params)
      updateSelectInput(session, 'lm_dist', selected = res$best_dist)
      analysis_message(sprintf('Ajuste LM completado: %s con %d observaciones', res$best_dist, length(x)))
    }, error = function(e) {
      analysis_message(paste('Error al ajustar LM desde histórico:', e$message))
    })
  })

  # Read an uploaded survey file into a data frame
  survey_upload_data <- reactive({
    req(input$survey_file)
    ext <- tools::file_ext(input$survey_file$name)
    if (tolower(ext) %in% c('xlsx', 'xls')) {
      readxl::read_excel(input$survey_file$datapath)
    } else {
      read.csv(input$survey_file$datapath, stringsAsFactors = FALSE)
    }
  })

  observeEvent(input$btn_add_scenario, {
    tryCatch({
      if (!is.null(input$survey_file)) {
        df <- survey_upload_data()
        import_uploaded_survey_data(df, default_domain_id = input$domain_id)
        analysis_message(sprintf("Se importaron %d filas desde el archivo de encuesta.", nrow(df)))
        return()
      }

      req(input$domain_id, input$scenario_description, input$tcomm, input$scenario_id)

      tef_val <- build_dimension_value(
        mode = input$tef_mode,
        qual = input$tef_cat,
        dist = input$tef_dist,
        params = input$tef_params,
        pert_min = input$tef_pert_min,
        pert_mode = input$tef_pert_mode,
        pert_max = input$tef_pert_max
      )

      tc_val <- build_dimension_value(
        mode = input$tc_mode,
        qual = input$tc,
        pert_min = input$tc_pert_min,
        pert_mode = input$tc_pert_mode,
        pert_max = input$tc_pert_max
      )

      lm_val <- build_dimension_value(
        mode = input$lm_mode,
        qual = input$lm_cat,
        dist = input$lm_dist,
        params = input$lm_params,
        pert_min = input$lm_pert_min,
        pert_mode = input$lm_pert_mode,
        pert_max = input$lm_pert_max
      )

      caps <- caps_module$capability_ids_csv()

      write_survey_scenario(
        domain_id = input$domain_id,
        scenario_description = input$scenario_description,
        tcomm = input$tcomm,
        tef = tef_val,
        tc = tc_val,
        lm = lm_val,
        scenario_id = input$scenario_id,
        capabilities = caps,
        lef = NULL
      )
      analysis_message(sprintf("Escenario añadido/actualizado en survey.xlsx en dominio %s.", input$domain_id))
    }, error = function(e) {
      analysis_message(paste("Error al escribir el escenario:", e$message))
    })
  })

  observeEvent(input$btn_run_analysis, {
    analysis_message(NULL)
    analysis_results(NULL)
    mitigation_results(NULL)
    withProgress(message = "Ejecutando simulación...", value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Importando escenarios")
        res <- run_evaluator_analysis(
          iterations = input$iterations,
          custom_diff_params = caps_module$capability_pert_params()
        )
        incProgress(0.9, detail = "Completado")
        analysis_results(res)
        analysis_message(sprintf("Análisis completado. Resultados en: %s", res$results_dir))
      }, error = function(e) {
        analysis_message(paste("Error en análisis:", e$message))
      })
    })
  })

  observeEvent(input$btn_run_mitigation, {
    mitigation_results(NULL)
    req(analysis_results())
    withProgress(message = "Ejecutando análisis de mitigación...", value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Simulando ALE inherente y residual")
        main <- analysis_results()
        mit <- run_mitigation_analysis(
          iterations = input$iterations,
          qualitative_scenarios = main$qualitative_scenarios,
          capabilities = main$capabilities,
          mappings = main$mappings,
          simulation_results = main$simulation_results,
          custom_diff_params = caps_module$capability_pert_params()
        )
        incProgress(0.9, detail = "Completado")
        mitigation_results(mit)
        analysis_message("Análisis de mitigación completado.")
      }, error = function(e) {
        mitigation_results(NULL)
        analysis_message(paste("Error en análisis de mitigación:", e$message))
      })
    })
  })

  output$dl_survey <- downloadHandler(
    filename = function() "survey.xlsx",
    content = function(file) {
      ws <- evaluator_workspace()
      src <- file.path(ws$inputs_dir, "survey.xlsx")
      if (!file.exists(src)) stop("survey.xlsx no encontrado", call. = FALSE)
      file.copy(src, file, overwrite = TRUE)
    }
  )

  output$dl_scenario_csv <- downloadHandler(
    filename = function() paste0("resumen_escenarios_", Sys.Date(), ".csv"),
    content = function(file) {
      req(analysis_results())
      write.csv(analysis_results()$scenario_summary, file, row.names = FALSE)
    }
  )

  output$dl_domain_csv <- downloadHandler(
    filename = function() paste0("resumen_dominios_", Sys.Date(), ".csv"),
    content = function(file) {
      req(analysis_results())
      write.csv(analysis_results()$domain_summary, file, row.names = FALSE)
    }
  )

  # --- TEF / LM summaries ---
  output$tef_summary <- renderText({
    mode <- input$tef_mode
    if (identical(mode, "Qualitative")) {
      sprintf("<b>TEF (Qualitative):</b> %s", input$tef_cat)
    } else if (identical(mode, "PERT")) {
      sprintf("<b>TEF (PERT):</b> Min=%s, Mode=%s, Max=%s",
              input$tef_pert_min, input$tef_pert_mode, input$tef_pert_max)
    } else if (identical(mode, "Distribution")) {
      sprintf("<b>TEF (Distribution):</b> %s<br>Parameters: %s", input$tef_dist, input$tef_params)
    } else {
      sprintf("<b>TEF (From file):</b> %s<br>Parameters: %s", input$tef_dist, input$tef_params)
    }
  })

  output$tef_plot <- renderPlotly({
    mode <- input$tef_mode
    req(mode)
    style <- plot_style()
    if (identical(mode, "Qualitative")) {
      return(empty_plotly("Selecciona PERT, Distribution o Fit from file para visualizar la distribución TEF.", style))
    }
    if (identical(mode, "PERT")) {
      req(input$tef_pert_min, input$tef_pert_mode, input$tef_pert_max)
      params <- list(min = input$tef_pert_min, mode = input$tef_pert_mode, max = input$tef_pert_max)
      p <- dist_density_plot("pert", params, style)
      return(if (is.null(p)) empty_plotly("No se pudo generar la densidad PERT.", style) else p)
    }
    # Distribution / Fit from file
    req(input$tef_dist, input$tef_params)
    params <- parse_params_string(input$tef_params)
    req(length(params) > 0)
    p <- dist_density_plot(input$tef_dist, params, style)
    if (is.null(p)) empty_plotly("No se pudo generar la densidad de la distribución seleccionada.", style) else p
  })

  output$lm_summary <- renderText({
    mode <- input$lm_mode
    if (identical(mode, "Qualitative")) {
      sprintf("<b>LM (Qualitative):</b> %s", input$lm_cat)
    } else if (identical(mode, "PERT")) {
      sprintf("<b>LM (PERT):</b> Min=%s, Mode=%s, Max=%s",
              input$lm_pert_min, input$lm_pert_mode, input$lm_pert_max)
    } else if (identical(mode, "Distribution")) {
      sprintf("<b>LM (Distribution):</b> %s<br>Parameters: %s", input$lm_dist, input$lm_params)
    } else {
      sprintf("<b>LM (From file):</b> %s<br>Parameters: %s", input$lm_dist, input$lm_params)
    }
  })

  output$lm_plot <- renderPlotly({
    mode <- input$lm_mode
    req(mode)
    style <- plot_style()
    if (identical(mode, "Qualitative")) {
      return(empty_plotly("Selecciona PERT, Distribution o Fit from file para visualizar la distribución LM.", style))
    }
    if (identical(mode, "PERT")) {
      req(input$lm_pert_min, input$lm_pert_mode, input$lm_pert_max)
      params <- list(min = input$lm_pert_min, mode = input$lm_pert_mode, max = input$lm_pert_max)
      p <- dist_density_plot("pert", params, style)
      return(if (is.null(p)) empty_plotly("No se pudo generar la densidad PERT.", style) else p)
    }
    # Distribution / Fit from file
    req(input$lm_dist, input$lm_params)
    params <- parse_params_string(input$lm_params)
    req(length(params) > 0)
    p <- dist_density_plot(input$lm_dist, params, style)
    if (is.null(p)) empty_plotly("No se pudo generar la densidad de la distribución seleccionada.", style) else p
  })

  # --- KPI value boxes ---
  output$kpi_ale_median <- renderText({
    req(analysis_results())
    scales::dollar(sum(analysis_results()$scenario_summary$ale_median, na.rm = TRUE), accuracy = 1)
  })

  output$kpi_ale_max <- renderText({
    req(analysis_results())
    scales::dollar(max(analysis_results()$scenario_summary$ale_max, na.rm = TRUE), accuracy = 1)
  })

  output$kpi_top_scenario <- renderText({
    req(analysis_results())
    ss <- analysis_results()$scenario_summary
    ss$scenario_id[which.max(ss$ale_median)]
  })

  output$kpi_top_tcomm <- renderText({
    req(analysis_results())
    ss <- analysis_results()$scenario_summary
    agg <- ss |>
      dplyr::group_by(tcomm) |>
      dplyr::summarise(total = sum(ale_median, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total))
    agg$tcomm[1]
  })

  # --- Threat community module ---
  tcomm_sum <- reactive({
    req(analysis_results())
    analysis_results()$scenario_summary |>
      dplyr::group_by(tcomm) |>
      dplyr::summarise(
        n_scenarios = dplyr::n(),
        ale_median = sum(ale_median, na.rm = TRUE),
        ale_max = sum(ale_max, na.rm = TRUE),
        loss_events = sum(loss_events_mean, na.rm = TRUE),
        mean_vuln = mean(mean_vuln, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(ale_median))
  })

  output$tcomm_donut <- renderPlotly({
    d <- tcomm_sum()
    req(nrow(d) > 0)
    palette <- c("#6366f1", "#0ea5e9", "#06b6d4", "#8b5cf6", "#ec4899",
                 "#f59e0b", "#10b981", "#ef4444", "#3b82f6", "#14b8a6")
    p <- plotly::plot_ly(
      d, labels = ~tcomm, values = ~ale_median, type = "pie", hole = 0.55,
      textinfo = "label+percent", textposition = "auto",
      marker = list(colors = palette[seq_len(min(nrow(d), length(palette)))],
                    line = list(color = "rgba(0,0,0,0.25)", width = 1)),
      hovertemplate = "%{label}<br>ALE Mediana: %{value:$,.0f} (%{percent})<extra></extra>"
    )
    dash_layout(p, plot_style()) |>
      plotly::layout(showlegend = TRUE,
                     legend = list(orientation = "h", y = -0.1, font = list(size = 11)))
  })

  output$tcomm_bars <- renderPlotly({
    d <- tcomm_sum()
    req(nrow(d) > 0)
    long <- d |>
      tidyr::pivot_longer(c(ale_median, ale_max), names_to = "metric", values_to = "ale") |>
      dplyr::mutate(metric = ifelse(metric == "ale_median", "ALE Mediana", "ALE Máximo"))
    p <- plotly::plot_ly(
      long, x = ~ale, y = ~tcomm, color = ~metric, type = "bar", orientation = "h",
      colors = c("#0ea5e9", "#6366f1"),
      hovertemplate = "%{y}<br>%{fullData.name}: %{x:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(barmode = "group",
                     xaxis = list(title = "ALE (USD)", tickprefix = "$"),
                     yaxis = list(title = ""))
    dash_layout(p, plot_style())
  })

  output$tcomm_table <- renderDT({
    d <- tcomm_sum()
    req(nrow(d) > 0)
    d |>
      dplyr::select(tcomm, n_scenarios, ale_median, ale_max, loss_events, mean_vuln) |>
      DT::datatable(
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10, dom = "Bfrtip",
          scrollX = TRUE, scrollY = "400px",
          autoWidth = TRUE, deferRender = TRUE
        ),
        colnames = c("Threat Community", "Escenarios", "ALE Mediana", "ALE Máximo",
                     "Eventos de Pérdida (media)", "Vulnerabilidad")
      ) |>
      DT::formatCurrency(c("ale_median", "ale_max"), currency = "$", digits = 0) |>
      DT::formatPercentage("mean_vuln", 1) |>
      DT::formatStyle("ale_median", background = DT::styleColorBar(range(d$ale_median), "#0ea5e9")) |>
      DT::formatStyle("ale_max", background = DT::styleColorBar(range(d$ale_max), "#6366f1"))
  })

  # --- Scenario results ---
  output$analysis_message_scenarios <- renderText({
    if (is.null(analysis_results())) "Ejecuta el análisis primero." else ""
  })

  output$scenario_summary_table <- renderDT({
    req(analysis_results())
    res <- analysis_results()$scenario_summary
    res |>
      dplyr::select(scenario_id, scenario_description, tcomm, domain_id,
                    ale_median, ale_max, ale_var, loss_events_mean, mean_vuln) |>
      DT::datatable(
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10, dom = "Bfrtip",
          scrollX = TRUE, scrollY = "400px",
          autoWidth = TRUE, deferRender = TRUE
        ),
        colnames = c("ID", "Descripción", "TComm", "Dominio", "ALE Mediana", "ALE Máximo",
                     "VaR 95%", "Eventos de Pérdida (media)", "Vulnerabilidad")
      ) |>
      DT::formatCurrency(c("ale_median", "ale_max", "ale_var"), currency = "$", digits = 0) |>
      DT::formatPercentage("mean_vuln", 1) |>
      DT::formatStyle("ale_median", background = DT::styleColorBar(range(res$ale_median), "#0ea5e9")) |>
      DT::formatStyle("ale_max", background = DT::styleColorBar(range(res$ale_max), "#6366f1"))
  })

  output$scenario_selector_ui <- renderUI({
    req(analysis_results())
    ids <- analysis_results()$scenario_summary$scenario_id
    selectInput("selected_scenario", "Seleccionar escenario", choices = ids)
  })

  output$scenario_scatter_plot <- renderPlotly({
    req(analysis_results(), input$selected_scenario)
    sim <- analysis_results()$simulation_results
    all_results <- tidyr::unnest(sim, results)
    dat <- all_results |> dplyr::filter(scenario_id == input$selected_scenario)
    req(nrow(dat) > 0)

    p <- plotly::plot_ly(
      dat, x = ~loss_events, y = ~ale, type = "scatter", mode = "markers",
      marker = list(color = "rgba(99,102,241,0.5)", size = 6),
      hovertemplate = "Eventos: %{x:,}<br>ALE: %{y:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(xaxis = list(title = "Eventos de pérdida (anualizado)"),
                     yaxis = list(title = "ALE (USD)", tickprefix = "$"))
    dash_layout(p, plot_style())
  })

  # --- Domain results ---
  output$domain_summary_table <- renderDT({
    req(analysis_results())
    res <- analysis_results()$domain_summary
    res |>
      dplyr::select(domain_id, ale_median, ale_mean, ale_max, ale_var,
                    mean_loss_events, mean_vuln) |>
      DT::datatable(
        rownames = FALSE,
        options = list(pageLength = 10, dom = "Bfrtip", scrollX = TRUE),
        colnames = c("Dominio", "ALE Mediana", "ALE Media", "ALE Máximo", "VaR 95%",
                     "Eventos de Pérdida (media)", "Vulnerabilidad")
      ) |>
      DT::formatCurrency(c("ale_median", "ale_mean", "ale_max", "ale_var"), currency = "$", digits = 0) |>
      DT::formatPercentage("mean_vuln", 1) |>
      DT::formatStyle("ale_median", background = DT::styleColorBar(range(res$ale_median), "#0ea5e9"))
  })

  output$domain_ale_bar_plot <- renderPlotly({
    req(analysis_results())
    dom <- analysis_results()$domain_summary
    p <- plotly::plot_ly(
      dom, x = ~ale_median, y = ~stats::reorder(domain_id, ale_median),
      type = "bar", orientation = "h",
      marker = list(
        color = ~ale_median,
        colorscale = list(c(0, "#0ea5e9"), c(0.5, "#06b6d4"), c(1, "#6366f1")),
        line = list(color = "rgba(0,0,0,0.2)", width = 1)
      ),
      hovertemplate = "%{y}<br>ALE Mediana: %{x:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(xaxis = list(title = "ALE Mediana (USD)", tickprefix = "$"),
                     yaxis = list(title = ""))
    dash_layout(p, plot_style())
  })

  # --- Loss exceedance curve ---
  output$exceedance_scenario_selector_ui <- renderUI({
    req(analysis_results())
    ids <- analysis_results()$scenario_summary$scenario_id
    selectInput("exceedance_scenario", "Seleccionar escenario", choices = ids)
  })

  output$exceedance_plot <- renderPlotly({
    req(analysis_results(), input$exceedance_scenario)
    sim <- analysis_results()$simulation_results
    all_results <- tidyr::unnest(sim, results)
    dat <- all_results |> dplyr::filter(scenario_id == input$exceedance_scenario)
    req(nrow(dat) > 0)

    exc <- dat |>
      dplyr::arrange(ale) |>
      dplyr::mutate(prob = 1 - dplyr::percent_rank(ale))

    p <- plotly::plot_ly(
      exc, x = ~prob, y = ~ale, type = "scatter", mode = "lines",
      line = list(color = "#6366f1", width = 3),
      fill = "tozeroy", fillcolor = "rgba(99,102,241,0.15)",
      hovertemplate = "Probabilidad ≥ pérdida: %{x:.1%}<br>ALE: %{y:$,.0f}<extra></extra>"
    )

    # Percentile markers: P10/P50/P90 (probability of exceedance)
    quant <- stats::quantile(dat$ale, probs = c(0.9, 0.5, 0.1), na.rm = TRUE)
    pcts <- c("P10" = 0.9, "P50" = 0.5, "P90" = 0.1)
    for (i in seq_along(pcts)) {
      nm <- names(pcts)[i]
      pr <- pcts[[i]]
      val <- quant[[i]]
      p <- p |>
        plotly::add_segments(
          x = pr, xend = pr, y = 0, yend = val,
          line = list(color = "rgba(244,63,94,0.85)", dash = "dash", width = 1.5),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::add_annotations(
          x = pr, y = val,
          text = sprintf("%s: %s", nm, scales::dollar(val, accuracy = 0)),
          showarrow = FALSE, yshift = 12, font = list(size = 11)
        )
    }

    dash_layout(p, plot_style()) |>
      plotly::layout(
        xaxis = list(title = "Probabilidad de pérdida igual o mayor",
                     tickformat = ".0%", autorange = "reversed"),
        yaxis = list(title = "Pérdida (ALE)", tickprefix = "$"),
        title = list(text = paste("Curva de excedencia:", input$exceedance_scenario), x = 0)
      )
  })

  # --- Mitigation analysis ---
  output$mitigation_message <- renderText({
    if (is.null(mitigation_results())) "Ejecuta el análisis de mitigación para ver la efectividad de los controles." else ""
  })

  output$mit_inherent <- renderText({
    req(mitigation_results())
    scales::dollar(sum(mitigation_results()$scenario_level$ale_inherent, na.rm = TRUE), accuracy = 1)
  })

  output$mit_residual <- renderText({
    req(mitigation_results())
    scales::dollar(sum(mitigation_results()$scenario_level$ale_residual, na.rm = TRUE), accuracy = 1)
  })

  output$mit_reduction <- renderText({
    req(mitigation_results())
    sl <- mitigation_results()$scenario_level
    tot_inh <- sum(sl$ale_inherent, na.rm = TRUE)
    tot_res <- sum(sl$ale_residual, na.rm = TRUE)
    pct <- if (tot_inh > 0) (tot_inh - tot_res) / tot_inh else 0
    scales::percent(pct, accuracy = 0.1)
  })

  output$mit_bars <- renderPlotly({
    req(mitigation_results())
    sl <- mitigation_results()$scenario_level |>
      dplyr::arrange(dplyr::desc(ale_inherent)) |>
      utils::head(15)
    long <- sl |>
      tidyr::pivot_longer(c(ale_inherent, ale_residual), names_to = "tipo", values_to = "ale") |>
      dplyr::mutate(tipo = ifelse(tipo == "ale_inherent", "Inherente (sin controles)", "Residual (con controles)"))
    p <- plotly::plot_ly(
      long, x = ~ale, y = ~scenario_id, color = ~tipo, type = "bar", orientation = "h",
      colors = c("#ef4444", "#10b981"),
      hovertemplate = "%{y}<br>%{fullData.name}: %{x:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(barmode = "group",
                     xaxis = list(title = "ALE (USD)", tickprefix = "$"),
                     yaxis = list(title = ""))
    dash_layout(p, plot_style())
  })

  control_sum <- reactive({
    req(mitigation_results())
    mitigation_results()$control_level |>
      dplyr::group_by(capability_id, capability) |>
      dplyr::summarise(
        n_scenarios = dplyr::n(),
        total_savings = sum(marginal_savings, na.rm = TRUE),
        mean_reduction = mean(reduction_pct, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(total_savings))
  })

  output$mit_control_bars <- renderPlotly({
    cs <- control_sum()
    req(nrow(cs) > 0)
    p <- plotly::plot_ly(
      cs, x = ~total_savings, y = ~capability_id, type = "bar", orientation = "h",
      marker = list(
        color = ~total_savings,
        colorscale = list(c(0, "#10b981"), c(1, "#0ea5e9")),
        line = list(color = "rgba(0,0,0,0.2)", width = 1)
      ),
      hovertemplate = "%{y}<br>Ahorro: %{x:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(xaxis = list(title = "Ahorro total (USD)", tickprefix = "$"),
                     yaxis = list(title = ""))
    dash_layout(p, plot_style())
  })

  output$mit_control_table <- renderDT({
    cs <- control_sum()
    req(nrow(cs) > 0)
    cs |>
      dplyr::select(capability_id, capability, n_scenarios, total_savings, mean_reduction) |>
      DT::datatable(
        rownames = FALSE,
        options = list(pageLength = 10, dom = "Bfrtip", scrollX = TRUE),
        colnames = c("Capability", "Descripción", "Escenarios", "Ahorro Total", "Reducción media")
      ) |>
      DT::formatCurrency("total_savings", currency = "$", digits = 0) |>
      DT::formatPercentage("mean_reduction", 1) |>
      DT::formatStyle("total_savings", background = DT::styleColorBar(range(cs$total_savings), "#10b981"))
  })
}

shinyApp(ui = ui, server = server)
