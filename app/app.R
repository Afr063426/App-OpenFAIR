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
  source("fit_dist.R")
  source("evaluator_helpers.R")
}, error = function(e) {
  startup_log(c(sprintf("Startup error at %s", Sys.time()), conditionMessage(e)))
  stop(e)
})

ui <- page_sidebar(
  title = "Evaluador Cuantitativo de Riesgos (TFM)",
  sidebar = sidebar(
    uiOutput("domain_ui"),
    textInput("scenario_description", "Descripción del escenario", value = ""),
    textInput("tcomm", "Threat Community", value = ""),
    selectInput("tef_mode", "TEF mode", choices = c("Qualitative", "Distribution", "Fit from file"), selected = "Qualitative"),
    conditionalPanel(
      condition = "input.tef_mode == 'Qualitative'",
      selectInput("tef_cat", "TEF", choices = c("Frequent", "Occasional", "Rare"), selected = "Frequent")
    ),
    conditionalPanel(
      condition = "input.tef_mode == 'Distribution'",
      tagList(
        selectInput("tef_dist", "TEF distribution", choices = c("pois", "nbinom", "zipois", "zinegbin", "pert"), selected = "pois"),
        textInput("tef_params", "TEF params (e.g. lambda=3 or size=2,mu=3)", value = ""),
        conditionalPanel(
          condition = "input.tef_dist == 'pert'",
          numericInput("tef_pert_min", "PERT min", value = 0),
          numericInput("tef_pert_mode", "PERT mode", value = 1),
          numericInput("tef_pert_max", "PERT max", value = 2),
          actionButton("btn_use_pert_tef", "Usar PERT para TEF", class = "btn-outline-secondary w-100")
        )
      )
    ),
    conditionalPanel(
      condition = "input.tef_mode == 'Fit from file'",
      fileInput("tef_hist_file", "Cargar histórico de conteos (TEF)", accept = c('.xlsx', '.xls', '.csv')),
      actionButton("btn_fit_tef", "Ajustar TEF desde histórico", class = "btn-outline-secondary w-100")
    ),
    selectInput("tc", "TC", choices = c("High", "Medium", "Low"), selected = "Medium"),
    selectInput("lm_mode", "LM mode", choices = c("Qualitative", "Distribution", "Fit from file"), selected = "Qualitative"),
    conditionalPanel(
      condition = "input.lm_mode == 'Qualitative'",
      selectInput("lm_cat", "LM", choices = c("High", "Medium", "Low"), selected = "Medium")
    ),
    conditionalPanel(
      condition = "input.lm_mode == 'Distribution'",
      tagList(
        selectInput("lm_dist", "LM distribution", choices = c("gamma", "lnorm", "weibull", "pert"), selected = "gamma"),
        textInput("lm_params", "LM params (e.g. shape=2,rate=0.5)", value = ""),
        conditionalPanel(
          condition = "input.lm_dist == 'pert'",
          numericInput("lm_pert_min", "PERT min", value = 0),
          numericInput("lm_pert_mode", "PERT mode", value = 1),
          numericInput("lm_pert_max", "PERT max", value = 2),
          actionButton("btn_use_pert_lm", "Usar PERT para LM", class = "btn-outline-secondary w-100")
        )
      )
    ),
    conditionalPanel(
      condition = "input.lm_mode == 'Fit from file'",
      fileInput("lm_hist_file", "Cargar histórico de pérdidas (LM)", accept = c('.xlsx', '.xls', '.csv')),
      actionButton("btn_fit_lm", "Ajustar LM desde histórico", class = "btn-outline-secondary w-100")
    ),
    textInput("scenario_id", "ScenarioID", value = "RS-001"),
    textInput("capabilities", "Capabilities (comma-separated IDs)", value = "CAP-01"),
    fileInput("survey_file", "Cargar archivo de encuesta para importar escenarios", accept = c(".xlsx", ".xls", ".csv")),
    actionButton("btn_add_scenario", "Añadir escenario al survey.xlsx", class = "btn-success w-100"),
    actionButton("btn_run_analysis", "Ejecutar análisis", class = "btn-primary w-100"),
    downloadButton("dl_survey", "Descargar survey.xlsx")
  ),
  card(
    card_header("Ajustes de TEF / LM"),
    htmlOutput("tef_summary"),
    plotlyOutput("tef_plot", height = "250px"),
    htmlOutput("lm_summary"),
    plotlyOutput("lm_plot", height = "250px"),
    tags$hr(),
    h4("Vista previa de la encuesta cargada"),
    tableOutput("survey_preview"),
    tags$hr(),
    verbatimTextOutput("analysis_message")
  )
)

server <- function(input, output, session) {
  analysis_message <- reactiveVal(NULL)

  output$domain_ui <- renderUI({
    selectInput("domain_id", "Dominio", choices = get_evaluator_domain_choices(), selected = "ISMP")
  })

  output$data_status <- renderUI({
    tags$p("Usa las secciones de TEF y LM en el panel lateral para configurar o ajustar distribuciones.")
  })

  output$analysis_message <- renderText({
    if (is.null(analysis_message())) {
      ""
    } else {
      analysis_message()
    }
  })

  # TEF fit from uploaded historical counts
  observeEvent(input$btn_fit_tef, {
    tryCatch({
      req(input$tef_hist_file)
      path <- input$tef_hist_file$datapath
      ext <- tools::file_ext(input$tef_hist_file$name)
      
      # Read file with more flexibility
      if (tolower(ext) %in% c('xlsx','xls')) {
        df <- readxl::read_excel(path, col_names = FALSE)
      } else {
        df <- read.csv(path, stringsAsFactors = FALSE, header = FALSE)
      }
      
      # Find numeric columns (may be mixed with text headers)
      numcols_idx <- which(vapply(df, function(col) {
        # Try to coerce to numeric and see if most values are numeric
        num_vals <- suppressWarnings(as.numeric(as.character(col)))
        sum(!is.na(num_vals)) / length(col) > 0.5
      }, logical(1)))
      
      if (length(numcols_idx) == 0) {
        stop('No numeric columns found. Asegúrate de que el archivo contenga números.')
      }
      
      # Use first numeric column
      col_data <- df[[numcols_idx[1]]]
      x <- suppressWarnings(as.numeric(as.character(col_data)))
      x <- x[!is.na(x)]
      
      if (length(x) < 2) stop('Necesitas al menos 2 valores numéricos.')
      
      x <- round(x)
      res <- fit_dist(x, type = 'count')
      # store suggested distribution info in session (tef_dist/tef_params)
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
      
      # Read file with more flexibility
      if (tolower(ext) %in% c('xlsx','xls')) {
        df <- readxl::read_excel(path, col_names = FALSE)
      } else {
        df <- read.csv(path, stringsAsFactors = FALSE, header = FALSE)
      }
      
      # Find numeric columns (may be mixed with text headers)
      numcols_idx <- which(vapply(df, function(col) {
        num_vals <- suppressWarnings(as.numeric(as.character(col)))
        sum(!is.na(num_vals)) / length(col) > 0.5
      }, logical(1)))
      
      if (length(numcols_idx) == 0) {
        stop('No numeric columns found. Asegúrate de que el archivo contenga números.')
      }
      
      # Use first numeric column
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

  # Use PERT buttons to populate params
  observeEvent(input$btn_use_pert_tef, {
    params <- sprintf('min=%s,mode=%s,max=%s', input$tef_pert_min, input$tef_pert_mode, input$tef_pert_max)
    updateTextInput(session, 'tef_params', value = params)
    analysis_message('PERT TEF copiado en parámetros.')
  })

  observeEvent(input$btn_use_pert_lm, {
    params <- sprintf('min=%s,mode=%s,max=%s', input$lm_pert_min, input$lm_pert_mode, input$lm_pert_max)
    updateTextInput(session, 'lm_params', value = params)
    analysis_message('PERT LM copiado en parámetros.')
  })

  # remove old lef fit buttons and fields if present



  observeEvent(input$btn_add_scenario, {
    tryCatch({
      if (!is.null(input$survey_file)) {
        df <- survey_upload_data()
        import_uploaded_survey_data(df, default_domain_id = input$domain_id)
        analysis_message(sprintf("Se importaron %d filas desde el archivo de encuesta.", nrow(df)))
        return()
      }

      req(input$domain_id, input$scenario_description, input$tcomm, input$tc, input$scenario_id, input$capabilities)

      # Build TEF value + attributes depending on mode
      tef_val <- NULL
      if (identical(input$tef_mode, "Qualitative")) {
        tef_val <- input$tef_cat
      } else if (identical(input$tef_mode, "Distribution")) {
        tef_val <- paste0("dist:", input$tef_dist, "|params:", input$tef_params)
        attr(tef_val, 'dist') <- input$tef_dist
        attr(tef_val, 'params') <- input$tef_params
      } else if (identical(input$tef_mode, "Fit from file")) {
        # use selected tef_dist and tef_params (populated by fit button)
        tef_val <- paste0("dist:", input$tef_dist, "|params:", input$tef_params)
        attr(tef_val, 'dist') <- input$tef_dist
        attr(tef_val, 'params') <- input$tef_params
      }

      # Build LM value + attributes depending on mode
      lm_val <- NULL
      if (identical(input$lm_mode, "Qualitative")) {
        lm_val <- input$lm_cat
      } else if (identical(input$lm_mode, "Distribution")) {
        lm_val <- paste0("dist:", input$lm_dist, "|params:", input$lm_params)
        attr(lm_val, 'dist') <- input$lm_dist
        attr(lm_val, 'params') <- input$lm_params
      } else if (identical(input$lm_mode, "Fit from file")) {
        lm_val <- paste0("dist:", input$lm_dist, "|params:", input$lm_params)
        attr(lm_val, 'dist') <- input$lm_dist
        attr(lm_val, 'params') <- input$lm_params
      }

      write_survey_scenario(
        domain_id = input$domain_id,
        scenario_description = input$scenario_description,
        tcomm = input$tcomm,
        tef = tef_val,
        tc = input$tc,
        lm = lm_val,
        scenario_id = input$scenario_id,
        capabilities = input$capabilities,
        lef = NULL
      )
      analysis_message(sprintf("Escenario añadido/actualizado en survey.xlsx en dominio %s.", input$domain_id))
    }, error = function(e) {
      analysis_message(paste("Error al escribir el escenario:", e$message))
    })
  })

  observeEvent(input$btn_run_analysis, {
    analysis_message(NULL)
    tryCatch({
      res <- run_evaluator_analysis(iterations = 1000)
      analysis_message(sprintf("Análisis completado. Resultados en: %s", res$results_dir))
    }, error = function(e) {
      analysis_message(paste("Error en análisis:", e$message))
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

  output$survey_preview <- renderTable({
    req(input$domain_id)
    ws <- evaluator_workspace()
    survey_file <- file.path(ws$inputs_dir, "survey.xlsx")
    if (!file.exists(survey_file)) return(NULL)
    dat <- readxl::read_excel(survey_file, sheet = input$domain_id, col_names = FALSE)
    threats_row <- which(dat[[1]] == "Threats")[1]
    if (is.na(threats_row)) return(head(dat, 6))
    header_row <- threats_row + 1
    data_rows <- seq(header_row + 1, nrow(dat))
    if (length(data_rows) == 0) return(NULL)
    maxcol <- min(ncol(dat), 11)
    df <- dat[data_rows, 1:maxcol]
    names_map <- c("Scenario", "TComm", "TEF", "TC", "LM", "ScenarioID", "Capabilities", "TEF_dist", "TEF_params", "LM_dist", "LM_params")
    colnames(df) <- names_map[1:ncol(df)]
    keep <- vapply(df, function(col) any(!is.na(col) & col != ""), logical(1))
    df <- df[, keep, drop = FALSE]
    head(df, 6)
  })

  # Placeholder outputs for TEF/LM summaries and plots (simple implementations)
  output$tef_summary <- renderText({
    mode <- input$tef_mode
    if (identical(mode, "Qualitative")) {
      sprintf("<b>TEF (Qualitative):</b> %s", input$tef_cat)
    } else if (identical(mode, "Distribution")) {
      sprintf("<b>TEF (Distribution):</b> %s<br>Parameters: %s", input$tef_dist, input$tef_params)
    } else {
      sprintf("<b>TEF (From file):</b> %s<br>Parameters: %s", input$tef_dist, input$tef_params)
    }
  })

  output$tef_plot <- renderPlotly({
    ggplotly(ggplot() + geom_blank() + theme_void() + ggtitle("TEF Distribution Plot"))
  })

  output$lm_summary <- renderText({
    mode <- input$lm_mode
    if (identical(mode, "Qualitative")) {
      sprintf("<b>LM (Qualitative):</b> %s", input$lm_cat)
    } else if (identical(mode, "Distribution")) {
      sprintf("<b>LM (Distribution):</b> %s<br>Parameters: %s", input$lm_dist, input$lm_params)
    } else {
      sprintf("<b>LM (From file):</b> %s<br>Parameters: %s", input$lm_dist, input$lm_params)
    }
  })

  output$lm_plot <- renderPlotly({
    ggplotly(ggplot() + geom_blank() + theme_void() + ggtitle("LM Distribution Plot"))
  })
}

shinyApp(ui = ui, server = server)
