# =============================================================================
# server.R — Lógica reactiva del dashboard. La lógica de negocio (simulación
# OpenFAIR, mitigación, capabilities) se mantiene 100% intacta; solo cambian
# el formato de KPIs (sufijos K/M/B) y la posición de leyendas de gráficos.
# =============================================================================

server <- function(input, output, session) {
  analysis_message <- reactiveVal(NULL)
  analysis_results <- reactiveVal(NULL)
  mitigation_results <- reactiveVal(NULL)

  # Colores que siguen el modo claro/oscuro (tipografía y rejilla de plotly)
  plot_style <- reactive({
    dark <- is.null(input$dark_mode) || identical(input$dark_mode, "dark")
    list(font = if (dark) "#dee2e6" else "#212529",
         grid = if (dark) "rgba(255,255,255,0.08)" else "rgba(0,0,0,0.12)")
  })

  # Contador para refrescar el selector de dominios tras agregar uno nuevo
  domain_refresh <- reactiveVal(0)

  output$domain_ui <- renderUI({
    domain_refresh()
    selectInput("domain_id", "Dominio", choices = get_evaluator_domain_choices(), selected = "ISMP")
  })

  # --- Agregar un dominio NUEVO (domains.csv + hoja en survey.xlsx) ----------
  observeEvent(input$btn_add_domain, {
    tryCatch({
      id <- add_new_domain(input$new_domain_id, input$new_domain_name)
      updateTextInput(session, "new_domain_id", value = "")
      updateTextInput(session, "new_domain_name", value = "")
      domain_refresh(domain_refresh() + 1)
      updateSelectInput(session, "domain_id", selected = id)
      analysis_message(sprintf("Dominio %s agregado. Añade escenarios para empezar.", id))
    }, error = function(e) {
      analysis_message(paste("Error al agregar dominio:", err_chain(e)))
    })
  })

  # --- Reiniciar proyecto: confirmación + limpieza total ---------------------
  observeEvent(input$btn_reset_project, {
    showModal(modalDialog(
      title = "Reiniciar proyecto",
      "Se borrarán TODOS los escenarios, los dominios personalizados y los ",
      "controles propios. El survey quedará como plantilla vacía (solo los ",
      "14 dominios predefinidos y los controles del catálogo). ¿Continuar?",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_confirm_reset", "Sí, reiniciar", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$btn_confirm_reset, {
    removeModal()
    tryCatch({
      reset_project()
      caps_module$clear()
      analysis_results(NULL)
      mitigation_results(NULL)
      convergence_check(NULL)
      sensitivity_results(NULL)
      domain_refresh(domain_refresh() + 1)
      analysis_message("Proyecto reiniciado: plantilla vacía. Añade escenarios para comenzar.")
    }, error = function(e) {
      analysis_message(paste("Error al reiniciar proyecto:", err_chain(e)))
    })
  })

  # Contador para refrescar el selector de controles tras importar
  # custom_capabilities.csv (el módulo observa este reactive).
  custom_caps_refresh <- reactiveVal(0)

  # Módulo de configuración de capabilities (controles) con parámetros Beta-PERT
  caps_module <- capabilities_server(
    "capabilities_mod",
    available_choices = reactive({
      custom_caps_refresh()
      get_evaluator_capabilities()
    })
  )

  # ---------------------------------------------------------------------------
  # MULTIPROYECTO: cambiar/crear proyecto. Todos los helpers resuelven el
  # workspace según options(tfm.project_dir); al cambiar se limpia el estado
  # reactivo, se refrescan los selectores y se intenta cargar la caché de
  # resultados guardados de ESE proyecto (sin re-simular).
  # ---------------------------------------------------------------------------
  proyecto_refresh <- reactiveVal(0)

  # Limpia el estado del análisis al cambiar de proyecto
  limpiar_estado_analisis <- function() {
    analysis_results(NULL)
    mitigation_results(NULL)
    if (exists("optimization_results", inherits = TRUE)) optimization_results(NULL)
    if (exists("convergence_check", inherits = TRUE)) convergence_check(NULL)
    if (exists("sensitivity_results", inherits = TRUE)) sensitivity_results(NULL)
  }

  # Cambia al proyecto cuya ruta se pasa (o NULL para el workspace por defecto)
  # y recarga lo que se pueda sin simular.
  cambiar_proyecto <- function(ruta = NULL) {
    if (is.null(ruta) || identical(ruta, "__default__")) {
      limpiar_proyecto_actual()
    } else {
      set_proyecto_actual(ruta)
    }
    limpiar_estado_analisis()
    domain_refresh(domain_refresh() + 1)
    custom_caps_refresh(custom_caps_refresh() + 1)
    caps_module$clear()
    proyecto_refresh(proyecto_refresh() + 1)
    # Cargar resultados guardados si la firma coincide
    cache <- tryCatch(cargar_cache_analisis(), error = function(e) NULL)
    if (!is.null(cache)) {
      analysis_results(cache)
      mit <- attr(cache, "mitigacion")
      if (!is.null(mit)) mitigation_results(mit)
      guardado <- attr(cache, "guardado")
      analysis_message(sprintf(
        "Resultados cargados de la sesión anterior (%d escenarios%s, guardados %s). Pulsa 'Ejecutar análisis' para recalcular.",
        nrow(cache$scenario_summary),
        if (is.null(mit)) "" else " y mitigación",
        if (is.null(guardado)) "?" else format(guardado, "%d/%m %H:%M")))
    } else {
      analysis_message(sprintf("Proyecto activo: %s", proyecto_actual_nombre()))
    }
  }

  # Lista de proyectos con su info (reactivo al cambiar/crear)
  proyectos_info <- reactive({
    proyecto_refresh()
    list_proyectos()
  })

  output$proyecto_actual_label <- renderText({
    proyecto_refresh()
    ws <- evaluator_workspace()
    sprintf("Activo: %s\nWorkspace: %s\nProyectos raíz: %s",
            proyecto_actual_nombre(), ws$base_dir, proyectos_root())
  })

  # Selector de proyecto: choices valor = ruta (o __default__)
  observe({
    info <- proyectos_info()
    choices <- stats::setNames(info$ruta, info$slug)
    if (!is.null(proyecto_actual_path())) {
      sel <- proyecto_actual_path()
    } else {
      sel <- info$ruta[info$slug == info$slug[grep("por defecto", info$slug)[1]]]
      sel <- if (length(sel)) sel[1] else "__default__"
    }
    updateSelectInput(session, "select_proyecto", choices = choices, selected = sel)
  })

  output$proyectos_info <- DT::renderDT({
    info <- proyectos_info()
    info$es_actual <- NULL
    info$ruta <- NULL
    DT::datatable(info, rownames = FALSE,
                  options = list(dom = "t", pageLength = 20, ordering = TRUE,
                                 scrollX = TRUE),
                  colnames = c("Proyecto", "Última modificación survey"))
  })

  observeEvent(input$btn_abrir_proyecto, {
    req(input$select_proyecto)
    cambiar_proyecto(input$select_proyecto)
    showNotification(sprintf("Proyecto activo: %s", proyecto_actual_nombre()),
                     type = "message")
  })

  observeEvent(input$btn_nuevo_proyecto, {
    tryCatch({
      nombre <- trimws(input$nuevo_proyecto_nombre)
      ruta <- nuevo_proyecto(nombre)
      updateTextInput(session, "nuevo_proyecto_nombre", value = "")
      cambiar_proyecto(ruta)
      showNotification(sprintf("Proyecto '%s' creado y activado.", nombre),
                       type = "message")
    }, error = function(e) {
      analysis_message(paste("Error al crear proyecto:", err_chain(e)))
      showNotification(paste("Error al crear proyecto:", conditionMessage(e)),
                       type = "error")
    })
  })

  # Al abrir la app: cargar resultados guardados del proyecto por defecto
  # (sin re-simular Monte Carlo) si coinciden con el survey actual.
  tryCatch({
    cache <- cargar_cache_analisis()
    if (!is.null(cache)) {
      analysis_results(cache)
      mit <- attr(cache, "mitigacion")
      if (!is.null(mit)) mitigation_results(mit)
      guardado <- attr(cache, "guardado")
      analysis_message(sprintf(
        "Resultados cargados de la sesión anterior (%d escenarios%s, guardados %s). Pulsa 'Ejecutar análisis' para recalcular.",
        nrow(cache$scenario_summary),
        if (is.null(mit)) "" else " y mitigación",
        if (is.null(guardado)) "?" else format(guardado, "%d/%m %H:%M")))
    }
  }, error = function(e) NULL)

  output$analysis_message <- renderText({
    if (is.null(analysis_message())) "" else analysis_message()
  })

  # Banner de error visible en el DASHBOARD: si el último análisis (o la
  # importación) falló, se muestra el motivo en rojo en lugar de dejar solo
  # el mensaje de espera. Se limpia automáticamente al iniciar un análisis.
  output$analysis_error_banner <- renderUI({
    msg <- analysis_message()
    if (is.null(msg) || !nzchar(trimws(msg))) return(NULL)
    if (grepl("Error|Faltan columnas|No se puede|no se puede leer", msg)) {
      tags$div(class = "alert alert-danger py-2 px-3 mb-3",
               style = "font-size:0.9rem;",
               vb_icon("exclamation-triangle-fill"), " ",
               htmltools::htmlEscape(msg))
    } else {
      NULL
    }
  })

  # ---------------------------------------------------------------------------
  # AJUSTE DE TEF / LM DESDE HISTÓRICO (Excel o CSV con selección de HOJA y
  # COLUMNA). Al subir el archivo se listan las hojas (Excel) y las columnas
  # candidatas con su nº de valores numéricos; el usuario elige y pulsa Ajustar.
  # ---------------------------------------------------------------------------

  # ---- TEF -------------------------------------------------------------------
  # Metadatos del archivo subido: ruta + hojas (NULL si es CSV)
  tef_hist_meta <- reactive({
    req(input$tef_hist_file)
    list(path = input$tef_hist_file$datapath,
         sheets = hist_file_sheets(input$tef_hist_file$datapath))
  })

  # Selector de hoja (solo si el archivo es Excel)
  output$tef_sheet_ui <- renderUI({
    m <- tef_hist_meta()
    if (is.null(m$sheets)) return(NULL)
    selectInput("tef_hist_sheet", "Hoja del Excel",
                choices = m$sheets, selected = m$sheets[1])
  })

  # Columnas de la hoja activa (se recalculan al cambiar de hoja)
  tef_hist_cols <- reactive({
    m <- tef_hist_meta()
    sheet <- if (is.null(input$tef_hist_sheet)) NULL else input$tef_hist_sheet
    hist_file_columns(m$path, sheet)
  })

  output$tef_col_ui <- renderUI({
    cols <- tef_hist_cols()
    if (nrow(cols) == 0) {
      return(tags$div(class = "text-muted", style = "font-size:0.85rem;",
                      "No se encontraron columnas con datos numéricos."))
    }
    selectInput("tef_hist_col", "Columna de datos",
                choices = stats::setNames(cols$index, cols$label))
  })

  observeEvent(input$btn_fit_tef, {
    tryCatch({
      req(input$tef_hist_file)
      path <- input$tef_hist_file$datapath
      sheet <- if (is.null(input$tef_hist_sheet)) NULL else input$tef_hist_sheet
      # Columna elegida, o la más poblada si aún no hay selector disponible
      if (is.null(input$tef_hist_col)) {
        cols <- hist_file_columns(path, sheet)
        if (nrow(cols) == 0) {
          stop('No hay columnas con datos numéricos en el archivo.')
        }
        col_idx <- cols$index[which.max(cols$n_numeric)]
      } else {
        col_idx <- as.integer(input$tef_hist_col)
      }
      x <- hist_file_vector(path, sheet, col_idx)
      if (length(x) < 2) stop('Necesitas al menos 2 valores numéricos.')

      x <- round(x)
      res <- fit_dist(x, type = 'count')
      params <- paste(names(res$best_fit$estimate), round(res$best_fit$estimate, 3), sep = '=', collapse = ', ')
      updateTextInput(session, 'tef_params', value = params)
      updateSelectInput(session, 'tef_dist', selected = res$best_dist)
      analysis_message(sprintf('Ajuste TEF completado: %s con %d observaciones', res$best_dist, length(x)))
    }, error = function(e) {
      analysis_message(paste('Error al ajustar TEF desde histórico:', err_chain(e)))
    })
  })

  # ---- LM (mismo flujo, ajuste de severidad) --------------------------------
  lm_hist_meta <- reactive({
    req(input$lm_hist_file)
    list(path = input$lm_hist_file$datapath,
         sheets = hist_file_sheets(input$lm_hist_file$datapath))
  })

  output$lm_sheet_ui <- renderUI({
    m <- lm_hist_meta()
    if (is.null(m$sheets)) return(NULL)
    selectInput("lm_hist_sheet", "Hoja del Excel",
                choices = m$sheets, selected = m$sheets[1])
  })

  lm_hist_cols <- reactive({
    m <- lm_hist_meta()
    sheet <- if (is.null(input$lm_hist_sheet)) NULL else input$lm_hist_sheet
    hist_file_columns(m$path, sheet)
  })

  output$lm_col_ui <- renderUI({
    cols <- lm_hist_cols()
    if (nrow(cols) == 0) {
      return(tags$div(class = "text-muted", style = "font-size:0.85rem;",
                      "No se encontraron columnas con datos numéricos."))
    }
    selectInput("lm_hist_col", "Columna de datos",
                choices = stats::setNames(cols$index, cols$label))
  })

  observeEvent(input$btn_fit_lm, {
    tryCatch({
      req(input$lm_hist_file)
      path <- input$lm_hist_file$datapath
      sheet <- if (is.null(input$lm_hist_sheet)) NULL else input$lm_hist_sheet
      if (is.null(input$lm_hist_col)) {
        cols <- hist_file_columns(path, sheet)
        if (nrow(cols) == 0) {
          stop('No hay columnas con datos numéricos en el archivo.')
        }
        col_idx <- cols$index[which.max(cols$n_numeric)]
      } else {
        col_idx <- as.integer(input$lm_hist_col)
      }
      x <- hist_file_vector(path, sheet, col_idx)
      if (length(x) < 2) stop('Necesitas al menos 2 valores numéricos.')

      res <- fit_dist(x, type = 'severity')
      params <- paste(names(res$best_fit$estimate), round(res$best_fit$estimate, 3), sep = '=', collapse = ', ')
      updateTextInput(session, 'lm_params', value = params)
      updateSelectInput(session, 'lm_dist', selected = res$best_dist)
      analysis_message(sprintf('Ajuste LM completado: %s con %d observaciones', res$best_dist, length(x)))
    }, error = function(e) {
      analysis_message(paste('Error al ajustar LM desde histórico:', err_chain(e)))
    })
  })

  # Lee el archivo de encuesta subido (xlsx/csv) a un data.frame para poder
  # importar escenarios en lote con import_uploaded_survey_data().
  survey_upload_data <- reactive({
    req(input$survey_file)
    ext <- tools::file_ext(input$survey_file$name)
    if (tolower(ext) %in% c('xlsx', 'xls')) {
      readxl::read_excel(input$survey_file$datapath)
    } else {
      read.csv(input$survey_file$datapath, stringsAsFactors = FALSE)
    }
  })

  # Al seleccionar un archivo, avisar de inmediato (el botón "Añadir escenario"
  # es el que realmente importa las filas al survey.xlsx).
  observeEvent(input$survey_file, {
    req(input$survey_file)
    showNotification(
      sprintf("Archivo cargado: %s. Pulsa 'Añadir escenario al survey.xlsx' para importar sus filas.",
              input$survey_file$name),
      type = "message", duration = 6)
  })

  # Limpia todos los campos del formulario de escenario (descripción, TComm,
  # TEF, TC, LM, controles configurados...) después de añadirlo correctamente.
  reset_scenario_form <- function() {
    updateTextInput(session, "scenario_description", value = "")
    updateTextInput(session, "tcomm", value = "")
    updateTextInput(session, "scenario_id", value = "")

    # TEF
    updateSelectInput(session, "tef_mode", selected = "Qualitative")
    updateSelectInput(session, "tef_cat", selected = "Frequent")
    updateSelectInput(session, "tef_dist", selected = "pois")
    updateTextInput(session, "tef_params", value = "")
    updateNumericInput(session, "tef_pert_min", value = 0)
    updateNumericInput(session, "tef_pert_mode", value = 1)
    updateNumericInput(session, "tef_pert_max", value = 2)

    # TC
    updateSelectInput(session, "tc_mode", selected = "Qualitative")
    updateSelectInput(session, "tc", selected = "Medium")
    updateNumericInput(session, "tc_pert_min", value = 0)
    updateNumericInput(session, "tc_pert_mode", value = 0.5)
    updateNumericInput(session, "tc_pert_max", value = 1)

    # LM
    updateSelectInput(session, "lm_mode", selected = "Qualitative")
    updateSelectInput(session, "lm_cat", selected = "Medium")
    updateSelectInput(session, "lm_dist", selected = "gamma")
    updateTextInput(session, "lm_params", value = "")
    updateNumericInput(session, "lm_pert_min", value = 0)
    updateNumericInput(session, "lm_pert_mode", value = 1)
    updateNumericInput(session, "lm_pert_max", value = 2)

    # Controles configurados (módulo de capabilities) y campo de control nuevo
    caps_module$clear()
    updateTextInput(session, "capabilities_mod-new_control_name", value = "")
  }

  observeEvent(input$btn_add_scenario, {
    tryCatch({
      if (!is.null(input$survey_file)) {
        # El archivo subido puede venir en dos formatos:
        #   1) survey.xlsx SECCIONADO (una hoja por dominio con la tabla
        #      "Threats"), como el que descarga la app: se extraen los
        #      escenarios con el parser de evaluator.
        #   2) Archivo plano (CSV/XLSX) con columnas scenario_id, scenario,
        #      tcomm, tef, tc, lm y capabilities.
        ext <- tolower(tools::file_ext(input$survey_file$name))
        if (ext %in% c("xlsx", "xls") &&
            is_sectioned_survey_file(input$survey_file$datapath)) {
          df <- import_sectioned_survey_scenarios(input$survey_file$datapath)
        } else {
          df <- survey_upload_data()
        }
        import_uploaded_survey_data(df, default_domain_id = input$domain_id)
        msg_import <- sprintf("Se importaron %d filas desde el archivo de encuesta.", nrow(df))
        analysis_message(msg_import)
        showNotification(msg_import, type = "message")
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

      # Persistir en custom_capabilities.csv los controles configurados que aún
      # no están en el catálogo (con su nombre), para no perder el avance.
      conf <- caps_module$configured()
      if (nrow(conf) > 0) {
        n_persist <- persist_unknown_controls(
          conf$capability_id,
          names = stats::setNames(conf$capability_desc, conf$capability_id),
          costs = stats::setNames(conf$cost, conf$capability_id),
          eff_df = conf[, c("capability_id", "eff_min", "eff_mode", "eff_max"),
                        drop = FALSE]
        )
        # Persistir también costo/efectividad de controles personalizados YA
        # registrados (upsert), para que la optimización no los vea en 0 tras
        # limpiarse el módulo.
        n_upd <- upsert_custom_capability_params(conf)
        if (n_persist > 0 || n_upd > 0) custom_caps_refresh(custom_caps_refresh() + 1)
      }

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
      # Confirmación visible y limpieza del formulario para el siguiente escenario
      sid <- input$scenario_id
      analysis_message(sprintf("Escenario %s añadido en dominio %s.", sid, input$domain_id))
      # La notificación es informativa: si falla, no debe enmascarar el éxito.
      # type válido: "default", "message", "warning" o "error" (NO "success").
      tryCatch(
        showNotification(sprintf("Escenario %s añadido correctamente", sid),
                         type = "message"),
        error = function(e) NULL
      )
      reset_scenario_form()
    }, error = function(e) {
      analysis_message(paste("Error al escribir el escenario:", err_chain(e)))
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
          custom_diff_params = capability_pert_params_effective(
            caps_module$capability_pert_params())
        )
        incProgress(0.9, detail = "Completado")
        analysis_results(res)
        # Mensaje diferenciado: si la plantilla sigue vacía, avisar en lugar
        # de fingir un análisis con resultados. Si hay controles referenciados
        # que no existen en el catálogo, avisarlo con claridad.
        if (nrow(res$missing_capabilities) > 0) {
          det <- paste(sprintf("%s → %s", res$missing_capabilities$scenario_id,
                               res$missing_capabilities$capability_id),
                       collapse = "; ")
          analysis_message(sprintf(
            "Aviso: %d control(es) referenciado(s) no existen en el catálogo: %s. Créalos en 'Controles de Seguridad' o corrígelos en survey.xlsx.",
            nrow(res$missing_capabilities), det))
        } else if (nrow(res$scenario_summary) == 0) {
          analysis_message("Análisis completado: no hay escenarios cargados en survey.xlsx. Añade escenarios para ver resultados.")
        } else {
          analysis_message(sprintf("Análisis completado. Resultados en: %s", res$results_dir))
        }
      }, error = function(e) {
        analysis_message(paste("Error en análisis:", err_chain(e)))
      })
    })
  })

  observeEvent(input$btn_run_mitigation, {
    mitigation_results(NULL)
    req(analysis_results())
    req(nrow(analysis_results()$scenario_summary) > 0)
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
          # Costos y efectividad efectivos: persistidos en custom_capabilities.csv
          # + los configurados ahora en el módulo (el módulo tiene prioridad).
          custom_diff_params = capability_pert_params_effective(
            caps_module$capability_pert_params()),
          control_costs = control_costs_effective(
            caps_module$control_costs())
        )
        incProgress(0.9, detail = "Completado")
        mitigation_results(mit)
        # Guardar análisis + mitigación en caché para no re-simular al abrir
        guardar_cache_analisis(main, input$iterations, mitigacion = mit)
        if (!is.null(main$missing_capabilities) &&
            nrow(main$missing_capabilities) > 0) {
          analysis_message(sprintf(
            "Mitigación completada con aviso: %d control(es) no existen en el catálogo y se ignoraron. Créalos en 'Controles de Seguridad' o corrígelos en survey.xlsx.",
            nrow(main$missing_capabilities)))
        } else {
          analysis_message("Análisis de mitigación completado.")
        }
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

  # --- Respaldo de controles propios (custom_capabilities.csv) --------------
  output$dl_custom_capabilities <- downloadHandler(
    filename = function() "custom_capabilities.csv",
    content = function(file) {
      # Siempre escribe un CSV válido (con encabezados) aunque no exista aún
      readr::write_csv(read_custom_capabilities(), file)
    }
  )

  # Importar un CSV de controles propios: fusiona (upsert por capability_id)
  # con los existentes y refresca el selector del módulo.
  observeEvent(input$upload_custom_capabilities, {
    tryCatch({
      req(input$upload_custom_capabilities)
      path <- input$upload_custom_capabilities$datapath
      df <- readr::read_csv(path,
                            col_types = readr::cols(.default = readr::col_character()))
      if (!all(c("capability_id", "capability") %in% names(df))) {
        stop("El CSV debe tener las columnas 'capability_id' y 'capability'.")
      }
      # Normalizar al esquema actual: cost, eff_min, eff_mode y eff_max son
      # opcionales (se conservan si vienen en el CSV de respaldo).
      for (col in c("cost", "eff_min", "eff_mode", "eff_max")) {
        if (!col %in% names(df)) df[[col]] <- NA_character_
      }
      df <- df[, c("capability_id", "capability", "cost", "eff_min",
                   "eff_mode", "eff_max"), drop = FALSE] |>
        dplyr::mutate(
          cost = suppressWarnings(as.numeric(.data$cost)),
          eff_min = suppressWarnings(as.numeric(.data$eff_min)),
          eff_mode = suppressWarnings(as.numeric(.data$eff_mode)),
          eff_max = suppressWarnings(as.numeric(.data$eff_max))
        )
      existing <- read_custom_capabilities()
      merged <- existing[!existing$capability_id %in% df$capability_id, , drop = FALSE]
      merged <- rbind(merged, df)
      readr::write_csv(merged, custom_capabilities_path())
      custom_caps_refresh(custom_caps_refresh() + 1)
      # Aviso claro si el CSV no trae costos: la optimización los necesita y
      # los vería en 0 (ROSI global N/D).
      n_cost <- sum(!is.na(merged$cost))
      if (n_cost == 0) {
        msg_imp <- sprintf(
          "Controles propios importados: %d, pero el CSV NO trae costos (columna 'cost' ausente o vacía). La optimización los verá en 0; configura los costos en el módulo o corrige el CSV.",
          nrow(df))
        analysis_message(msg_imp)
        showNotification(msg_imp, type = "warning", duration = 10)
      } else {
        msg_imp <- sprintf("Controles propios importados: %d (total %d, %d con costo).",
                           nrow(df), nrow(merged), n_cost)
        analysis_message(msg_imp)
        showNotification(msg_imp, type = "message")
      }
    }, error = function(e) {
      analysis_message(paste("Error al importar controles propios:", err_chain(e)))
      showNotification(paste("Error al importar controles propios:",
                             conditionMessage(e)), type = "error")
    })
  })

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

  # --- KPI value boxes (formato compacto K/M/B) -----------------------------
  # Con 0 escenarios los req() dejan la tarjeta en blanco (nunca "$0").
  output$kpi_ale_median <- renderText({
    req(analysis_results())
    req(nrow(analysis_results()$scenario_summary) > 0)
    fmt_compact_money(sum(analysis_results()$scenario_summary$ale_median, na.rm = TRUE))
  })

  output$kpi_ale_max <- renderText({
    req(analysis_results())
    req(nrow(analysis_results()$scenario_summary) > 0)
    fmt_compact_money(max(analysis_results()$scenario_summary$ale_max, na.rm = TRUE))
  })

  output$kpi_top_scenario <- renderText({
    req(analysis_results())
    req(nrow(analysis_results()$scenario_summary) > 0)
    ss <- analysis_results()$scenario_summary
    ss$scenario_id[which.max(ss$ale_median)]
  })

  output$kpi_top_tcomm <- renderText({
    req(analysis_results())
    req(nrow(analysis_results()$scenario_summary) > 0)
    ss <- analysis_results()$scenario_summary
    agg <- ss |>
      dplyr::group_by(tcomm) |>
      dplyr::summarise(total = sum(ale_median, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total))
    agg$tcomm[1]
  })

  # --- Módulo de Threat Community (agregación por comunidad de amenazas) ---
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
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_plot(plot_style()))
    }
    d <- tcomm_sum()
    req(nrow(d) > 0)
    palette <- c("#3b82f6", "#f97316", "#22c55e", "#f59e0b", "#8b5cf6",
                 "#ec4899", "#06b6d4", "#ef4444", "#14b8a6", "#a3a3a3")
    p <- plotly::plot_ly(
      d, labels = ~tcomm, values = ~ale_median, type = "pie", hole = 0.55,
      # textinfo = "none": la leyenda (abajo) describe cada grupo; evitar
      # etiquetas de % encimadas sobre la dona
      textinfo = "none",
      marker = list(colors = palette[seq_len(min(nrow(d), length(palette)))],
                    line = list(color = "rgba(0,0,0,0.25)", width = 1)),
      hovertemplate = "%{label}<br>ALE Mediana: %{value:$,.0f} (%{percent})<extra></extra>"
    )
    # Leyenda horizontal debajo (no se solapa con la dona)
    dash_layout(p, plot_style(), legend_pos = "bottom", margin_l = 20)
  })

  output$tcomm_bars <- renderPlotly({
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_plot(plot_style()))
    }
    d <- tcomm_sum()
    req(nrow(d) > 0)
    long <- d |>
      tidyr::pivot_longer(c(ale_median, ale_max), names_to = "metric", values_to = "ale") |>
      dplyr::mutate(metric = ifelse(metric == "ale_median", "ALE Mediana", "ALE Máximo"))
    p <- plotly::plot_ly(
      long, x = ~ale, y = ~tcomm, color = ~metric, type = "bar", orientation = "h",
      colors = c("#3b82f6", "#f97316"),
      hovertemplate = "%{y}<br>%{fullData.name}: %{x:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(barmode = "group",
                     xaxis = list(title = "ALE (USD)", tickprefix = "$"),
                     yaxis = list(title = ""))
    # Leyenda arriba + margen izquierdo amplio para nombres largos de TComm
    dash_layout(p, plot_style(), legend_pos = "top", margin_l = 150)
  })

  output$tcomm_table <- renderDT({
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_table())
    }
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
      DT::formatStyle("ale_max", background = DT::styleColorBar(range(d$ale_max), "#3b82f6"))
  })

  # --- Resultados por Escenario ---
  output$analysis_message_scenarios <- renderText({
    if (is.null(analysis_results())) "Ejecuta el análisis primero." else ""
  })

  output$scenario_summary_table <- renderDT({
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_table())
    }
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
          autoWidth = TRUE, deferRender = TRUE,
          # Columna 1 (Descripción): wrap + max-width para textos largos
          columnDefs = list(list(targets = 1, className = "dt-desc"))
        ),
        colnames = c("ID", "Descripción", "TComm", "Dominio", "ALE Mediana", "ALE Máximo",
                     "VaR 95%", "Eventos de Pérdida (media)", "Vulnerabilidad")
      ) |>
      DT::formatCurrency(c("ale_median", "ale_max", "ale_var"), currency = "$", digits = 0) |>
      DT::formatPercentage("mean_vuln", 1) |>
      DT::formatStyle("ale_median", background = DT::styleColorBar(range(res$ale_median), "#0ea5e9")) |>
      DT::formatStyle("ale_max", background = DT::styleColorBar(range(res$ale_max), "#3b82f6"))
  })

  output$scenario_selector_ui <- renderUI({
    req(analysis_results())
    # Sin escenarios no hay nada que seleccionar; el gráfico mostrará el
    # mensaje de espera ("Esperando ingreso de escenarios...").
    req(nrow(analysis_results()$scenario_summary) > 0)
    ids <- analysis_results()$scenario_summary$scenario_id
    selectInput("selected_scenario", "Seleccionar escenario", choices = ids)
  })

  output$scenario_scatter_plot <- renderPlotly({
    # Estado vacío ANTES de exigir el selector: con 0 escenarios se muestra
    # el mensaje "Esperando ingreso de escenarios..." en lugar de un lienzo
    # en blanco.
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_plot(plot_style()))
    }
    req(input$selected_scenario)
    sim <- analysis_results()$simulation_results
    all_results <- tidyr::unnest(sim, results)
    dat <- all_results |> dplyr::filter(scenario_id == input$selected_scenario)
    req(nrow(dat) > 0)

    p <- plotly::plot_ly(
      dat, x = ~loss_events, y = ~ale, type = "scatter", mode = "markers",
      marker = list(color = "rgba(59,130,246,0.5)", size = 6),
      hovertemplate = "Eventos: %{x:,}<br>ALE: %{y:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(xaxis = list(title = "Eventos de pérdida (anualizado)"),
                     yaxis = list(title = "ALE (USD)", tickprefix = "$"))
    dash_layout(p, plot_style())
  })

  # --- Resultados por Dominio ---
  output$domain_summary_table <- renderDT({
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_table())
    }
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
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_plot(plot_style()))
    }
    dom <- analysis_results()$domain_summary
    p <- plotly::plot_ly(
      dom, x = ~ale_median, y = ~stats::reorder(domain_id, ale_median),
      type = "bar", orientation = "h",
      marker = list(
        color = ~ale_median,
        colorscale = list(c(0, "#3b82f6"), c(0.5, "#60a5fa"), c(1, "#f97316")),
        line = list(color = "rgba(0,0,0,0.2)", width = 1)
      ),
      hovertemplate = "%{y}<br>ALE Mediana: %{x:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(xaxis = list(title = "ALE Mediana (USD)", tickprefix = "$"),
                     yaxis = list(title = ""))
    dash_layout(p, plot_style(), margin_l = 70)
  })

  # --- Curva de excedencia (LEC): probabilidad de pérdida >= X ---
  output$exceedance_scenario_selector_ui <- renderUI({
    req(analysis_results())
    req(nrow(analysis_results()$scenario_summary) > 0)
    ids <- analysis_results()$scenario_summary$scenario_id
    selectInput("exceedance_scenario", "Seleccionar escenario", choices = ids)
  })

  output$exceedance_plot <- renderPlotly({
    # Estado vacío ANTES de exigir el selector (ver scenario_scatter_plot)
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_plot(plot_style()))
    }
    req(input$exceedance_scenario)
    sim <- analysis_results()$simulation_results
    all_results <- tidyr::unnest(sim, results)
    dat <- all_results |> dplyr::filter(scenario_id == input$exceedance_scenario,
                                        is.finite(.data$ale))
    # Si la simulación de este escenario quedó sin valores finitos (p. ej.
    # parámetros de distribución inválidos), mostrar mensaje en lugar de
    # marcadores P10/P50/P90 con NA.
    if (nrow(dat) == 0) {
      return(empty_plotly("Sin valores finitos de pérdida para este escenario",
                          plot_style()))
    }

    exc <- dat |>
      dplyr::arrange(ale) |>
      dplyr::mutate(prob = 1 - dplyr::percent_rank(ale))

    p <- plotly::plot_ly(
      exc, x = ~prob, y = ~ale, type = "scatter", mode = "lines",
      line = list(color = "#3b82f6", width = 3),
      fill = "tozeroy", fillcolor = "rgba(59,130,246,0.15)",
      hovertemplate = "Probabilidad ≥ pérdida: %{x:.1%}<br>ALE: %{y:$,.0f}<extra></extra>"
    )

    # --- Apetito al riesgo: línea punteada + sombreado rojo del gap ----------
    appetite <- if (is.null(input$appetite_loss)) 0 else input$appetite_loss
    if (!is.null(appetite) && is.finite(appetite) && appetite > 0 && any(exc$ale > appetite)) {
      exceed <- exc[exc$ale > appetite, ]
      p <- p |>
        plotly::add_segments(
          x = 0, xend = 1, y = appetite, yend = appetite,
          line = list(color = "#f97316", dash = "dash", width = 2),
          showlegend = TRUE, name = "Apetito al riesgo",
          hovertemplate = paste0("Apetito: ", scales::dollar(appetite, accuracy = 0), "<extra></extra>")
        ) |>
        # Polígono del gap (área donde la curva supera el apetito)
        plotly::add_trace(
          x = c(1, exceed$prob, min(exceed$prob)),
          y = c(appetite, exceed$ale, appetite),
          type = "scatter", mode = "lines",
          fill = "toself", fillcolor = "rgba(239,68,68,0.30)",
          line = list(color = "transparent"),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::add_annotations(
          x = 0.03, y = max(exc$ale),
          text = sprintf("Gap de riesgo: pérdida > %s con probabilidad ~%.0f%%",
                         scales::dollar(appetite, accuracy = 0),
                         100 * mean(exc$ale > appetite)),
          showarrow = FALSE, font = list(size = 11, color = "#f87171"),
          xanchor = "left"
        )
    }

    # Marcadores de percentiles P10/P50/P90 (en probabilidad de excedencia):
    # líneas punteadas verticales con la pérdida correspondiente.
    quant <- stats::quantile(dat$ale, probs = c(0.9, 0.5, 0.1), na.rm = TRUE)
    pcts <- c("P10" = 0.9, "P50" = 0.5, "P90" = 0.1)
    for (i in seq_along(pcts)) {
      nm <- names(pcts)[i]
      pr <- pcts[[i]]
      val <- quant[[i]]
      if (!is.finite(val)) next  # sin valor finito: no dibujar el marcador
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

  # --- Análisis de mitigación (inherente vs residual + ROSI) ---
  output$mitigation_message <- renderText({
    if (is.null(mitigation_results())) "Ejecuta el análisis de mitigación para ver la efectividad de los controles." else ""
  })

  output$mit_inherent <- renderText({
    req(mitigation_results())
    req(nrow(mitigation_results()$scenario_level) > 0)
    fmt_compact_money(sum(mitigation_results()$scenario_level$ale_inherent, na.rm = TRUE))
  })

  output$mit_residual <- renderText({
    req(mitigation_results())
    req(nrow(mitigation_results()$scenario_level) > 0)
    fmt_compact_money(sum(mitigation_results()$scenario_level$ale_residual, na.rm = TRUE))
  })

  output$mit_reduction <- renderText({
    req(mitigation_results())
    req(nrow(mitigation_results()$scenario_level) > 0)
    sl <- mitigation_results()$scenario_level
    tot_inh <- sum(sl$ale_inherent, na.rm = TRUE)
    tot_res <- sum(sl$ale_residual, na.rm = TRUE)
    pct <- if (tot_inh > 0) (tot_inh - tot_res) / tot_inh else 0
    scales::percent(pct, accuracy = 0.1)
  })

  # --- KPIs ROSI (Ahorro Neto y retorno de la inversión en seguridad) ---
  output$kpi_net_savings <- renderText({
    req(mitigation_results())
    req(nrow(mitigation_results()$scenario_level) > 0)
    sl <- mitigation_results()$scenario_level
    fmt_compact_money(sum(sl$net_savings, na.rm = TRUE))
  })

  output$kpi_rosi <- renderText({
    req(mitigation_results())
    req(nrow(mitigation_results()$scenario_level) > 0)
    sl <- mitigation_results()$scenario_level
    tot_cost <- sum(sl$control_cost, na.rm = TRUE)
    tot_saved <- sum(sl$amount_saved, na.rm = TRUE)
    if (tot_cost > 0) {
      sprintf("%.1f%%", (tot_saved - tot_cost) / tot_cost * 100)
    } else {
      "N/D"
    }
  })

  output$mit_bars <- renderPlotly({
    if (is.null(mitigation_results()) || nrow(mitigation_results()$scenario_level) == 0) {
      return(no_scenarios_plot(plot_style()))
    }
    sl <- mitigation_results()$scenario_level |>
      dplyr::arrange(dplyr::desc(ale_inherent)) |>
      utils::head(15)
    long <- sl |>
      tidyr::pivot_longer(c(ale_inherent, ale_residual), names_to = "tipo", values_to = "ale") |>
      dplyr::mutate(tipo = ifelse(tipo == "ale_inherent", "Inherente (sin controles)", "Residual (con controles)"))
    p <- plotly::plot_ly(
      long, x = ~ale, y = ~scenario_id, color = ~tipo, type = "bar", orientation = "h",
      colors = c("#f97316", "#22c55e"),
      hovertemplate = "%{y}<br>%{fullData.name}: %{x:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(barmode = "group",
                     xaxis = list(title = "ALE (USD)", tickprefix = "$"),
                     yaxis = list(title = ""))
    dash_layout(p, plot_style(), legend_pos = "top", margin_l = 90)
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
      # Nombre corto en español (capability_es del sidecar) para la UI
      dplyr::mutate(capability_es = capability_es_name(.data$capability_id)) |>
      dplyr::arrange(dplyr::desc(total_savings))
  })

  output$mit_control_bars <- renderPlotly({
    if (is.null(mitigation_results()) || nrow(mitigation_results()$control_level) == 0) {
      return(no_scenarios_plot(plot_style()))
    }
    cs <- control_sum()
    req(nrow(cs) > 0)
    p <- plotly::plot_ly(
      cs, x = ~total_savings, y = ~stats::reorder(capability_es, total_savings),
      type = "bar", orientation = "h",
      marker = list(
        color = ~total_savings,
        colorscale = list(c(0, "#22c55e"), c(1, "#3b82f6")),
        line = list(color = "rgba(0,0,0,0.2)", width = 1)
      ),
      hovertemplate = "%{y}<br>Ahorro: %{x:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(xaxis = list(title = "Ahorro total (USD)", tickprefix = "$"),
                     yaxis = list(title = ""))
    dash_layout(p, plot_style(), margin_l = 220)
  })

  output$mit_control_table <- renderDT({
    if (is.null(mitigation_results()) || nrow(mitigation_results()$control_level) == 0) {
      return(no_scenarios_table())
    }
    cs <- control_sum()
    req(nrow(cs) > 0)
    cs |>
      dplyr::select(capability_id, capability_es, n_scenarios, total_savings, mean_reduction) |>
      DT::datatable(
        rownames = FALSE,
        options = list(pageLength = 10, dom = "Bfrtip", scrollX = TRUE),
        colnames = c("Capability", "Control (español)", "Escenarios", "Ahorro Total", "Reducción media")
      ) |>
      DT::formatCurrency("total_savings", currency = "$", digits = 0) |>
      DT::formatPercentage("mean_reduction", 1) |>
      DT::formatStyle("total_savings", background = DT::styleColorBar(range(cs$total_savings), "#10b981"))
  })

  # --- Optimización de controles por escenario (mochila / ROSI) ---------------
  optimization_results <- reactiveVal(NULL)

  observeEvent(input$btn_run_opt, {
    optimization_results(NULL)
    req(analysis_results())
    req(nrow(analysis_results()$scenario_summary) > 0)
    req(mitigation_results())
    tryCatch({
      budget <- if (is.null(input$opt_budget)) 0 else input$opt_budget
      mode <- if (is.null(input$opt_mode)) "per_scenario" else input$opt_mode
      costs <- control_costs_effective(caps_module$control_costs())
      if (length(costs) == 0) {
        analysis_message("No hay costos definidos. Configura costos en el módulo Controles de Seguridad o importa un custom_capabilities.csv con la columna 'cost'.")
        return()
      }
      cl <- mitigation_results()$control_level
      if (identical(mode, "global")) {
        # Presupuesto GLOBAL: reparte el monto entre todos los escenarios (MCKP)
        opt <- optimize_controls_global(cl, costs, budget)
      } else {
        # Presupuesto POR ESCENARIO: mochila independiente en cada escenario
        bs <- optimize_controls_rosi(cl, costs, budget)
        opt <- list(
          by_scenario = bs,
          totals = list(
            cost = sum(bs$opt_cost),
            savings = sum(bs$opt_savings),
            net = sum(bs$opt_net),
            rosi = if (sum(bs$opt_cost) > 1e-9) sum(bs$opt_net) / sum(bs$opt_cost) else NA_real_,
            n_scenarios = nrow(bs)
          )
        )
      }
      optimization_results(opt)
      analysis_message(sprintf(
        "Optimización completada: costo óptimo %s, ahorro neto %s%s",
        fmt_compact_money(opt$totals$cost),
        fmt_compact_money(opt$totals$net),
        if (is.na(opt$totals$rosi)) " (ROSI N/D: costos en 0)" else
          sprintf(", ROSI global %.0f%%", 100 * opt$totals$rosi)))
    }, error = function(e) {
      analysis_message(paste("Error en optimización:", err_chain(e)))
    })
  })

  output$opt_message <- renderText({
    opt <- optimization_results()
    if (is.null(opt)) {
      "Ejecuta primero 'Ejecutar análisis' y 'Ejecutar análisis de mitigación', define los costos de los controles y pulsa 'Ejecutar optimización'."
    } else if (nrow(opt$by_scenario) == 0) {
      "No hay escenarios con controles costeados para optimizar. Configura controles con costo en el módulo Controles de Seguridad."
    } else if (is.null(opt$totals) || opt$totals$cost < 1e-9) {
      "Costo total $0: ningún control superó su costo con los beneficios estimados. Revisa los costos y la efectividad de los controles."
    } else {
      ""
    }
  })

  # Resumen de totales (presupuesto usado, ahorro neto y ROSI agregado)
  output$opt_totals <- renderText({
    opt <- optimization_results()
    if (is.null(opt) || nrow(opt$by_scenario) == 0) return("")
    t <- opt$totals
    n_scen <- t$n_scenarios
    n_scen_txt <- if (length(n_scen) == 0 || is.na(n_scen)) "—" else as.character(n_scen)
    sprintf("Costo total: %s | Ahorro neto total: %s | ROSI global: %s | Escenarios optimizados: %s",
            fmt_compact_money(t$cost), fmt_compact_money(t$net),
            if (is.na(t$rosi)) "N/D" else sprintf("%.1f%%", t$rosi * 100),
            n_scen_txt)
  })

  output$opt_table <- renderDT({
    opt <- optimization_results()
    if (is.null(opt) || nrow(opt$by_scenario) == 0) {
      return(DT::datatable(
        data.frame(Info = "Ejecuta la optimización para ver los controles óptimos por escenario."),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 1, ordering = FALSE,
                       searching = FALSE)
      ))
    }
    opt$by_scenario |>
      dplyr::select(scenario_id, control_names, opt_cost, opt_savings,
                    opt_net, opt_rosi) |>
      DT::datatable(
        rownames = FALSE,
        options = list(pageLength = 10, dom = "Bfrtip", scrollX = TRUE),
        colnames = c("Escenario", "Controles óptimos", "Costo",
                     "Ahorro esperado", "Ahorro neto", "ROSI")
      ) |>
      DT::formatCurrency(c("opt_cost", "opt_savings", "opt_net"),
                         currency = "$", digits = 0) |>
      DT::formatPercentage("opt_rosi", 1)
  })

  # --- Sensibilidad y convergencia de Monte Carlo -----------------------------
  convergence_check <- reactiveVal(NULL)
  sensitivity_results <- reactiveVal(NULL)

  observeEvent(input$btn_run_sensitivity, {
    req(analysis_results())
    req(nrow(analysis_results()$scenario_summary) > 0)
    withProgress(message = "Ejecutando análisis de sensibilidad...", value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Verificando convergencia")
        conv <- mc_convergence_check(analysis_results()$simulation_results,
                                     input$iterations)
        convergence_check(conv)

        incProgress(0.5, detail = "Simulando tornado (±20% en TEF/TC/LM)")
        ss <- analysis_results()$scenario_summary
        top_id <- ss$scenario_id[which.max(ss$ale_median)]
        scen <- analysis_results()$qualitative_scenarios |>
          dplyr::filter(scenario_id == top_id)
        req(nrow(scen) > 0)
        tor <- run_sensitivity_tornado(
          scen,
          analysis_results()$capabilities,
          analysis_results()$mappings,
          iterations = input$iterations
        )
        sensitivity_results(list(tornado = tor, scenario_id = top_id))
        analysis_message("Análisis de sensibilidad completado.")
      }, error = function(e) {
        analysis_message(paste("Error en sensibilidad:", err_chain(e)))
      })
    })
  })

  output$convergence_message <- renderUI({
    conv <- convergence_check()
    if (is.null(conv)) return(NULL)
    cls <- if (conv$low) "alert alert-warning" else "alert alert-success"
    tags$div(class = cls, style = "font-size:0.9rem;",
             tags$strong("Convergencia: "), conv$message)
  })

  output$convergence_table <- renderDT({
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_table())
    }
    conv <- convergence_check()
    req(conv)
    conv$table |>
      dplyr::select(scenario_id, domain_id, ale_median, moe, moe_pct) |>
      DT::datatable(
        rownames = FALSE,
        options = list(pageLength = 8, dom = "ft", scrollX = TRUE),
        colnames = c("Escenario", "Dominio", "ALE Mediana", "Error Est. Mediana", "Error %")
      ) |>
      DT::formatCurrency(c("ale_median", "moe"), currency = "$", digits = 0) |>
      DT::formatPercentage("moe_pct", 1)
  })

  output$sensitivity_title <- renderText({
    res <- sensitivity_results()
    if (is.null(res)) "" else
      sprintf("Tornado de sensibilidad — escenario: %s (±20%% en TEF / TC / LM)", res$scenario_id)
  })

  output$sensitivity_tornado_plot <- renderPlotly({
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(no_scenarios_plot(plot_style()))
    }
    res <- sensitivity_results()
    req(res)
    tor <- res$tornado
    long <- tor |>
      tidyr::pivot_longer(c(low, high), names_to = "dir", values_to = "ale") |>
      dplyr::mutate(dir = ifelse(dir == "low", "-20%", "+20%"))
    p <- plotly::plot_ly(
      long, x = ~ale, y = ~variable, color = ~dir, type = "bar", orientation = "h",
      colors = c("#f97316", "#3b82f6"),
      hovertemplate = "%{y}<br>%{fullData.name}: %{x:$,.0f}<extra></extra>"
    ) |>
      plotly::layout(barmode = "overlay",
                     xaxis = list(title = "ALE (USD)", tickprefix = "$"),
                     yaxis = list(title = ""))
    dash_layout(p, plot_style(), legend_pos = "top", margin_l = 60)
  })

  # --- Narrativa ejecutiva (Copilot de IA) -----------------------------------
  # KPIs reutilizables (narrativa determinista + prompt de IA local/Ollama).
  # Sin escenarios el req() silencia la salida; los renders con estado vacío
  # muestran el mensaje "Esperando ingreso de escenarios...".
  kpis_for_narrative <- reactive({
    req(analysis_results())
    ss <- analysis_results()$scenario_summary
    req(nrow(ss) > 0)
    agg <- ss |>
      dplyr::group_by(tcomm) |>
      dplyr::summarise(total = sum(ale_median, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total))

    kpis <- list(
      total_ale = sum(ss$ale_median, na.rm = TRUE),
      var95 = max(ss$ale_var, na.rm = TRUE),
      top_tcomm = agg$tcomm[1],
      top_scenario = ss$scenario_id[which.max(ss$ale_median)],
      reduction_pct = 0,
      net_savings = 0,
      rosi = NA_real_,
      top_control = "los controles de seguridad",
      top_controls = NULL
    )
    if (!is.null(mitigation_results())) {
      sl <- mitigation_results()$scenario_level
      cl <- mitigation_results()$control_level
      tot_inh <- sum(sl$ale_inherent, na.rm = TRUE)
      tot_res <- sum(sl$ale_residual, na.rm = TRUE)
      tot_cost <- sum(sl$control_cost, na.rm = TRUE)
      kpis$reduction_pct <- if (tot_inh > 0) (tot_inh - tot_res) / tot_inh else 0
      kpis$net_savings <- sum(sl$net_savings, na.rm = TRUE)
      kpis$rosi <- if (tot_cost > 0) (sum(sl$amount_saved, na.rm = TRUE) - tot_cost) / tot_cost * 100 else NA_real_
      # Control con mayor ahorro marginal (nombre en español para la narrativa)
      if (nrow(cl) > 0) {
        top_id <- cl$capability_id[which.max(cl$marginal_savings)]
        kpis$top_control <- capability_es_name(top_id)
      }
      # Bloque de controles en español (nombre + descripción ejecutiva) para
      # los prompts del Copilot (Ollama).
      kpis$top_controls <- top_controls_text(cl)
    }
    kpis
  })

  output$exec_narrative <- renderUI({
    # Sin escenarios no hay narrativa posible: se muestra el mensaje de espera
    if (is.null(analysis_results()) || nrow(analysis_results()$scenario_summary) == 0) {
      return(tags$div(class = "exec-narrative text-muted",
                      "Esperando ingreso de escenarios..."))
    }
    tags$div(class = "exec-narrative", HTML(generate_executive_narrative(kpis_for_narrative())))
  })

  # --- IA LOCAL con Ollama (módulo opcional, no rompe la app) ---------------
  # Se comprueba al iniciar la sesión: si el módulo está activo el botón se
  # habilita; si no, se muestra deshabilitado con un badge informativo.
  ollama_active <- reactiveVal({
    ok <- check_ollama_status()
    # Log de diagnóstico (visible en docker logs): ayuda a ver si el problema
    # es la variable de entorno o la conexión al servicio.
    message(sprintf(
      "Estado IA local: ENABLE_OLLAMA=%s, servicio Ollama activo=%s",
      Sys.getenv("ENABLE_OLLAMA", unset = "<vacío>"), ok))
    ok
  })

  output$ollama_status_badge <- renderUI({
    if (ollama_active()) {
      tags$span(class = "badge text-bg-success", "IA local: activa")
    } else {
      tags$span(class = "badge text-bg-secondary", "IA local: inactiva")
    }
  })

  output$ai_button_ui <- renderUI({
    if (ollama_active()) {
      actionButton("btn_ai_copilot", "🤖 Generar Resumen con IA",
                   class = "btn-primary", icon = vb_icon("robot"))
    } else {
      tagList(
        tags$span(class = "badge text-bg-secondary mb-2",
                  style = "font-size:0.75rem;",
                  "IA local inactiva · la app opera en modo normal"),
        actionButton("btn_ai_copilot", "🤖 Generar Resumen con IA",
                     class = "btn-secondary", disabled = TRUE)
      )
    }
  })

  ai_narrative <- reactiveVal(NULL)

  observeEvent(input$btn_ai_copilot, {
    ai_narrative(NULL)
    req(analysis_results())
    withProgress(message = "Generando resumen con IA local (Ollama)...", value = 0.3, {
      txt <- generar_resumen_ia(kpis_for_narrative())
      ai_narrative(txt)
    })
  })

  output$ai_narrative_output <- renderUI({
    txt <- ai_narrative()
    if (is.null(txt)) return(NULL)
    paragraphs <- strsplit(txt, "\n\n")[[1]]
    body <- paste(sprintf("<p>%s</p>", paragraphs), collapse = "")
    tags$div(class = "ai-narrative",
      tags$div(class = "ai-narrative-header",
               vb_icon("robot"), "Resumen ejecutivo generado con IA local · Ollama"),
      tags$div(class = "ai-narrative-body", HTML(body))
    )
  })

  # --- Chat / Copilot contextual (Ollama) -------------------------------------
  # Historial de la conversación: lista de list(role = c("user","assistant"), content)
  chat_history <- reactiveVal(list())
  chat_busy <- reactiveVal(FALSE)

  # KPIs contextuales para el chat (KPIs + LEF media)
  datos_dashboard_chat <- reactive({
    req(analysis_results())
    k <- kpis_for_narrative()
    ss <- analysis_results()$scenario_summary
    k$lef <- sum(ss$loss_events_mean, na.rm = TRUE)
    k$n_scenarios <- nrow(ss)
    k
  })

  observeEvent(input$btn_send, {
    # Red de seguridad TOTAL: cualquier error inesperado se muestra como
    # burbuja del chat y NUNCA tumba la sesión del servidor.
    tryCatch({
      pregunta <- if (is.null(input$chat_input)) "" else trimws(input$chat_input)
      if (!nzchar(pregunta)) return()
      updateTextInput(session, "chat_input", value = "")

      hist <- chat_history()
      chat_history(c(hist, list(list(role = "user", content = pregunta))))
      chat_busy(TRUE)
      # NOTA: type solo admite "default", "message", "warning" o "error"
      showNotification("Ollama procesando...", id = "ollama_loading",
                       duration = NULL, type = "message")

      # Guard: sin análisis ejecutado (o sin escenarios) no hay contexto real;
      # en lugar de enviar N/A al modelo (que responde genérico), avisamos.
      if (is.null(analysis_results()) ||
          nrow(analysis_results()$scenario_summary) == 0) {
        hint <- paste0(
          "Todavía no hay escenarios cargados. Añade escenarios en el panel ",
          "lateral y ejecuta 'Ejecutar análisis' (y después 'Ejecutar análisis ",
          "de mitigación') para que el Copilot tenga contexto del dashboard ",
          "(ALE, P90, ROSI, controles...).")
        chat_history(c(chat_history(), list(list(role = "assistant", content = hint))))
        chat_busy(FALSE)
        removeNotification("ollama_loading")
        return()
      }

      # Capturamos el contexto ANTES de lanzar el future (los reactives no se
      # pueden leer dentro de un future).
      ctx <- tryCatch(datos_dashboard_chat(), error = function(e) {
        list(total_ale = NA_real_, var95 = NA_real_, lef = NA_real_,
             top_tcomm = "N/D", top_scenario = "N/D",
             reduction_pct = NA_real_, net_savings = 0, rosi = NA_real_)
      })

      append_assistant <- function(txt) {
        chat_history(c(chat_history(), list(list(role = "assistant", content = txt))))
      }

      run_sync <- function() {
        resp <- tryCatch(preguntar_a_ollama(pregunta, ctx, hist),
                         error = function(e) {
                           message("Error en chat: ", conditionMessage(e))
                           "Ocurrió un error al consultar a Ollama. Inténtalo de nuevo."
                         })
        append_assistant(resp)
        chat_busy(FALSE)
        removeNotification("ollama_loading")
      }

      if (isTRUE(getOption("app.chat_async", TRUE)) &&
          requireNamespace("future", quietly = TRUE) &&
          requireNamespace("promises", quietly = TRUE)) {
        # ASYNC: la sesión NO se bloquea mientras Ollama responde. Si la
        # construcción del future falla en el entorno (p. ej. en Docker),
        # se cae a la vía síncrona automáticamente.
        tryCatch({
          promises::future_promise(preguntar_a_ollama(pregunta, ctx, hist)) |>
            promises::then(
              onFulfilled = function(resp) {
                try(append_assistant(resp), silent = TRUE)
              },
              onRejected = function(err) {
                message("Error en chat: ", conditionMessage(err))
                try(append_assistant("Ocurrió un error al consultar a Ollama. Inténtalo de nuevo."), silent = TRUE)
              }
            ) |>
            promises::finally(~{
              chat_busy(FALSE)
              removeNotification("ollama_loading")
            })
        }, error = function(e) {
          message("Chat async no disponible, usando síncrono: ", conditionMessage(e))
          run_sync()
        })
      } else {
        # SINCRONO (sin future/promises): simple y a prueba de fallos
        run_sync()
      }
    }, error = function(e) {
      # Última red de seguridad: nunca tumba la sesión. Muestra el error REAL
      # en la burbuja para facilitar el diagnóstico en los logs.
      message("Error crítico en chat: ", conditionMessage(e))
      chat_busy(FALSE)
      try(removeNotification("ollama_loading"), silent = TRUE)
      msg <- sprintf("Ocurrió un error en el chat: %s. Inténtalo de nuevo.",
                     conditionMessage(e))
      try(chat_history(c(chat_history(), list(list(role = "assistant",
        content = msg)))), silent = TRUE)
    })
  })

  # Indicador "Ollama pensando..."
  output$chat_loading <- renderUI({
    if (chat_busy()) {
      tags$div(class = "chat-bubble chat-bubble-bot",
               tags$span(class = "chat-typing"), " Ollama pensando...")
    }
  })

  # Render de las burbujas de la conversación
  output$chat_history <- renderUI({
    hist <- chat_history()
    if (length(hist) == 0) {
      return(tags$div(class = "chat-empty",
        "Pregúntale al Copilot sobre el análisis actual (ALE, P90, ROSI, controles...). Ejecuta primero el análisis y la mitigación para obtener contexto completo."))
    }
    bubbles <- lapply(hist, function(m) {
      cls <- if (identical(m$role, "user")) "chat-bubble chat-bubble-user" else "chat-bubble chat-bubble-bot"
      tags$div(class = cls, m$content)
    })
    do.call(tagList, bubbles)
  })
}
