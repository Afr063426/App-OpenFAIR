# =============================================================================
# ui.R — Interfaz del dashboard moderno (bslib + Tailwind CSS).
# TODOS los IDs de input/output se conservan respecto a la versión anterior
# para no tocar la lógica del server.
# =============================================================================

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", href = "css/app.css")
  ),
  page_sidebar(
    title = div(class = "app-brand",
                vb_icon("shield-check"),
                div(span("Evaluador Cuantitativo de Riesgos"),
                    span(class = "app-brand-sub", "OpenFAIR · Monte Carlo"))),
    theme = app_theme,
    sidebar = sidebar(
      width = 380,
      class = "app-sidebar",
      input_dark_mode(id = "dark_mode"),
      accordion(
        id = "sidebar_accordion",
        open = "params",
        accordion_panel(
          div(class = "acc-title", vb_icon("sliders"), "Parámetros del Escenario"),
          value = "params",
          uiOutput("domain_ui"),
          tags$hr(),
          h6("Agregar dominio nuevo"),
          textInput("new_domain_id", "ID del dominio (ej. GDPR)", value = ""),
          textInput("new_domain_name", "Nombre del dominio (ej. Protección de Datos)", value = ""),
          actionButton("btn_add_domain", "Agregar dominio", class = "btn-outline-primary w-100"),
          tags$hr(),
          textInput("scenario_description", "Descripción del escenario", value = ""),
          textInput("tcomm", "Comunidad de Amenazas (TComm)", value = ""),

          # NOTA: los valores de los selectores (código a la derecha de "=")
          # se conservan en inglés porque son los que entiende el pipeline
          # OpenFAIR/evaluator (qualitative_mappings.csv). Solo la ETIQUETA
          # visible se muestra en español.
          selectInput("tef_mode", "Modo TEF",
                      choices = c("Cualitativo" = "Qualitative",
                                  "Distribución" = "Distribution",
                                  "Ajuste desde archivo" = "Fit from file",
                                  "PERT" = "PERT"),
                      selected = "Qualitative"),
          conditionalPanel(
            condition = "input.tef_mode == 'Qualitative'",
            selectInput("tef_cat", "TEF",
                        choices = c("Frecuente" = "Frequent",
                                    "Ocasional" = "Occasional",
                                    "Raro" = "Rare"),
                        selected = "Frequent")
          ),
          conditionalPanel(
            condition = "input.tef_mode == 'Distribution' || input.tef_mode == 'Fit from file'",
            tagList(
              selectInput("tef_dist", "Distribución TEF", choices = c("pois", "nbinom", "zipois", "zinegbin"), selected = "pois"),
              textInput("tef_params", "Parámetros TEF (ej. lambda=3 o size=2,mu=3)", value = "")
            )
          ),
          conditionalPanel(
            condition = "input.tef_mode == 'Fit from file'",
            # Contenedor con id para poder limpiar el fileInput tras añadir
            div(id = "tef_hist_file_container",
                fileInput("tef_hist_file", "Cargar histórico de conteos (TEF)", accept = c('.xlsx', '.xls', '.csv'))),
            # Selectores de HOJA y COLUMNA (se rellenan al subir el archivo)
            uiOutput("tef_sheet_ui"),
            uiOutput("tef_col_ui"),
            actionButton("btn_fit_tef", "Ajustar TEF desde histórico", class = "btn-outline-secondary w-100")
          ),
          conditionalPanel(
            condition = "input.tef_mode == 'PERT'",
            tagList(
              numericInput("tef_pert_min", "TEF PERT Mínimo", value = 0),
              numericInput("tef_pert_mode", "TEF PERT Moda", value = 1),
              numericInput("tef_pert_max", "TEF PERT Máximo", value = 2)
            )
          ),

          selectInput("tc_mode", "Modo TC",
                      choices = c("Cualitativo" = "Qualitative", "PERT" = "PERT"),
                      selected = "Qualitative"),
          conditionalPanel(
            condition = "input.tc_mode == 'Qualitative'",
            selectInput("tc", "TC",
                        choices = c("Alto" = "High", "Medio" = "Medium", "Bajo" = "Low"),
                        selected = "Medium")
          ),
          conditionalPanel(
            condition = "input.tc_mode == 'PERT'",
            tagList(
              numericInput("tc_pert_min", "TC PERT Mínimo", value = 0),
              numericInput("tc_pert_mode", "TC PERT Moda", value = 0.5),
              numericInput("tc_pert_max", "TC PERT Máximo", value = 1)
            )
          ),

          selectInput("lm_mode", "Modo LM",
                      choices = c("Cualitativo" = "Qualitative",
                                  "Distribución" = "Distribution",
                                  "Ajuste desde archivo" = "Fit from file",
                                  "PERT" = "PERT"),
                      selected = "Qualitative"),
          conditionalPanel(
            condition = "input.lm_mode == 'Qualitative'",
            selectInput("lm_cat", "LM",
                        choices = c("Alta" = "High", "Media" = "Medium", "Baja" = "Low"),
                        selected = "Medium")
          ),
          conditionalPanel(
            condition = "input.lm_mode == 'Distribution' || input.lm_mode == 'Fit from file'",
            tagList(
              selectInput("lm_dist", "Distribución LM", choices = c("gamma", "lnorm", "weibull", "exp", "norm", "beta", "unif", "pert"), selected = "gamma"),
              textInput("lm_params", "Parámetros LM (ej. shape=2,rate=0.5)", value = ""),
              helpText("Añade max=<monto> para limitar la pérdida máxima (ej. max=50000). Aplica a todas las distribuciones.")
            )
          ),
          conditionalPanel(
            condition = "input.lm_mode == 'Fit from file'",
            div(id = "lm_hist_file_container",
                fileInput("lm_hist_file", "Cargar histórico de pérdidas (LM)", accept = c('.xlsx', '.xls', '.csv'))),
            # Selectores de HOJA y COLUMNA (se rellenan al subir el archivo)
            uiOutput("lm_sheet_ui"),
            uiOutput("lm_col_ui"),
            actionButton("btn_fit_lm", "Ajustar LM desde histórico", class = "btn-outline-secondary w-100")
          ),
          conditionalPanel(
            condition = "input.lm_mode == 'PERT'",
            tagList(
              numericInput("lm_pert_min", "LM PERT Mínimo", value = 0),
              numericInput("lm_pert_mode", "LM PERT Moda", value = 1),
              numericInput("lm_pert_max", "LM PERT Máximo", value = 2)
            )
          ),

          textInput("scenario_id", "ID del Escenario (ScenarioID)", value = "RS-001")
        ),
        accordion_panel(
          div(class = "acc-title", vb_icon("shield-check"), "Controles de Seguridad (Capabilities)"),
          value = "capabilities",
          capabilities_ui("capabilities_mod"),
          helpText("Define la efectividad (Beta-PERT) de cada control. Se guardan en %, se convierten a 0-1 para la simulación.")
        ),
        accordion_panel(
          div(class = "acc-title", vb_icon("play-circle"), "Acciones"),
          value = "actions",
          numericInput("iterations", "Iteraciones de simulación", value = 1000, min = 100, step = 100),
          fileInput("survey_file", "Cargar encuesta (importar escenarios)", accept = c(".xlsx", ".xls", ".csv")),
          actionButton("btn_add_scenario", "Añadir escenario al survey.xlsx", class = "btn-success w-100"),
          actionButton("btn_run_analysis", "Ejecutar análisis", class = "btn-primary w-100"),
          downloadButton("dl_survey", "Descargar survey.xlsx", class = "w-100"),
          tags$hr(),
          h6("Controles propios (respaldo)"),
          helpText("Importa tu custom_capabilities.csv para restaurar los controles creados antes de cerrar el proyecto."),
          fileInput("upload_custom_capabilities", "Importar controles propios (CSV)",
                    accept = ".csv"),
          downloadButton("dl_custom_capabilities", "Descargar controles propios (CSV)",
                         class = "btn-outline-secondary w-100"),
          tags$hr(),
          actionButton("btn_reset_project", "Reiniciar proyecto", class = "btn-outline-danger w-100")
        )
      )
    ),
    navset_card_tab(
      nav_panel(
        "Proyectos",
        div(class = "dash-content",
          div(class = "dash-row",
            card(
              full_screen = FALSE,
              card_header("Proyecto actual"),
              verbatimTextOutput("proyecto_actual_label"),
              helpText("Cada proyecto tiene su propio survey.xlsx, controles propios y resultados guardados.")
            ),
            card(
              full_screen = FALSE,
              card_header("Cambiar de proyecto"),
              selectInput("select_proyecto", "Selecciona un proyecto", choices = character(0)),
              actionButton("btn_abrir_proyecto", "Abrir proyecto seleccionado",
                           class = "btn-primary w-100")
            ),
            card(
              full_screen = FALSE,
              card_header("Nuevo proyecto"),
              textInput("nuevo_proyecto_nombre", "Nombre del proyecto",
                        placeholder = "Ej: Análisis financiero 2026"),
              actionButton("btn_nuevo_proyecto", "Crear y abrir proyecto nuevo",
                           class = "btn-success w-100")
            )
          ),
          div(class = "dash-row",
            card(
              full_screen = TRUE,
              card_header("Proyectos disponibles"),
              helpText("Resultados de simulación guardados se cargan automáticamente al abrir un proyecto."),
              DTOutput("proyectos_info")
            )
          )
        )
      ),
      nav_panel(
        "Dashboard",
        div(class = "dash-content",
          # Banner de errores del análisis (visible aquí para diagnóstico)
          uiOutput("analysis_error_banner"),
          div(class = "dash-row",
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              style = "gap: 1rem;",
              kpi_card("ALE Total Mediana", "kpi_ale_median", "cash-coin", "indigo"),
              kpi_card("ALE Máximo", "kpi_ale_max", "fire", "rose"),
              kpi_card("Escenario Mayor Riesgo", "kpi_top_scenario", "exclamation-triangle-fill", "amber"),
              kpi_card("Threat Community Predominante", "kpi_top_tcomm", "people-fill", "cyan")
            )
          ),
          div(class = "dash-row",
            layout_columns(
              col_widths = c(5, 7),
              style = "gap: 1rem;",
              card(
                class = "chart-card",
                full_screen = TRUE,
                card_header("Distribución de ALE por Threat Community"),
                plotlyOutput("tcomm_donut", height = "400px")
              ),
              card(
                class = "chart-card",
                full_screen = TRUE,
                card_header("ALE Mediana vs Máximo por Threat Community"),
                plotlyOutput("tcomm_bars", height = "400px")
              )
            )
          ),
          div(class = "dash-row",
            card(
              class = "chart-card",
              full_screen = TRUE,
              card_header("Frecuencia de Pérdidas por Threat Community"),
              DTOutput("tcomm_table")
            )
          ),
          div(class = "dash-row",
            card(
              full_screen = TRUE,
              card_header("Resumen Ejecutivo · AI Copilot"),
              uiOutput("ai_button_ui"),
              htmlOutput("exec_narrative"),
              htmlOutput("ai_narrative_output")
            )
          )
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
        div(class = "dash-content",
          div(class = "dash-row",
            card_header("Resumen por Escenario"),
            verbatimTextOutput("analysis_message_scenarios"),
            downloadButton("dl_scenario_csv", "Descargar resumen escenarios (CSV)", class = "btn-download")
          ),
          # Tabla aislada en su propio contenedor: clear:both, sin floats ni
          # posicionamiento absoluto, para que ningún gráfico se superponga.
          div(class = "table-wrapper",
            DTOutput("scenario_summary_table")
          ),
          tags$hr(),
          div(class = "dash-row",
            uiOutput("scenario_selector_ui"),
            div(class = "chart-card",
              plotlyOutput("scenario_scatter_plot", height = "400px")
            )
          )
        )
      ),
      nav_panel(
        "Resultados por Dominio",
        card_header("Resumen por Dominio"),
        downloadButton("dl_domain_csv", "Descargar resumen dominios (CSV)", class = "btn-download"),
        layout_columns(
          col_widths = c(7, 5),
          card(class = "chart-card", full_screen = TRUE, card_header("ALE Mediana por Dominio"), plotlyOutput("domain_ale_bar_plot", height = "400px")),
          card(full_screen = TRUE, card_header("Resumen por Dominio"), DTOutput("domain_summary_table"))
        )
      ),
      nav_panel(
        "Curva de Excedencia",
        div(class = "dash-content",
          div(class = "dash-row",
            card_header("Loss Exceedance Curve (LEC) · Apetito al Riesgo"),
            layout_columns(
              col_widths = c(4, 4, 4),
              uiOutput("exceedance_scenario_selector_ui"),
              numericInput("appetite_loss", "Pérdida máxima aceptable ($)", value = 5000000, min = 0, step = 100000),
              helpText("La línea naranja punteada marca el apetito; el área roja es el gap donde la curva supera el límite permitido.")
            )
          ),
          div(class = "dash-row",
            div(class = "chart-card", plotlyOutput("exceedance_plot", height = "480px"))
          )
        )
      ),
      nav_panel(
        "Sensibilidad y Convergencia",
        div(class = "dash-content",
          div(class = "dash-row",
            card_header("Validación Monte Carlo"),
            actionButton("btn_run_sensitivity", "Ejecutar análisis de sensibilidad", class = "btn-primary"),
            uiOutput("convergence_message"),
            div(class = "table-wrapper", DTOutput("convergence_table"))
          ),
          div(class = "dash-row",
            textOutput("sensitivity_title"),
            div(class = "chart-card", plotlyOutput("sensitivity_tornado_plot", height = "360px"))
          )
        )
      ),
      nav_panel(
        "Efectividad de Controles",
        div(class = "dash-content",
          div(class = "dash-row",
            layout_columns(
              col_widths = c(2, 2, 2, 3, 3),
              style = "gap: 1rem;",
              kpi_card("ALE Inherente Total (sin controles)", "mit_inherent", "shield-slash", "rose"),
              kpi_card("ALE Residual Total (con controles)", "mit_residual", "shield-check", "emerald"),
              kpi_card("Reducción de Pérdida", "mit_reduction", "arrow-down-right-circle", "indigo"),
              kpi_card("Ahorro Neto (tras costo de controles)", "kpi_net_savings", "cash-coin", "cyan"),
              kpi_card("ROSI (Retorno de la inversión)", "kpi_rosi", "graph-up-arrow", "amber")
            )
          ),
          div(class = "dash-row",
            actionButton("btn_run_mitigation", "Ejecutar análisis de mitigación", class = "btn-warning w-100 mt-2 mb-3"),
            card(class = "chart-card", full_screen = TRUE, card_header("ALE Inherente vs Residual por Escenario (Top 15)"),
                 plotlyOutput("mit_bars", height = "450px")),
            layout_columns(
              col_widths = c(5, 7),
              card(class = "chart-card", full_screen = TRUE, card_header("Ahorro Total por Capacidad"), plotlyOutput("mit_control_bars", height = "400px")),
              card(full_screen = TRUE, card_header("Detalle por Capacidad"), DTOutput("mit_control_table"))
            )
          ),
          verbatimTextOutput("mitigation_message")
        )
      ),
      nav_panel(
        "Optimización de Controles",
        div(class = "dash-content",
          div(class = "dash-row",
            card_header("Optimización de controles (mochila / ROSI)"),
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              selectInput("opt_mode", "Tipo de presupuesto",
                          choices = c("Por escenario" = "per_scenario",
                                      "Global (todos los escenarios)" = "global"),
                          selected = "per_scenario"),
              numericInput("opt_budget", "Presupuesto ($)",
                           value = 500000, min = 0, step = 10000),
              div(class = "mt-4",
                  actionButton("btn_run_opt", "Ejecutar optimización",
                               class = "btn-primary w-100")),
              helpText("Por escenario: cada escenario puede gastar hasta el ",
                       "monto. Global: el monto se reparte entre todos los ",
                       "escenarios maximizando el ahorro neto total. Requiere ",
                       "análisis + mitigación y costos en Controles de Seguridad.")
            )
          ),
          div(class = "dash-row",
            textOutput("opt_message"),
            textOutput("opt_totals"),
            div(class = "table-wrapper", DTOutput("opt_table"))
          )
        )
      ),
      nav_panel(
        "🤖 Ask Copilot (Ollama)",
        div(class = "chat-widget",
          div(class = "chat-header", vb_icon("robot"), "Ask Copilot · contexto del Dashboard",
              htmlOutput("ollama_status_badge")),
          div(class = "chat-body",
            uiOutput("chat_history"),
            uiOutput("chat_loading")
          ),
          div(class = "chat-input-row",
            textInput("chat_input", NULL,
                      placeholder = "Ej: ¿Por qué el P90 es tan alto y qué control me recomiendas?"),
            actionButton("btn_send", "Enviar", class = "btn-primary")
          )
        )
      )
    )
  )
)
