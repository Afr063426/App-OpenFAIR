# =============================================================================
# capabilities_module.R
# -----------------------------------------------------------------------------
# Módulo Shiny para configurar "Capabilities" (controles de seguridad) con
# parámetros Beta-PERT de efectividad (mitigación), integrado con el pipeline
# Monte Carlo del paquete `evaluator` (OpenFAIR).
#
# Cómo se integra con `evaluator`:
#   - survey.xlsx, columna V7 "Capabilities": IDs separados por ", "
#     (ej. "CAP-01, CAP-02").
#   - `evaluator::derive_controls()` (evaluator/R/encode.R) convierte cada ID
#     en una lista de parámetros PERT derivados de la tabla cualitativa:
#       list(min, mode, max, shape, func = "mc2d::rpert")
#   - `evaluator::sample_diff()` llama mc2d::rpert(n, min, mode, max, shape)
#     para simular la efectividad del control en cada iteración Monte Carlo.
#
# Controles nuevos:
#   - El selector usa selectize CLIENT-SIDE con create = TRUE, por lo que se
#     puede escribir un ID que no exista (ej. "CAP-99") y pulsar Enter.
#   - Si el ID no está en la lista maestra, se persiste en
#     evaluator_workspace/inputs/custom_capabilities.csv (junto a la
#     descripción) para que reaparezca en el dropdown en futuras sesiones.
#   - Aunque no esté en la lista maestra, el pipeline funciona: la lista
#     generada por `capability_params_to_evaluator()` se fusiona sobre
#     scenario$parameters$diff antes de la simulación.
# =============================================================================

# ---- Simulación Beta-PERT de la efectividad de un control -------------------
# n        : número de muestras Monte Carlo
# min/mode/max : efectividad como proporciones entre 0 y 1
# shape    : factor de forma PERT (4 = PERT estándar)
simulate_capability_effectiveness <- function(n, min, mode, max, shape = 4) {
  mc2d::rpert(n = n, min = min, mode = mode, max = max, shape = shape)
}

# ---- Helper: data.frame reactivo -> lista en formato evaluator --------------
# Convierte un data.frame con columnas (capability_id, eff_min, eff_mode,
# eff_max) — con las efectividades en % (0-100) — en la lista nombrada que
# `evaluator` espera en scenario$parameters$diff:
#   list(`CAP-01` = list(min=0.70, mode=0.85, max=0.98, shape=4,
#                        func="mc2d::rpert"))
# NOTA: se seleccionan solo las columnas numéricas; la descripción (u otras
# columnas) NO debe entrar en la lista porque el simulador la pasaría como
# argumento a rpert() y fallaría.
capability_params_to_evaluator <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  df <- df[c("capability_id", "eff_min", "eff_mode", "eff_max")]
  purrr::pmap(df, function(capability_id, eff_min, eff_mode, eff_max) {
    list(min = eff_min / 100,
         mode = eff_mode / 100,
         max = eff_max / 100,
         shape = 4,
         func = "mc2d::rpert")
  }) |> rlang::set_names(df$capability_id)
}

# ---- Módulo UI ---------------------------------------------------------------
capabilities_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Selector de control renderizado en el server (client-side, create = TRUE)
    # -> permite elegir de la lista maestra O escribir un ID nuevo (ej. CAP-99)
    uiOutput(ns("capability_selector")),

    # Descripción opcional (se guarda para controles nuevos)
    textInput(ns("capability_desc"), "Descripción (opcional)", value = ""),

    # Parámetros Beta-PERT de efectividad (en porcentaje 0-100; se convierten
    # a proporciones 0-1 al construir la lista para el simulador).
    sliderInput(ns("eff_min"), "Efectividad mínima (%)", 0, 100, 0, step = 1),
    sliderInput(ns("eff_mode"), "Efectividad más probable (%)", 0, 100, 50, step = 1),
    sliderInput(ns("eff_max"), "Efectividad máxima (%)", 0, 100, 100, step = 1),

    # Validación visual Min <= Mode <= Max
    uiOutput(ns("validation_feedback")),

    actionButton(ns("btn_add"), "Añadir / Actualizar control",
                 class = "btn-outline-info w-100"),
    tags$hr(),
    h6("Controles configurados (este escenario)"),
    tableOutput(ns("configured_table")),
    actionButton(ns("btn_clear"), "Limpiar todos",
                 class = "btn-outline-danger w-100")
  )
}

# ---- Módulo Server -----------------------------------------------------------
# available_choices: reactive() que devuelve el vector nombrado de capabilities
# disponibles (de get_evaluator_capabilities()), o character(0).
capabilities_server <- function(id, available_choices = reactive(character())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # data.frame reactivo: una fila por control configurado para el escenario.
    # Columnas: capability_id, capability_desc, eff_min, eff_mode, eff_max (%).
    configured <- reactiveVal(data.frame(
      capability_id = character(),
      capability_desc = character(),
      eff_min = numeric(),
      eff_mode = numeric(),
      eff_max = numeric(),
      stringsAsFactors = FALSE
    ))

    # Opciones actuales del dropdown (lista maestra + controles personalizados)
    choices_rv <- reactiveVal(character(0))

    observe({
      choices_rv(available_choices())
    })

    # Selector client-side: create = TRUE permite escribir un ID nuevo
    output$capability_selector <- renderUI({
      selectizeInput(
        ns("capability_id"),
        "Capability / Control",
        choices = choices_rv(),
        multiple = FALSE,
        options = list(
          placeholder = "Selecciona o escribe un ID (ej. CAP-99)...",
          create = TRUE,
          persist = FALSE
        )
      )
    })

    # Validación: los tres campos presentes y Min <= Mode <= Max
    valid_inputs <- reactive({
      req(input$capability_id, input$eff_min, input$eff_mode, input$eff_max)
      nzchar(trimws(input$capability_id)) &&
        input$eff_min <= input$eff_mode &&
        input$eff_mode <= input$eff_max
    })

    # Feedback visual (verde/ámbar) bajo los sliders
    output$validation_feedback <- renderUI({
      if (is.null(input$capability_id) || !nzchar(trimws(input$capability_id))) {
        return(NULL)
      }
      if (valid_inputs()) {
        tags$div(class = "alert alert-success py-1 px-2 mb-2",
                 style = "font-size:0.85rem;",
                 shiny::icon("check-circle"),
                 " Min ≤ Mode ≤ Max: parámetros válidos")
      } else {
        tags$div(class = "alert alert-warning py-1 px-2 mb-2",
                 style = "font-size:0.85rem;",
                 shiny::icon("triangle-exclamation"),
                 " Requiere Min ≤ Mode ≤ Max")
      }
    })

    # Añadir o actualizar (upsert por capability_id)
    observeEvent(input$btn_add, {
      req(input$capability_id, input$eff_min, input$eff_mode, input$eff_max)
      id <- trimws(input$capability_id)
      if (!nzchar(id)) return()
      # No guardamos filas inválidas
      if (!(input$eff_min <= input$eff_mode && input$eff_mode <= input$eff_max)) {
        return()
      }

      desc <- if (is.null(input$capability_desc)) "" else trimws(input$capability_desc)

      current <- configured()

      # Si el control ya está configurado: actualizar (y conservar su
      # descripción si el campo viene vacío)
      if (id %in% current$capability_id) {
        idx <- which(current$capability_id == id)
        current$eff_min[idx] <- input$eff_min
        current$eff_mode[idx] <- input$eff_mode
        current$eff_max[idx] <- input$eff_max
        if (nzchar(desc)) current$capability_desc[idx] <- desc
      } else {
        # Nuevo control: persistir en la lista de personalizados si no existe
        # en la lista maestra, para que aparezca en el dropdown más adelante.
        # NOTA: el vector de choices tiene names = texto mostrado y values = ID.
        master_ids <- unname(available_choices())
        if (!(id %in% master_ids)) {
          tryCatch(
            write_custom_capability(id, desc),
            error = function(e) NULL
          )
        }
        current <- rbind(current, data.frame(
          capability_id = id,
          capability_desc = desc,
          eff_min = input$eff_min,
          eff_mode = input$eff_mode,
          eff_max = input$eff_max,
          stringsAsFactors = FALSE
        ))
        # Refrescar el dropdown para incluir el nuevo control
        choices_rv(c(available_choices(),
                     stats::setNames(id, paste0(id, " - ", if (nzchar(desc)) desc else "Control personalizado"))))
      }
      configured(current)
    })

    # Limpiar la configuración del escenario
    observeEvent(input$btn_clear, {
      configured(data.frame(
        capability_id = character(),
        capability_desc = character(),
        eff_min = numeric(),
        eff_mode = numeric(),
        eff_max = numeric(),
        stringsAsFactors = FALSE
      ))
    })

    # Tabla resumen de controles configurados
    output$configured_table <- renderTable({
      df <- configured()
      if (nrow(df) == 0) {
        return(data.frame(Controles = "Sin controles configurados"))
      }
      df |>
        dplyr::mutate(
          Descripción = ifelse(nzchar(capability_desc), capability_desc, "—")
        ) |>
        dplyr::select(capability_id, Descripción,
                      `Mín %` = eff_min, `Mode %` = eff_mode, `Max %` = eff_max)
    })

    # ---- Valores de retorno (para integrar con la app) ----------------------
    list(
      # data.frame reactivo completo (capability_id, capability_desc,
      # eff_min, eff_mode, eff_max)
      configured = configured,

      # IDs separados por ", " para la columna V7 del survey.xlsx
      capability_ids_csv = reactive({
        df <- configured()
        if (nrow(df) == 0) "" else paste(df$capability_id, collapse = ", ")
      }),

      # Lista nombrada en formato evaluator para sobrescribir scenario$parameters$diff
      capability_pert_params = reactive({
        capability_params_to_evaluator(configured())
      }),

      # Función para reiniciar desde la app
      clear = function() {
        configured(data.frame(
          capability_id = character(),
          capability_desc = character(),
          eff_min = numeric(),
          eff_mode = numeric(),
          eff_max = numeric(),
          stringsAsFactors = FALSE
        ))
      }
    )
  })
}
