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
    # Selector de control: permite elegir un control EXISTENTE del catálogo
    # (predefinidos o personalizados ya creados). El flujo para crear un
    # control NUEVO por nombre es el botón "Crear control propio" de abajo
    # (independiente del selectize y a prueba de fallos).
    uiOutput(ns("capability_selector")),

    # Crear un control PROPIO escribiendo su nombre: flujo dedicado y robusto.
    # Se auto-genera un ID interno (CTRL-xx) y el nombre se persiste en
    # custom_capabilities.csv para que reaparezca en futuras sesiones.
    textInput(ns("new_control_name"), "O crea un control propio (nombre)",
              value = "", placeholder = "Ej: Firewall perimetral"),
    actionButton(ns("btn_create_control"), "Crear control propio",
                 class = "btn-outline-success w-100 mb-2"),

    # Descripción opcional (se guarda para controles nuevos)
    textInput(ns("capability_desc"), "Descripción (opcional)", value = ""),

    # Costo de implementación/mantenimiento del control ($) — se usa para ROSI
    numericInput(ns("control_cost"), "Costo del control ($)", value = 0, min = 0, step = 1000),

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
    # Columnas: capability_id, capability_desc, cost, eff_min, eff_mode, eff_max (%).
    configured <- reactiveVal(data.frame(
      capability_id = character(),
      capability_desc = character(),
      cost = numeric(),
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

    # Selector client-side: permite elegir un control EXISTENTE. También admite
    # escribir un nombre nuevo + Enter, pero el flujo recomendado para crear un
    # control propio es el botón "Crear control propio" (más abajo).
    output$capability_selector <- renderUI({
      selectizeInput(
        ns("capability_id"),
        "Elige un control existente",
        choices = choices_rv(),
        multiple = FALSE,
        options = list(
          placeholder = "Selecciona un control del catálogo...",
          create = TRUE,
          persist = FALSE
        )
      )
    })

    # Al seleccionar un control existente con parámetros persistidos en
    # custom_capabilities.csv, precargar costo/efectividad/descripción. Sin
    # esto, re-añadir un control propio tras limpiar el módulo dejaba el costo
    # en 0 y la optimización sugería usar todos los controles "gratis".
    observeEvent(input$capability_id, {
      id <- input$capability_id
      if (is.null(id) || !nzchar(id)) return()
      custom <- read_custom_capabilities()
      if (nrow(custom) == 0) return()
      hit <- custom[custom$capability_id == id, , drop = FALSE]
      if (nrow(hit) == 0) return()
      h <- hit[1, ]
      if (!is.na(h$cost)) updateNumericInput(session, "control_cost", value = h$cost)
      if (!is.na(h$eff_min)) updateSliderInput(session, "eff_min", value = h$eff_min)
      if (!is.na(h$eff_mode)) updateSliderInput(session, "eff_mode", value = h$eff_mode)
      if (!is.na(h$eff_max)) updateSliderInput(session, "eff_max", value = h$eff_max)
      if (nzchar(h$capability) && h$capability != id) {
        updateTextInput(session, "capability_desc", value = h$capability)
      }
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

    # Upsert de una fila en `configured` (por capability_id). Compartido por
    # el botón "Añadir / Actualizar" y por "Crear control propio".
    upsert_configured <- function(id, desc, cost, eff_min, eff_mode, eff_max) {
      current <- configured()
      if (id %in% current$capability_id) {
        idx <- which(current$capability_id == id)
        current$eff_min[idx] <- eff_min
        current$eff_mode[idx] <- eff_mode
        current$eff_max[idx] <- eff_max
        current$cost[idx] <- cost
        if (nzchar(desc)) current$capability_desc[idx] <- desc
      } else {
        current <- rbind(current, data.frame(
          capability_id = id,
          capability_desc = desc,
          cost = cost,
          eff_min = eff_min,
          eff_mode = eff_mode,
          eff_max = eff_max,
          stringsAsFactors = FALSE
        ))
      }
      configured(current)
    }

    # --- Crear un control PROPIO escribiendo su nombre ----------------------
    # Flujo dedicado: independiente del selectize, con feedback explícito.
    observeEvent(input$btn_create_control, {
      name <- trimws(input$new_control_name)
      if (!nzchar(name)) {
        showNotification("Escribe un nombre para el control nuevo.", type = "warning")
        return()
      }
      if (grepl(",", name, fixed = TRUE)) {
        showNotification("El nombre no puede contener comas (las usa el pipeline).",
                         type = "error")
        return()
      }
      # ID interno único + persistencia (id, nombre, costo y efectividad) en
      # custom_capabilities.csv, para no perder los parámetros al cerrar.
      id <- tryCatch(next_custom_id(),
                     error = function(e) sprintf("CTRL-%06d", as.integer(Sys.time()) %% 1e6))
      cost <- if (is.null(input$control_cost) || is.na(input$control_cost)) 0 else input$control_cost
      eff_min <- if (is.null(input$eff_min)) 0 else input$eff_min
      eff_mode <- if (is.null(input$eff_mode)) 50 else input$eff_mode
      eff_max <- if (is.null(input$eff_max)) 100 else input$eff_max
      ok_write <- tryCatch({
        write_custom_capability(id, name, cost = cost,
                                eff_min = eff_min, eff_mode = eff_mode,
                                eff_max = eff_max)
        TRUE
      }, error = function(e) {
        message("No se pudo persistir el control propio: ", conditionMessage(e))
        FALSE
      })

      # Añadir a la configuración del escenario con los parámetros actuales
      # (con valores por defecto si algún slider aún no está inicializado)
      cost <- if (is.null(input$control_cost) || is.na(input$control_cost)) 0 else input$control_cost
      eff_min <- if (is.null(input$eff_min)) 0 else input$eff_min
      eff_mode <- if (is.null(input$eff_mode)) 50 else input$eff_mode
      eff_max <- if (is.null(input$eff_max)) 100 else input$eff_max
      upsert_configured(id, name, cost, eff_min, eff_mode, eff_max)

      # Refrescar el dropdown para incluir el nuevo control con su nombre
      choices_rv(c(available_choices(),
                   stats::setNames(id, paste0(id, " - ", name))))
      updateTextInput(session, "new_control_name", value = "")

      if (ok_write) {
        showNotification(sprintf("Control creado: %s (%s)", name, id),
                         type = "message")
      } else {
        showNotification(sprintf("Control añadido al escenario (%s), pero NO se pudo guardar en el catálogo (revisa OneDrive).", id),
                         type = "warning")
      }
    })

    # Añadir o actualizar (upsert por capability_id) un control EXISTENTE
    observeEvent(input$btn_add, {
      req(input$capability_id, input$eff_min, input$eff_mode, input$eff_max, input$control_cost)
      typed <- trimws(input$capability_id)
      if (!nzchar(typed)) return()
      # No guardamos filas inválidas
      if (!(input$eff_min <= input$eff_mode && input$eff_mode <= input$eff_max)) {
        showNotification("Requiere Min ≤ Mode ≤ Max.", type = "warning")
        return()
      }

      desc <- if (is.null(input$capability_desc)) "" else trimws(input$capability_desc)
      cost <- if (is.null(input$control_cost) || is.na(input$control_cost)) 0 else input$control_cost
      master_ids <- unname(available_choices())

      # --- ¿Control conocido o NUEVO escrito en el selector? -----------------
      if (!(typed %in% master_ids)) {
        # Se escribió un nombre nuevo directamente en el selector: se crea el
        # control (misma lógica que el botón dedicado).
        name <- typed
        id <- tryCatch(next_custom_id(),
                       error = function(e) sprintf("CTRL-%06d", as.integer(Sys.time()) %% 1e6))
        tryCatch(
          write_custom_capability(id, name, cost = cost,
                                  eff_min = input$eff_min,
                                  eff_mode = input$eff_mode,
                                  eff_max = input$eff_max),
          error = function(e) message("No se pudo persistir el control propio: ", conditionMessage(e))
        )
        if (!nzchar(desc)) desc <- name
        choices_rv(c(available_choices(),
                     stats::setNames(id, paste0(id, " - ", name))))
      } else {
        id <- typed
        # Si el usuario no escribió descripción, pre-rellenar con el nombre
        # visible del control (español o personalizado)
        if (!nzchar(desc)) {
          nm <- capability_es_name(id)
          if (nzchar(nm) && nm != id) desc <- nm
        }
        # Persistir costo/efectividad de controles personalizados YA
        # registrados (upsert), para que la optimización use el costo aunque
        # el módulo se limpie al añadir el escenario.
        upsert_custom_capability_params(data.frame(
          capability_id = id, capability_desc = desc, cost = cost,
          eff_min = input$eff_min, eff_mode = input$eff_mode,
          eff_max = input$eff_max, stringsAsFactors = FALSE
        ))
      }

      upsert_configured(id, desc, cost, input$eff_min, input$eff_mode, input$eff_max)
      showNotification(sprintf("Control añadido al escenario: %s", desc),
                       type = "message")
    })

    # Limpiar la configuración del escenario
    observeEvent(input$btn_clear, {
      configured(data.frame(
        capability_id = character(),
        capability_desc = character(),
        cost = numeric(),
        eff_min = numeric(),
        eff_mode = numeric(),
        eff_max = numeric(),
        stringsAsFactors = FALSE
      ))
    })

    # Tabla resumen de controles configurados (etiquetas en español)
    output$configured_table <- renderTable({
      df <- configured()
      if (nrow(df) == 0) {
        return(data.frame(Controles = "Sin controles configurados"))
      }
      df |>
        dplyr::mutate(
          Descripción = ifelse(nzchar(capability_desc), capability_desc, "—"),
          `Costo $` = fmt_compact_money(cost)
        ) |>
        dplyr::select(capability_id, Descripción, `Costo $`,
                      `Mín %` = eff_min, `Moda %` = eff_mode, `Máx %` = eff_max)
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

      # Costo por control (vector nombrado capability_id -> $) para ROSI
      control_costs = reactive({
        df <- configured()
        if (nrow(df) == 0) return(stats::setNames(numeric(0), character(0)))
        stats::setNames(df$cost, df$capability_id)
      }),

      # Función para reiniciar desde la app
      clear = function() {
        configured(data.frame(
          capability_id = character(),
          capability_desc = character(),
          cost = numeric(),
          eff_min = numeric(),
          eff_mode = numeric(),
          eff_max = numeric(),
          stringsAsFactors = FALSE
        ))
      }
    )
  })
}
