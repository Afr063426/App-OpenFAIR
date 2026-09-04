# -----------------------------------------------------------------------------
# Resolución DETERMINISTA del directorio de la app: se busca el directorio que
# contiene global.R + ui.R + server.R subiendo desde el directorio de trabajo
# actual (y comprobando también un subdirectorio ./app en cada nivel). Así el
# workspace NO depende de desde dónde se lance la app (app/ o la raíz del
# proyecto), evitando usar un evaluator_workspace equivocado.
# -----------------------------------------------------------------------------
find_app_dir <- function() {
  start <- normalizePath(getwd(), mustWork = FALSE)
  d <- start
  repeat {
    if (file.exists(file.path(d, "global.R")) &&
        file.exists(file.path(d, "ui.R")) &&
        file.exists(file.path(d, "server.R"))) {
      return(d)
    }
    if (file.exists(file.path(d, "app", "global.R")) &&
        file.exists(file.path(d, "app", "ui.R")) &&
        file.exists(file.path(d, "app", "server.R"))) {
      return(file.path(d, "app"))
    }
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  start
}

evaluator_workspace <- function() {
  app_dir <- find_app_dir()
  # Multiproyecto: si hay un proyecto activo (options(tfm.project_dir)),
  # el workspace es ESE directorio; si no, el clásico app/evaluator_workspace.
  override <- getOption("tfm.project_dir", NULL)
  base_dir <- if (!is.null(override) && nzchar(override)) {
    normalizePath(override, mustWork = FALSE)
  } else {
    file.path(app_dir, "evaluator_workspace")
  }
  inputs_dir <- file.path(base_dir, "inputs")
  results_dir <- file.path(base_dir, "results")

  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
  if (!dir.exists(inputs_dir)) dir.create(inputs_dir, recursive = TRUE)
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

  if (!file.exists(file.path(inputs_dir, "survey.xlsx")) ||
      !file.exists(file.path(inputs_dir, "domains.csv")) ||
      !file.exists(file.path(inputs_dir, "qualitative_mappings.csv"))
  ) {
    # Al crear la plantilla por primera vez (deployment nuevo) se copia la
    # plantilla de ejemplo del paquete evaluator, que incluye escenarios de
    # muestra. Inmediatamente después se vacía (solo encabezados) para que la
    # app arranque SIEMPRE con 0 escenarios ("Esperando ingreso de
    # escenarios...") en lugar de datos de ejemplo.
    evaluator::create_templates(base_dir)
    reset_survey_template(base_dir)
  }

  list(base_dir = base_dir,
       inputs_dir = inputs_dir,
       results_dir = results_dir)
}

get_evaluator_domain_choices <- function() {
  ws <- evaluator_workspace()
  domains <- readr::read_csv(file.path(ws$inputs_dir, "domains.csv"),
                             col_types = readr::cols(
                               domain_id = readr::col_character(),
                               domain = readr::col_character()
                             ))
  setNames(domains$domain_id, paste0(domains$domain_id, " - ", domains$domain))
}

import_uploaded_survey_data <- function(df, default_domain_id = NULL, base_dir = evaluator_workspace()$base_dir) {
  if (!is.data.frame(df)) {
    stop("Uploaded survey data must be a data frame.", call. = FALSE)
  }

  alias_map <- list(
    scenario_description = c("scenario_description", "description", "scenario", "scenario desc"),
    tcomm = c("tcomm", "threat_community", "threat community"),
    tef = c("tef", "threat expected frequency", "threat frequency"),
    tc = c("tc", "threat capability"),
    lm = c("lm", "loss magnitude"),
    scenario_id = c("scenarioid", "scenario_id", "scenario id"),
    capabilities = c("capabilities", "controls"),
    domain_id = c("domain_id", "domainid", "domain id", "domain"),
    lef = c("lef", "loss event frequency", "loss event frequency")
  )

  lower_names <- tolower(names(df))
  matched <- vapply(alias_map, function(al) {
    hits <- which(lower_names %in% al)
    if (length(hits) > 0) names(df)[hits[1]] else NA_character_
  }, character(1))

  required <- c("scenario_description", "tcomm", "tef", "tc", "lm", "scenario_id", "capabilities")
  if (any(is.na(matched[required]))) {
    missing <- required[is.na(matched[required])]
    stop(sprintf(
      paste0("Faltan columnas obligatorias en el archivo: %s. ",
             "Formato esperado: archivo plano con columnas 'scenario_id', ",
             "'scenario' o 'scenario_description', 'tcomm', 'tef', 'tc', 'lm' ",
             "y 'capabilities' (o un survey.xlsx seccionado con hojas por ",
             "dominio, como el que descarga la app)."),
      paste(missing, collapse = ", ")), call. = FALSE)
  }

  if (is.null(default_domain_id) && is.na(matched["domain_id"])) {
    stop("Debe especificar un dominio o incluir domain_id en el archivo cargado.", call. = FALSE)
  }

  for (i in seq_len(nrow(df))) {
    domain_id <- if (!is.na(matched["domain_id"])) {
      as.character(df[[matched["domain_id"]]][i])
    } else {
      default_domain_id
    }
    write_survey_scenario(
      domain_id = domain_id,
      scenario_description = as.character(df[[matched["scenario_description"]]][i]),
      tcomm = as.character(df[[matched["tcomm"]]][i]),
      tef = as.character(df[[matched["tef"]]][i]),
      tc = as.character(df[[matched["tc"]]][i]),
      lm = as.character(df[[matched["lm"]]][i]),
      scenario_id = as.character(df[[matched["scenario_id"]]][i]),
      capabilities = as.character(df[[matched["capabilities"]]][i]),
      lef = if (!is.na(matched["lef"])) as.character(df[[matched["lef"]]][i]) else NULL,
      # append = FALSE -> UPSERT por ScenarioID: si el escenario ya existe en
      # el workspace se actualiza en lugar de añadir una copia. Así importar
      # el mismo archivo dos veces NO genera duplicados.
      append = FALSE,
      base_dir = base_dir
    )
  }

  invisible(nrow(df))
}

# -----------------------------------------------------------------------------
# Cuenta los escenarios (filas de datos de la tabla "Threats") presentes en un
# survey.xlsx. Devuelve NA si el archivo no existe o no se puede leer (p. ej.
# placeholder de OneDrive). Se usa en el diagnóstico de arranque de la app.
# -----------------------------------------------------------------------------
count_survey_scenarios <- function(survey_file) {
  if (!file.exists(survey_file)) return(NA_integer_)
  wb <- tryCatch(openxlsx::loadWorkbook(survey_file), error = function(e) NULL)
  if (is.null(wb)) return(NA_integer_)
  total <- 0L
  sheets <- setdiff(openxlsx::getSheetNames(survey_file),
                    c("Introduction", "Definitions", "Reference"))
  for (sh in sheets) {
    dat <- tryCatch(openxlsx::readWorkbook(wb, sheet = sh, colNames = FALSE,
                                           skipEmptyRows = FALSE),
                    error = function(e) NULL)
    if (is.null(dat) || nrow(dat) == 0) next
    col1 <- as.character(dat[[1]])
    tr <- which(!is.na(col1) & col1 == "Threats")[1]
    if (is.na(tr) || nrow(dat) < tr + 2) next
    vals <- as.character(dat[[1]][(tr + 2):nrow(dat)])
    total <- total + sum(!is.na(vals) & nzchar(trimws(vals)))
  }
  total
}

# -----------------------------------------------------------------------------
# Utilidades para cargar históricos (TEF/LM) desde Excel/CSV con selección de
# HOJA y COLUMNA: se listan las hojas, las columnas candidatas (con número de
# valores numéricos y una muestra) y se extrae el vector numérico de la
# columna elegida.
# -----------------------------------------------------------------------------

# Hojas de un archivo Excel (NULL si el archivo es CSV)
hist_file_sheets <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) readxl::excel_sheets(path) else NULL
}

# Lee el archivo (hoja indicada, o la primera si sheet es NULL) como data.frame
# con columnas sin nombres interpretados (X1, X2...). Para CSV lee la única hoja.
hist_file_data <- function(path, sheet = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    s <- if (is.null(sheet)) 1 else sheet
    readxl::read_excel(path, sheet = s, col_names = FALSE)
  } else {
    read.csv(path, stringsAsFactors = FALSE, header = FALSE, check.names = FALSE)
  }
}

# Resumen de columnas candidatas de una hoja: índice, etiqueta (nº de valores
# numéricos + muestra) y n_numeric. Devuelve un data.frame index/label/n_numeric.
hist_file_columns <- function(path, sheet = NULL) {
  df <- hist_file_data(path, sheet)
  if (ncol(df) == 0 || nrow(df) == 0) {
    return(data.frame(index = integer(), label = character(),
                      n_numeric = integer(), stringsAsFactors = FALSE))
  }
  rows <- lapply(seq_len(ncol(df)), function(j) {
    num <- suppressWarnings(as.numeric(as.character(df[[j]])))
    n_num <- sum(!is.na(num))
    muestra <- utils::head(num[!is.na(num)], 3)
    label <- sprintf("Columna %d (%d numéricos%s)", j, n_num,
                     if (length(muestra) > 0)
                       paste0(" · muestra: ", paste(sprintf("%g", muestra), collapse = ", "))
                     else "")
    data.frame(index = j, label = label, n_numeric = n_num,
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# Extrae el vector numérico de la columna indicada (con su hoja)
hist_file_vector <- function(path, sheet = NULL, col = 1L) {
  df <- hist_file_data(path, sheet)
  if (col < 1 || col > ncol(df)) stop("Columna no válida.", call. = FALSE)
  x <- suppressWarnings(as.numeric(as.character(df[[col]])))
  x[!is.na(x)]
}

# -----------------------------------------------------------------------------
# Extrae la cadena completa de errores (causa raíz incluida). Los errores de
# dplyr/tidyr suelen tener la causa en e$parent; esta función la recorre para
# mostrar el mensaje REAL y no solo el contexto ("In argument: ...").
# -----------------------------------------------------------------------------
err_chain <- function(e) {
  msgs <- character()
  while (!is.null(e)) {
    msg <- conditionMessage(e)
    if (nzchar(msg) && !(msg %in% msgs)) msgs <- c(msgs, msg)
    e <- e$parent
  }
  paste(msgs, collapse = " | ")
}

# -----------------------------------------------------------------------------
# Carga el workbook del survey con un mensaje de error accionable si el archivo
# existe pero no se puede leer (p. ej. placeholder de OneDrive sin hidratar o
# archivo corrupto).
# -----------------------------------------------------------------------------
load_survey_workbook <- function(survey_file) {
  tryCatch(
    openxlsx::loadWorkbook(survey_file),
    error = function(e) {
      stop(sprintf(paste0("No se puede leer %s: %s. Si el archivo existe pero ",
                          "no abre, probablemente es un placeholder de OneDrive ",
                          "sin descargar: ábrelo en Excel para forzar su ",
                          "descarga, o bórralo para que la app regenere la ",
                          "plantilla vacía."),
                   basename(survey_file), conditionMessage(e)), call. = FALSE)
    }
  )
}

# -----------------------------------------------------------------------------
# Detección del formato SECCIONADO de survey.xlsx (una hoja por dominio con la
# tabla "Threats" de escenarios). Devuelve TRUE si el libro Excel contiene esa
# estructura. Este es el formato que descarga la app con el botón
# "Descargar survey.xlsx" y que un usuario puede rellenar en Excel.
# -----------------------------------------------------------------------------
is_sectioned_survey_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  sheets <- tryCatch(openxlsx::getSheetNames(path), error = function(e) character(0))
  meta <- c("Introduction", "Definitions", "Reference")
  for (sh in setdiff(sheets, meta)) {
    dat <- tryCatch(openxlsx::readWorkbook(path, sheet = sh, colNames = FALSE),
                    error = function(e) NULL)
    if (is.null(dat) || nrow(dat) == 0) next
    col1 <- as.character(dat[[1]])
    if (any(!is.na(col1) & col1 == "Threats")) return(TRUE)
  }
  FALSE
}

# -----------------------------------------------------------------------------
# Extrae los escenarios de un survey.xlsx SECCIONADO en un data.frame plano
# (scenario_id, scenario, tcomm, tef, tc, lm, domain_id, controls) reutilizando
# el parser de evaluator (import_scenarios + split_sheet parcheado). Este marco
# plano es exactamente el que espera import_uploaded_survey_data().
# -----------------------------------------------------------------------------
import_sectioned_survey_scenarios <- function(path,
                                              base_dir = evaluator_workspace()$base_dir) {
  domains <- readr::read_csv(file.path(base_dir, "inputs", "domains.csv"),
                             col_types = readr::cols(.default = readr::col_character()))
  evaluator::import_scenarios(path, domains)
}

write_survey_scenario <- function(domain_id,
                                  scenario_description,
                                  tcomm,
                                  tef,
                                  tc,
                                  lm,
                                  scenario_id,
                                  capabilities,
                                  lef = NULL,
                                  append = TRUE,
                                  base_dir = evaluator_workspace()$base_dir) {
  # Formatear la descripción si viene con LEF
  if (!is.null(lef) && !is.na(lef) && nzchar(as.character(lef))) {
    scenario_description <- paste0(scenario_description, " [LEF=", lef, "]")
  }

  ws <- evaluator_workspace()
  survey_file <- file.path(ws$inputs_dir, "survey.xlsx")

  if (!file.exists(survey_file)) {
    evaluator::create_templates(ws$base_dir)
  }

  wb <- load_survey_workbook(survey_file)

  if (!(domain_id %in% names(wb))) {
    stop(sprintf("No se encuentra la hoja de dominio '%s' en survey.xlsx.", domain_id), call. = FALSE)
  }

  # 1. Leer la hoja sin omitir filas vacías para tener el mapa exacto de Excel
  dat <- openxlsx::readWorkbook(survey_file, sheet = domain_id, colNames = FALSE, skipEmptyRows = FALSE)

  if (is.null(dat) || nrow(dat) == 0) {
    stop(sprintf("La hoja '%s' está vacía.", domain_id), call. = FALSE)
  }

  # Buscar dónde está la palabra "Threats" en la columna 1
  col1_vals <- as.character(dat[[1]])
  threats_row <- which(!is.na(col1_vals) & col1_vals == "Threats")[1]

  if (is.na(threats_row)) {
    stop("No se pudo encontrar la fila 'Threats' en la hoja del dominio.", call. = FALSE)
  }

  header_row <- threats_row + 1

  # 2. Buscar todas las filas que tengan texto en la columna 1 (Scenario) después del encabezado
  data_rows <- seq(header_row + 1, max(nrow(dat), header_row + 1))
  
  filled_rows <- integer(0)
  if (nrow(dat) >= (header_row + 1)) {
    for (r in seq(header_row + 1, nrow(dat))) {
      val <- dat[r, 1]
      if (!is.na(val) && nzchar(trimws(as.character(val)))) {
        filled_rows <- c(filled_rows, r)
      }
    }
  }

  # 3. Buscar si existe el ScenarioID solo si append = FALSE
  existing_row <- NA_integer_
  if (!append && !is.null(scenario_id) && nzchar(as.character(scenario_id))) {
    for (r in seq(header_row + 1, nrow(dat))) {
      scen_val <- dat[r, 6] # Columna 6 es ScenarioID
      if (!is.na(scen_val) && as.character(scen_val) == as.character(scenario_id)) {
        existing_row <- r
        break
      }
    }
  }

  # 4. Determinar con precisión matemática la fila de inserción
  if (!is.na(existing_row)) {
    target_row <- existing_row
  } else if (length(filled_rows) > 0) {
    target_row <- max(filled_rows) + 1  # Fila inmediatamente posterior al último escenario
  } else {
    target_row <- header_row + 1        # Si está vacía, primera fila de datos
  }

  # 5. Crear el dataframe de 1 sola fila
  ext_row_data <- data.frame(
    V1 = scenario_description,
    V2 = tcomm,
    V3 = tef,
    V4 = tc,
    V5 = lm,
    V6 = scenario_id,
    V7 = capabilities,
    stringsAsFactors = FALSE
  )

  # 6. Escribir directamente en la fila objetivo
  openxlsx::writeData(
    wb = wb,
    sheet = domain_id,
    x = ext_row_data,
    startRow = target_row,
    colNames = FALSE
  )

  openxlsx::saveWorkbook(wb, survey_file, overwrite = TRUE)

  survey_file
}

# -----------------------------------------------------------------------------
# Controles referenciados por los escenarios que NO existen en el catálogo
# (predefinidos + personalizados). Devuelve un data.frame con las columnas
# scenario_id y capability_id (una fila por control faltante).
# -----------------------------------------------------------------------------
scenario_missing_capabilities <- function(qualitative_scenarios, capabilities) {
  if (is.null(qualitative_scenarios) || nrow(qualitative_scenarios) == 0) {
    return(tibble::tibble(scenario_id = character(), capability_id = character()))
  }
  known <- unique(capabilities$capability_id)
  rows <- lapply(seq_len(nrow(qualitative_scenarios)), function(i) {
    ids <- trimws(unlist(strsplit(as.character(qualitative_scenarios$controls[i]), ",")))
    ids <- ids[!is.na(ids) & nzchar(ids)]
    missing <- ids[!ids %in% known]
    if (length(missing) == 0) {
      NULL
    } else {
      data.frame(scenario_id = qualitative_scenarios$scenario_id[i],
                 capability_id = missing, stringsAsFactors = FALSE)
    }
  })
  dplyr::bind_rows(rows)
}

run_evaluator_analysis <- function(iterations = 1e3,
                                   custom_diff_params = NULL,
                                   base_dir = evaluator_workspace()$base_dir) {
  ws <- evaluator_workspace()
  inputs_dir <- ws$inputs_dir
  results_dir <- ws$results_dir
  
  domains <- readr::read_csv(file.path(inputs_dir, "domains.csv"), col_types = readr::cols(.default = readr::col_character()))
  
  # Limpiar survey.xlsx: eliminar las columnas extra (V8-V11) antes de
  # importar, porque el paquete evaluator solo espera 7 columnas.
  survey_file <- file.path(inputs_dir, "survey.xlsx")
  wb <- load_survey_workbook(survey_file)
  
  for (sheet_name in openxlsx::getSheetNames(survey_file)) {
    if (sheet_name %in% c("Introduction", "Definitions", "Reference")) next
    
    dat <- openxlsx::readWorkbook(wb, sheet = sheet_name, colNames = FALSE)
    if (nrow(dat) == 0) next
    
    # Conservar únicamente las primeras 7 columnas
    if (ncol(dat) > 7) {
      dat_clean <- dat[, 1:7, drop = FALSE]
      openxlsx::writeData(wb, sheet = sheet_name, x = dat_clean, startRow = 1, colNames = FALSE)
    }
  }
  openxlsx::saveWorkbook(wb, survey_file, overwrite = TRUE)
  domains <- readr::read_csv(file.path(inputs_dir, "domains.csv"),
                    col_types = readr::cols(.default = readr::col_character()))
  evaluator::import_spreadsheet(survey_file, domains, inputs_dir)
  qual_inputs <- evaluator::read_qualitative_inputs(inputs_dir)
  # Registrar automáticamente en custom_capabilities.csv los controles
  # referenciados por los escenarios que aún no existen en el catálogo (cubre
  # formulario, importación y edición manual del survey). Así el avance no se
  # pierde al cerrar el proyecto y el pipeline los trata como controles reales.
  persist_unknown_controls(qual_inputs$qualitative_scenarios$controls)
  # Controles personalizados (CTRL-xx creados en la UI): se fusionan al
  # catálogo para que el pipeline los trate como controles de primera clase.
  qual_inputs <- merge_custom_capabilities(qual_inputs)

  # Validación explícita de los valores cualitativos/dist de TEF/TC/LM: si un
  # escenario tiene un valor no reconocido (p. ej. "Frecuente" en vez de
  # "Frequent", o una distribución mal escrita), se avisa con un mensaje claro
  # en lugar del error críptico de new_tidyrisk_scenario.
  qs <- qual_inputs$qualitative_scenarios
  m <- qual_inputs$mappings
  labs <- list(
    tef = tolower(m$label[m$type == "tef"]),
    tc = tolower(m$label[m$type == "tc"]),
    lm = tolower(m$label[m$type == "lm"])
  )
  # NOTA: con NA/"" la fila NO debe descartarse (filter trataría NA como FALSE);
  # por eso se marcan explícitamente los valores vacíos como inválidos.
  is_bad <- function(x, labels) {
    is.na(x) | !nzchar(trimws(as.character(x))) |
      (!(tolower(as.character(x)) %in% labels) & !grepl("^dist:", as.character(x)))
  }
  bad <- qs |>
    dplyr::mutate(
      bad_tef = is_bad(.data$tef, labs$tef),
      bad_tc = is_bad(.data$tc, labs$tc),
      bad_lm = is_bad(.data$lm, labs$lm)
    ) |>
    dplyr::filter(.data$bad_tef | .data$bad_tc | .data$bad_lm)
  if (nrow(bad) > 0) {
    show <- function(x) ifelse(is.na(x) | !nzchar(trimws(as.character(x))),
                               "(vacío)", as.character(x))
    stop(sprintf(
      paste0("El escenario '%s' tiene valores de TEF/TC/LM no reconocidos o ",
             "vacíos: tef='%s', tc='%s', lm='%s'. Usa las etiquetas ",
             "cualitativas (Frequent/Occasional/Rare, High/Medium/Low) o una ",
             "distribución válida con formato dist:<nombre>|params:clave=valor,..."),
      bad$scenario_id[1], show(bad$tef[1]), show(bad$tc[1]), show(bad$lm[1])),
      call. = FALSE)
  }

  evaluator::validate_scenarios(qual_inputs$qualitative_scenarios,
                               qual_inputs$capabilities,
                               domains,
                               qual_inputs$mappings)

  # Controles referenciados por escenarios que NO existen en el catálogo
  # (predefinidos + personalizados). Se reportan para avisar claramente en la
  # UI: el análisis continúa (el control se ignora), pero el usuario debe
  # registrarlo o corregirlo.
  missing_capabilities <- scenario_missing_capabilities(
    qual_inputs$qualitative_scenarios, qual_inputs$capabilities)

  quantitative_scenarios <- tryCatch(
    evaluator::encode_scenarios(scenarios = qual_inputs$qualitative_scenarios,
                                capabilities = qual_inputs$capabilities,
                                mappings = qual_inputs$mappings),
    error = function(e) {
      stop(sprintf(
        paste0("No se pudo codificar un escenario (revisa los valores de ",
               "TEF/TC/LM y los controles del escenario): %s"),
        err_chain(e)), call. = FALSE)
    }
  )

  # Sobrescribir la efectividad (DIFF) de controles configurados con Beta-PERT
  # personalizada. custom_diff_params: lista nombrada en formato evaluator,
  # ej. list(`CAP-01` = list(min=0.7, mode=0.85, max=0.98, shape=4,
  #                          func="mc2d::rpert")) generada por el módulo de
  # capabilities de la app.
  if (!is.null(custom_diff_params) && length(custom_diff_params) > 0) {
    quantitative_scenarios <- quantitative_scenarios |>
      dplyr::mutate(scenario = purrr::map(.data$scenario, function(sc) {
        sc$parameters$diff <- utils::modifyList(sc$parameters$diff,
                                                custom_diff_params)
        sc
      }))
  }

  # --- Plantilla vacía (0 escenarios): corto-circuito seguro -----------------
  # Si el survey no tiene escenarios cargados, el pipeline de simulación y
  # resumen de evaluator falla (p. ej. summarize_domains agrupa por
  # "iteration" sobre resultados vacíos). En lugar de propagar el error, se
  # devuelve un resultado vacío con las mismas columnas esperadas por la UI;
  # los gráficos muestran "Esperando ingreso de escenarios...".
  if (nrow(quantitative_scenarios) == 0) {
    simulation_results <- quantitative_scenarios |>
      dplyr::mutate(results = list()) |>
      dplyr::select(scenario_id, domain_id, results)

    empty_scenario_summary <- tibble::tibble(
      scenario_id = character(), domain_id = character(),
      scenario_description = character(), tcomm = character(),
      ale_median = numeric(), ale_max = numeric(), ale_var = numeric(),
      ale_mean = numeric(), loss_events_mean = numeric(),
      mean_vuln = numeric()
    )
    empty_domain_summary <- tibble::tibble(
      domain_id = character(), ale_median = numeric(), ale_mean = numeric(),
      ale_max = numeric(), ale_var = numeric(), mean_loss_events = numeric(),
      mean_vuln = numeric()
    )

    return(list(results_dir = results_dir,
                simulation_results = simulation_results,
                scenario_summary = empty_scenario_summary,
                domain_summary = empty_domain_summary,
                qualitative_scenarios = qual_inputs$qualitative_scenarios,
                capabilities = qual_inputs$capabilities,
                mappings = qual_inputs$mappings,
                missing_capabilities = tibble::tibble(
                  scenario_id = character(), capability_id = character())))
  }

  simulation_results <- quantitative_scenarios %>%
    dplyr::mutate(results = purrr::map2(.data$scenario, .data$scenario_id,
                                       function(sc, sid) {
      tryCatch(
        evaluator::run_simulation(sc, iterations = iterations),
        error = function(e) {
          stop(sprintf("Escenario '%s': %s", sid, err_chain(e)), call. = FALSE)
        }
      )
    })) %>%
    dplyr::select(scenario_id, domain_id, results)

  saveRDS(simulation_results, file = file.path(results_dir, "simulation_results.rds"))

  evaluator::summarize_to_disk(simulation_results = simulation_results, results_dir)

  scenario_summary <- evaluator::summarize_scenarios(simulation_results)
  domain_summary <- evaluator::summarize_domains(simulation_results)

  # Enriquecer el resumen por escenario con metadatos cualitativos
  # (tcomm, descripción) para mostrar en las tablas y narrativas.
  scenario_meta <- qual_inputs$qualitative_scenarios |>
    dplyr::select(scenario_id, tcomm, scenario_description = scenario)
  scenario_summary <- scenario_summary |>
    dplyr::left_join(scenario_meta, by = "scenario_id")

  res <- list(results_dir = results_dir,
              simulation_results = simulation_results,
              scenario_summary = scenario_summary,
              domain_summary = domain_summary,
              qualitative_scenarios = qual_inputs$qualitative_scenarios,
              capabilities = qual_inputs$capabilities,
              mappings = qual_inputs$mappings,
              missing_capabilities = missing_capabilities)

  # Persistencia: guardar la caché para no re-simular al abrir la app.
  guardar_cache_analisis(res, iterations)
  res
}

# Ruta del archivo sidecar con capacidades/controles personalizados creados
# desde la UI (sobrevive a los imports, que sobrescriben capabilities.csv)
custom_capabilities_path <- function(ws = evaluator_workspace()) {
  file.path(ws$inputs_dir, "custom_capabilities.csv")
}

# Leer controles personalizados: capability_id, capability (nombre), cost
# (costo anual $) y parámetros Beta-PERT de efectividad (eff_min/mode/max en %).
# Compatible con CSVs antiguos que solo tengan capability_id y capability.
read_custom_capabilities <- function() {
  path <- custom_capabilities_path()
  cols <- c(capability_id = "character", capability = "character",
            cost = "numeric", eff_min = "numeric", eff_mode = "numeric",
            eff_max = "numeric")
  if (!file.exists(path)) {
    return(data.frame(capability_id = character(), capability = character(),
                      cost = numeric(), eff_min = numeric(), eff_mode = numeric(),
                      eff_max = numeric(), stringsAsFactors = FALSE))
  }
  df <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))
  # Descartar filas corruptas cuyo capability_id sea una lista separada por
  # comas (bug antiguo de persist_unknown_controls): no son IDs válidos y solo
  # enturbian el catálogo, el dropdown y los costos.
  df <- df[!grepl(",", df$capability_id, fixed = TRUE), , drop = FALSE]
  # Añadir columnas ausentes (archivos de respaldo antiguos)
  for (col in setdiff(names(cols), names(df))) df[[col]] <- NA_character_
  df |>
    dplyr::mutate(
      cost = suppressWarnings(as.numeric(.data$cost)),
      eff_min = suppressWarnings(as.numeric(.data$eff_min)),
      eff_mode = suppressWarnings(as.numeric(.data$eff_mode)),
      eff_max = suppressWarnings(as.numeric(.data$eff_max))
    )
}

# Añadir/actualizar un control personalizado (upsert por capability_id),
# persistiendo también el costo y los parámetros Beta-PERT de efectividad (%).
write_custom_capability <- function(capability_id, capability = "",
                                    cost = NA_real_, eff_min = NA_real_,
                                    eff_mode = NA_real_, eff_max = NA_real_) {
  df <- read_custom_capabilities()
  # Descartar filas con ID ausente (lectura parcial transitoria bajo OneDrive
  # Files-On-Demand): rbind fallaría con "row names contain missing values".
  df <- df[!is.na(df$capability_id) & df$capability_id != capability_id, , drop = FALSE]
  existing <- read_custom_capabilities()
  existing <- existing[!is.na(existing$capability_id) &
                         existing$capability_id == capability_id, , drop = FALSE]
  new_row <- data.frame(
    capability_id = capability_id,
    capability = capability,
    cost = if (is.na(cost)) NA_real_ else as.numeric(cost),
    eff_min = if (is.na(eff_min)) NA_real_ else as.numeric(eff_min),
    eff_mode = if (is.na(eff_mode)) NA_real_ else as.numeric(eff_mode),
    eff_max = if (is.na(eff_max)) NA_real_ else as.numeric(eff_max),
    stringsAsFactors = FALSE
  )
  # Preservar valores existentes cuando el nuevo es NA: protege los costos
  # importados de un re-registro accidental con NA (p. ej. lectura parcial del
  # sidecar bajo OneDrive). Un costo 0 explícito SÍ sobrescribe.
  if (nrow(existing) > 0) {
    if (is.na(new_row$cost)) new_row$cost <- existing$cost[1]
    if (is.na(new_row$eff_min)) new_row$eff_min <- existing$eff_min[1]
    if (is.na(new_row$eff_mode)) new_row$eff_mode <- existing$eff_mode[1]
    if (is.na(new_row$eff_max)) new_row$eff_max <- existing$eff_max[1]
    if (!nzchar(new_row$capability)) new_row$capability <- existing$capability[1]
  }
  df <- rbind(df, new_row)
  readr::write_csv(df, custom_capabilities_path())
  df
}

# -----------------------------------------------------------------------------
# Costo por control EFECTIVO para la mitigación/ROSI: los persistidos en
# custom_capabilities.csv fusionados con los configurados en el módulo (el
# módulo tiene prioridad). Devuelve un vector nombrado capability_id -> costo.
# -----------------------------------------------------------------------------
control_costs_effective <- function(module_costs = NULL) {
  custom <- read_custom_capabilities()
  out <- numeric(0)
  if (nrow(custom) > 0) {
    out <- stats::setNames(custom$cost, custom$capability_id)
    out <- out[!is.na(out)]
  }
  if (!is.null(module_costs) && length(module_costs) > 0) {
    for (nm in names(module_costs)) {
      # El módulo pisa el costo del CSV SOLO si configuró un costo > 0 o si el
      # control no tiene costo persistido. Un 0 del módulo (valor por defecto
      # cuando el usuario no toca el campo) no debe borrar el costo importado.
      if (module_costs[[nm]] > 0 || !(nm %in% names(out))) {
        out[nm] <- unname(module_costs[[nm]])
      }
    }
  }
  out
}

# -----------------------------------------------------------------------------
# Parámetros Beta-PERT EFECTIVOS para la simulación: los persistidos en
# custom_capabilities.csv (cuando tienen las 3 efectividades) fusionados con
# los configurados en el módulo (el módulo tiene prioridad).
# -----------------------------------------------------------------------------
capability_pert_params_effective <- function(module_params = NULL) {
  custom <- read_custom_capabilities()
  out <- list()
  if (nrow(custom) > 0 &&
      all(c("eff_min", "eff_mode", "eff_max") %in% names(custom))) {
    d <- custom[!is.na(custom$eff_min) & !is.na(custom$eff_mode) &
                !is.na(custom$eff_max), , drop = FALSE]
    if (nrow(d) > 0) {
      out <- purrr::pmap(list(
        eff_min = d$eff_min, eff_mode = d$eff_mode, eff_max = d$eff_max
      ), function(eff_min, eff_mode, eff_max) {
        list(min = eff_min / 100, mode = eff_mode / 100, max = eff_max / 100,
             shape = 4, func = "mc2d::rpert")
      }) |> rlang::set_names(d$capability_id)
    }
  }
  if (!is.null(module_params) && length(module_params) > 0) {
    out[names(module_params)] <- module_params
  }
  out
}

# Genera el siguiente ID libre para controles personalizados (CTRL-01, CTRL-02...).
# El usuario escribe un NOMBRE; el ID interno se auto-genera para el pipeline.
next_custom_id <- function() {
  custom <- read_custom_capabilities()
  n <- 0L
  repeat {
    n <- n + 1L
    id <- sprintf("CTRL-%02d", n)
    if (!(id %in% custom$capability_id)) return(id)
  }
}

# -----------------------------------------------------------------------------
# Registra en custom_capabilities.csv los controles que aún no existen en el
# catálogo (predefinidos + personalizados). Se usa al añadir escenarios y al
# ejecutar el análisis, para que los controles referenciados por escenarios
# queden persistidos y no se pierdan al cerrar el proyecto.
#   control_ids: vector con los IDs/códigos de control.
#   names      : vector nombrado opcional (id -> nombre visible).
#   costs      : vector nombrado opcional (id -> costo $).
#   eff_df     : data.frame opcional con columnas capability_id, eff_min,
#                eff_mode, eff_max (efectividad Beta-PERT en %).
# Devuelve el número de controles nuevos registrados.
# -----------------------------------------------------------------------------
persist_unknown_controls <- function(control_ids, names = NULL, costs = NULL,
                                     eff_df = NULL) {
  # La columna de controles de los escenarios llega como un string con varios
  # IDs separados por comas (p. ej. "CTRL-01, CTRL-02"); separar ANTES de
  # registrar, o cada lista completa se escribiría como un único ID corrupto.
  ids <- unique(trimws(unlist(strsplit(as.character(control_ids), ",", fixed = TRUE))))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0) return(0L)
  known <- unname(get_evaluator_capabilities())
  # Nunca re-registrar controles que YA existen en el sidecar custom aunque el
  # catálogo no los vea (p. ej. lectura parcial bajo OneDrive): re-registrarlos
  # con costo NA pisaría los costos importados.
  custom_ids <- tryCatch(read_custom_capabilities()$capability_id,
                         error = function(e) character(0))
  known <- c(known, custom_ids)
  new_ids <- ids[!ids %in% known]
  if (length(new_ids) == 0) return(0L)
  n <- 0L
  for (id in new_ids) {
    nm <- if (!is.null(names) && nzchar(names[id])) names[id] else id
    costo <- if (!is.null(costs) && !is.na(costs[id])) costs[id] else NA_real_
    eff <- NULL
    if (!is.null(eff_df) && nrow(eff_df) > 0) {
      hit <- eff_df[eff_df$capability_id == id, , drop = FALSE]
      if (nrow(hit) > 0) {
        eff <- c(hit$eff_min[1], hit$eff_mode[1], hit$eff_max[1])
      }
    }
    ok <- tryCatch({
      write_custom_capability(
        id, nm,
        cost = if (length(costo) == 1 && is.numeric(costo) && !is.na(costo)) costo else NA_real_,
        eff_min = if (!is.null(eff) && is.numeric(eff[1])) eff[1] else NA_real_,
        eff_mode = if (!is.null(eff) && is.numeric(eff[2])) eff[2] else NA_real_,
        eff_max = if (!is.null(eff) && is.numeric(eff[3])) eff[3] else NA_real_
      )
      TRUE
    }, error = function(e) {
      message("No se pudo registrar el control '", id, "': ", conditionMessage(e))
      FALSE
    })
    if (ok) n <- n + 1L
  }
  n
}

# -----------------------------------------------------------------------------
# Actualiza costo/efectividad en custom_capabilities.csv de controles
# personalizados YA registrados (upsert preservando valores existentes cuando
# el nuevo valor es NA). Se usa al añadir un escenario con el módulo
# configurado y al añadir un control existente en el módulo. Sin esto, el
# costo configurado en el módulo se perdía al limpiarlo tras añadir el
# escenario (persist_unknown_controls solo registra controles NUEVOS) y la
# optimización veía costos en 0.
#   conf: data.frame con columnas capability_id, capability_desc, cost,
#         eff_min, eff_mode, eff_max (como configured() del módulo).
# Devuelve el número de controles actualizados.
# -----------------------------------------------------------------------------
upsert_custom_capability_params <- function(conf) {
  if (is.null(conf) || nrow(conf) == 0) return(0L)
  custom <- read_custom_capabilities()
  if (nrow(custom) == 0) return(0L)
  ids <- intersect(conf$capability_id, custom$capability_id)
  if (length(ids) == 0) return(0L)
  n <- 0L
  for (id in ids) {
    hit <- conf[conf$capability_id == id, , drop = FALSE][1, ]
    ex <- custom[custom$capability_id == id, , drop = FALSE][1, ]
    costo   <- if (!is.na(hit$cost)) hit$cost else ex$cost
    eff_min <- if (!is.na(hit$eff_min)) hit$eff_min else ex$eff_min
    eff_mode <- if (!is.na(hit$eff_mode)) hit$eff_mode else ex$eff_mode
    eff_max <- if (!is.na(hit$eff_max)) hit$eff_max else ex$eff_max
    nm <- if (nzchar(hit$capability_desc)) hit$capability_desc else ex$capability
    ok <- tryCatch({
      write_custom_capability(id, nm, cost = costo,
                              eff_min = eff_min, eff_mode = eff_mode,
                              eff_max = eff_max)
      TRUE
    }, error = function(e) {
      message("No se pudo actualizar el control '", id, "': ", conditionMessage(e))
      FALSE
    })
    if (ok) n <- n + 1L
  }
  n
}

# -----------------------------------------------------------------------------
# Añade los controles personalizados (custom_capabilities.csv) al catálogo de
# capabilities que consume el pipeline, para que sean tratados como controles
# de primera clase (sin warnings de "undefined capabilities"). A los
# personalizados se les asigna un nivel de madurez DIFF por defecto (el módulo
# los sobrescribe con los parámetros Beta-PERT configurados en la UI).
# -----------------------------------------------------------------------------
merge_custom_capabilities <- function(qual_inputs) {
  custom_caps <- read_custom_capabilities()
  if (nrow(custom_caps) == 0) return(qual_inputs)
  diff_labels <- qual_inputs$mappings$label[qual_inputs$mappings$type == "diff"]
  default_diff <- if ("3 - Definido" %in% diff_labels) "3 - Definido" else
    if ("3 - Defined" %in% diff_labels) "3 - Defined" else diff_labels[1]
  custom_ready <- data.frame(
    capability_id = custom_caps$capability_id,
    domain_id = "CUSTOM",
    capability = custom_caps$capability,
    diff = default_diff,
    stringsAsFactors = FALSE
  )
  qual_inputs$capabilities <- dplyr::bind_rows(qual_inputs$capabilities, custom_ready) |>
    dplyr::filter(!duplicated(.data$capability_id))
  qual_inputs
}

# -----------------------------------------------------------------------------
# Siguiente ID de escenario libre (RS-001, RS-002, ...) para el formulario de
# la app. Escanea todas las hojas del survey y devuelve el siguiente número
# disponible, evitando duplicados al añadir escenarios en cadena.
# -----------------------------------------------------------------------------
next_scenario_id <- function(base_dir = evaluator_workspace()$base_dir) {
  ws <- evaluator_workspace()
  survey_file <- file.path(ws$inputs_dir, "survey.xlsx")
  wb <- tryCatch(load_survey_workbook(survey_file), error = function(e) NULL)
  if (is.null(wb)) return("RS-001")
  sheets <- setdiff(openxlsx::getSheetNames(survey_file),
                    c("Introduction", "Definitions", "Reference"))
  ids <- character(0)
  for (sh in sheets) {
    dat <- tryCatch(openxlsx::readWorkbook(wb, sheet = sh, colNames = FALSE,
                                           skipEmptyRows = FALSE),
                    error = function(e) NULL)
    if (is.null(dat) || nrow(dat) == 0) next
    col1 <- as.character(dat[[1]])
    tr <- which(!is.na(col1) & col1 == "Threats")[1]
    if (is.na(tr) || nrow(dat) < tr + 2) next
    vals <- as.character(dat[[6]][(tr + 2):nrow(dat)])
    ids <- c(ids, vals[!is.na(vals) & nzchar(trimws(vals))])
  }
  nums <- suppressWarnings(as.integer(sub("^RS-", "", ids)))
  nums <- nums[!is.na(nums)]
  nxt <- if (length(nums) == 0) 1L else max(nums) + 1L
  sprintf("RS-%03d", nxt)
}

# -----------------------------------------------------------------------------
# Agrega un dominio NUEVO al proyecto: lo registra en domains.csv y crea su
# hoja en survey.xlsx con la estructura estándar (secciones Capabilities y
# Threats con 0 escenarios). El nuevo dominio aparece al instante en el
# selector de dominios.
#   domain_id  : código corto alfanumérico (ej. "GDPR", "APP")
#   domain_name: nombre visible en español (ej. "Protección de Datos")
# -----------------------------------------------------------------------------
add_new_domain <- function(domain_id, domain_name,
                           base_dir = evaluator_workspace()$base_dir) {
  ws <- evaluator_workspace()
  inputs_dir <- ws$inputs_dir
  survey_file <- file.path(inputs_dir, "survey.xlsx")

  # --- Validaciones ----------------------------------------------------------
  domain_id <- toupper(trimws(domain_id))
  domain_name <- trimws(domain_name)
  if (!nzchar(domain_id)) stop("El ID del dominio no puede estar vacío.", call. = FALSE)
  if (!grepl("^[A-Z0-9_]{2,10}$", domain_id)) {
    stop("El ID debe tener entre 2 y 10 caracteres alfanuméricos (ej. GDPR, APP).",
         call. = FALSE)
  }
  if (domain_id %in% c("Introduction", "Definitions", "Reference")) {
    stop("Ese ID está reservado para hojas internas del libro.", call. = FALSE)
  }
  if (!nzchar(domain_name)) stop("El nombre del dominio no puede estar vacío.", call. = FALSE)

  # --- domains.csv: comprobar duplicado y registrar --------------------------
  domains <- readr::read_csv(file.path(inputs_dir, "domains.csv"),
                             col_types = readr::cols(.default = readr::col_character()))
  if (domain_id %in% domains$domain_id) {
    stop(sprintf("El dominio %s ya existe.", domain_id), call. = FALSE)
  }
  domains <- rbind(domains, data.frame(domain_id = domain_id,
                                       domain = domain_name,
                                       stringsAsFactors = FALSE))
  readr::write_csv(domains, file.path(inputs_dir, "domains.csv"))

  # --- survey.xlsx: crear la hoja del nuevo dominio (0 escenarios) -----------
  wb <- load_survey_workbook(survey_file)
  openxlsx::addWorksheet(wb, domain_id)
  # Fila 1-2: sección Capabilities (solo encabezados; 0 controles por defecto)
  openxlsx::writeData(wb, sheet = domain_id, x = "Capabilities",
                      startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet = domain_id,
                      x = data.frame(Name = "Name", DIFF = "DIFF",
                                     Evidence = "Evidence", CapabilityID = "CapabilityID"),
                      startRow = 2, startCol = 1, colNames = FALSE)
  # Fila 3-4: sección Threats (encabezado de escenarios; 0 filas de datos)
  openxlsx::writeData(wb, sheet = domain_id, x = "Threats",
                      startRow = 3, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet = domain_id,
                      x = data.frame(Scenario = "Scenario", TComm = "TComm", TEF = "TEF",
                                     TC = "TC", LM = "LM", ScenarioID = "ScenarioID",
                                     Capabilities = "Capabilities"),
                      startRow = 4, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, survey_file, overwrite = TRUE)

  invisible(domain_id)
}

# -----------------------------------------------------------------------------
# Domains base (los 14 predefinidos del proyecto) y sus nombres en español.
# Se usan al reiniciar el proyecto para restaurar el estado original.
# -----------------------------------------------------------------------------
base_domains_es <- data.frame(
  domain_id = c("ISMP","AC","HR","RISK","POL","ORG","COMP","ASSET","PHY","OPS","ADM","IM","BC","PRI"),
  domain = c("Programa de Gestión de Seguridad de la Información",
             "Control de Acceso",
             "Seguridad de Recursos Humanos",
             "Gestión de Riesgos",
             "Política de Seguridad",
             "Organización de la Seguridad de la Información",
             "Cumplimiento",
             "Gestión de Activos",
             "Seguridad Física y Ambiental",
             "Gestión de Comunicaciones y Operaciones",
             "Adquisición, Desarrollo y Mantenimiento de Sistemas de Información",
             "Gestión de Incidentes de Seguridad de la Información",
             "Gestión de Continuidad del Negocio",
             "Prácticas de Privacidad"),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# REINICIA el proyecto a un estado "recién creado":
#   1. Survey.xlsx -> plantilla vacía (0 escenarios, secciones predefinidas).
#   2. Se eliminan los dominios personalizados (se restauran los 14 base).
#   3. Se borran los controles personalizados (custom_capabilities.csv).
# NO toca el código de la app; los resultados de análisis se regeneran.
# -----------------------------------------------------------------------------
reset_project <- function(base_dir = evaluator_workspace()$base_dir) {
  ws <- evaluator_workspace()
  inputs_dir <- ws$inputs_dir
  survey_file <- file.path(inputs_dir, "survey.xlsx")
  meta <- c("Introduction", "Definitions", "Reference")

  # 1. Plantilla vacía (borra escenarios, conserva secciones de capabilities)
  reset_survey_template(base_dir)

  # 2. Eliminar dominios personalizados (hojas no base) y restaurar domains.csv
  wb <- load_survey_workbook(survey_file)
  sheets <- openxlsx::getSheetNames(survey_file)
  for (sh in setdiff(sheets, c(meta, base_domains_es$domain_id))) {
    openxlsx::removeWorksheet(wb, sh)
  }
  openxlsx::saveWorkbook(wb, survey_file, overwrite = TRUE)
  readr::write_csv(base_domains_es, file.path(inputs_dir, "domains.csv"))

  # 3. Borrar controles personalizados
  custom_path <- custom_capabilities_path()
  if (file.exists(custom_path)) file.remove(custom_path)

  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Plantilla Excel vacía: mantiene ÚNICAMENTE los encabezados (y la sección de
# Capabilities de referencia) y pone 0 filas de escenarios en cada dominio.
# Útil para entregar la app sin escenarios de ejemplo.
# -----------------------------------------------------------------------------
reset_survey_template <- function(base_dir = evaluator_workspace()$base_dir) {
  ws <- evaluator_workspace()
  survey_file <- file.path(ws$inputs_dir, "survey.xlsx")
  wb <- openxlsx::loadWorkbook(survey_file)

  for (sheet_name in openxlsx::getSheetNames(survey_file)) {
    if (sheet_name %in% c("Introduction", "Definitions", "Reference")) next
    dat <- openxlsx::readWorkbook(survey_file, sheet = sheet_name,
                                  colNames = FALSE, skipEmptyRows = FALSE)
    if (nrow(dat) == 0) next
    col1 <- as.character(dat[[1]])
    threats_row <- which(!is.na(col1) & col1 == "Threats")[1]
    if (is.na(threats_row)) next
    header_row <- threats_row + 1
    # Limpiar únicamente las filas de datos (después del encabezado de Threats)
    if (nrow(dat) > header_row) {
      rows <- seq(header_row + 1, nrow(dat))
      blank <- as.data.frame(matrix(NA_character_, nrow = length(rows),
                                    ncol = ncol(dat)), stringsAsFactors = FALSE)
      openxlsx::writeData(wb, sheet = sheet_name, x = blank,
                          startRow = header_row + 1, colNames = FALSE)
    }
  }
  openxlsx::saveWorkbook(wb, survey_file, overwrite = TRUE)
  invisible(survey_file)
}

# -----------------------------------------------------------------------------
# Catálogo de controles en español (sidecar capabilities_es.csv).
# Devuelve un data.frame (capability_id, capability_es, descripcion_es) o vacío
# si no existe. capability_es = nombre corto de catálogo; descripcion_es =
# descripción ejecutiva de 1 línea que consume el Copilot de IA.
# -----------------------------------------------------------------------------
read_capabilities_es <- function() {
  path <- file.path(evaluator_workspace()$inputs_dir, "capabilities_es.csv")
  if (!file.exists(path)) {
    return(data.frame(capability_id = character(), capability_es = character(),
                      descripcion_es = character(), stringsAsFactors = FALSE))
  }
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))
}

# Nombre visible de un control para la UI: primero el nombre corto en español
# (capabilities_es.csv), luego el nombre del control personalizado
# (custom_capabilities.csv) y, si no existe, el propio ID.
capability_es_name <- function(capability_ids) {
  es <- read_capabilities_es()
  custom <- read_custom_capabilities()
  vapply(capability_ids, function(id) {
    hit <- es$capability_es[es$capability_id == id]
    if (length(hit) > 0 && !is.na(hit[1]) && nzchar(hit[1])) return(hit[1])
    hit2 <- custom$capability[custom$capability_id == id]
    if (length(hit2) > 0 && !is.na(hit2[1]) && nzchar(hit2[1])) return(hit2[1])
    id
  }, character(1), USE.NAMES = FALSE)
}

# Descripción ejecutiva en español de un control (descripcion_es), usada por el
# Copilot. Si no existe, cadena vacía.
capability_es_description <- function(capability_ids) {
  es <- read_capabilities_es()
  vapply(capability_ids, function(id) {
    hit <- es$descripcion_es[es$capability_id == id]
    if (length(hit) > 0 && !is.na(hit[1]) && nzchar(hit[1])) hit[1] else ""
  }, character(1), USE.NAMES = FALSE)
}

# IDs/descripciones de controles disponibles para el selector múltiple de la
# UI. Usa el nombre en español (capabilities_es.csv) cuando está disponible.
# ROBUSTO: si capabilities.csv no se puede leer (p. ej. placeholder de OneDrive
# sin descargar, "compressed,dataless"), se cae al sidecar en español
# (capabilities_es.csv contiene los 60 capability_id) y la app NO revienta.
get_evaluator_capabilities <- function() {
  ws <- evaluator_workspace()
  # La lista maestra real generada por import_spreadsheet es capabilities.csv
  path <- file.path(ws$inputs_dir, "capabilities.csv")
  if (!file.exists(path)) {
    return(stats::setNames(c("CAP-01", "CAP-02", "CAP-03"),
                           c("CAP-01 - Control placeholder", "CAP-02 - Control placeholder", "CAP-03 - Control placeholder")))
  }
  caps <- tryCatch(
    readr::read_csv(path, col_types = readr::cols(.default = readr::col_character())),
    error = function(e) {
      message("Aviso: no se pudo leer capabilities.csv (", conditionMessage(e),
              "). Se usará el catálogo en español.")
      data.frame(capability_id = character(), capability = character(),
                 stringsAsFactors = FALSE)
    }
  )
  if (!all(c("capability_id", "capability") %in% names(caps))) {
    message("Aviso: capabilities.csv no tiene las columnas esperadas; se usará el catálogo en español.")
    caps <- data.frame(capability_id = character(), capability = character(),
                       stringsAsFactors = FALSE)
  }
  custom <- read_custom_capabilities()
  es <- read_capabilities_es()
  # Si la lista maestra está vacía o ilegible, usar el sidecar en español
  # (tiene los 60 IDs y nombres), de modo que el selector no quede vacío.
  if (nrow(caps) == 0 && nrow(es) > 0) {
    caps <- data.frame(capability_id = es$capability_id,
                       capability = es$capability_es, stringsAsFactors = FALSE)
  }
  all <- rbind(caps[, c("capability_id", "capability"), drop = FALSE],
               custom[, c("capability_id", "capability"), drop = FALSE])
  all <- all[!duplicated(all$capability_id), , drop = FALSE]
  # Traducir al español cuando exista la entrada en el sidecar
  all$nombre <- ifelse(
    all$capability_id %in% es$capability_id,
    es$capability_es[match(all$capability_id, es$capability_id)],
    all$capability
  )
  stats::setNames(all$capability_id, paste0(all$capability_id, " - ", all$nombre))
}

# -----------------------------------------------------------------------------
# Texto con los controles de mayor ahorro (top k) en español, listo para los
# prompts del Copilot. Incluye nombre corto + descripción ejecutiva + ahorro.
# control_level: data.frame con columnas capability_id y marginal_savings
# (salida de run_mitigation_analysis()).
# -----------------------------------------------------------------------------
top_controls_text <- function(control_level, k = 5) {
  if (is.null(control_level) || nrow(control_level) == 0) {
    return("- No hay datos de controles (ejecuta el análisis de mitigación).")
  }
  es <- read_capabilities_es()
  cl <- control_level |>
    dplyr::group_by(.data$capability_id) |>
    dplyr::summarise(ahorro = sum(.data$marginal_savings, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$ahorro)) |>
    utils::head(k)
  lines <- character(nrow(cl))
  for (i in seq_len(nrow(cl))) {
    id <- cl$capability_id[i]
    nm <- capability_es_name(id)
    ex <- capability_es_description(id)
    lines[i] <- sprintf("- %s (%s): ahorro estimado de %s. %s",
                        id, nm, fmt_compact_money(cl$ahorro[i]), ex)
  }
  paste(lines, collapse = "\n")
}

# -----------------------------------------------------------------------------
# OPTIMIZACIÓN DE CONTROLES POR ESCENARIO (problema de la mochila 0/1)
#
# Dado un presupuesto por escenario, se busca la combinación de controles que
# maximiza el ROSI (retorno de la inversión en seguridad) sin superar el
# presupuesto. Se usa la enumeración exhaustiva de subconjuntos porque el
# número de controles por escenario es pequeño (límite práctico de 24).
# -----------------------------------------------------------------------------

# Encuentra el subconjunto de controles que maximiza el ROSI sujeto al
# presupuesto. costos/savings: vectores numéricos por control. Devuelve una
# lista con selected (índices), cost, savings, net y rosi.
knapsack_best_controls <- function(costs, savings, budget = 0) {
  n <- length(costs)
  if (n == 0) {
    return(list(selected = integer(0), cost = 0, savings = 0,
                net = 0, rosi = 0))
  }
  if (n > 24) {
    stop("Demasiados controles en un escenario para optimizar (máx. 24).",
         call. = FALSE)
  }

  # Criterio: máximo ROSI; desempate por ahorro neto (savings - cost).
  is_better <- function(rosi_a, net_a, rosi_b, net_b) {
    if (rosi_a > rosi_b + 1e-9) TRUE
    else if (rosi_a < rosi_b - 1e-9) FALSE
    else net_a > net_b + 1e-9
  }

  # Conjunto vacío como referencia: ROSI 0 / ahorro neto 0 (no invertir).
  best_mask <- 0L
  best_rosi <- 0
  best_net <- 0

  for (mask in seq_len(2^n - 1)) {
    sel <- which(as.logical(intToBits(mask)[seq_len(n)]))
    c <- sum(costs[sel])
    if (c > budget + 1e-9) next
    s <- sum(savings[sel])
    net <- s - c
    rosi <- if (c > 1e-9) (s - c) / c else if (s > 1e-9) Inf else 0
    if (is_better(rosi, net, best_rosi, best_net)) {
      best_mask <- mask
      best_rosi <- rosi
      best_net <- net
    }
  }

  sel <- which(as.logical(intToBits(best_mask)[seq_len(n)]))
  c <- sum(costs[sel])
  s <- sum(savings[sel])
  list(selected = sel, cost = c, savings = s, net = s - c,
       rosi = if (c > 1e-9) (s - c) / c else if (s > 1e-9) Inf else 0)
}

# Optimiza los controles de CADA escenario dado el presupuesto.
# - control_level: data.frame de run_mitigation_analysis() con columnas
#   scenario_id, capability_id y marginal_savings (beneficio por control).
# - control_costs: vector nombrado (capability_id -> costo $) del módulo de
#   controles. Solo se optimizan los controles con costo definido.
# - budget: presupuesto máximo por escenario ($).
# Devuelve un data.frame con la combinación óptima por escenario.
optimize_controls_rosi <- function(control_level, control_costs, budget = 0) {
  empty <- data.frame(
    scenario_id = character(), n_candidates = integer(),
    controls = character(), control_names = character(),
    opt_cost = numeric(), opt_savings = numeric(), opt_net = numeric(),
    opt_rosi = numeric(), all_cost = numeric(), all_savings = numeric(),
    all_rosi = numeric(), stringsAsFactors = FALSE
  )
  if (is.null(control_level) || nrow(control_level) == 0) return(empty)
  if (is.null(control_costs) || length(control_costs) == 0) return(empty)

  dat <- control_level |>
    dplyr::filter(.data$capability_id %in% names(control_costs)) |>
    dplyr::mutate(cost = unname(control_costs[.data$capability_id]),
                  savings = .data$marginal_savings)

  if (nrow(dat) == 0) return(empty)

  dat |>
    dplyr::group_by(.data$scenario_id) |>
    dplyr::group_split() |>
    purrr::map_dfr(function(d) {
      ids <- d$capability_id
      best <- knapsack_best_controls(d$cost, d$savings, budget)
      sel <- ids[best$selected]
      all_cost <- sum(d$cost)
      all_sav <- sum(d$savings)
      all_rosi <- if (all_cost > 1e-9) (all_sav - all_cost) / all_cost else 0
      data.frame(
        scenario_id = d$scenario_id[1],
        n_candidates = length(ids),
        controls = paste(sel, collapse = ", "),
        control_names = paste(capability_es_name(sel), collapse = ", "),
        opt_cost = best$cost,
        opt_savings = best$savings,
        opt_net = best$net,
        opt_rosi = best$rosi,
        all_cost = all_cost,
        all_savings = all_sav,
        all_rosi = all_rosi,
        stringsAsFactors = FALSE
      )
    })
}

# -----------------------------------------------------------------------------
# OPTIMIZACIÓN CON PRESUPUESTO GLOBAL (problema de la mochila de selección
# múltiple, MCKP): un solo presupuesto B repartido entre TODOS los escenarios.
# Por cada escenario se elige UNA combinación de controles (posiblemente la
# vacía) maximizando el ahorro neto total (Σ savings − Σ cost) con costo total
# ≤ B. Se resuelve con programación dinámica sobre los costos alcanzables.
# -----------------------------------------------------------------------------

# Índices de opciones NO dominadas (Pareto): al ordenar por costo creciente,
# se conservan las opciones que mejoran el valor. El resto es dominado.
pareto_indices <- function(cost, value) {
  ord <- order(cost, -value)
  keep <- logical(length(cost))
  best_value <- -Inf
  for (i in ord) {
    if (value[i] > best_value + 1e-9) {
      keep[i] <- TRUE
      best_value <- value[i]
    }
  }
  which(keep)
}

# Optimiza la asignación GLOBAL del presupuesto entre todos los escenarios.
# control_level: data.frame (scenario_id, capability_id, marginal_savings).
# control_costs: vector nombrado (capability_id -> costo $).
# budget: presupuesto TOTAL ($).
# Devuelve list(by_scenario = data.frame por escenario, totals = list(...)).
optimize_controls_global <- function(control_level, control_costs, budget = 0) {
  empty_df <- data.frame(
    scenario_id = character(), n_candidates = integer(),
    controls = character(), control_names = character(),
    opt_cost = numeric(), opt_savings = numeric(), opt_net = numeric(),
    opt_rosi = numeric(), stringsAsFactors = FALSE
  )
  empty_totals <- list(cost = 0, savings = 0, net = 0,
                       rosi = NA_real_, n_scenarios = 0L)
  if (is.null(control_level) || nrow(control_level) == 0) {
    return(list(by_scenario = empty_df, totals = empty_totals))
  }
  if (is.null(control_costs) || length(control_costs) == 0) {
    return(list(by_scenario = empty_df, totals = empty_totals))
  }

  dat <- control_level |>
    dplyr::filter(.data$capability_id %in% names(control_costs)) |>
    dplyr::mutate(cost = unname(control_costs[.data$capability_id]),
                  savings = .data$marginal_savings)
  if (nrow(dat) == 0) return(list(by_scenario = empty_df, totals = empty_totals))

  groups <- dat |> dplyr::group_by(.data$scenario_id) |> dplyr::group_split()

  # Opciones por escenario: máscara de controles, costo y valor neto (Pareto)
  opts_list <- lapply(groups, function(d) {
    ids <- d$capability_id
    costs <- d$cost
    savings <- d$savings
    n <- length(ids)
    masks <- 0:(2^n - 1)
    cost <- vapply(masks, function(m) {
      sel <- which(as.logical(intToBits(m)[seq_len(n)]))
      sum(costs[sel])
    }, numeric(1))
    value <- vapply(masks, function(m) {
      sel <- which(as.logical(intToBits(m)[seq_len(n)]))
      sum(savings[sel] - costs[sel])
    }, numeric(1))
    keep <- pareto_indices(cost, value)
    data.frame(mask = masks[keep], cost = cost[keep], value = value[keep])
  })

  # DP: costo total alcanzable -> mejor valor + opción elegida por escenario
  dp <- list("0" = list(value = 0, choices = integer(0)))
  for (s in seq_along(opts_list)) {
    opts <- opts_list[[s]]
    new_dp <- list()
    for (nm in names(dp)) {
      cur_cost <- as.numeric(nm)
      cur_value <- dp[[nm]]$value
      for (i in seq_len(nrow(opts))) {
        c2 <- cur_cost + opts$cost[i]
        if (c2 > budget + 1e-9) next
        v2 <- cur_value + opts$value[i]
        key <- as.character(round(c2))
        if (is.null(new_dp[[key]]) || v2 > new_dp[[key]]$value + 1e-9) {
          new_dp[[key]] <- list(value = v2, choices = c(dp[[nm]]$choices, i))
        }
      }
    }
    dp <- new_dp
  }

  # Mejor estado con costo total <= presupuesto
  best <- NULL
  best_cost <- 0
  for (nm in names(dp)) {
    if (is.null(best) || dp[[nm]]$value > best$value + 1e-9) {
      best <- dp[[nm]]
      best_cost <- as.numeric(nm)
    }
  }

  # Reconstrucción por escenario
  rows <- lapply(seq_along(groups), function(s) {
    ids <- groups[[s]]$capability_id
    opt_i <- best$choices[s]
    o <- opts_list[[s]][opt_i, ]
    sel <- which(as.logical(intToBits(o$mask)[seq_len(length(ids))]))
    chosen <- ids[sel]
    opt_net <- o$value
    opt_cost <- o$cost
    opt_sav <- opt_net + opt_cost
    opt_rosi <- if (opt_cost > 1e-9) opt_net / opt_cost else if (opt_net > 1e-9) Inf else 0
    data.frame(
      scenario_id = groups[[s]]$scenario_id[1],
      n_candidates = length(ids),
      controls = paste(chosen, collapse = ", "),
      control_names = paste(capability_es_name(chosen), collapse = ", "),
      opt_cost = opt_cost, opt_savings = opt_sav, opt_net = opt_net,
      opt_rosi = opt_rosi, stringsAsFactors = FALSE
    )
  })
  by_scenario <- dplyr::bind_rows(rows)
  totals <- list(
    cost = sum(by_scenario$opt_cost),
    savings = sum(by_scenario$opt_savings),
    net = sum(by_scenario$opt_net),
    rosi = if (sum(by_scenario$opt_cost) > 1e-9)
      sum(by_scenario$opt_net) / sum(by_scenario$opt_cost) else NA_real_,
    n_scenarios = nrow(by_scenario)
  )
  list(by_scenario = by_scenario, totals = totals)
}

# Compara el ALE inherente (sin controles) vs residual (con controles) y
# atribuye a cada capability su beneficio INDIVIDUAL (ALE inherente − ALE con
# solo ese control), estimado con multistart (mediana de medianas sobre varias
# semillas) para que los valores que alimentan la optimización sean estables.
run_mitigation_analysis <- function(iterations = 1e3,
                                    qualitative_scenarios = NULL,
                                    capabilities = NULL,
                                    mappings = NULL,
                                    simulation_results = NULL,
                                    custom_diff_params = NULL,
                                    control_costs = NULL,
                                    sim_seeds = c(31337, 31337001),
                                    base_dir = evaluator_workspace()$base_dir) {
  ws <- evaluator_workspace()
  inputs_dir <- ws$inputs_dir
  results_dir <- ws$results_dir

  if (is.null(qualitative_scenarios) || is.null(capabilities) || is.null(mappings)) {
    domains <- readr::read_csv(file.path(inputs_dir, "domains.csv"),
                               col_types = readr::cols(.default = readr::col_character()))
    survey_file <- file.path(inputs_dir, "survey.xlsx")
    evaluator::import_spreadsheet(survey_file, domains, inputs_dir)
    qual_inputs <- evaluator::read_qualitative_inputs(inputs_dir)
    qual_inputs <- merge_custom_capabilities(qual_inputs)
    qualitative_scenarios <- qual_inputs$qualitative_scenarios
    capabilities <- qual_inputs$capabilities
    mappings <- qual_inputs$mappings
  }

  # --- Plantilla vacía (0 escenarios): corto-circuito seguro -----------------
  # Sin escenarios no hay ALE inherente/residual que simular; se devuelven
  # marcos vacíos con las columnas que espera la UI de "Efectividad de
  # Controles" para mostrar su mensaje de espera.
  if (nrow(qualitative_scenarios) == 0) {
    empty_scenario_level <- tibble::tibble(
      scenario_id = character(), scenario_description = character(),
      tcomm = character(), domain_id = character(),
      ale_inherent = numeric(), ale_residual = numeric(),
      amount_saved = numeric(), reduction_pct = numeric(),
      control_cost = numeric(), net_savings = numeric(), rosi = numeric()
    )
    empty_control_level <- tibble::tibble(
      scenario_id = character(), capability_id = character(),
      ale_solo = numeric(), scenario_description = character(),
      tcomm = character(), domain_id = character(),
      ale_inherent = numeric(), ale_residual = numeric(),
      marginal_savings = numeric(), reduction_pct = numeric(),
      capability = character()
    )
    return(list(scenario_level = empty_scenario_level,
                control_level = empty_control_level))
  }

  # Mediana de ALE para un escenario codificado, con ESTIMACIÓN MULTISTART:
  # se corre el modelo con VARIAS semillas y se toma la mediana de las medianas.
  # El modelo (openfair_tef_tc_diff_lm) lee la semilla de options(tfm.evaluator.seed),
  # por defecto 31337 (determinista). Sin multistart, comparar configuraciones
  # con controles distintos es ruidoso (el muestreo de LM cambia de corrida a
  # corrida) y el "ahorro por control" sale ~0 o negativo.
  # custom_diff_params se aplica solo si el escenario tiene controles (no en el
  # caso inherente, donde controls = "").
  est_mediana <- function(scen_df, n_iters) {
    qs <- evaluator::encode_scenarios(scen_df, capabilities, mappings)
    has_controls <- nzchar(trimws(paste(scen_df$controls, collapse = "")))
    if (has_controls && !is.null(custom_diff_params) && length(custom_diff_params) > 0) {
      qs$scenario[[1]]$parameters$diff <-
        utils::modifyList(qs$scenario[[1]]$parameters$diff, custom_diff_params)
    }
    meds <- vapply(sim_seeds, function(sd) {
      options(tfm.evaluator.seed = sd)
      on.exit(options(tfm.evaluator.seed = NULL), add = TRUE)
      evaluator::run_simulation(qs$scenario[[1]], iterations = n_iters) |>
        dplyr::pull(.data$ale) |> stats::median(na.rm = TRUE)
    }, numeric(1))
    stats::median(meds, na.rm = TRUE)
  }

  # ALE inherente (sin controles): se simula cada escenario con controls = ""
  inherent_ale <- vapply(seq_len(nrow(qualitative_scenarios)), function(i) {
    s <- qualitative_scenarios[i, ]
    s$controls <- ""
    est_mediana(s, iterations)
  }, numeric(1))

  # ALE residual (con todos los controles). Se estima con el mismo multistart
  # para que la comparación con los escenarios "solo control" sea consistente
  # (ya no se reutiliza la simulación principal de una sola semilla).
  residual_ale <- vapply(seq_len(nrow(qualitative_scenarios)), function(i) {
    est_mediana(qualitative_scenarios[i, ], iterations)
  }, numeric(1))

  scenario_level <- qualitative_scenarios |>
    dplyr::select(scenario_id, scenario_description = scenario, tcomm, domain_id) |>
    dplyr::mutate(
      ale_inherent = inherent_ale,
      ale_residual = residual_ale,
      amount_saved = .data$ale_inherent - .data$ale_residual,
      reduction_pct = ifelse(.data$ale_inherent > 0,
                             .data$amount_saved / .data$ale_inherent, 0)
    )

  # ---- ROSI: costo por control -> ahorro neto y retorno de inversión ---------
  # control_costs: vector nombrado (capability_id -> costo $) definido en el
  # módulo de capabilities. Costo del escenario = suma de costos de sus controles.
  if (!is.null(control_costs) && length(control_costs) > 0) {
    scenario_level$control_cost <- vapply(seq_len(nrow(qualitative_scenarios)), function(i) {
      ids <- unique(trimws(unlist(strsplit(as.character(qualitative_scenarios$controls[i]), ","))))
      ids <- ids[nzchar(ids)]
      sum(control_costs[ids], na.rm = TRUE)
    }, numeric(1))
  } else {
    scenario_level$control_cost <- 0
  }
  scenario_level <- scenario_level |>
    dplyr::mutate(
      net_savings = .data$amount_saved - .data$control_cost,
      # ROSI = [(ALE inherente - ALE residual) - Costo] / Costo * 100
      rosi = ifelse(.data$control_cost > 0,
                    .data$net_savings / .data$control_cost * 100,
                    NA_real_)
    )

  # Beneficio por control para la mochila: reducción INDIVIDUAL del ALE respecto
  # a NO tener controles (ALE inherente − ALE con SOLO ese control).
  # NOTA METODOLÓGICA: con leave-one-out (quitar un control del set completo) y
  # varios controles similares, el ahorro marginal individual colapsa a ~0 (la
  # fuerza media apenas cambia) y además es ruidoso por muestreo no emparejado;
  # por eso la optimización "no elegía nada". El beneficio individual es la
  # métrica natural de una mochila (valor por ítem), con la salvedad de que si
  # dos controles se solapan sus beneficios no son aditivos (se sobre-contaría
  # al sumarlos); la mochila elige por presupuesto, no por suma ciega.
  control_rows <- list()
  for (i in seq_len(nrow(qualitative_scenarios))) {
    s <- qualitative_scenarios[i, ]
    ctrl_ids <- unique(trimws(unlist(strsplit(as.character(s$controls), ","))))
    ctrl_ids <- ctrl_ids[nzchar(ctrl_ids)]
    for (ci in ctrl_ids) {
      s2 <- s
      s2$controls <- ci
      control_rows[[length(control_rows) + 1]] <- tibble::tibble(
        scenario_id = s$scenario_id,
        capability_id = ci,
        ale_solo = est_mediana(s2, iterations)
      )
    }
  }
  control_sims <- dplyr::bind_rows(control_rows)

  control_level <- control_sims |>
    dplyr::left_join(
      scenario_level |> dplyr::select(scenario_id, scenario_description, tcomm, domain_id,
                                      ale_inherent, ale_residual),
      by = "scenario_id"
    ) |>
    dplyr::mutate(
      marginal_savings = .data$ale_inherent - .data$ale_solo,
      reduction_pct = ifelse(.data$ale_inherent > 0,
                             .data$marginal_savings / .data$ale_inherent, 0)
    ) |>
    dplyr::left_join(capabilities |> dplyr::select(capability_id, capability),
                     by = "capability_id")

  saveRDS(list(scenario_level = scenario_level, control_level = control_level),
          file = file.path(results_dir, "mitigation_results.rds"))
  readr::write_csv(scenario_level, file.path(results_dir, "mitigation_scenario_level.csv"))
  readr::write_csv(control_level, file.path(results_dir, "mitigation_control_level.csv"))

  list(scenario_level = scenario_level,
       control_level = control_level)
}

# -----------------------------------------------------------------------------
# Convergencia de Monte Carlo: alerta si iteraciones < 10,000 con margen de
# error de la mediana de ALE (SE ~ 1.2533 * sd / sqrt(n)).
# -----------------------------------------------------------------------------
mc_convergence_check <- function(simulation_results, iterations) {
  conv <- simulation_results |>
    dplyr::mutate(
      ale_median = purrr::map_dbl(.data$results, ~stats::median(.x$ale, na.rm = TRUE)),
      ale_sd = purrr::map_dbl(.data$results, ~stats::sd(.x$ale, na.rm = TRUE))
    ) |>
    dplyr::mutate(
      moe = 1.2533 * .data$ale_sd / sqrt(iterations),
      moe_pct = ifelse(.data$ale_median > 0,
                       .data$moe / .data$ale_median * 100, NA_real_)
    ) |>
    dplyr::select(scenario_id, domain_id, ale_median, ale_sd, moe, moe_pct)

  low <- iterations < 10000
  if (low) {
    avg_moe <- mean(conv$moe_pct, na.rm = TRUE)
    msg <- sprintf(
      paste0("Advertencia de convergencia: %d iteraciones (< 10,000). ",
             "El margen de error estándar de la mediana de ALE promedia %.2f%%; ",
             "aumenta las iteraciones para reducir ruido en las estimaciones."),
      iterations, avg_moe)
  } else {
    msg <- sprintf("Convergencia aceptable: %d iteraciones (>= 10,000).", iterations)
  }

  list(message = msg, table = conv, low = low)
}

# -----------------------------------------------------------------------------
# Sensibilidad (Tornado): para un escenario, perturba ±20% los parámetros de
# TEF, TC y LM y mide el cambio en la mediana de ALE.
# -----------------------------------------------------------------------------
run_sensitivity_tornado <- function(scenario_row, capabilities, mappings,
                                    iterations = 1000) {
  qs <- evaluator::encode_scenarios(scenario_row, capabilities, mappings)
  sc <- qs$scenario[[1]]
  base <- stats::median(evaluator::run_simulation(sc, iterations = iterations)$ale)

  rows <- list()
  for (dim in c("tef", "tc", "lm")) {
    p <- sc$parameters[[dim]]
    num_idx <- vapply(p, is.numeric, logical(1))
    num_names <- names(p)[num_idx]
    if (length(num_names) == 0) next

    p_lo <- p; p_hi <- p
    for (nm in num_names) {
      p_lo[[nm]] <- p[[nm]] * 0.8
      p_hi[[nm]] <- p[[nm]] * 1.2
    }
    s_lo <- sc; s_lo$parameters[[dim]] <- p_lo
    s_hi <- sc; s_hi$parameters[[dim]] <- p_hi

    lo <- stats::median(evaluator::run_simulation(s_lo, iterations = iterations)$ale)
    hi <- stats::median(evaluator::run_simulation(s_hi, iterations = iterations)$ale)
    rows[[length(rows) + 1]] <- tibble::tibble(
      variable = toupper(dim),
      low = lo, high = hi, baseline = base,
      spread = abs(hi - lo)
    )
  }
  dplyr::bind_rows(rows) |> dplyr::arrange(dplyr::desc(.data$spread))
}

# -----------------------------------------------------------------------------
# Narrativa ejecutiva (Copilot de IA): resumen de 3 párrafos en lenguaje no
# técnico, listo para la Junta Directiva. Determinista (sin LLM externo).
# -----------------------------------------------------------------------------
generate_executive_narrative <- function(kpis) {
  p1 <- sprintf(
    paste0("Nuestra organización enfrenta una exposición financiera anual ",
           "esperada de <b>%s</b> en el peor escenario, concentrada ",
           "principalmente en <b>%s</b>. El escenario de mayor riesgo es ",
           "<b>%s</b>, que requiere atención prioritaria por parte de la dirección."),
    fmt_compact_money(kpis$total_ale), kpis$top_tcomm, kpis$top_scenario
  )
  p2 <- sprintf(
    paste0("Con los controles de seguridad actualmente implementados, logramos ",
           "una reducción de pérdida de <b>%s</b>, generando un ahorro neto de ",
           "<b>%s</b> tras considerar los costos de implementación. El retorno ",
           "sobre la inversión en seguridad (ROSI) es de <b>%s</b>, lo que ",
           "confirma que el programa de controles es financieramente justificable."),
    scales::percent(kpis$reduction_pct, accuracy = 0.1),
    fmt_compact_money(kpis$net_savings),
    ifelse(is.na(kpis$rosi), "N/D", scales::percent(kpis$rosi / 100, accuracy = 0.1))
  )
  p3 <- sprintf(
    paste0("Se recomienda mantener la inversión en <b>%s</b> y los controles ",
           "de mayor eficacia, monitorear continuamente los escenarios de alto ",
           "impacto y revisar periódicamente el apetito al riesgo definido por ",
           "la junta. Las estimaciones presentadas provienen de simulaciones ",
           "Monte Carlo y deben interpretarse como rangos probables, no como ",
           "cifras exactas."),
    kpis$top_control
  )
  paste(p1, p2, p3, sep = "\n\n")
}

# -----------------------------------------------------------------------------
# IA LOCAL con Ollama (módulo OPCIONAL). La app funciona siempre en modo
# estándar; si el módulo está activo (ENABLE_OLLAMA=true y el servicio
# responde), genera resúmenes y responde preguntas con llama3.2:1b.
# EXTREMADAMENTE robusto: NUNCA lanza excepciones; detecta la URL del servicio
# y deja logs de depuración en la consola de R/Docker.
# -----------------------------------------------------------------------------

# --- Detección de la URL del servicio Ollama (Docker-friendly) --------------
# Prioridad: 1) env OLLAMA_HOST, 2) 127.0.0.1:11434 (Ollama en el MISMO
# contenedor), 3) host.docker.internal:11434 (Ollama en el host, Docker Desktop).
ollama_hosts <- function() {
  host <- Sys.getenv("OLLAMA_HOST", unset = "")
  if (nzchar(host)) {
    if (!grepl("^https?://", host)) host <- paste0("http://", host)
    return(sub("/+$", "", host))
  }
  c("http://127.0.0.1:11434", "http://host.docker.internal:11434")
}

# Helpers: nunca dejan NULL/NA en el prompt (usar "N/A" como valor por defecto)
safe_money <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) "N/A" else fmt_compact_money(x)
}
safe_pct <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) "N/A" else scales::percent(x, accuracy = 0.1)
}
safe_txt <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) {
    "N/A"
  } else {
    as.character(x)
  }
}

# Verifica si el servicio de Ollama responde (timeout muy corto, por host).
# El módulo se considera ACTIVO si el servicio responde, salvo que se haya
# desactivado explícitamente (ENABLE_OLLAMA=false). Esto evita depender de
# que la variable de entorno se propague a la sesión de Shiny Server (donde
# el entrypoint la exporta pero shiny-server puede no heredarla).
check_ollama_status <- function(timeout = 1) {
  if (identical(Sys.getenv("ENABLE_OLLAMA"), "false")) return(FALSE)
  for (base in ollama_hosts()) {
    ok <- tryCatch({
      resp <- httr2::request(paste0(base, "/api/tags")) |>
        httr2::req_timeout(timeout) |>
        httr2::req_perform()
      httr2::resp_status(resp) == 200
    }, error = function(e) {
      message("check_ollama_status: no responde ", base, " -> ", conditionMessage(e))
      FALSE
    })
    if (ok) return(TRUE)
  }
  FALSE
}

# Prompt estructurado con los KPIs del análisis (resumen ejecutivo)
build_ollama_prompt <- function(datos_simulacion) {
  sprintf(
    paste0(
      "Eres un analista de riesgos senior. Redacta un resumen ejecutivo de ",
      "EXACTAMENTE 3 parrafos, en espanol, en lenguaje no tecnico, dirigido a ",
      "una Junta Directiva y un Comite de Riesgos. No uses viñetas ni listas; ",
      "usa prosa fluida. Basate unicamente en estos datos:\n",
      "- Exposicion anual esperada (ALE total, mediana): %s\n",
      "- Perdida maxima probable (VaR 95%%): %s\n",
      "- Threat community dominante: %s\n",
      "- Escenario de mayor riesgo: %s\n",
      "- Reduccion de perdida por controles: %s\n",
      "- Ahorro neto tras costos de controles: %s\n",
      "- Retorno de inversion en seguridad (ROSI): %s\n",
      "- Controles de mayor ahorro (nombre, descripcion ejecutiva y ahorro):\n%s\n\n",
      "Parrafo 1: situacion general y exposicion. Parrafo 2: efectividad de ",
      "los controles y retorno de inversion. Parrafo 3: recomendaciones accionables."
    ),
    safe_money(datos_simulacion$total_ale),
    safe_money(datos_simulacion$var95),
    safe_txt(datos_simulacion$top_tcomm),
    safe_txt(datos_simulacion$top_scenario),
    safe_pct(datos_simulacion$reduction_pct),
    safe_money(datos_simulacion$net_savings),
    if (is.null(datos_simulacion$rosi) || is.na(datos_simulacion$rosi)) "N/A" else sprintf("%.1f%%", datos_simulacion$rosi),
    safe_txt(datos_simulacion$top_controls)
  )
}

# Genera el resumen ejecutivo con Ollama (/api/generate). Nunca lanza errores.
generar_resumen_ia <- function(datos_simulacion, model = "llama3.2:1b", timeout = 120) {
  if (!check_ollama_status()) {
    return("El módulo de IA local (Ollama) no está activo. Esta instancia opera en modo estándar (sin IA). Si necesitas el resumen generado por IA, inicia el contenedor con `docker run -it` y responde 's' a la pregunta de activación.")
  }

  prompt <- build_ollama_prompt(datos_simulacion)
  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    options = list(temperature = 0.7, num_predict = 600)
  )

  resp <- NULL
  for (base in ollama_hosts()) {
    resp <- tryCatch(
      httr2::request(paste0(base, "/api/generate")) |>
        httr2::req_timeout(timeout) |>
        httr2::req_body_json(body) |>
        httr2::req_perform(),
      error = function(e) {
        message("Error en Ollama (", base, "): ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(resp)) break
  }

  if (is.null(resp)) {
    return("No se pudo conectar con el servicio local de Ollama. La aplicación continúa en modo estándar.")
  }
  if (httr2::resp_status(resp) != 200) {
    err_txt <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    message("Ollama HTTP ", httr2::resp_status(resp), ": ", substr(err_txt, 1, 300))
    return(sprintf("El servicio de Ollama respondió con error (HTTP %s). La aplicación continúa en modo estándar.",
                   httr2::resp_status(resp)))
  }

  out <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(out) || is.null(out$response) || !nzchar(trimws(out$response))) {
    return("El modelo de IA local no devolvió texto. La aplicación continúa en modo estándar.")
  }

  trimws(out$response)
}

# -----------------------------------------------------------------------------
# CHAT / COPILOT CONTEXTUAL con Ollama (API local /api/chat).
# Permite preguntas de seguimiento: se envía el historial completo al modelo.
# Nunca lanza excepciones: si el módulo está inactivo o falla, retorna un
# mensaje amigable y la app continúa en modo estándar.
# -----------------------------------------------------------------------------

# Líneas compactas con los KPIs del dashboard (sin NULL/NA). Se usan tanto en
# el system_prompt como en el mensaje del usuario: los modelos pequeños (1B)
# siguen mucho mejor instrucciones que aparecen justo antes de la pregunta.
dashboard_kpis_text <- function(d) {
  sprintf(
    paste0("- ALE Total (mediana): %s\n",
           "- Perdida maxima probable (VaR 95%% / P90): %s\n",
           "- Frecuencia de eventos de perdida (LEF media): %s eventos/año\n",
           "- Threat community principal: %s\n",
           "- Escenario mas critico: %s\n",
           "- Reduccion de perdida por controles: %s\n",
           "- Ahorro neto tras costos de controles: %s\n",
           "- ROSI: %s\n",
           "- Controles de mayor ahorro (nombre y descripcion ejecutiva):\n%s"),
    safe_money(d$total_ale),
    safe_money(d$var95),
    safe_money(d$lef),
    safe_txt(d$top_tcomm),
    safe_txt(d$top_scenario),
    safe_pct(d$reduction_pct),
    safe_money(d$net_savings),
    if (is.null(d$rosi) || is.na(d$rosi)) "N/A" else sprintf("%.1f%%", d$rosi),
    safe_txt(d$top_controls)
  )
}

# system_prompt dinámico con los KPIs actuales del Dashboard
build_chat_system_prompt <- function(datos_dashboard) {
  sprintf(
    paste0(
      "Eres el asistente de analisis de riesgos de una plataforma ",
      "OpenFAIR/Monte Carlo. Responde en espanol, breve y util, con tono ",
      "ejecutivo. Usa ÚNICAMENTE los datos siguientes; NO inventes cifras. ",
      "Si la pregunta no se puede responder con los datos, dilo y sugiere ",
      "un siguiente paso.\n",
      "DATOS ACTUALES DEL DASHBOARD:\n%s\n\n",
      "Si te preguntan por controles, recomienda basandote en estos datos."
    ),
    dashboard_kpis_text(datos_dashboard)
  )
}

# Estructura ESTRICTA de mensajes para /api/chat: cada elemento tiene
# role ("system"/"user"/"assistant") y content como string no nulo.
build_ollama_messages <- function(pregunta_usuario, datos_dashboard,
                                  historial_chat = list()) {
  messages <- list(list(role = "system",
                        content = build_chat_system_prompt(datos_dashboard)))
  if (length(historial_chat) > 0) {
    for (m in historial_chat) {
      role <- if (is.null(m$role)) "user" else as.character(m$role)
      content <- if (is.null(m$content)) "" else as.character(m$content)
      messages[[length(messages) + 1]] <- list(role = role, content = content)
    }
  }
  # Para modelos pequeños (1B): repetir los KPIs en el mensaje del usuario,
  # justo antes de la pregunta, para que realmente los use.
  user_content <- paste0(
    "DATOS DEL DASHBOARD:\n",
    dashboard_kpis_text(datos_dashboard),
    "\n\nPregunta del usuario: ",
    as.character(pregunta_usuario)
  )
  messages[[length(messages) + 1]] <- list(role = "user", content = user_content)
  messages
}

# Envía la pregunta a Ollama (/api/chat) incluyendo el historial previo.
#   pregunta_usuario : texto del usuario
#   datos_dashboard  : lista con KPIs (total_ale, var95, lef, top_tcomm, ...)
#   historial_chat   : lista de mensajes previos list(role = c("user","assistant"), content = "...")
preguntar_a_ollama <- function(pregunta_usuario, datos_dashboard,
                               historial_chat = list(),
                               model = "llama3.2:1b", timeout = 120) {
  msg_inactivo <- paste0(
    "El módulo de IA local (Ollama) no está activo en esta instancia. ",
    "La aplicación opera en modo estándar. Para activarlo, inicia el ",
    "contenedor con `docker run -it` y responde 's', o define ",
    "ENABLE_OLLAMA=true en el compose."
  )

  if (!check_ollama_status()) return(msg_inactivo)

  messages <- build_ollama_messages(pregunta_usuario, datos_dashboard, historial_chat)
  body <- list(
    model = model,
    messages = messages,
    stream = FALSE,
    options = list(temperature = 0.7, num_predict = 600)
  )

  resp <- NULL
  for (base in ollama_hosts()) {
    resp <- tryCatch(
      httr2::request(paste0(base, "/api/chat")) |>
        httr2::req_timeout(timeout) |>
        httr2::req_body_json(body) |>
        httr2::req_perform(),
      error = function(e) {
        message("Error en Ollama (", base, "): ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(resp)) break
  }

  if (is.null(resp)) {
    return("No se pudo conectar con el servicio local de Ollama. La aplicación continúa en modo estándar.")
  }
  if (httr2::resp_status(resp) != 200) {
    err_txt <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    message("Ollama HTTP ", httr2::resp_status(resp), ": ", substr(err_txt, 1, 300))
    return(sprintf("Ollama respondió con error (HTTP %s): %s. La aplicación continúa en modo estándar.",
                   httr2::resp_status(resp), substr(err_txt, 1, 150)))
  }

  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) {
    message("Error al parsear respuesta de Ollama: ", conditionMessage(e))
    NULL
  })
  if (is.null(parsed) || is.null(parsed$message$content) ||
      !nzchar(trimws(parsed$message$content))) {
    return("El modelo de IA local no devolvió texto. Inténtalo de nuevo.")
  }

  trimws(parsed$message$content)
}

# =============================================================================
# MULTIPROYECTO + PERSISTENCIA DE RESULTADOS
# =============================================================================
# Un "proyecto" es un evaluator_workspace propio (inputs/ + results/). La app
# trabaja SIEMPRE sobre el proyecto actual; por defecto es el workspace
# clásico app/evaluator_workspace. Los proyectos nuevos se crean como
# subcarpetas de app/proyectos/ sembradas desde app/proyectos_plantilla/
# (plantilla en español, 0 escenarios, 14 dominios base). En Docker, la
# carpeta app/proyectos debe montarse como volumen para persistir.
# -----------------------------------------------------------------------------

proyectos_root <- function() {
  d <- file.path(find_app_dir(), "proyectos")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

# Plantilla "recién creada" (en español) con la que se siembran proyectos
# nuevos. Vive dentro del código de la app (no en un volumen).
proyectos_plantilla <- function() {
  file.path(find_app_dir(), "proyectos_plantilla")
}

proyecto_actual_path <- function() {
  getOption("tfm.project_dir", NULL)
}

set_proyecto_actual <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  options(tfm.project_dir = path)
  invisible(path)
}

limpiar_proyecto_actual <- function() {
  options(tfm.project_dir = NULL)
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Persistencia del proyecto activo ENTRE reinicios (p. ej. docker down/up):
# se guarda la ruta del proyecto elegido en un archivo dentro de proyectos_root()
# (que en Docker es un volumen), y al arrancar se restaura. Sin esto, al
# reiniciar la app volvía al workspace por defecto (que puede estar vacío) y la
# sesión se caía ("Disconnected from the server").
# -----------------------------------------------------------------------------
proyecto_actual_file <- function() {
  file.path(proyectos_root(), ".proyecto_actual")
}

guardar_proyecto_actual <- function() {
  p <- proyecto_actual_path()
  tryCatch(
    writeLines(if (is.null(p)) "" else p, proyecto_actual_file()),
    error = function(e) message("No se pudo guardar el proyecto activo: ", conditionMessage(e))
  )
}

# Restaura el proyecto guardado (si existe y tiene inputs/). Devuelve la ruta o NULL.
restaurar_proyecto_actual <- function() {
  f <- proyecto_actual_file()
  if (!file.exists(f)) return(NULL)
  p <- tryCatch(trimws(readLines(f, warn = FALSE)[1]), error = function(e) "")
  if (is.na(p) || !nzchar(p)) return(NULL)
  if (!dir.exists(file.path(p, "inputs"))) {
    message("[App] Proyecto guardado no disponible: ", p)
    return(NULL)
  }
  set_proyecto_actual(p)
  p
}

# Nombre visible del proyecto actual (para la UI).
proyecto_actual_nombre <- function() {
  ws <- evaluator_workspace()
  dirname_ws <- basename(ws$base_dir)
  override <- proyecto_actual_path()
  if (is.null(override)) return(dirname_ws)  # workspace clásico
  slug <- basename(override)
  slug
}

# Lista los proyectos disponibles: el clásico (por defecto) + las subcarpetas
# de proyectos_root() que contienen inputs/survey.xlsx.
list_proyectos <- function() {
  cl <- data.frame(
    slug = "evaluator_workspace (por defecto)",
    ruta = file.path(find_app_dir(), "evaluator_workspace"),
    es_actual = is.null(proyecto_actual_path()),
    stringsAsFactors = FALSE
  )
  sub <- list.dirs(proyectos_root(), recursive = FALSE, full.names = TRUE)
  if (length(sub) > 0) {
    tiene_survey <- vapply(sub, function(d) file.exists(file.path(d, "inputs", "survey.xlsx")),
                           logical(1))
    sub <- sub[tiene_survey]
    if (length(sub) > 0) {
      extra <- data.frame(
        slug = basename(sub),
        ruta = sub,
        es_actual = vapply(sub, function(d) identical(normalizePath(d), normalizePath(proyecto_actual_path())), logical(1)),
        stringsAsFactors = FALSE
      )
      cl <- rbind(cl, extra)
    }
  }
  # Fecha de última modificación del survey de cada proyecto
  cl$mtime <- vapply(cl$ruta, function(d) {
    f <- file.path(d, "inputs", "survey.xlsx")
    if (file.exists(f)) format(file.mtime(f), "%d/%m %H:%M") else "—"
  }, character(1))
  cl
}

# Crea un proyecto nuevo a partir de la plantilla limpia en español.
# Devuelve la ruta del proyecto creado.
nuevo_proyecto <- function(nombre) {
  if (is.null(nombre) || !nzchar(trimws(nombre))) {
    stop("Escribe un nombre para el proyecto.", call. = FALSE)
  }
  slug <- tolower(trimws(nombre))
  slug <- gsub("[áàäâ]", "a", slug)
  slug <- gsub("[éèëê]", "e", slug)
  slug <- gsub("[íìïî]", "i", slug)
  slug <- gsub("[óòöô]", "o", slug)
  slug <- gsub("[úùüû]", "u", slug)
  slug <- gsub("ñ", "n", slug)
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("(^-+|-+$)", "", slug)
  if (!nzchar(slug)) slug <- sprintf("proyecto-%d", as.integer(Sys.time()))
  destino <- file.path(proyectos_root(), slug)
  if (dir.exists(destino)) {
    stop(sprintf("Ya existe un proyecto '%s'. Elige otro nombre.", slug), call. = FALSE)
  }
  plantilla <- proyectos_plantilla()
  if (!dir.exists(file.path(plantilla, "inputs"))) {
    stop("No se encontró la plantilla de proyecto (app/proyectos_plantilla).",
         call. = FALSE)
  }
  dir.create(destino, recursive = TRUE)
  contenido <- list.files(plantilla, full.names = TRUE, all.files = TRUE,
                          no.. = TRUE)
  ok <- all(file.copy(contenido, destino, recursive = TRUE))
  if (!ok) stop("No se pudo crear el proyecto (revisa permisos/OneDrive).", call. = FALSE)
  destino
}

# -----------------------------------------------------------------------------
# Persistencia de resultados: guarda la salida completa de run_evaluator_analysis
# con una "firma" de los inputs. Al abrir la app (o cambiar de proyecto) se
# cargan los resultados guardados si la firma coincide con el survey actual,
# evitando re-simular Monte Carlo.
# -----------------------------------------------------------------------------
cache_analisis_path <- function() {
  file.path(evaluator_workspace()$results_dir, "cache_analisis.rds")
}

# Firma canónica de los inputs: escenarios del survey + catálogo de controles
# (incluye costos/efectividad custom) + dominios. Cualquier cambio invalida la
# caché.
analisis_firma <- function() {
  ws <- evaluator_workspace()
  piezas <- list()
  survey_file <- file.path(ws$inputs_dir, "survey.xlsx")
  if (file.exists(survey_file)) {
    wb <- tryCatch(load_survey_workbook(survey_file), error = function(e) NULL)
    if (!is.null(wb)) {
      for (sh in setdiff(openxlsx::getSheetNames(survey_file),
                         c("Introduction", "Definitions", "Reference"))) {
        dat <- tryCatch(openxlsx::readWorkbook(wb, sheet = sh, colNames = FALSE),
                        error = function(e) NULL)
        if (!is.null(dat)) piezas[[sh]] <- dat
      }
    }
  }
  for (f in c("domains.csv", "capabilities.csv", "qualitative_mappings.csv")) {
    p <- file.path(ws$inputs_dir, f)
    if (file.exists(p)) piezas[[f]] <- readLines(p, warn = FALSE)
  }
  custom <- read_custom_capabilities()
  if (nrow(custom) > 0) piezas[["custom_capabilities"]] <- custom
  tmp <- tempfile(fileext = ".rds")
  saveRDS(piezas, tmp)
  on.exit(unlink(tmp), add = TRUE)
  unname(tools::md5sum(tmp))
}

# Guarda la caché de un análisis completo (solo si hay escenarios).
# mitigacion: opcional, salida de run_mitigation_analysis() (también costosa).
guardar_cache_analisis <- function(res, iterations, mitigacion = NULL) {
  if (is.null(res) || is.null(res$simulation_results)) return(invisible(FALSE))
  if (nrow(res$simulation_results) == 0) return(invisible(FALSE))
  firma <- tryCatch(analisis_firma(), error = function(e) NULL)
  if (is.null(firma)) return(invisible(FALSE))
  tryCatch({
    saveRDS(list(res = res, firma = firma, iterations = iterations,
                 mitigacion = mitigacion, guardado = Sys.time()),
            cache_analisis_path())
    invisible(TRUE)
  }, error = function(e) {
    message("No se pudo guardar la caché de resultados: ", conditionMessage(e))
    invisible(FALSE)
  })
}

# Carga la caché si la firma de los inputs actuales coincide.
# Devuelve el mismo objeto que run_evaluator_analysis() (con un atributo
# guardado) o NULL si no hay caché válida.
cargar_cache_analisis <- function() {
  f <- cache_analisis_path()
  if (!file.exists(f)) return(NULL)
  cache <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(cache) || is.null(cache$res)) return(NULL)
  firma <- tryCatch(analisis_firma(), error = function(e) NULL)
  if (is.null(firma) || !identical(firma, cache$firma)) return(NULL)
  res <- cache$res
  attr(res, "guardado") <- cache$guardado
  attr(res, "iteraciones") <- cache$iterations
  attr(res, "mitigacion") <- if (!is.null(cache$mitigacion)) cache$mitigacion else NULL
  res
}

