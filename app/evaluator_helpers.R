evaluator_workspace <- function() {
  app_dir <- if (file.exists("app.R")) {
    dirname(normalizePath("app.R"))
  } else if (file.exists(file.path("app", "app.R"))) {
    dirname(normalizePath(file.path("app", "app.R")))
  } else {
    normalizePath(".")
  }

  base_dir <- file.path(app_dir, "evaluator_workspace")
  inputs_dir <- file.path(base_dir, "inputs")
  results_dir <- file.path(base_dir, "results")

  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
  if (!dir.exists(inputs_dir)) dir.create(inputs_dir, recursive = TRUE)
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

  if (!file.exists(file.path(inputs_dir, "survey.xlsx")) ||
      !file.exists(file.path(inputs_dir, "domains.csv"))) {
    evaluator::create_templates(base_dir)
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
    stop(sprintf("Faltan columnas obligatorias en el archivo: %s", paste(missing, collapse = ", ")), call. = FALSE)
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
      base_dir = base_dir
    )
  }

  invisible(nrow(df))
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
                                  base_dir = evaluator_workspace()$base_dir) {
  # Allow lef to be appended to description or kept separate; we'll append for compatibility
  if (!is.null(lef) && !is.na(lef) && nzchar(as.character(lef))) {
    scenario_description <- paste0(scenario_description, " [LEF=", lef, "]")
  }
  ws <- evaluator_workspace()
  survey_file <- file.path(ws$inputs_dir, "survey.xlsx")
  if (!file.exists(survey_file)) {
    evaluator::create_templates(ws$base_dir)
  }

  wb <- openxlsx::loadWorkbook(survey_file)
  if (!domain_id %in% names(wb)) {
    stop(sprintf("No se encuentra la hoja de dominio '%s' en survey.xlsx.", domain_id), call. = FALSE)
  }

  dat <- openxlsx::readWorkbook(survey_file, sheet = domain_id, colNames = FALSE)
  threats_row <- which(dat[[1]] == "Threats")[1]
  if (is.na(threats_row)) {
    stop("No se pudo encontrar la fila 'Threats' en la hoja del dominio.", call. = FALSE)
  }
  header_row <- threats_row + 1
  data_rows <- seq(header_row + 1, nrow(dat))
  if (length(data_rows) == 0) {
    insert_row <- header_row + 1
  } else {
    filled_rows <- data_rows[!is.na(dat[data_rows, 1]) & dat[data_rows, 1] != ""]
    last_filled <- if (length(filled_rows) == 0) header_row else max(filled_rows)
    insert_row <- last_filled + 1
  }

  # If a scenario with the same ScenarioID exists, update that row instead of appending
  existing_row <- NA_integer_
  if (!is.null(scenario_id) && nzchar(as.character(scenario_id))) {
    # search column 6 (ScenarioID) in data_rows
    if (length(data_rows) > 0) {
      vals <- as.character(dat[data_rows, 6])
      matches <- which(!is.na(vals) & vals == as.character(scenario_id))
      if (length(matches) > 0) {
        existing_row <- data_rows[matches[1]]
      }
    }
  }

  # Extend row to include extra columns for TEF/LM distributions and params if the template supports them
  # We'll write V1:V7 as before; V8 TEF_dist, V9 TEF_params, V10 LM_dist, V11 LM_params
  ext_row_data <- data.frame(
    V1 = scenario_description,
    V2 = tcomm,
    V3 = tef,
    V4 = tc,
    V5 = lm,
    V6 = scenario_id,
    V7 = capabilities,
    V8 = if (!is.null(attr(tef, 'dist'))) attr(tef, 'dist') else NA_character_,
    V9 = if (!is.null(attr(tef, 'params'))) attr(tef, 'params') else NA_character_,
    V10 = if (!is.null(attr(lm, 'dist'))) attr(lm, 'dist') else NA_character_,
    V11 = if (!is.null(attr(lm, 'params'))) attr(lm, 'params') else NA_character_,
    stringsAsFactors = FALSE
  )

  write_data <- ext_row_data

  if (!is.na(existing_row)) {
    openxlsx::writeData(wb, sheet = domain_id, x = write_data, startRow = existing_row, colNames = FALSE)
  } else {
    openxlsx::writeData(wb, sheet = domain_id, x = write_data, startRow = insert_row, colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, survey_file, overwrite = TRUE)

  survey_file
}

run_evaluator_analysis <- function(iterations = 10000, base_dir = evaluator_workspace()$base_dir) {
  ws <- evaluator_workspace()
  inputs_dir <- ws$inputs_dir
  results_dir <- ws$results_dir

  domains <- readr::read_csv(file.path(inputs_dir, "domains.csv"), col_types = readr::cols(.default = readr::col_character()))
  
  # Clean survey.xlsx: remove extra columns (V8-V11) before import
  # evaluator expects only 7 columns; our extended columns break import
  survey_file <- file.path(inputs_dir, "survey.xlsx")
  wb <- openxlsx::loadWorkbook(survey_file)
  
  for (sheet_name in openxlsx::getSheetNames(wb)) {
    if (sheet_name %in% c("Introduction", "Definitions", "Reference")) next
    
    dat <- openxlsx::readWorkbook(wb, sheet = sheet_name, colNames = FALSE)
    if (nrow(dat) == 0) next
    
    # Keep only first 7 columns
    if (ncol(dat) > 7) {
      dat_clean <- dat[, 1:7, drop = FALSE]
      openxlsx::writeData(wb, sheet = sheet_name, x = dat_clean, startRow = 1, colNames = FALSE)
    }
  }
  openxlsx::saveWorkbook(wb, survey_file, overwrite = TRUE)
  
  evaluator::import_spreadsheet(survey_file, domains, inputs_dir)
  qual_inputs <- evaluator::read_qualitative_inputs(inputs_dir)
  evaluator::validate_scenarios(qual_inputs$qualitative_scenarios,
                               qual_inputs$capabilities,
                               domains,
                               qual_inputs$mappings)

  quantitative_scenarios <- evaluator::encode_scenarios(qual_inputs$qualitative_scenarios,
                                                        qual_inputs$capabilities,
                                                        qual_inputs$mappings)

  simulation_results <- quantitative_scenarios %>%
    dplyr::mutate(results = purrr::map(.data$scenario,
                                      evaluator::run_simulation,
                                      iterations = iterations)) %>%
    dplyr::select(scenario_id, domain_id, results)

  saveRDS(simulation_results, file = file.path(results_dir, "simulation_results.rds"))

  evaluator::summarize_to_disk(simulation_results = simulation_results, results_dir)

  list(results_dir = results_dir,
       simulation_results = simulation_results)
}
