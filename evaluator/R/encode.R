# parse_string <- function(lm_str) {
#   # Si el valor no tiene formato de parámetros (ej: es "medium" o "low"), retorna NULL o la cadena
#   if (is.na(lm_str) || !grepl("params:", lm_str)) {
#     return(NULL)
#   }
  
#   # Extraer la parte después de "params:"
#   params_part <- sub(".*params:\\s*", "", lm_str)
  
#   # Extraer la distribución si la hay (ej: "lnorm")
#   dist_type <- sub(".*dist:\\s*([^;]+);.*", "\\1", lm_str)
  
#   # Separar los pares clave=valor por coma
#   pairs <- unlist(strsplit(params_part, ",\\s*"))
  
#   # Crear lista nombrada con los valores numéricos
#   params_list <- list()
#   for (pair in pairs) {
#     kv <- unlist(strsplit(pair, "="))
#     if (length(kv) == 2) {
#       params_list[[trimws(kv[1])]] <- as.numeric(trimws(kv[2]))
#     }
#   }
  
#   # Opcional: incluir la función/distribución dentro de la lista
#   if (dist_type == "lnorm") {
#     params_list$func <- "stats::lnorm"
#   }
  
#   return(params_list)
# }

parse_string <- function(lm_str) {
  # Return NULL if NA or missing "params:" pattern
  if (is.na(lm_str) || !grepl("params:", lm_str)) {
    return(NULL)
  }
  
  # Extract distribution type (matches anything after 'dist:' up to '|' or ';')
  dist_type <- ifelse(
    grepl("dist:", lm_str),
    sub(".*dist:\\s*([^|;]+).*", "\\1", lm_str),
    NA
  )
  
  # Extract everything after "params:"
  params_part <- sub(".*params:\\s*", "", lm_str)
  
  # Split key=value pairs by comma
  pairs <- unlist(strsplit(params_part, ",\\s*"))
  
  # Build named numeric list
  params_list <- list()
  for (pair in pairs) {
    kv <- unlist(strsplit(pair, "="))
    if (length(kv) == 2) {
      params_list[[trimws(kv[1])]] <- as.numeric(trimws(kv[2]))
    }
  }
  
  # Map distribution type to its random-generation function. The simulator
  # resolves func by splitting on "::" and calling get(fn, asNamespace(pkg)),
  # so the function name must include the "r" prefix.
  dist_func_map <- c(
    pois     = "stats::rpois",
    nbinom   = "stats::rnbinom",
    geom     = "stats::rgeom",
    exp      = "stats::rexp",
    gamma    = "stats::rgamma",
    lnorm    = "stats::rlnorm",
    weibull  = "stats::rweibull",
    norm     = "stats::rnorm",
    beta     = "stats::rbeta",
    unif     = "stats::runif",
    pert     = "mc2d::rpert",
    zipois   = "evaluator::rzipois",
    zinegbin = "evaluator::rzinegbin"
  )

  if (!is.na(dist_type) && dist_type %in% names(dist_func_map)) {
    params_list$func <- dist_func_map[[dist_type]]
  }

  return(params_list)
}

#' Sample from a zero-inflated Poisson distribution
#'
#' @param n Number of samples.
#' @param pi Zero-inflation probability.
#' @param lambda Poisson rate for the count component.
#' @return Numeric vector of samples.
#' @keywords internal
rzipois <- function(n, pi, lambda) {
  zero <- stats::runif(n) < pi
  out <- stats::rpois(n, lambda)
  out[zero] <- 0
  out
}

#' Sample from a zero-inflated negative binomial distribution
#'
#' @param n Number of samples.
#' @param pi Zero-inflation probability.
#' @param lambda Mean of the count component (mu).
#' @param theta Dispersion (size) parameter.
#' @return Numeric vector of samples.
#' @keywords internal
rzinegbin <- function(n, pi, lambda, theta) {
  zero <- stats::runif(n) < pi
  out <- stats::rnbinom(n, mu = lambda, size = theta)
  out[zero] <- 0
  out
}


#' Encode qualitative data to quantitative parameters
#'
#' Given an input of:
#'   * qualitative risk scenarios
#'   * qualitative capabilities
#'   * translation table from qualitative labels to quantitative parameters
#'
#'   Create a unified dataframe of quantitative scenarios ready for simulation.
#'
#' @importFrom dplyr rename select left_join filter rowwise mutate do
#' @importFrom rlang .data
#' @importFrom purrr map pmap
#' @param scenarios Qualitative risk scenarios dataframe.
#' @param capabilities Qualitative program capabilities dataframe.
#' @param mappings Qualitative to quantitative mapping dataframe.
#'
#' @export
#' @return A dataframe of capabilities for the scenario and parameters for quantified simulation.
#' @examples
#' data(mc_qualitative_scenarios, mc_capabilities, mc_mappings)
#' encode_scenarios(mc_qualitative_scenarios, mc_capabilities, mc_mappings)

encode_scenarios <- function(scenarios, capabilities, mappings) {
  # fetch DIFF params
  scenarios$diff_params <- purrr::map(scenarios$controls,
                             ~derive_controls(capability_ids = .x,
                                              capabilities = capabilities,
                                              mappings = mappings))
  scenarios$control_descriptions <- purrr::map(scenarios$controls,
                                               ~derive_control_key(capability_ids = .x,
                                                                   capabilities = capabilities))
  scenarios <- dplyr::select(scenarios, -c(.data$controls))

  # fetch TEF params
  tef_nested <- dplyr::filter(mappings, .data$type == "tef") %>%
    dplyr::rowwise() %>%
    dplyr::do(tef_params = list(min = .data$l, mode = .data$ml, max = .data$h,
                                shape = .data$conf, func = "mc2d::rpert"),
              label = .data$label) %>%
    dplyr::mutate(label = as.character(.data$label))
  
  scenarios <- dplyr::left_join(scenarios, tef_nested,
                                by = c("tef" = "label")) 

  
  scenarios <- scenarios %>% 
    dplyr::mutate(
      tef_params = purrr::map2(tef_params, tef, function(param, lm_text) {
        if (is.null(param)) {
          return(parse_string(lm_text))
        } else {
          return(param)
        }
      })
    )
  
  # fetch TC params
  tc_nested <- dplyr::filter(mappings, .data$type == "tc") %>%
    dplyr::rowwise() %>%
    dplyr::do(tc_params = list(min = .data$l, mode = .data$ml, max = .data$h,
                               shape = .data$conf, func = "mc2d::rpert"),
              label = .data$label) %>%
    dplyr::mutate(label = as.character(.data$label))
  scenarios <- dplyr::left_join(scenarios, tc_nested,
                                by = c("tc" = "label")) 
  scenarios <- scenarios %>% 
    dplyr::mutate(
      tc_params = purrr::map2(tc_params, tc, function(param, lm_text) {
        if (is.null(param)) {
          return(parse_string(lm_text))
        } else {
          return(param)
        }
      })
    )
  
  # fetch LM params
  lm_nested <- dplyr::filter(mappings, .data$type == "lm") %>%
    dplyr::rowwise() %>%
    dplyr::do(lm_params = list(min = .data$l, mode = .data$ml, max = .data$h,
                               shape = .data$conf, func = "mc2d::rpert"),
              label = .data$label) %>%
    dplyr::mutate(label = as.character(.data$label))
  scenarios <- dplyr::left_join(scenarios, lm_nested,
                                by = c("lm" = "label")) 
  
  scenarios <- scenarios %>% 
    dplyr::mutate(
      lm_params = purrr::map2(lm_params, lm, function(param, lm_text) {
        if (is.null(param)) {
          return(parse_string(lm_text))
        } else {
          return(param)
        }
      })
    )

  scenarios <- dplyr::rename(scenarios, scenario_description = .data$scenario)

  scenarios <- dplyr::mutate(scenarios, scenario = purrr::pmap(
    list(tef_params = .data$tef_params, tc_params = .data$tc_params,
         diff_params = .data$diff_params, lm_params = .data$lm_params), tidyrisk_scenario)) %>%
    dplyr::select(-c(.data$diff_params, .data$tef_params, .data$tc_params, .data$lm_params, .data$tef, .data$tc, .data$lm, .data$tef))
  scenarios
}

#' Derive control ID to control description mappings
#'
#' Given a comma-separated list of control IDs, return a named list
#'   of descriptions for each control with the names set to the control
#'   IDs.
#'
#' @importFrom dplyr filter select
#' @importFrom tibble deframe
#' @importFrom stringi stri_split_fixed
#' @importFrom vctrs vec_cast
#' @param capability_ids Comma-delimited list of capabilities in scope for a scenario.
#' @param capabilities Dataframe of master list of all qualitative capabilities.
#'
#' @return A named list of control IDs and descriptions.
#' @export
#' @examples
#' data(mc_capabilities)
#' capability_ids <- c("CAP-01", "CAP-03")
#' derive_control_key(capability_ids, mc_capabilities)
derive_control_key <- function(capability_ids, capabilities) {
  control_list <- stringi::stri_split_fixed(capability_ids, ", ") %>% unlist()

  control_frame <- dplyr::filter(capabilities, .data$capability_id %in% control_list) %>%
    dplyr::select(.data$capability_id, .data$capability)

  #rlang::as_list(control_frame$capability) %>%
  # vctrs::vec_cast(control_frame$capability, to = list()) %>%
  #   rlang::set_names(control_frame$capability_id)
  tibble::deframe(control_frame) %>% as.list()

}


#'
#' Derive control difficulty parameters for a given qualitative scenario
#'
#' Given a comma-separated list of control IDs in a scenario, identify
#'   the qualitative rankings associated with each scenario, convert to
#'   their quantitative parameters, and return a dataframe of the set of
#'   parameters.
#'
#' @importFrom dplyr left_join mutate select rename pull
#' @importFrom rlang .data set_names
#' @importFrom stringi stri_split_fixed
#' @param capability_ids Comma-delimited list of capabilities in scope for a scenario.
#' @param capabilities Dataframe of master list of all qualitative capabilities.
#' @param mappings Qualitative mappings dataframe.
#'
#' @return A named list of quantitative estimate parameters for the capabilities
#'   applicable to a given scenario.
#' @export
#' @examples
#' data(mc_capabilities)
#' capability_ids <- c("1, 3")
#' mappings <- data.frame(type = "diff", label = "1 - Immature", l = 0, ml = 2, h = 10,
#'                        conf = 3, stringsAsFactors = FALSE)
#' derive_controls(capability_ids, mc_capabilities, mappings)
derive_controls <- function(capability_ids, capabilities, mappings) {
  control_list <- stringi::stri_split_fixed(capability_ids, ", ") %>% unlist()

  control_list <- capabilities[capabilities$capability_id %in% control_list, ] %>%
    dplyr::rename(control_id = .data$capability_id)

  # Find the qualitative rating for each control ID, then lookup its
  # distribution parameters from the mappings table
  #results <- capabilities[capabilities$id %in%
  #                          as.numeric(control_list), "diff"] %>%
  results <- control_list %>%
    dplyr::mutate(label = as.character(.data$diff)) %>%
    dplyr::select(-diff) %>%
    dplyr::left_join(mappings[mappings$type == "diff", ],
                     by = c(label = "label")) %>%
    dplyr::rowwise() %>%
    dplyr::do(diff_params = list(min = .data$l, mode = .data$ml, max = .data$h,
                                 shape = .data$conf, func = "mc2d::rpert")) %>%
    dplyr::pull() %>%
    rlang::set_names(nm = control_list$control_id)

  return(results)
}
