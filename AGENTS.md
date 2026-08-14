# TFM Evaluator App - Project Memory

## App structure
- `app/app.R` - Shiny dashboard (bslib darkly theme + dark mode toggle). Tabs: Dashboard (KPIs + threat community), Configuración, Resultados por Escenario, Resultados por Dominio, Curva de Excedencia (LEC), Efectividad de Controles (mitigation).
- `app/evaluator_helpers.R` - workspace mgmt, survey write/import, `run_evaluator_analysis()` (returns results_dir, simulation_results, scenario_summary [enriched with tcomm/scenario_description], domain_summary, qualitative_scenarios, capabilities, mappings), `run_mitigation_analysis()` (ALE inherente vs residual + leave-one-out por capability), `get_evaluator_capabilities()`.
- `app/fit_dist.R` - `fit_dist()` fits count (zipois, pois, nbinom, zinegbin, geom) and severity (exp, gamma, lnorm, weibull) distributions; selects by BIC.
- `app/capabilities_module.R` - Shiny module (`capabilities_ui`/`capabilities_server`) to configure controls with Beta-PERT effectiveness (min/mode/max %). Client-side selectize (`create = TRUE`) lets users create NEW control IDs (persisted with description to `inputs/custom_capabilities.csv`). Returns `configured` (reactive df incl. `capability_desc`), `capability_ids_csv()` (V7 survey string), `capability_pert_params()` (evaluator-format list, numeric cols only). Integration: `run_evaluator_analysis(custom_diff_params = ...)` merges over `scenario$parameters$diff` via `utils::modifyList`. `get_evaluator_capabilities()` reads `inputs/capabilities.csv` (import output) + custom sidecar; choices vector has names=display, values=ID.
- `evaluator/` - local fork of the evaluator package. `parse_string()` maps dist names to r-prefixed RNG funcs (e.g. lnorm -> stats::rlnorm, pert -> mc2d::rpert). `select_loss_opportunities()` handles empty diff (inherent risk). Reinstall after changes: `devtools::install("evaluator", quick = TRUE, upgrade = FALSE)`.

## Survey format
- `app/evaluator_workspace/inputs/survey.xlsx` - evaluator template; scenarios written with `write_survey_scenario()` (V1-V7, append mode). Modes: Qualitative (label), Distribution/Fit (`dist:<name>|params:k=v,...`), PERT (`dist:pert|params:min=X,mode=Y,max=Z`).
- Analysis strips survey to 7 cols before `evaluator::import_spreadsheet()`; metadata columns V8-V11 not persisted to Excel.

## Key gotchas
- Simulator resolves `func` via `get(fn, asNamespace(pkg))` - must use r-prefixed RNG names.
- `openfair_tef_tc_diff_lm` sets `set.seed(31337)` internally (deterministic runs).
- Run app from `app/` directory (sources fit_dist.R/evaluator_helpers.R with relative paths).
- ISMP sheet contains an Excel table object - avoid rewriting header row with openxlsx (use cell clears instead).
- DT used for tables (reactable not installed); bsicons installed for value_box icons.
