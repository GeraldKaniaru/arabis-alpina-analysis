# 1. Local publication support functions -------------------------------------

# Local definitions: this script never sources another R file.
publication_root <- "outputs"
publication_captions <- c(
  Table_3 = "Table 3. Baseline and projected reproductive trait summaries and changes under mid-century scenarios. Baseline values are conditional model predictions for June 2021; future values refer to 2041-2060 and are averaged equally across three climate models. For MISM, exponentiating log-scale predictions without a retransformation correction yields plant-level conditional medians; reported summaries average those medians, rather than estimating arithmetic expected seed mass. Other traits summarise expected values conditional on their predictors. Gaussian projections retain fitted block and plot effects. For seed number, two negligible random-intercept components were removed after the nested model failed numerical checks; the resolved model retains the NB2 family, log link and all environmental predictors. Change is future minus baseline. Flowering onset and duration are in days, MISM in mg per seed, and seed number in seeds per plant. Flowering duration is retained as the projection response; the H3 final-date re-expression does not change these projections. Confidence limits are 95% percentile bootstrap intervals for scenario mean changes, based on 1,000 retained replicates per trait with no refitting errors or non-finite predictions. Retention does not establish that all refits passed convergence-warning, Hessian or singularity checks. Equal model weighting does not quantify the full uncertainty among future climate trajectories. Reported values were rounded independently.",
  Figure_S7 = "Figure S7. Projected changes in Arabis alpina reproductive traits for 2041-2060 relative to June 2021. Panels show (a) flowering onset, (b) duration, (c) MISM and (d) seed number. Brown and yellow identify SSP2-4.5 and SSP5-8.5. Violin widths show densities of scenario mean changes from 1,000 parametric bootstrap replicates. Points show model-estimated changes averaged equally across three climate models. Bars are 95% percentile bootstrap intervals; dashed lines mark zero."
)
publication_table_3 <- function() {
  td <- file.path(publication_root, "projected_trait_changes_mid_century", "tables")
  f <- file.path(td, "Table_RQ6_scenario_mean_changes.csv")
  if (!file.exists(f)) {
    return(invisible(NULL))
  }
  t <- utils::read.csv(f, check.names = FALSE)
  names(t)[names(t) == "Successful_bootstraps"] <- "Retained_bootstraps"
  t$Response_unit <- ifelse(t$Trait %in% c("Flowering_onset", "Flowering_duration"), "days",
    ifelse(t$Trait == "MISM", "mg per seed", "seeds per plant")
  )
  t$Prediction_summary <- ifelse(t$Trait == "MISM", "Mean of plant-level conditional medians",
    "Mean of plant-level conditional expected values"
  )
  utils::write.csv(t, file.path(td, "Table_3.csv"), row.names = FALSE)
  writeLines(publication_captions[["Table_3"]], file.path(td, "Table_3_caption.txt"))
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Table 3")
    openxlsx::writeData(wb, "Table 3", t)
    openxlsx::setColWidths(wb, "Table 3", cols = seq_len(ncol(t)), widths = c(16, 25, 17, 17, 17, 20, 20, 19, 20, 49))
    style <- openxlsx::createStyle(fontName = "Arial", fontSize = 10, wrapText = TRUE, valign = "center")
    openxlsx::addStyle(wb, "Table 3", style, rows = 1:(nrow(t) + 1), cols = seq_len(ncol(t)), gridExpand = TRUE)
    openxlsx::setRowHeights(wb, "Table 3", rows = 1:(nrow(t) + 1), heights = 33)
    openxlsx::freezePane(wb, "Table 3", firstActiveRow = 2, firstActiveCol = 3)
    openxlsx::writeData(wb, "Table 3", "Table 3 notes", startRow = 12, colNames = FALSE)
    openxlsx::writeData(wb, "Table 3", publication_captions[["Table_3"]], startRow = 13, colNames = FALSE)
    openxlsx::mergeCells(wb, "Table 3", cols = 1:10, rows = 13:17)
    openxlsx::addStyle(wb, "Table 3", style, rows = 13:17, cols = 1:10, gridExpand = TRUE)
    openxlsx::saveWorkbook(wb, file.path(td, "Table_3.xlsx"), overwrite = TRUE)
  }
  invisible(t)
}
publication_render_tables <- function() {
  fp <- file.path(publication_root, "flowering_phenology", "tables")
  msa <- file.path(publication_root, "maternal_seed_allocation", "tables")
  ge <- file.path(publication_root, "germination_elevation", "tables")
  files <- file.path(publication_root, c("projected_trait_changes_mid_century/tables/Table_3.csv"))
  for (f in files[file.exists(files)]) {
    d <- utils::read.csv(f, check.names = FALSE)
    stem <- tools::file_path_sans_ext(basename(f))
    out <- file.path(dirname(f), "gt")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    title <- if (stem == "Table_1") "Table 1" else if (grepl("Table_S1", stem)) "Table S1" else if (grepl("^Table_2", stem)) "Table 2" else if (stem == "Table_3") "Table 3" else gsub("_", " ", stem)
    caption <- if (title %in% c("Table 1", "Table 2", "Table 3", "Table S1")) publication_captions[[gsub(" ", "_", title)]] else "H3 coefficients describe final flowering date; P_zero tests zero and P_duration tests one."
    tab <- gt::gt(d) |>
      gt::tab_header(title = title) |>
      gt::tab_source_note(caption) |>
      gt::fmt_number(columns = names(d)[vapply(d, is.numeric, logical(1))], decimals = 6) |>
      gt::sub_missing(missing_text = "-")
    gt::gtsave(tab, file.path(out, paste0(stem, ".html")), inline_css = TRUE)
    gt::gtsave(tab, file.path(out, paste0(stem, ".rtf")))
  }
}
publication_figure_exports <- function() {
  from <- file.path(publication_root, "projected_trait_changes_mid_century/figures/Figure_RQ6_bootstrap_trait_changes")
  to <- file.path(dirname(from), "Figure_S7_bootstrap_trait_changes")
  records <- list()
  for (ext in c("png", "pdf", "tiff")) {
    input <- paste0(from, ".", ext)
    output <- paste0(to, ".", ext)
    if (file.exists(input)) {
      # A publication filename may already be the source filename.
      input_path <- normalizePath(input, winslash = "/", mustWork = TRUE)
      output_path <- file.path(
        normalizePath(dirname(output), winslash = "/", mustWork = TRUE),
        basename(output)
      )
      if (file.exists(output)) {
        output_path <- normalizePath(output, winslash = "/", mustWork = TRUE)
      }
      if (.Platform$OS.type == "windows") {
        input_path <- tolower(input_path)
        output_path <- tolower(output_path)
      }
      if (!identical(input_path, output_path)) {
        stopifnot(file.copy(input, output, overwrite = TRUE))
      }
      records[[length(records) + 1L]] <- data.frame(Number = "Figure S7", Format = ext, File = output)
    }
  }
  if (!length(records)) {
    return(invisible(NULL))
  }
  writeLines(publication_captions[["Figure_S7"]], paste0(to, "_caption.txt"))
  writeLines(publication_captions[["Figure_S7"]], paste0(from, "_caption.txt"))
  manifest_file <- file.path(publication_root, "Publication_figure_manifest.csv")
  updated <- do.call(rbind, records)
  if (file.exists(manifest_file)) {
    previous <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)
    previous <- previous[previous$Number != "Figure S7", , drop = FALSE]
    updated <- rbind(previous[, names(updated), drop = FALSE], updated)
  }
  utils::write.csv(updated, manifest_file, row.names = FALSE)
  invisible(updated)
}
# END INLINE PUBLICATION SUPPORT

# Flowering calendar: 1 June 2021 = Day 1; date = 2021-06-01 + day - 1.
# Duration = final flowering day minus onset day (elapsed days).
# ============================================================================

# Script 10: CONDITIONAL MID-CENTURY TRAIT PROJECTIONS

# Research question 5. Run from the "arabis analysis" project root.

# ============================================================================

# 2. Marginal and conditional R-squared: calculation and figure notation -----

# Nakagawa marginal and conditional R-squared. No degrees-of-freedom adjustment.
model_r2 <- function(model, null_model = NULL) {
  if (inherits(model, "lm") && !inherits(model, "glm")) {
    r <- summary(model)$r.squared
    return(c(Marginal = r, Conditional = r))
  }
  if (inherits(model, "merMod")) {
    vf <- var(as.numeric(lme4::getME(model, "X") %*% lme4::fixef(model)))
    vr <- sum(vapply(lme4::VarCorr(model), function(v) as.numeric(v[1, 1]), numeric(1)))
    total <- vf + vr + sigma(model)^2
    return(c(Marginal = vf / total, Conditional = (vf + vr) / total))
  }
  if (inherits(model, "glmmTMB")) {
    # Explicit null fits avoid dependence on data/family names in saved calls.
    if (is.null(null_model)) {
      random <- lme4::findbars(formula(model))
      rhs <- paste(c("1", vapply(random, function(x) paste0("(", paste(deparse(x), collapse = ""), ")"), character(1))), collapse = " + ")
      response <- paste(deparse(formula(model)[[2]]), collapse = "")
      null_model <- suppressWarnings(glmmTMB::glmmTMB(
        as.formula(paste(response, "~", rhs)),
        data = model$frame,
        family = family(model)
      ))
    }
    values <- suppressWarnings(MuMIn::r.squaredGLMM(model, null = null_model))
    row <- if ("delta" %in% rownames(values)) "delta" else 1L
    result <- c(Marginal = unname(values[row, "R2m"]), Conditional = unname(values[row, "R2c"]))
    if (any(!is.finite(result))) stop("Non-finite mixed-model R-squared; inspect the fitted model.")
    return(result)
  }
  stop("Unsupported model class for marginal and conditional R-squared.")
}
r2_plotmath <- function(marginal, conditional, digits = 3) {
  if (is.finite(marginal) && is.finite(conditional) &&
    round(marginal, digits) == round(conditional, digits)) {
    return(c(sprintf(paste0("R[m]^2~~'&'~~R[c]^2 == %.", digits, "f"), marginal), NA_character_))
  }
  c(
    sprintf(paste0("R[m]^2 == %.", digits, "f"), marginal),
    sprintf(paste0("R[c]^2 == %.", digits, "f"), conditional)
  )
}
# End of marginal and conditional r-squared: calculation and figure notation.

# 3. Packages and output directories -----------------------------------------

required <- c("readxl", "dplyr", "tidyr", "ggplot2", "lme4", "glmmTMB", "ggh4x", "ggpubr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lme4)
  library(glmmTMB)
})
if (!file.exists("arabis analysis.Rproj")) stop("Run from the 'arabis analysis' project root.")
set.seed(12191)
n_boot <- as.integer(Sys.getenv("PROJECTION_BOOTSTRAPS", "1000"))
if (!is.finite(n_boot) || n_boot < 1) stop("PROJECTION_BOOTSTRAPS must be a positive integer.")
input_dir_mc <- file.path("data", "raw", "Datasets")
climate_file <- file.path(input_dir_mc, "plot_abiotic_factors.xlsx")
trait_file <- file.path(input_dir_mc, "trait_data.xlsx")
output_dir_mc <- file.path("outputs", "projected_trait_changes_mid_century")
table_dir_mc <- file.path(output_dir_mc, "tables")
figure_dir_mc <- file.path(output_dir_mc, "figures")
model_dir_mc <- file.path(output_dir_mc, "models")
diagnostic_dir_mc <- file.path(output_dir_mc, "diagnostics")
# Keep HTML styling caches local to this R session, including restricted hosts.
if (!nzchar(Sys.getenv("R_USER_CACHE_DIR"))) {
  projection_render_cache <- file.path(tempdir(), "arabis_projection_render_cache")
  dir.create(projection_render_cache, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(R_USER_CACHE_DIR = projection_render_cache)
}
invisible(lapply(c(table_dir_mc, figure_dir_mc, model_dir_mc, diagnostic_dir_mc), dir.create,
  recursive = TRUE, showWarnings = FALSE
))
cache_requested <- identical(Sys.getenv("REUSE_RQ6_BOOTSTRAP", "0"), "1")
explicit_cache_dir <- Sys.getenv("PROJECTION_BOOTSTRAP_CACHE_DIR", "")
cache_dir <- if (nzchar(explicit_cache_dir)) explicit_cache_dir else model_dir_mc
cache_model_file <- file.path(cache_dir, "Models_RQ6_full_climate_trait_models.rds")
cache_draw_file <- Sys.getenv("PROJECTION_BOOTSTRAP_CACHE_FILE", file.path(cache_dir, "Bootstrap_RQ6_change_draws.rds"))
cached_models <- if (cache_requested && file.exists(cache_model_file)) readRDS(cache_model_file) else NULL
cached_draws <- if (cache_requested && file.exists(cache_draw_file)) readRDS(cache_draw_file) else NULL

# Standardise reported precision without reducing precision used in analyses.
write.csv <- function(x, file = "", ..., row.names = TRUE) {
  x <- as.data.frame(x)
  x[] <- lapply(x, function(v) if (is.numeric(v)) round(v, 3) else v)
  utils::write.csv(x, file = file, ..., row.names = row.names)
}
gcms <- c("ACCESS-CM2", "BCC-CSM2-MR", "CMCC-ESM2")
scenarios <- c("SSP2-4.5", "SSP5-8.5")
theme_manuscript <- function(base_size = 11) {
  theme_bw(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(family = "Arial", colour = "black"),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(colour = "black", size = 11),
      strip.text = element_text(face = "bold", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "bottom"
    )
}
save_figure <- function(p, stem, width = 150, height = 120) {
  ggsave(file.path(figure_dir_mc, paste0(stem, ".png")), p,
    width = width,
    height = height, units = "mm", dpi = 600, bg = "white"
  )
  ggsave(file.path(figure_dir_mc, paste0(stem, ".pdf")), p,
    width = width,
    height = height, units = "mm", device = cairo_pdf, bg = "white"
  )
}

# 4. Historical and CMIP6 climate data ---------------------------------------

historical <- read_excel(climate_file, sheet = "June summary", skip = 3) |>
  transmute(Plot_ID,
    Block = factor(Block), Plot = as.integer(Plot),
    Elevation_m = as.numeric(Elevation_m),
    Temperature_C = as.numeric(June_mean_temperature_C),
    Precipitation_mm = as.numeric(June_total_precipitation_mm),
    Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude)
  )
future <- read_excel(climate_file, sheet = "2050 model values", skip = 3) |>
  transmute(Plot_ID, GCM, Scenario, Period,
    Block = factor(Block), Plot = as.integer(Plot),
    Elevation_m = as.numeric(Elevation_m),
    Temperature_C = as.numeric(Projected_June_mean_temperature_C),
    Precipitation_mm = as.numeric(Projected_June_precipitation_mm)
  ) |>
  filter(GCM %in% gcms, Scenario %in% scenarios)
expected_future <- tidyr::crossing(
  Plot_ID = historical$Plot_ID,
  GCM = gcms, Scenario = scenarios
)
future_keys <- distinct(future, Plot_ID, GCM, Scenario)
if (nrow(historical) != 27L || anyDuplicated(historical$Plot_ID)) {
  stop("Historical climate data must contain 27 unique source plots.")
}
if (nrow(future_keys) != nrow(expected_future) ||
  nrow(anti_join(expected_future, future_keys,
    by = c("Plot_ID", "GCM", "Scenario")
  )) > 0) {
  stop("Future climate data do not contain every plot x GCM x scenario combination.")
}
if (anyNA(historical) || anyNA(future[, c("Temperature_C", "Precipitation_mm")])) {
  stop("Historical or future climate predictors contain missing values.")
}

# 5. Plant-level soil PC1 and historical predictor scaling -------------------

trait_raw <- read_excel(trait_file, sheet = "Sheet2", .name_repair = function(x) make.unique(x, sep = "_duplicate"))
soil_plant <- trait_raw |>
  transmute(
    plant_no = as.integer(plant_no), Block = factor(Block), Plot = as.integer(Plot),
    Elevation_m = as.numeric(Elevation),
    Carbon = as.numeric(Carbon), pH = as.numeric(pH),
    EC = as.numeric(Electrical.Conductivity),
    Nitrogen = as.numeric(Nitrogen), Phosphorus = as.numeric(Phosphorus)
  )
if (nrow(soil_plant) != nrow(trait_raw) || anyNA(soil_plant)) {
  stop("Expected complete soil predictors for every plant.")
}
soil_plot <- soil_plant |>
  group_by(Block, Plot, Elevation_m) |>
  summarise(across(c(Carbon, pH, EC, Nitrogen, Phosphorus), mean), .groups = "drop")
if (nrow(soil_plot) != 27L) stop("Expected 27 plot soil records.")
soil_pca <- prcomp(select(soil_plot, Carbon, pH, EC, Nitrogen, Phosphorus),
  center = TRUE, scale. = TRUE
)
if (mean(soil_pca$rotation[c("Carbon", "pH", "Nitrogen", "Phosphorus"), "PC1"]) < 0) {
  soil_pca$rotation[, "PC1"] <- -soil_pca$rotation[, "PC1"]
  soil_pca$x[, "PC1"] <- -soil_pca$x[, "PC1"]
}
soil_plot$Soil_PC1 <- soil_pca$x[, "PC1"]
plot_env <- soil_plot |> left_join(historical, by = c("Block", "Plot", "Elevation_m"))
plant_env <- soil_plant |>
  select(plant_no, Block, Plot, Elevation_m) |>
  left_join(plot_env, by = c("Block", "Plot", "Elevation_m"))
scaling <- plot_env |>
  summarise(across(
    c(Elevation_m, Soil_PC1, Temperature_C, Precipitation_mm),
    list(mean = mean, sd = sd)
  ))
z_from_baseline <- function(x, variable) {
  (x - scaling[[paste0(variable, "_mean")]]) /
    scaling[[paste0(variable, "_sd")]]
}
plant_env <- plant_env |>
  mutate(
    Elevation_z = z_from_baseline(Elevation_m, "Elevation_m"),
    Soil_PC1_z = z_from_baseline(Soil_PC1, "Soil_PC1"),
    Temperature_z = z_from_baseline(Temperature_C, "Temperature_C"),
    Precipitation_z = z_from_baseline(Precipitation_mm, "Precipitation_mm"),
    Plot_factor = factor(interaction(Block, Plot, drop = TRUE))
  )
future_env <- future |>
  left_join(select(
    plant_env, plant_no, Plot_ID, Soil_PC1, Soil_PC1_z, Elevation_z,
    Plot_factor
  ), by = "Plot_ID", relationship = "many-to-many") |>
  mutate(
    Temperature_z = z_from_baseline(Temperature_C, "Temperature_C"),
    Precipitation_z = z_from_baseline(Precipitation_mm, "Precipitation_mm")
  )
write.csv(plant_env, file.path(table_dir_mc, "Table_RQ6_historical_predictors.csv"), row.names = FALSE)
write.csv(future_env, file.path(table_dir_mc, "Table_RQ6_future_predictors_by_GCM.csv"), row.names = FALSE)
write.csv(
  data.frame(
    Variable = names(soil_pca$rotation[, 1]),
    PC1_loading = soil_pca$rotation[, 1]
  ),
  file.path(table_dir_mc, "Table_RQ6_soil_PC1_loadings.csv"),
  row.names = FALSE
)
write.csv(t(scaling), file.path(table_dir_mc, "Table_RQ6_predictor_scaling.csv"))

# 6. Plant-level model data --------------------------------------------------

plant_data <- trait_raw |>
  transmute(
    plant_no = as.integer(plant_no), Block = factor(Block), Plot = as.integer(Plot),
    Elevation_m = as.numeric(Elevation),
    Flowering_onset = as.numeric(FloweringOnset),
    Flowering_duration = as.numeric(Final_flowering_date) - as.numeric(FloweringOnset),

    # Mean individual seed mass (MISM), in mg per seed, from trait_data.
    MISM = as.numeric(MISM),
    Seed_number = as.numeric(TotalSeedNumber)
  ) |>
  left_join(
    select(
      plant_env, plant_no, Plot_ID, Elevation_z,
      Soil_PC1_z, Temperature_z, Precipitation_z, Plot_factor
    ),
    by = "plant_no"
  )
fixed_terms <- c("Elevation_z", "Soil_PC1_z", "Temperature_z", "Precipitation_z")
random_term <- "(1 | Block/Plot_factor)"
complete_for <- function(response) {
  plant_data |>
    filter(if_all(
      all_of(c(response, fixed_terms, "Block", "Plot_factor")),
      ~ !is.na(.x)
    ))
}
onset_data <- complete_for("Flowering_onset")
duration_data <- complete_for("Flowering_duration")
mass_data <- complete_for("MISM") |>
  filter(MISM > 0) |>
  mutate(Log_MISM = log(MISM))
number_data <- complete_for("Seed_number") |>
  filter(Seed_number >= 0) |>
  mutate(Seed_number = as.integer(round(Seed_number)))
full_formula <- function(response) {
  as.formula(paste(
    response, "~", paste(fixed_terms, collapse = " + "), "+", random_term
  ))
}
# Resolve an unstable projection count model only when both random-intercept
# variances are negligible. Keep the NB2 family, log link and all fixed effects.
fit_projection_seed_number <- function(number_data) {
  full <- glmmTMB::glmmTMB(full_formula("Seed_number"), data = number_data,
    family = glmmTMB::nbinom2(link = "log"), na.action = na.fail)
  variances <- unlist(lapply(glmmTMB::VarCorr(full)$cond, diag))
  full_se <- summary(full)$coefficients$cond[, "Std. Error"]
  full_ok <- isTRUE(full$fit$convergence == 0) && isTRUE(full$sdr$pdHess) &&
    all(is.finite(full_se))
  negligible <- length(variances) == 2L && all(variances < 1e-8)
  if (full_ok) return(full)
  if (!negligible) stop("Unstable seed-number model with non-negligible random effects; investigate before inference.")
  saveRDS(full, file.path(model_dir_mc, "Models_seed_number_original_unstable.rds"))
  reduced <- glmmTMB::glmmTMB(reformulate(fixed_terms, response = "Seed_number"),
    data = number_data, family = glmmTMB::nbinom2(link = "log"), na.action = na.fail)
  reduced_se <- summary(reduced)$coefficients$cond[, "Std. Error"]
  stopifnot(isTRUE(reduced$fit$convergence == 0), isTRUE(reduced$sdr$pdHess),
    all(is.finite(reduced_se)))
  comparison <- data.frame(Model = c("Original nested random intercepts", "Resolved NB2 without random intercepts"),
    LogLik = c(as.numeric(logLik(full)), as.numeric(logLik(reduced))),
    AIC = c(AIC(full), AIC(reduced)),
    Finite_fixed_effect_SE = c(all(is.finite(full_se)), all(is.finite(reduced_se))))
  utils::write.csv(comparison, file.path(table_dir_mc,
    "Table_seed_number_projection_structure_resolution.csv"), row.names = FALSE)
  message("Seed-number projection: removed two negligible random-intercept components (<1e-8 variance) after the full model failed numerical checks.")
  reduced
}

models <- list(
  Flowering_onset = lmer(full_formula("Flowering_onset"), onset_data,
    REML = FALSE, na.action = na.fail
  ),
  Flowering_duration = lmer(full_formula("Flowering_duration"), duration_data,
    REML = FALSE, na.action = na.fail
  ),
  MISM = lmer(full_formula("Log_MISM"), mass_data,
    REML = FALSE, na.action = na.fail
  ),
  Total_seed_number = fit_projection_seed_number(number_data)
)
saveRDS(models, file.path(model_dir_mc, "Models_RQ6_full_climate_trait_models.rds"))
capture.output(lapply(models, summary),
  file = file.path(model_dir_mc, "Models_RQ6_summaries.txt")
)
model_info <- data.frame(
  Trait = names(models),
  Distribution = c("Gaussian", "Gaussian", "Gaussian on log scale", "negative binomial (nbinom2)"),
  N = vapply(models, nobs, numeric(1)),
  Random_effects = vapply(models, function(model) {
    if (length(reformulas::findbars(formula(model)))) "Nested block and plot intercepts" else
      "None: negligible components removed after numerical failure"
  }, character(1)),
  Positive_definite_Hessian = vapply(models, function(m) {
    if (inherits(m, "glmmTMB")) isTRUE(m$sdr$pdHess) else NA
  }, logical(1)),
  Marginal_R2 = vapply(models, function(m) model_r2(m)[["Marginal"]], numeric(1)),
  Conditional_R2 = vapply(models, function(m) model_r2(m)[["Conditional"]], numeric(1)),
  Singular = c(vapply(models[1:3], lme4::isSingular, logical(1), tol = 1e-4), NA),
  stringsAsFactors = FALSE
)
write.csv(model_info, file.path(table_dir_mc, "Table_RQ6_model_information.csv"), row.names = FALSE)

# 7. Baseline and future predictions on original scales ----------------------

predict_original <- function(model, newdata, trait) {
  # Plants in the same plot share model predictors. Predict once per distinct
  # predictor/grouping vector, then expand to the original plant rows.
  predictor_names <- all.vars(formula(model)[[3L]])
  keys <- as.data.frame(newdata)[, predictor_names, drop = FALSE]
  unique_rows <- !duplicated(keys)
  index <- vctrs::vec_match(keys, keys[unique_rows, , drop = FALSE])
  prediction_data <- newdata[unique_rows, , drop = FALSE]
  value <- if (trait == "Total_seed_number") {
    as.numeric(predict(model, prediction_data,
      type = "response", re.form = NULL, allow.new.levels = TRUE))
  } else {
    as.numeric(predict(model, prediction_data,
      re.form = NULL, allow.new.levels = TRUE))
  }
  # For MISM this is a conditional median, not an arithmetic expected mass.
  if (trait == "MISM") value <- exp(value)
  value[index]
}
baseline_predictions <- bind_rows(lapply(names(models), function(trait) {
  data.frame(select(plant_env, plant_no, Elevation_m),
    Trait = trait,
    Baseline = predict_original(models[[trait]], plant_env, trait)
  )
}))
gcm_predictions <- bind_rows(lapply(names(models), function(trait) {
  data.frame(select(future_env, plant_no, Elevation_m, GCM, Scenario),
    Trait = trait,
    Future = predict_original(models[[trait]], future_env, trait)
  )
})) |>
  left_join(baseline_predictions, by = c("plant_no", "Elevation_m", "Trait")) |>
  mutate(Change = Future - Baseline)
ensemble_predictions <- gcm_predictions |>
  group_by(plant_no, Elevation_m, Scenario, Trait) |>
  summarise(
    Baseline = first(Baseline), Future = mean(Future),
    GCM_min = min(Future), GCM_max = max(Future),
    Change = mean(Change), .groups = "drop"
  )

# 8. Parametric bootstrap: simulate, refit, and recompute paired changes -----

projection_keys <- ensemble_predictions |>
  select(plant_no, Elevation_m, Scenario, Trait)
projection_refit_diagnostics <- function(fit) {
  gaussian <- inherits(fit, "merMod")
  hessian <- if (gaussian) fit@optinfo$derivs$Hessian else NULL
  hessian_ok <- if (gaussian) {
    if (is.null(hessian) || any(!is.finite(hessian))) NA else
      all(eigen(hessian, symmetric = TRUE, only.values = TRUE)$values > 0)
  } else isTRUE(fit$sdr$pdHess)
  convergence_messages <- if (gaussian) fit@optinfo$conv$lme4$messages else fit$fit$message
  converged <- if (gaussian) {
    all(fit@optinfo$conv$opt == 0) && !length(setdiff(
      convergence_messages, "boundary (singular) fit: see help('isSingular')"))
  } else isTRUE(fit$fit$convergence == 0)
  standard_errors <- if (gaussian) coef(summary(fit))[, "Std. Error"] else
    summary(fit)$coefficients$cond[, "Std. Error"]
  singular <- if (gaussian) lme4::isSingular(fit, tol = 1e-4) else {
    # Variance-component boundary screen; glmmTMB has no isSingular method.
    variances <- unlist(lapply(glmmTMB::VarCorr(fit)$cond, diag))
    any(variances < 1e-8)
  }
  list(Converged = converged, Positive_definite_Hessian = hessian_ok,
    Finite_fixed_effect_SE = all(is.finite(standard_errors)), Singular = singular,
    Convergence_messages = paste(convergence_messages, collapse = " | "))
}
bootstrap_trait <- function(model, trait, n_sim) {
  row_keys <- tidyr::crossing(plant_no = plant_env$plant_no, Scenario = scenarios) |>
    left_join(select(plant_env, plant_no, Elevation_m), by = "plant_no")
  workers <- as.integer(Sys.getenv("PROJECTION_BOOTSTRAP_WORKERS", "4"))
  stopifnot(workers >= 1L)
  one <- function(b) {
    warnings <- messages <- character()
    error_text <- ""
    stage <- "simulation"
    diagnostics <- list(Converged = NA, Positive_definite_Hessian = NA,
      Finite_fixed_effect_SE = NA, Singular = NA, Convergence_messages = "")
    value <- tryCatch(withCallingHandlers({
      simulated <- simulate(model, nsim = 1)[[1]]
      stage <- "refit"
      fit <- if (inherits(model, "glmmTMB")) {
        glmmTMB::refit(model, simulated)
      } else {
        # Some saved fits omit derivatives; explicitly calculate them on refit.
        bootstrap_data <- model.frame(model)
        bootstrap_data[[1L]] <- simulated
        lme4::lmer(formula(model), data = bootstrap_data,
          REML = lme4::isREML(model), control = lme4::lmerControl(calc.derivs = TRUE))
      }
      stage <- "diagnostics"
      diagnostics <- projection_refit_diagnostics(fit)
      stage <- "prediction"
      baseline <- predict_original(fit, plant_env, trait)
      future <- data.frame(dplyr::select(future_env, plant_no, Scenario),
        value = predict_original(fit, future_env, trait)) |>
        dplyr::group_by(plant_no, Scenario) |>
        dplyr::summarise(value = mean(value), .groups = "drop") |>
        dplyr::left_join(data.frame(plant_no = plant_env$plant_no, base = baseline), by = "plant_no") |>
        dplyr::mutate(change = value - base)
      dplyr::left_join(row_keys, dplyr::select(future, plant_no, Scenario, change),
        by = c("plant_no", "Scenario"))$change
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    }, message = function(m) {
      messages <<- c(messages, conditionMessage(m)); invokeRestart("muffleMessage")
    }), error = function(e) {
      error_text <<- conditionMessage(e)
      rep(NA_real_, nrow(row_keys))
    })
    finite <- length(value) == nrow(row_keys) && all(is.finite(value))
    log <- data.frame(Trait = trait, Replicate = b, Stage = stage,
      Error = error_text, Finite_predictions = finite,
      Converged = diagnostics$Converged,
      Positive_definite_Hessian = diagnostics$Positive_definite_Hessian,
      Finite_fixed_effect_SE = diagnostics$Finite_fixed_effect_SE,
      Singular = diagnostics$Singular,
      Convergence_messages = diagnostics$Convergence_messages,
      Warnings = paste(unique(warnings), collapse = " | "),
      Messages = paste(unique(messages), collapse = " | "))
    list(value = value, log = log)
  }
  out <- matrix(NA_real_, nrow(row_keys), n_sim)
  logs <- vector("list", n_sim)
  if (workers > 1L) {
    task_cluster <- parallel::makePSOCKcluster(workers)
    on.exit(parallel::stopCluster(task_cluster), add = TRUE)
    parallel::clusterEvalQ(task_cluster, {
      suppressPackageStartupMessages({library(lme4); library(glmmTMB); library(dplyr)})
      NULL
    })
    parallel::clusterSetRNGStream(task_cluster, 12191L)
    parallel::clusterExport(task_cluster, c("model", "trait", "plant_env", "future_env",
      "row_keys", "predict_original", "projection_refit_diagnostics", "full_formula",
      "fixed_terms", "random_term", "number_data"), envir = environment())
    environment(one) <- .GlobalEnv
  }
  for (start in seq.int(1L, n_sim, by = 50L)) {
    ids <- seq.int(start, min(start + 49L, n_sim))
    batch <- if (workers > 1L) parallel::parLapply(task_cluster, ids, one) else lapply(ids, one)
    for (j in seq_along(ids)) {
      out[, ids[j]] <- batch[[j]]$value
      logs[[ids[j]]] <- batch[[j]]$log
    }
    utils::write.csv(do.call(rbind, logs[seq_len(max(ids))]),
      file.path(diagnostic_dir_mc, paste0("Bootstrap_refit_log_", trait, ".csv")), row.names = FALSE)
    message(trait, ": logged ", max(ids), "/", n_sim, " bootstrap replicates")
  }
  successful <- colSums(is.finite(out)) == nrow(out)
  log <- do.call(rbind, logs)
  log$Retained <- successful
  log$Strict_diagnostic_pass <- successful & log$Converged &
    log$Positive_definite_Hessian & log$Finite_fixed_effect_SE
  utils::write.csv(log, file.path(diagnostic_dir_mc,
    paste0("Bootstrap_refit_log_", trait, ".csv")), row.names = FALSE)
  if (sum(log$Strict_diagnostic_pass, na.rm = TRUE) < ceiling(.9 * n_sim)) {
    warning(trait, ": fewer than 90% of refits passed the numerical checks; inspect the log and interval sensitivity before inference.")
  }
  list(keys = mutate(row_keys, Trait = trait), draws = out[, successful, drop = FALSE],
    successful = sum(successful), attempted = n_sim, failed_replicates = which(!successful),
    refit_log = log)
}
bootstrap_file <- file.path(model_dir_mc, "Bootstrap_RQ6_change_draws.rds")
model_signature <- function(model) {
  list(
    procedure = "20261009_conditional_random_effects_independent_seed_mass",
    frame = lapply(model.frame(model), as.vector),
    beta = if (inherits(model, "glmmTMB")) fixef(model)$cond else fixef(model),
    sigma = sigma(model), random = lapply(if (inherits(model, "glmmTMB")) {
      VarCorr(model)$cond
    } else {
      VarCorr(model)
    }, unclass)
  )
}
new_signatures <- lapply(models, model_signature)
old_signatures <- if (!is.null(cached_draws)) attr(cached_draws, "model_signatures") else NULL
if (is.null(old_signatures) && nzchar(explicit_cache_dir) && !is.null(cached_models)) {
  old_signatures <- lapply(cached_models, model_signature)
}
old_future <- if (!is.null(cached_draws)) attr(cached_draws, "future_predictors") else NULL
future_match <- !is.null(old_future) && isTRUE(all.equal(old_future, future_env, check.attributes = FALSE))
if (is.null(old_future) && nzchar(explicit_cache_dir)) {
  old_table <- file.path(dirname(cache_dir), "tables", "Table_RQ6_future_predictors_by_GCM.csv")
  new_table <- file.path(table_dir_mc, "Table_RQ6_future_predictors_by_GCM.csv")
  if (file.exists(old_table) && file.exists(new_table)) {
    future_match <- isTRUE(all.equal(read.csv(old_table), read.csv(new_table), check.attributes = FALSE))
  }
}
bootstrap_objects <- list()
for (trait in names(models)) {
  verified <- cache_requested && future_match && !is.null(cached_draws[[trait]]$refit_log) &&
    !is.null(old_signatures[[trait]]) &&
    isTRUE(all.equal(old_signatures[[trait]], new_signatures[[trait]],
      tolerance = 1e-8, check.attributes = FALSE
    )) &&
    (if (!is.null(cached_draws[[trait]]$attempted)) cached_draws[[trait]]$attempted == n_boot else ncol(cached_draws[[trait]]$draws) == n_boot)
  if (cache_requested && !verified && identical(Sys.getenv("PROJECTION_REQUIRE_VERIFIED_CACHE", "0"), "1")) {
    stop("Cached logged bootstrap did not match current model/predictors for ", trait)
  }
  if (verified) {
    object <- cached_draws[[trait]]
    object$attempted <- n_boot
    object$successful <- ncol(object$draws)
    message(trait, ": reusing ", n_boot, " draws after verifying model inputs, coefficients and climate predictors")
  } else {
    object <- bootstrap_trait(models[[trait]], trait, n_boot)
  }
  bootstrap_objects[[trait]] <- object
  if (!is.null(object$refit_log)) {
    utils::write.csv(object$refit_log, file.path(diagnostic_dir_mc,
      paste0("Bootstrap_refit_log_", trait, ".csv")), row.names = FALSE)
  }
  attr(bootstrap_objects, "model_signatures") <- new_signatures
  attr(bootstrap_objects, "future_predictors") <- future_env
  saveRDS(bootstrap_objects, file.path(model_dir_mc, "Bootstrap_RQ6_progress.rds"))
}
saveRDS(bootstrap_objects, bootstrap_file)
bootstrap_plant_ci <- bind_rows(lapply(bootstrap_objects, function(obj) {
  data.frame(obj$keys,
    Change_CI_low = apply(obj$draws, 1, quantile, .025, na.rm = TRUE),
    Change_CI_high = apply(obj$draws, 1, quantile, .975, na.rm = TRUE),
    Successful_bootstraps = obj$successful
  )
}))
ensemble_predictions <- ensemble_predictions |>
  left_join(bootstrap_plant_ci,
    by = c("plant_no", "Elevation_m", "Scenario", "Trait")
  )
scenario_summary <- ensemble_predictions |>
  group_by(Scenario, Trait) |>
  summarise(
    Baseline_mean = mean(Baseline), Future_mean = mean(Future),
    Mean_change = mean(Change), .groups = "drop"
  )
scenario_boot_ci <- bind_rows(lapply(bootstrap_objects, function(obj) {
  bind_rows(lapply(scenarios, function(scenario) {
    rows <- obj$keys$Scenario == scenario
    draws <- colMeans(obj$draws[rows, , drop = FALSE])
    data.frame(
      Trait = unique(obj$keys$Trait), Scenario = scenario,
      Mean_change_CI_low = quantile(draws, .025),
      Mean_change_CI_high = quantile(draws, .975),
      Successful_bootstraps = obj$successful
    )
  }))
}))
scenario_summary <- left_join(scenario_summary, scenario_boot_ci,
  by = c("Trait", "Scenario")
)

# Retain the scenario-level mean change from every successful bootstrap

# replicate. These draws are used to display the full uncertainty distribution,

# rather than only its percentile limits.
scenario_boot_draws <- bind_rows(lapply(bootstrap_objects, function(obj) {
  bind_rows(lapply(scenarios, function(scenario) {
    rows <- obj$keys$Scenario == scenario
    data.frame(
      Trait = unique(obj$keys$Trait),
      Scenario = scenario,
      Bootstrap_replicate = seq_len(ncol(obj$draws)),
      Mean_change = colMeans(obj$draws[rows, , drop = FALSE]),
      Successful_bootstraps = obj$successful
    )
  }))
}))
write.csv(gcm_predictions, file.path(table_dir_mc, "Table_RQ6_predictions_by_GCM.csv"), row.names = FALSE)
write.csv(ensemble_predictions, file.path(table_dir_mc, "Table_RQ6_plant_ensemble_changes.csv"), row.names = FALSE)
write.csv(scenario_summary, file.path(table_dir_mc, "Table_RQ6_scenario_mean_changes.csv"), row.names = FALSE)
write.csv(scenario_boot_draws,
  file.path(table_dir_mc, "Table_RQ6_bootstrap_mean_change_draws.csv"),
  row.names = FALSE
)

# 9. Extrapolation audit relative to the observed historical climate range ----

extrapolation <- future_env |>
  mutate(
    Temperature_extrapolated = Temperature_C < min(plant_env$Temperature_C) |
      Temperature_C > max(plant_env$Temperature_C),
    Precipitation_extrapolated = Precipitation_mm < min(plant_env$Precipitation_mm) |
      Precipitation_mm > max(plant_env$Precipitation_mm)
  ) |>
  group_by(GCM, Scenario) |>
  summarise(
    N = n(), Temperature_extrapolated = sum(Temperature_extrapolated),
    Precipitation_extrapolated = sum(Precipitation_extrapolated), .groups = "drop"
  )
write.csv(extrapolation, file.path(table_dir_mc, "Table_RQ6_climate_extrapolation_audit.csv"), row.names = FALSE)

# 10. Diagnostics and elevation-gradient projection figure -------------------

png(file.path(diagnostic_dir_mc, "Diagnostics_RQ6_models.png"), 2600, 2200,
  res = 300, type = "cairo"
)
old <- par(mfrow = c(2, 2), family = "Arial", mar = c(4, 4, 2, 1))
for (trait in names(models)) {
  fit <- models[[trait]]
  plot(fitted(fit), residuals(fit, type = "pearson"),
    pch = 21,
    bg = "#2CB89A", xlab = "Fitted values", ylab = "Pearson residuals",
    main = trait
  )
  abline(h = 0, lty = 2)
}
par(old)
dev.off()
trait_labels <- c(
  Flowering_onset = "Flowering onset (days)",
  Flowering_duration = "Flowering duration (days)",
  MISM = "MISM (mg per seed)",
  Total_seed_number = "Total seed number per plant"
)

# Bootstrap uncertainty figure. Violin widths show the density of scenario-level

# mean changes across successful parametric-bootstrap refits. Points and thick

# horizontal intervals show the ensemble mean change and percentile 95% CI.
bootstrap_plot_data <- scenario_boot_draws |>
  mutate(
    Scenario = factor(Scenario, levels = scenarios),
    Trait = factor(Trait, levels = names(trait_labels))
  )

bootstrap_label_positions <- bootstrap_plot_data |>
  group_by(Trait) |>
  summarise(
    Label_x = (min(Mean_change) + max(Mean_change)) / 2,
    .groups = "drop"
  )
bootstrap_interval_data <- scenario_summary |>
  mutate(
    Scenario = factor(Scenario, levels = scenarios),
    Trait = factor(Trait, levels = names(trait_labels)),
    Label = if_else(Trait == "MISM",
      sprintf(
        "%+.4f [%+.4f, %+.4f]", Mean_change,
        Mean_change_CI_low, Mean_change_CI_high
      ),
      sprintf(
        "%+.2f [%+.2f, %+.2f]", Mean_change,
        Mean_change_CI_low, Mean_change_CI_high
      )
    )
  ) |>
  left_join(bootstrap_label_positions, by = "Trait") |>
  mutate(
    Estimate_label = paste0(sub(" \\[.*$", "", Label), " "),
    Interval_label = sub("^[^[]+", "", Label)
  )
p_bootstrap <- ggplot(
  bootstrap_plot_data,
  aes(
    x = Mean_change, y = as.numeric(Scenario), fill = Scenario,
    colour = Scenario, group = Scenario
  )
) +
  geom_vline(
    xintercept = 0, linetype = 2, linewidth = .65,
    colour = "grey35"
  ) +
  geom_violin(
    scale = "width", trim = FALSE, alpha = .30,
    linewidth = .55, width = .7
  ) +
  geom_errorbar(
    data = bootstrap_interval_data,
    aes(
      xmin = Mean_change_CI_low, xmax = Mean_change_CI_high,
      x = Mean_change, y = as.numeric(Scenario)
    ),
    inherit.aes = FALSE, orientation = "y", height = .13,
    linewidth = 1.05, colour = "black"
  ) +
  geom_point(
    data = bootstrap_interval_data,
    aes(x = Mean_change, y = as.numeric(Scenario), fill = Scenario),
    inherit.aes = FALSE, shape = 21, colour = "black", size = 3.5,
    stroke = .7
  ) +
  geom_label(
    data = bootstrap_interval_data,
    aes(x = Mean_change, y = as.numeric(Scenario) + .40, label = Estimate_label),
    inherit.aes = FALSE, hjust = 1, vjust = 0, colour = "black",
    family = "Arial", fontface = "plain", size = 4,
    fill = "white", linewidth = 0, label.padding = grid::unit(0, "lines"),
    show.legend = FALSE
  ) +
  geom_label(
    data = bootstrap_interval_data,
    aes(x = Mean_change, y = as.numeric(Scenario) + .40, label = Interval_label),
    inherit.aes = FALSE, hjust = 0, vjust = 0, colour = "black",
    family = "Arial", fontface = "plain", size = 4,
    fill = "white", linewidth = 0, label.padding = grid::unit(0, "lines"),
    show.legend = FALSE
  ) +
  facet_wrap(~Trait,
    scales = "free_x", ncol = 2,
    labeller = as_labeller(setNames(
      paste0(
        "(", letters[seq_along(trait_labels)], ") ",
        unname(trait_labels)
      ), names(trait_labels)
    ))
  ) +
  scale_fill_manual(values = c(
    "SSP2-4.5" = "#79402E",
    "SSP5-8.5" = "#E5CA28"
  )) +
  scale_colour_manual(values = c(
    "SSP2-4.5" = "#79402E",
    "SSP5-8.5" = "#E5CA28"
  )) +
  scale_x_continuous(expand = expansion(mult = c(.04, .04))) +
  scale_y_continuous(
    breaks = c(1, 2), labels = scenarios,
    limits = c(.5, 2.7), expand = expansion(mult = 0)
  ) +
  labs(
    x = "Projected change from June 2021 baseline",
    y = "2041-2060 climate scenario\n", fill = "2041-2060 scenario", colour = "2041-2060 scenario"
  ) +
  theme_manuscript() +
  theme(
    text = element_text(family = "Arial"),
    strip.background = element_rect(fill = "grey85", colour = "grey40", linewidth = .5),
    strip.text = element_text(
      hjust = .5, face = "bold", size = 11,
      margin = margin(2, 2, 2, 2, unit = "mm")
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(colour = "black", size = 11),
    axis.title = element_text(colour = "black", face = "bold", size = 12),
    plot.subtitle = element_text(colour = "black", size = 12, face = "bold"),
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.spacing.y = grid::unit(2, "mm"),
    panel.spacing.x = grid::unit(1, "mm"),
    plot.margin = margin(3, 6, 3, 3, unit = "mm")
  )
save_figure(p_bootstrap, "Figure_RQ6_bootstrap_trait_changes", 205, 155)
writeLines(paste(
  "Conditional projected trait changes for 2041-2060 relative to the June 2021 baseline",
  "under SSP2-4.5 and SSP5-8.5. Panels show flowering onset, flowering duration,",
  "mean individual seed mass (MISM) and total seed number per plant. Violin widths show",
  "the density of scenario-level mean changes from 1,000 parametric bootstrap replicates.",
  "Points indicate the observed-data projections averaged across three global climate models;",
  "horizontal bars show percentile 95% confidence intervals. Annotations give the projected",
  "change followed by its 95% confidence interval in brackets. The dashed line marks zero change.",
  "More precise estimates are reported in Table 3. Projections assume unchanged plant-level",
  "soil conditions and that present-day spatial climate-trait relationships represent temporal responses."
), file.path(figure_dir_mc, "Figure_RQ6_bootstrap_trait_changes_caption.txt"))

# Baseline-versus-future figure. Error bars show the 2.5th-97.5th

# percentiles of plant-level expected values; open future points show the

# three GCM means, and filled points show their ensemble mean.
baseline_plot <- baseline_predictions |>
  group_by(Trait) |>
  summarise(
    Value = mean(Baseline), Low = quantile(Baseline, .025),
    High = quantile(Baseline, .975), .groups = "drop"
  )
future_plot <- ensemble_predictions |>
  group_by(Trait, Scenario) |>
  summarise(
    Value = mean(Future), Low = quantile(Future, .025),
    High = quantile(Future, .975), Change = mean(Change),
    .groups = "drop"
  ) |>
  left_join(select(baseline_plot, Trait, Baseline = Value), by = "Trait") |>
  mutate(Percent_change = 100 * Change / Baseline)
period_plot <- future_plot |>
  transmute(Trait, Scenario,
    Period_x = 2, Value, Low, High,
    Change, Percent_change
  ) |>
  bind_rows(future_plot |>
    transmute(Trait, Scenario,
      Period_x = 1, Value = Baseline,
      Low = NA_real_, High = NA_real_, Change, Percent_change
    )) |>
  mutate(Scenario = factor(Scenario, levels = scenarios))
baseline_errorbars <- baseline_plot |>
  mutate(Period_x = 1)
gcm_plot <- gcm_predictions |>
  group_by(Trait, Scenario, GCM) |>
  summarise(Value = mean(Future), .groups = "drop") |>
  mutate(Scenario = factor(Scenario, levels = scenarios), Period_x = 2)
label_plot <- future_plot |>
  group_by(Trait) |>
  mutate(
    Span = max(c(High, Baseline)) - min(c(Low, Baseline)),
    Label_y = Baseline + .58 * (Value - Baseline) +
      if_else(Scenario == "SSP2-4.5", .11 * Span, -.11 * Span),
    Label = if_else(Trait == "Flowering_onset",
      sprintf("%+.2f", Change),
      if_else(Trait == "MISM",
        sprintf("%+.4f (%+.1f%%)", Change, Percent_change),
        sprintf("%+.2f (%+.1f%%)", Change, Percent_change)
      )
    ),
    Label_x = 1.53
  ) |>
  ungroup() |>
  mutate(
    Label_y = if_else(Trait == "Total_seed_number" & Scenario == "SSP2-4.5",
      Baseline + 2.0, Label_y
    ),
    Scenario = factor(Scenario, levels = scenarios)
  )
panel_order <- names(trait_labels)
panel_names <- setNames(paste0(
  "(", letters[seq_along(panel_order)], ") ",
  unname(trait_labels)
), panel_order)
period_plot$Trait <- factor(period_plot$Trait, levels = panel_order)
baseline_errorbars$Trait <- factor(baseline_errorbars$Trait, levels = panel_order)
gcm_plot$Trait <- factor(gcm_plot$Trait, levels = panel_order)
label_plot$Trait <- factor(label_plot$Trait, levels = panel_order)
p_change <- ggplot(
  period_plot,
  aes(Period_x, Value, colour = Scenario, group = Scenario)
) +
  geom_line(linewidth = .8) +
  geom_errorbar(
    data = baseline_errorbars,
    aes(x = Period_x, ymin = Low, ymax = High),
    inherit.aes = FALSE, width = .14, colour = "grey40",
    linewidth = .7
  ) +
  geom_point(
    data = baseline_errorbars, aes(Period_x, Value),
    inherit.aes = FALSE, shape = 21, fill = "white",
    colour = "grey40", size = 3.5, stroke = .7
  ) +
  geom_errorbar(
    data = filter(period_plot, Period_x == 2),
    aes(ymin = Low, ymax = High), width = .14, linewidth = .7,
    position = position_dodge(width = .10)
  ) +
  geom_point(
    data = gcm_plot, aes(Period_x, Value, colour = Scenario),
    inherit.aes = FALSE, shape = 21, fill = "white", size = 2.1,
    stroke = .65, position = position_jitter(width = .045, height = 0)
  ) +
  geom_point(data = filter(period_plot, Period_x == 2), size = 3) +
  geom_text(
    data = label_plot,
    aes(Label_x, Label_y, label = Label, colour = Scenario),
    inherit.aes = FALSE, family = "Arial", fontface = "plain",
    size = 4, show.legend = FALSE
  ) +
  facet_wrap(~Trait,
    scales = "free_y", ncol = 2,
    labeller = as_labeller(panel_names)
  ) +
  ggh4x::facetted_pos_scales(y = list(
    Trait == "MISM" ~ scale_y_continuous(
      breaks = scales::breaks_pretty(n = 6), expand = expansion(mult = c(.08, .12))
    ),
    Trait == "Total_seed_number" ~ scale_y_continuous(
      limits = c(270, 310), breaks = seq(270, 310, 10), oob = scales::oob_keep
    )
  )) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("June 2021\nbaseline", "2041-2060\nprojection"),
    limits = c(.88, 2.12)
  ) +
  scale_colour_manual(values = c(
    "SSP2-4.5" = "#79402E",
    "SSP5-8.5" = "#E5CA28"
  )) +
  labs(x = NULL, y = "Expected trait value\n", colour = "2041-2060 climate scenarios") +
  theme_manuscript() +
  theme(
    strip.background = element_blank(), strip.text = element_text(hjust = 0),
    panel.spacing.x = grid::unit(1, "mm"),
    panel.spacing.y = grid::unit(1.5, "mm")
  ) +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 11, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 11, face = "plain"),
    axis.title = element_text(colour = "black", size = 12, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 12, face = "bold"),
    legend.text = element_text(colour = "black", size = 11, face = "plain"),
    legend.title = element_text(colour = "black", size = 12, face = "bold"),
    legend.position = "bottom",
    legend.justification = c(.5, .5),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 1, unit = "mm")
  )

# Separate panels combined with ggarrange; units appear in panel titles.

# 11. Scenario annotations: line-parallel labels with a 2-mm gap -------------

# Position and rotate labels in physical panel coordinates, so the 2-mm gap
# remains correct for both PNG and PDF regardless of the trait's axis units.
makeContent.rq6_line_annotations <- function(x) {
  width_mm <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  height_mm <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  children <- lapply(seq_len(nrow(x$data)), function(i) {
    d <- x$data[i, ]
    angle <- atan2((d$yend - d$y) * height_mm, (d$xend - d$x) * width_mm)
    gp <- grid::gpar(
      col = d$colour, fontsize = d$size * 72.27 / 25.4,
      fontfamily = d$family, fontface = d$fontface
    )
    label <- grid::textGrob(d$label, gp = gp)
    half_height <- grid::convertHeight(grid::grobHeight(label), "mm", valueOnly = TRUE) / 2
    distance <- d$side * (half_height + 2)
    grid::textGrob(d$label,
      x = grid::unit(d$x, "npc") + grid::unit(-sin(angle) * distance, "mm"),
      y = grid::unit(d$y, "npc") + grid::unit(cos(angle) * distance, "mm"),
      rot = angle * 180 / pi, gp = gp
    )
  })
  grid::setChildren(x, do.call(grid::gList, children))
}
GeomScenarioLineAnnotation <- ggplot2::ggproto("GeomScenarioLineAnnotation", ggplot2::Geom,
  required_aes = c("x", "y", "xend", "yend", "label", "side"),
  default_aes = ggplot2::aes(colour = "black", size = 4, family = "Arial", fontface = "plain"),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)
    grid::gTree(data = data, cl = "rq6_line_annotations")
  }
)
geom_scenario_line_annotation <- function(mapping = NULL, data = NULL, ...) {
  ggplot2::layer(
    geom = GeomScenarioLineAnnotation, mapping = mapping, data = data,
    stat = "identity", position = "identity", inherit.aes = FALSE,
    show.legend = FALSE, params = list(...)
  )
}
# End of scenario annotations: line-parallel labels with a 2-mm gap.

label_plot <- label_plot |>
  mutate(Label_y = case_when(
    Trait == "MISM" & Scenario == "SSP2-4.5" ~ Baseline + .42 * Span,
    Trait == "MISM" & Scenario == "SSP5-8.5" ~ Baseline - .10 * Span,
    Trait == "Total_seed_number" & Scenario == "SSP5-8.5" ~ Baseline - 11,
    TRUE ~ Label_y
  ))
change_panel <- function(trait) {
  pp <- ggplot(
    filter(period_plot, Trait == trait),
    aes(Period_x, Value, colour = Scenario, group = Scenario)
  ) +
    geom_line(linewidth = .8) +
    geom_errorbar(
      data = filter(baseline_errorbars, Trait == trait),
      aes(x = Period_x, ymin = Low, ymax = High),
      inherit.aes = FALSE, width = .10, colour = "grey40", linewidth = .7
    ) +
    geom_point(
      data = filter(baseline_errorbars, Trait == trait),
      aes(Period_x, Value), inherit.aes = FALSE, shape = 21,
      fill = "white", colour = "grey40", size = 3.5, stroke = .7
    ) +
    geom_errorbar(
      data = filter(period_plot, Trait == trait, Period_x == 2),
      aes(ymin = Low, ymax = High), width = .10, linewidth = .7,
      position = position_dodge(width = .08)
    ) +
    geom_point(
      data = filter(gcm_plot, Trait == trait),
      aes(Period_x, Value, colour = Scenario), inherit.aes = FALSE,
      shape = 21, fill = "white", size = 2.1, stroke = .65,
      position = position_jitter(width = .035, height = 0, seed = 12191)
    ) +
    geom_point(data = filter(period_plot, Trait == trait, Period_x == 2), size = 3) +
    geom_scenario_line_annotation(
      data = filter(label_plot, Trait == trait),
      aes(
        x = Label_x, y = Baseline + (Label_x - 1) * (Value - Baseline),
        xend = 2, yend = Value, label = Label, colour = Scenario,
        side = ifelse(Scenario == "SSP2-4.5", 1, -1)
      ),
      family = "Arial", size = 4
    ) +
    facet_wrap(~Trait, labeller = as_labeller(panel_names)) +
    scale_colour_manual(values = c("SSP2-4.5" = "#79402E", "SSP5-8.5" = "#E5CA28")) +
    scale_x_continuous(
      breaks = c(1, 2),
      labels = c("June 2021\nbaseline", "2041-2060\nprojection"),
      limits = c(.90, 2.14), expand = expansion(mult = 0)
    ) +
    labs(
      x = NULL, y = NULL,
      colour = "2041-2060 scenarios"
    ) +
    theme_manuscript() +
    theme(
      text = element_text(family = "Arial"),
      plot.subtitle = element_text(face = "bold", size = 12),
      strip.background = element_rect(fill = "grey85", colour = "grey40", linewidth = .5),
      strip.text = element_text(
        face = "bold", size = 12, hjust = .5,
        margin = margin(2, 2, 2, 2, unit = "mm")
      ),
      axis.text = element_text(colour = "black", size = 12),
      panel.grid = element_blank(),
      legend.text = element_text(size = 12, color = "black"),
      legend.title = element_text(face = "bold", size = 12, color = "black"),
      plot.margin = margin(.5, 2, .5, 2, unit = "mm")
    )
  if (trait == "Total_seed_number") {
    pp <- pp +
      scale_y_continuous(
        limits = c(260, 310), breaks = seq(260, 310, 10),
        oob = scales::oob_keep, expand = expansion(mult = .03)
      )
  }
  if (trait %in% c("Flowering_onset", "Flowering_duration")) {
    pp <- pp +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  pp
}
p_change <- ggpubr::ggarrange(
  plotlist = lapply(panel_order, change_panel),
  ncol = 2, nrow = 2, common.legend = TRUE,
  legend = "bottom", align = "v"
)
p_change <- ggpubr::annotate_figure(
  p_change,
  left = ggpubr::text_grob("Expected trait value",
    rot = 90,
    size = 13, face = "bold", family = "Arial"
  )
)
save_figure(p_change, "Figure_RQ6_projected_trait_changes_elevation", 220, 170)
message(
  "RQ5 conditional mid-century projections complete with ", n_boot,
  " requested bootstrap replicates. Outputs: ", output_dir_mc
)

# Combine all CSV tables from this analysis into one multi-sheet workbook.
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Install the openxlsx package to export the combined table workbook.")
}
table_csv_files <- list.files(table_dir_mc, pattern = "\\.csv$", full.names = TRUE)
if (length(table_csv_files)) {
  table_sheet_names <- tools::file_path_sans_ext(basename(table_csv_files)) |>
    sub("^Table_", "", x = _) |>
    gsub("[^A-Za-z0-9 _-]", "_", x = _) |>
    substr(1, 27) |>
    make.unique(sep = "_")
  table_workbook <- setNames(
    lapply(table_csv_files, read.csv, check.names = FALSE),
    table_sheet_names
  )
  openxlsx::write.xlsx(
    table_workbook,
    file = file.path(table_dir_mc, "summary_tables_RQ6_mid_century_projections.xlsx"),
    overwrite = TRUE
  )
  message(
    "Combined table workbook written: ",
    file.path(table_dir_mc, "summary_tables_RQ6_mid_century_projections.xlsx")
  )
}

# 12. Publication-ready gt tables --------------------------------------------

# Shared by analysis scripts 06-11. Run scripts from the project root.

fix_r2_rtf <- function(file) {
  text <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (suffix in c("m", "c")) {
    token <- paste0("R<sup>2</sup><sub>", suffix, "</sub>")
    replacement <- paste0("R{\\super 2}{\\sub ", suffix, "}")
    text <- paste(strsplit(text, token, fixed = TRUE)[[1]], collapse = replacement)
  }
  writeLines(text, file, useBytes = TRUE)
}
save_publication_table <- function(table, file, ...) {
  gt::gtsave(table, file, ...)
  if (tolower(tools::file_ext(file)) == "rtf") fix_r2_rtf(file)
  invisible(file)
}

format_table_2_effect <- function(x) {
  small <- !is.na(x) & x != 0 & round(x, 3) == 0
  formatted <- sprintf("%.3f", round(x, 3))
  formatted[small] <- sprintf("%.3e", x[small])
  formatted[is.na(x)] <- NA_character_
  formatted
}

format_candidate_table_export <- function(data) {
  result <- data
  for (column in intersect(c("logLik", "AIC", "AICc", "Delta_AICc"), names(result))) {
    result[[column]] <- ifelse(is.na(data[[column]]), NA_character_, sprintf("%.1f", as.numeric(data[[column]])))
  }
  if ("Akaike_weight" %in% names(result)) {
    result$Akaike_weight <- ifelse(is.na(data$Akaike_weight), NA_character_,
      sprintf("%.3f", as.numeric(data$Akaike_weight))
    )
  }
  result
}

publication_table_title <- function(stem) {
  clean <- sub("^Table_", "", stem)
  if (identical(clean, "1")) {
    return("Table 1. Flowering phenology and maternal seed allocation")
  }
  if (identical(clean, "2")) {
    return("Table 2. Elevation, maternal seed mass and germination responses")
  }
  if (identical(clean, "S1")) {
    return("Table S1. Candidate-model comparisons for flowering phenology and maternal seed allocation")
  }
  if (identical(clean, "S2")) {
    return("Table S2. H7 elevation model results and H8 seed-mass candidate models")
  }
  clean <- gsub("_", " ", clean)
  clean <- gsub("\\bAICc\\b", "AICc", clean)
  clean <- gsub("\\bR2\\b", "R-squared", clean)
  tools::toTitleCase(clean)
}

publication_table_subtitle <- function(stem, analysis_label) {
  type <- dplyr::case_when(
    grepl("AICc|distribution_comparison", stem, ignore.case = TRUE) ~
      "Candidate-model comparison and model-support statistics",
    grepl("coefficient|model_results", stem, ignore.case = TRUE) ~
      "Model estimates, uncertainty and goodness-of-fit statistics",
    grepl("prediction|change", stem, ignore.case = TRUE) ~
      "Model-based estimates on the reported response scale",
    grepl("diagnostic|audit|check|skewness", stem, ignore.case = TRUE) ~
      "Analysis diagnostics and quality-control summary",
    grepl("PCA|loading|scaling", stem, ignore.case = TRUE) ~
      "Dimension reduction and predictor-scaling summary",
    TRUE ~ "Analysis data and derived quantities"
  )
  paste0(analysis_label, " — ", type)
}

publication_column_labels <- function(columns) {
  labels <- c(
    logLik = "Log-likelihood", AIC = "AIC", AICc = "AICc",
    Delta_AICc = "ΔAICc", Akaike_weight = "Akaike weight",
    Positive_definite_Hessian = "Positive-definite Hessian",
    CI_low = "Lower 95% CI", CI_high = "Upper 95% CI",
    P_value = "P value", P_Value = "P value", p_value = "P value",
    Dispersion_P = "Dispersion P", Zero_inflation_P = "Zero-inflation P",
    Outlier_P = "Outlier P", Marginal_R2 = "R²m",
    Conditional_R2 = "R²c", R_squared = "R²",
    IRR = "Rate ratio", IRR_low = "Rate-ratio lower CI",
    IRR_high = "Rate-ratio upper CI", SE = "SE", N = "N", K = "K",
    Percent_change_per_100m_elevation_increase =
      "% change per 100-m elevation increase",
    Percent_change_in_seed_number_per_1mg_seed_mass_decrease =
      "% change per 1-mg MISM decrease"
  )
  fallback <- tools::toTitleCase(gsub("_", " ", columns))
  names(fallback) <- columns
  matched <- intersect(columns, names(labels))
  fallback[matched] <- labels[matched]
  result <- as.list(fallback)
  if ("Marginal_R2" %in% columns) result$Marginal_R2 <- gt::html("R<sup>2</sup><sub>m</sub>")
  if ("Conditional_R2" %in% columns) result$Conditional_R2 <- gt::html("R<sup>2</sup><sub>c</sub>")
  result
}

add_available_spanner <- function(table, label, columns, used) {
  available <- setdiff(intersect(columns, colnames(table$`_data`)), used)
  if (!length(available)) {
    return(list(table = table, used = used))
  }
  list(
    table = gt::tab_spanner(table,
      label = label,
      columns = tidyselect::all_of(available)
    ),
    used = c(used, available)
  )
}

build_publication_gt <- function(data, stem, analysis_label) {
  data <- as.data.frame(data, check.names = FALSE)
  stub_candidates <- c(
    "Model", "Trait", "Response", "Variable", "Component",
    "PlotID", "Plot_ID", "Distribution", "Scenario"
  )
  stub <- intersect(stub_candidates, names(data))[1]
  group <- if ("Hypothesis" %in% names(data) && !is.na(stub) &&
    stub != "Hypothesis") {
    "Hypothesis"
  } else {
    NULL
  }

  table <- if (!is.na(stub) && length(stub)) {
    gt::gt(data, rowname_col = stub, groupname_col = group)
  } else {
    gt::gt(data)
  }

  table <- table |>
    gt::tab_header(
      title = gt::md(paste0("**", publication_table_title(stem), "**")),
      subtitle = publication_table_subtitle(stem, analysis_label)
    ) |>
    gt::cols_label(.list = publication_column_labels(names(data)))

  used <- character()
  spanners <- list(
    list("Model fit and support", c(
      "N", "K", "logLik", "AIC", "AICc",
      "Delta_AICc", "Akaike_weight", "Converged",
      "Positive_definite_Hessian", "Singular"
    )),
    list("Coefficient estimate", c(
      "Estimate", "SE", "CI_low", "CI_high",
      "z_value", "z_approximation", "t_value",
      "P_value", "P_Value", "p_value"
    )),
    list("Effect on response scale", c(
      "IRR", "IRR_low", "IRR_high",
      "Percent_change_per_100m_elevation_increase",
      "Percent_change_in_seed_number_per_1mg_seed_mass_decrease"
    )),
    list("Explained variation", c("Marginal_R2", "Conditional_R2", "R_squared")),
    list("Residual diagnostics", c(
      "Dispersion_statistic", "Dispersion_P",
      "Zero_inflation_P", "Outlier_P"
    ))
  )
  for (definition in spanners) {
    result <- add_available_spanner(table, definition[[1]], definition[[2]], used)
    table <- result$table
    used <- result$used
  }

  numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]
  count_columns <- intersect(c(
    "N", "K", "n", "Count", "N_accessions",
    "Replicate", "Day"
  ), numeric_columns)
  percent_columns <- intersect(grep("percent|percentage|Percent", names(data),
    value = TRUE
  ), numeric_columns)
  p_columns <- intersect(grep("(^P_|_P$|P_value|p_value|P_Value)", names(data),
    value = TRUE
  ), numeric_columns)
  general_numeric <- setdiff(
    numeric_columns,
    union(count_columns, union(percent_columns, p_columns))
  )

  if (length(general_numeric)) {
    table <- gt::fmt_number(table,
      columns = tidyselect::all_of(general_numeric),
      decimals = 3, use_seps = TRUE
    )
  }
  # Preserve small per-mg effects rather than displaying them as zero.
  effect_columns <- intersect(c("Estimate", "SE", "CI_low", "CI_high"), general_numeric)
  h6_rows <- if (identical(stem, "2")) integer(0) else if ("Hypothesis" %in% names(data)) which(data$Hypothesis %in% c("H6", "H8b", "H8c")) else integer(0)
  if (length(effect_columns) && length(h6_rows)) {
    table <- gt::fmt_number(table,
      columns = tidyselect::all_of(effect_columns),
      rows = h6_rows, decimals = 6, use_seps = TRUE
    )
  }
  if (stem %in% c("S1", "S2", "S1_all_candidate_models")) {
    fit_columns <- intersect(c("logLik", "AIC", "AICc", "Delta_AICc"), numeric_columns)
    if (length(fit_columns)) {
      table <- gt::fmt_number(table,
        columns = tidyselect::all_of(fit_columns), decimals = 1, use_seps = TRUE
      )
    }
    if ("Akaike_weight" %in% numeric_columns) {
      table <- gt::fmt_number(table,
        columns = "Akaike_weight", decimals = 3
      )
    }
  }
  if (stem %in% c("2", "S2") && length(effect_columns)) {
    table <- gt::fmt(table,
      columns = tidyselect::all_of(effect_columns),
      fns = format_table_2_effect
    )
  }
  if (length(count_columns)) {
    table <- gt::fmt_integer(table,
      columns = tidyselect::all_of(count_columns),
      use_seps = TRUE
    )
  }
  if (length(percent_columns)) {
    table <- gt::fmt_number(table,
      columns = tidyselect::all_of(percent_columns),
      decimals = 2, pattern = "{x}%"
    )
  }
  if (length(p_columns)) {
    p_formatter <- function(x) {
      ifelse(is.na(x), "—", ifelse(x < .001, "<0.001", sprintf("%.3f", x)))
    }
    table <- gt::fmt(table,
      columns = tidyselect::all_of(p_columns),
      fns = p_formatter
    )
  }

  table <- table |>
    gt::sub_missing(columns = gt::everything(), missing_text = "—") |>
    gt::opt_row_striping() |>
    gt::opt_table_font(font = c("Arial", "Helvetica", "sans-serif")) |>
    gt::tab_options(
      table.width = gt::pct(100),
      table.font.size = gt::px(12),
      heading.align = "left",
      heading.background.color = "#E8F1F5",
      column_labels.background.color = "#D9E8EF",
      row_group.background.color = "#EEF4F7",
      stub.background.color = "#F5F8FA",
      table.border.top.color = "#244A5A",
      table.border.top.width = gt::px(2),
      table.border.bottom.color = "#244A5A",
      table.border.bottom.width = gt::px(2),
      data_row.padding = gt::px(5),
      source_notes.font.size = gt::px(10)
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold", color = "#173947"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_source_note(
      source_note = gt::md(
        paste0(
          if (stem %in% c("S1", "S1_all_candidate_models")) {
            "*Note:* logLik, AIC, AICc and ΔAICc are displayed to one decimal place; Akaike weights to three decimal places. "
          } else {
            "*Note:* Numeric results are displayed to three decimal places "
          },
          if (stem %in% c("S1", "S1_all_candidate_models")) {
            "Counts are integers. "
          } else if (identical(stem, "2")) {
            paste0(
              "with integer counts. Nonzero estimates, SEs and confidence limits that round to zero ",
              "are shown in scientific notation with three decimal places in the mantissa. "
            )
          } else {
            "with six decimals for small per-mg effects and other formats for percentages or counts. "
          },
          "Rows: ", nrow(data), ". Generated reproducibly with **gt**."
        )
      )
    )

  if ("Delta_AICc" %in% names(data)) {
    table <- table |>
      gt::data_color(
        columns = tidyselect::all_of("Delta_AICc"),
        palette = c("#E4F3E8", "#FFF4CC", "#F7D8D5"),
        domain = c(0, max(data$Delta_AICc, na.rm = TRUE))
      ) |>
      gt::tab_footnote(
        footnote = "Lower ΔAICc indicates greater relative support within the candidate set.",
        locations = gt::cells_column_labels(columns = "Delta_AICc")
      )
  }
  if ("P_value" %in% names(data)) {
    significant_rows <- which(is.finite(data$P_value) & data$P_value < .05)
    if (length(significant_rows)) {
      table <- gt::tab_style(
        table,
        style = gt::cell_text(weight = "bold", color = "#8B1A1A"),
        locations = gt::cells_body(columns = "P_value", rows = significant_rows)
      )
    }
  }
  table
}

render_gt_tables <- function(table_dir, analysis_label, png_row_limit = 0L,
                             rtf_row_limit = 200L) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Install the gt package to create publication-ready tables.")
  }
  csv_files <- list.files(table_dir, pattern = "^Table_.*\\.csv$", full.names = TRUE)
  if (!length(csv_files)) {
    return(invisible(character()))
  }
  output_dir <- file.path(table_dir, "gt")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  outputs <- character()
  for (csv_file in csv_files) {
    data <- utils::read.csv(csv_file, check.names = FALSE)
    empty_names <- which(is.na(names(data)) | !nzchar(names(data)))
    if (length(empty_names)) {
      names(data)[empty_names] <- paste0("Statistic", seq_along(empty_names))
    }
    stem <- tools::file_path_sans_ext(basename(csv_file))
    table <- build_publication_gt(data, stem, analysis_label)
    html_file <- file.path(output_dir, paste0(stem, ".html"))
    gt::gtsave(table, html_file, inline_css = TRUE)
    outputs <- c(outputs, html_file)
    if (nrow(data) <= rtf_row_limit) {
      rtf_file <- file.path(output_dir, paste0(stem, ".rtf"))
      save_publication_table(table, rtf_file)
      outputs <- c(outputs, rtf_file)
    }
    if (nrow(data) <= png_row_limit) {
      png_file <- file.path(output_dir, paste0(stem, ".png"))
      png_saved <- tryCatch(
        {
          gt::gtsave(table, png_file, zoom = 2, expand = 12)
          TRUE
        },
        error = function(e) {
          warning("PNG export skipped for ", stem, ": ", conditionMessage(e))
          FALSE
        }
      )
      if (png_saved) outputs <- c(outputs, png_file)
    }
  }
  message("Publication-ready gt tables written to: ", output_dir)
  invisible(outputs)
}
# End of publication tables: formatting, r-squared notation and exports.

render_gt_tables(table_dir_mc, "Conditional mid-century trait projections (RQ5)")

# 13. Numbered publication exports -------------------------------------------

# Numbered publication exports preserve the descriptive working filenames.

publication_figure_exports()

publication_table_3()
publication_render_tables()

# 14. Audit bootstrap records and prediction population ----------------------

# This audit reads saved objects; it neither reruns nor changes bootstrap draws.
projection_audit_root <- file.path("outputs", "projected_trait_changes_mid_century")
projection_audit_models <- readRDS(file.path(projection_audit_root, "models",
  "Models_RQ6_full_climate_trait_models.rds"))
projection_audit_draws <- readRDS(file.path(projection_audit_root, "models",
  "Bootstrap_RQ6_change_draws.rds"))
projection_audit_future <- attr(projection_audit_draws, "future_predictors")
if (is.null(projection_audit_future)) stop("Full-precision future predictors are required for the audit.")
stopifnot(all(table(projection_audit_future$plant_no, projection_audit_future$Scenario) == 3L))
projection_audit_predict <- function(model, data, trait, retain_random) {
  random_setting <- if (retain_random) NULL else NA
  value <- if (inherits(model, "glmmTMB")) {
    predict(model, newdata = data, type = "response", re.form = random_setting)
  } else {
    predict(model, newdata = data, re.form = random_setting)
  }
  if (trait == "MISM") exp(as.numeric(value)) else as.numeric(value)
}
projection_audit_results <- list()
projection_bootstrap_audit <- list()
for (trait in names(projection_audit_models)) {
  model <- projection_audit_models[[trait]]
  baseline_frame <- model.frame(model)
  stopifnot(nrow(baseline_frame) == 581L)
  object <- projection_audit_draws[[trait]]
  projection_bootstrap_audit[[trait]] <- data.frame(Trait = trait,
    Attempted = object$attempted, Retained = ncol(object$draws),
    Failed_replicates = length(object$failed_replicates),
    All_retained_draws_finite = all(is.finite(object$draws)),
    Refit_warning_counts = if (is.null(object$refit_log)) NA_integer_ else
      sum(nzchar(object$refit_log$Warnings)),
    Convergence_checks = if (is.null(object$refit_log)) "Not recorded per replicate" else "See refit log",
    Hessian_checks = if (is.null(object$refit_log)) "Not recorded per replicate" else "See refit log",
    Singularity_checks = if (is.null(object$refit_log)) "Not recorded per replicate" else "See refit log")
  for (retain_random in c(TRUE, FALSE)) {
    baseline_value <- projection_audit_predict(model, baseline_frame, trait, retain_random)
    future_value <- projection_audit_predict(model, projection_audit_future, trait, retain_random)
    for (scenario in unique(projection_audit_future$Scenario)) {
      future_mean <- mean(future_value[projection_audit_future$Scenario == scenario])
      projection_audit_results[[length(projection_audit_results) + 1L]] <- data.frame(
        Trait = trait, Scenario = scenario,
        Prediction = if (retain_random) "Conditional: fitted random effects retained" else "Fixed effects: random effects set to zero",
        Baseline = mean(baseline_value), Future = future_mean,
        Mean_change = future_mean - mean(baseline_value))
    }
  }
}
write.csv(do.call(rbind, projection_bootstrap_audit),
  file.path(projection_audit_root, "tables", "Table_bootstrap_record_audit.csv"), row.names = FALSE)
projection_population_audit <- do.call(rbind, projection_audit_results)
write.csv(projection_population_audit,
  file.path(projection_audit_root, "tables", "Table_prediction_population_audit.csv"), row.names = FALSE)
print(projection_population_audit)
# Setting random effects to zero is not integration over their distribution.
# Under log links it need not give the marginal arithmetic population mean.

# 15. Summarise logged bootstrap diagnostics and interval sensitivity ----------

# Prefer the newly logged audit run while historical publication draws remain
# unchanged. A full normal rerun already stores logs in the main bootstrap file.
logged_bootstrap_file <- file.path(model_dir_mc, "Bootstrap_RQ6_logged_draws.rds")
main_logged_objects <- readRDS(file.path(model_dir_mc, "Bootstrap_RQ6_change_draws.rds"))
main_has_logs <- all(vapply(main_logged_objects, function(x) !is.null(x$refit_log), logical(1)))
logged <- if (main_has_logs) main_logged_objects else if (file.exists(logged_bootstrap_file)) {
  readRDS(logged_bootstrap_file)
} else main_logged_objects
if (all(vapply(logged, function(x) !is.null(x$refit_log), logical(1)))) {
  root <- output_dir_mc
  all_refit_logs <- bind_rows(lapply(logged, function(x) x$refit_log))
  utils::write.csv(all_refit_logs,
    file.path(root, "tables", "Table_bootstrap_refit_log_all_traits.csv"), row.names = FALSE)
  baseline_diagnostics <- bind_rows(lapply(names(models), function(trait) {
    data.frame(Trait = trait, projection_refit_diagnostics(models[[trait]]))
  }))
  utils::write.csv(baseline_diagnostics,
    file.path(root, "tables", "Table_projection_baseline_diagnostic_audit.csv"), row.names = FALSE)
  summary <- bind_rows(lapply(logged,function(x) {
   z<-x$refit_log
   data.frame(Trait=unique(z$Trait),Attempted=nrow(z),Retained=sum(z$Retained),
   Errors=sum(nzchar(z$Error)),Warning_replicates=sum(nzchar(z$Warnings)),
   Convergence_flags=sum(!z$Converged,na.rm=TRUE),
   Hessian_flags=sum(!z$Positive_definite_Hessian,na.rm=TRUE),
   Hessian_unavailable=sum(is.na(z$Positive_definite_Hessian)),
   Singular=sum(z$Singular,na.rm=TRUE),Finite_SE_flags=sum(!z$Finite_fixed_effect_SE,na.rm=TRUE),
   Strict_diagnostic_pass=sum(z$Strict_diagnostic_pass,na.rm=TRUE))
  }))
  utils::write.csv(summary,file.path(root,"tables","Table_bootstrap_refit_diagnostic_summary.csv"),row.names=FALSE)
  print(summary)
  ci<-bind_rows(lapply(logged,function(x)bind_rows(lapply(unique(x$keys$Scenario),function(s) {
   ix<-x$keys$Scenario==s
   means<-colMeans(x$draws[ix,,drop=FALSE])
   z<-x$refit_log[x$refit_log$Retained,,drop=FALSE]
   good<-which(!is.na(z$Strict_diagnostic_pass)&z$Strict_diagnostic_pass)
   rbind(data.frame(Trait=unique(x$keys$Trait),Scenario=s,Set="All finite draws",
   N=length(means),Low=quantile(means,.025),High=quantile(means,.975)),
   data.frame(Trait=unique(x$keys$Trait),Scenario=s,Set="Diagnostic pass",
   N=length(good),Low=if(length(good))quantile(means[good],.025)else NA_real_,
   High=if(length(good))quantile(means[good],.975)else NA_real_))
  }))))
  utils::write.csv(ci,file.path(root,"tables","Table_bootstrap_logged_intervals.csv"),row.names=FALSE)
  print(ci)
}

# 16. Verify residuals and prediction sensitivity after seed-model resolution ---

if (!requireNamespace("DHARMa", quietly = TRUE)) stop("Missing package: DHARMa")
seed_projection_model <- models$Total_seed_number
seed_simulation <- DHARMa::simulateResiduals(seed_projection_model,
  n = 1000, plot = FALSE, seed = 12191)
seed_residual_checks <- data.frame(
  Model = "Resolved seed-number projection model",
  Uniformity_P = DHARMa::testUniformity(seed_simulation, plot = FALSE)$p.value,
  Dispersion_P = DHARMa::testDispersion(seed_simulation, plot = FALSE)$p.value,
  Outlier_P = DHARMa::testOutliers(seed_simulation, plot = FALSE)$p.value)
for (group in c("Block", "Plot_factor")) {
  grouped_simulation <- DHARMa::recalculateResiduals(seed_simulation, group = plant_env[[group]])
  seed_residual_checks[[paste0(group, "_aggregate_uniformity_P")]] <-
    DHARMa::testUniformity(grouped_simulation, plot = FALSE)$p.value
  seed_residual_checks[[paste0(group, "_aggregate_dispersion_P")]] <-
    DHARMa::testDispersion(grouped_simulation, plot = FALSE)$p.value
}
utils::write.csv(seed_residual_checks, file.path(table_dir_mc,
  "Table_seed_number_stability_residual_checks.csv"), row.names = FALSE)
png(file.path(diagnostic_dir_mc, "Seed_number_NB2_reduced_DHARMa.png"),
  width = 1800, height = 1200, res = 180)
plot(seed_simulation)
dev.off()
original_seed_file <- file.path(model_dir_mc, "Models_seed_number_original_unstable.rds")
if (file.exists(original_seed_file)) {
  original_seed_model <- readRDS(original_seed_file)
  original_seed_base <- predict_original(original_seed_model, plant_env, "Total_seed_number")
  resolved_seed_base <- predict_original(seed_projection_model, plant_env, "Total_seed_number")
  original_seed_future <- predict_original(original_seed_model, future_env, "Total_seed_number")
  resolved_seed_future <- predict_original(seed_projection_model, future_env, "Total_seed_number")
  seed_prediction_comparison <- bind_rows(lapply(scenarios, function(scenario) {
    rows <- future_env$Scenario == scenario
    data.frame(Scenario = scenario,
      Original_mean_change = mean(original_seed_future[rows]) - mean(original_seed_base),
      Resolved_mean_change = mean(resolved_seed_future[rows]) - mean(resolved_seed_base),
      Maximum_absolute_prediction_difference = max(abs(c(
        resolved_seed_base - original_seed_base,
        resolved_seed_future[rows] - original_seed_future[rows]))))
  }))
  utils::write.csv(seed_prediction_comparison, file.path(table_dir_mc,
    "Table_seed_number_stability_prediction_comparison.csv"), row.names = FALSE)
}

# Refresh the workbook after the final audit CSVs have been updated.
table_csv_files <- list.files(table_dir_mc, pattern = "\\.csv$", full.names = TRUE)
table_sheet_names <- tools::file_path_sans_ext(basename(table_csv_files)) |>
  sub("^Table_", "", x = _) |>
  gsub("[^A-Za-z0-9 _-]", "_", x = _) |>
  substr(1, 27) |>
  make.unique(sep = "_")
openxlsx::write.xlsx(setNames(lapply(table_csv_files, read.csv, check.names = FALSE),
  table_sheet_names), file = file.path(table_dir_mc,
    "summary_tables_RQ6_mid_century_projections.xlsx"), overwrite = TRUE)
