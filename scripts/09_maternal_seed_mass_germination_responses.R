# 1. Local publication support functions -------------------------------------

# Local definitions: this script never sources another R file.
publication_root <- "outputs"
publication_captions <- c(
  Table_2 = "Table 2. Source elevation, 100-seed mass and germination responses in 581 maternal accessions. H7a is time to first germination (T0), H7b mean germination rate (MGR), H7c germination percentage (GP), and H7d time spread (Tg-T0). H7d has no directional prediction. H8a, H8b and H8c concern T0, MGR and GP, respectively; H8 time spread also has no directional prediction. Coefficients and confidence limits are per 100-m source-elevation increase for H7 and per 1-mg increase in measured 100-seed mass for H8. Timing responses are in days, MGR in inverse days and GP in percentage points. P values test coefficients against zero. Each H8 response has an unadjusted and an elevation-adjusted model; Table S1 compares these pairs. Marginal_R2 and Conditional_R2 describe marginal and conditional R-squared. Convergence and singularity flags are retained from the fitted models.",
  Table_S1 = "Table S1. Candidate-model comparisons for H1-H6 and seed-mass-germination models. Each comparison uses the same 581 maternal plants and random-effects structure. H1, H2, H4 and H5 each compare elevation alone, elevation + climate, elevation + soil, and elevation + climate + soil. Climate includes temperature and precipitation jointly; soil is soil PC1. H3 compares onset alone, onset + elevation, onset + elevation + climate, onset + elevation + soil, and onset + elevation + climate + soil. H3 AICc values were calculated from duration models and are equivalent for final flowering date with the same observations, onset predictor, covariates, random effects, maximum-likelihood fitting and residual covariance model. H3 coefficient tables and Figure 4 use final flowering date. H6 has one MISM candidate, not an environmental covariate comparison. H8 and germination time spread compare unadjusted and elevation-adjusted 100-seed-mass models within response. H7 is omitted. K is the parameter count; AICc is small-sample-corrected AIC; Delta_AICc and Akaike_weight quantify relative support within a candidate set."
)
publication_response <- function(h) {
  unname(c(
    H1 = "Flowering onset", H2 = "Flowering duration", H3 = "Final flowering date",
    H4 = "ln(MISM)", H5 = "Total seed number", H6 = "Total seed number"
  )[h])
}
publication_candidates <- function() {
  files <- c(
    file.path(publication_root, "maternal_seed_allocation", "tables", "Table_S1.csv"),
    file.path(publication_root, "germination_elevation", "tables", "Table_S1_all_candidate_models.csv")
  )
  for (f in files[file.exists(files)]) {
    t <- utils::read.csv(f, check.names = FALSE)
    # Table S1 is the manuscript candidate set; H7 results belong in their own tables.
    t <- t[!grepl("^H7", t$Hypothesis), , drop = FALSE]
    if (!"Response" %in% names(t)) t$Response <- NA_character_
    h <- t$Hypothesis %in% paste0("H", 1:6)
    t$Response[h] <- publication_response(t$Hypothesis[h])
    t$AICc_response_scale <- t$Response
    t$AICc_response_scale[t$Hypothesis == "H3"] <- "Duration; equivalent final-date comparison"
    t$Candidate_set <- ifelse(t$Hypothesis %in% c("H1", "H2", "H4", "H5"), "Elevation; +climate; +soil; +climate+soil",
      ifelse(t$Hypothesis == "H3", "Onset; +elevation; +elevation+climate; +elevation+soil; +elevation+climate+soil",
        ifelse(t$Hypothesis == "H6", "MISM only; no environmental candidate set", "100-seed mass; 100-seed mass+elevation")
      )
    )
    utils::write.csv(t, f, row.names = FALSE)
    writeLines(publication_captions[["Table_S1"]], sub("\\.csv$", "_caption.txt", f))
  }
  if (all(file.exists(files))) {
    full <- utils::read.csv(files[2], check.names = FALSE)
    full$Table_section <- ifelse(full$Hypothesis %in% c("H1", "H2", "H3"), "Flowering phenology",
      ifelse(full$Hypothesis %in% c("H4", "H5", "H6"), "Maternal seed allocation", "Seed mass and germination")
    )
    full <- full[, c("Table_section", setdiff(names(full), "Table_section"))]
    for (f in files) utils::write.csv(full, f, row.names = FALSE)
    stopifnot(nrow(full) == 30, !any(grepl("^H7", full$Hypothesis)))
  }
  invisible(files)
}
publication_table_2 <- function() {
  files <- c(
    file.path(publication_root, "maternal_seed_mass_germination_responses", "tables", "Table_2.csv"),
    file.path(publication_root, "germination_elevation", "tables", "Table_2_H7_H8_model_results.csv")
  )
  for (f in files[file.exists(files)]) {
    t <- utils::read.csv(f, check.names = FALSE, colClasses = "character", na.strings = "NA")
    h7 <- grepl("^H7", t$Hypothesis)
    key <- tolower(gsub("[^A-Za-z0-9]", "", t$Response))
    mapping <- c(t0 = "H7a", timespread = "H7d", mgr = "H7b", gp = "H7c")
    t$Hypothesis[h7] <- unname(mapping[key[h7]])
    stopifnot(!anyNA(t$Hypothesis[h7]))
    spread <- h7 & key == "timespread"
    t$Expected_direction[spread] <- "not specified"
    t$Direction_consistent[spread] <- NA_character_
    utils::write.csv(t, f, row.names = FALSE, na = "NA")
    writeLines(publication_captions[["Table_2"]], sub("\\.csv$", "_caption.txt", f))
  }
  invisible(files)
}
publication_render_tables <- function() {
  fp <- file.path(publication_root, "flowering_phenology", "tables")
  msa <- file.path(publication_root, "maternal_seed_allocation", "tables")
  ge <- file.path(publication_root, "germination_elevation", "tables")
  files <- file.path(publication_root, c("maternal_seed_mass_germination_responses/tables/Table_2.csv", "germination_elevation/tables/Table_2_H7_H8_model_results.csv", "germination_elevation/tables/Table_S1_all_candidate_models.csv"))
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
# END INLINE PUBLICATION SUPPORT

# Confirmed protocol for every plant: 25/10 C (8/16 h), 60% eRH, 8 h light / 16 h dark.

# Constant conditions cannot estimate temperature or moisture responses.

# ============================================================================

# Script 09: MATERNAL SEED MASS AND GERMINATION RESPONSES (H8a-H8c)

# Implements manuscript Research Question 4. Run from the "arabis analysis"

# project root.

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

required <- c(
  "lme4", "performance", "reformulas", "readxl", "dplyr", "tidyr", "ggplot2",
  "germinationmetrics", "patchwork", "cowplot", "openxlsx"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})
if (!file.exists("arabis analysis.Rproj")) {
  stop("Run this script from the 'arabis analysis' project root.")
}
set.seed(12191)
input_dir_smg <- file.path("data", "raw", "Datasets")
germination_file <- file.path("data", "raw", "Datasets", "germination_counts.xlsx")
trait_file <- file.path(input_dir_smg, "trait_data.xlsx")
output_dir_smg <- file.path(
  "outputs",
  "maternal_seed_mass_germination_responses"
)
figure_dir_smg <- file.path(output_dir_smg, "figures")
table_dir_smg <- file.path(output_dir_smg, "tables")
model_dir_smg <- file.path(output_dir_smg, "models")
diagnostic_dir_smg <- file.path(output_dir_smg, "diagnostics")
invisible(lapply(c(figure_dir_smg, table_dir_smg, model_dir_smg, diagnostic_dir_smg), dir.create,
  recursive = TRUE, showWarnings = FALSE
))

# Standardise reported precision without reducing precision used in analyses.
write.csv <- function(x, file = "", ..., row.names = TRUE) {
  x <- as.data.frame(x)
  x[] <- Map(function(v, nm) {
    if (!is.numeric(v)) {
      return(v)
    }
    round(v, if (grepl("^SE($|_)|Estimate|CI_low|CI_high|CI_lower|CI_upper", nm)) 9 else 6)
  }, x, names(x))
  utils::write.csv(x, file = file, ..., row.names = row.names)
}
theme_manuscript <- function(base_size = 11) {
  theme_bw(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(family = "Arial", colour = "black"),
      axis.title = element_text(face = "bold", size = 13),
      axis.text = element_text(colour = "black", size = 12),
      plot.subtitle = element_text(face = "bold", hjust = 0, size = 13),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}
save_figure <- function(plot, stem, width = 150, height = 120) {
  ggsave(file.path(figure_dir_smg, paste0(stem, ".png")), plot,
    width = width, height = height, units = "mm", dpi = 600,
    bg = "white"
  )
  ggsave(file.path(figure_dir_smg, paste0(stem, ".pdf")), plot,
    width = width, height = height, units = "mm", device = cairo_pdf,
    bg = "white"
  )
}

# 4. Plant-level germination indices -----------------------------------------

# Each maternal plant contributes its own observed germination trajectory.

# 5. Germination count import and protocol validation ------------------------

# Cumulative counts: four dishes of 25 seeds (100 seeds per maternal plant).
read_germination_counts <- function(count_file, trait_file) {
  counts <- readxl::read_excel(count_file, sheet = "Sheet 1") |>
    dplyr::transmute(
      Day = as.numeric(Day), plant_no = as.integer(Plant_no),
      Replicate = as.integer(Replicate), Count = as.numeric(Germination_records)
    )
  plants <- readxl::read_excel(trait_file, sheet = "Sheet2") |>
    dplyr::select(plant_no, Block, Plot, Elevation, VegetationZone)
  if (anyNA(counts) || anyNA(plants) || anyDuplicated(plants$plant_no) ||
    anyDuplicated(counts[c("plant_no", "Replicate", "Day")]) ||
    any(counts$Count < 0 | counts$Count > 25 | counts$Count != floor(counts$Count))) {
    stop("Invalid plant IDs, duplicate records, missing values or counts outside 0-25 per dish.")
  }
  if (!setequal(counts$plant_no, plants$plant_no)) {
    stop("Germination and trait workbooks must contain the same plant IDs.")
  }
  checks <- counts |>
    dplyr::arrange(plant_no, Replicate, Day) |>
    dplyr::group_by(plant_no, Replicate) |>
    dplyr::summarise(Valid = identical(Day, as.numeric(0:34)) &&
      dplyr::first(Count) == 0 && all(diff(Count) >= 0), .groups = "drop")
  if (any(!checks$Valid) || !setequal(checks$Replicate, 1:4) || any(table(checks$plant_no) != 4L)) {
    stop("Expected four cumulative replicate series per plant, days 0-34, starting at zero.")
  }
  assignments <- plants |>
    dplyr::mutate(Source_plot_ID = paste(Block, Plot, Elevation, sep = "_"))
  plant_counts <- counts |>
    dplyr::group_by(plant_no, Day) |>
    dplyr::summarise(Cumulative_germination = sum(Count), .groups = "drop") |>
    dplyr::left_join(plants, by = "plant_no")
  # Sum only the four dishes belonging to the same maternal plant.
  trajectories <- plant_counts |>
    dplyr::select(plant_no, Day, Cumulative_germination) |>
    dplyr::filter(Day > 0)
  list(trajectories = trajectories, assignments = assignments)
}
# End of germination count import and protocol validation.

# 6. Plant-level germination model fitting and predictions -------------------

# Plant observations with block and source-plot random intercepts.
fit_plant_model <- function(formula, data, ...) {
  fixed <- reformulas::nobars(formula)
  f <- update(fixed, . ~ . + (1 | Block) + (1 | Source_plot_ID))
  lme4::lmer(f, data = data, REML = FALSE, ...)
}
# Nakagawa fixed-effect (marginal) or fixed-plus-random (conditional) R-squared.
plant_r2 <- function(model, conditional = FALSE) {
  fixed_variance <- var(as.numeric(lme4::getME(model, "X") %*% lme4::fixef(model)))
  random_variance <- sum(vapply(lme4::VarCorr(model), function(v) as.numeric(v[1, 1]), numeric(1)))
  total <- fixed_variance + random_variance + sigma(model)^2
  if (!is.finite(total) || total <= 0) {
    return(NA_real_)
  }
  (fixed_variance + if (conditional) random_variance else 0) / total
}
plant_prediction <- function(model, newdata, interval = "confidence", ...) {
  fixed <- reformulas::nobars(formula(model))
  design <- model.matrix(delete.response(terms(fixed)), newdata)
  beta <- lme4::fixef(model)
  design <- design[, names(beta), drop = FALSE]
  fit <- as.numeric(design %*% beta)
  se <- sqrt(rowSums((design %*% as.matrix(vcov(model))) * design))
  cbind(fit = fit, lwr = fit - qnorm(.975) * se, upr = fit + qnorm(.975) * se)
}
plant_diagnostics <- function(model) {
  x <- fitted(model)
  e <- resid(model)
  z <- as.numeric(scale(e))
  plot(x, z, xlab = "Fitted", ylab = "Standardized residual")
  abline(h = 0, lty = 2)
  qqnorm(z)
  qqline(z)
  hist(z, main = "Residual distribution", xlab = "Standardized residual")
  plot(x, sqrt(abs(z)), xlab = "Fitted", ylab = "Square root absolute residual")
}

# Fixed-effect Wald P-values use a normal approximation.
plant_coef_table <- function(model) {
  tab <- coef(summary(model))
  cbind(tab, `Pr(>|t|)` = 2 * pnorm(abs(tab[, "t value"]), lower.tail = FALSE))
}
# End of plant-level germination model fitting and predictions.

germination_input <- read_germination_counts(
  germination_file, file.path("data", "raw", "Datasets", "trait_data.xlsx")
)
plant_trajectories <- germination_input$trajectories
if (anyNA(plant_trajectories) || anyDuplicated(plant_trajectories[c("plant_no", "Day")])) {
  stop("Missing values or duplicate plant-day records in germination input.")
}
trajectory_checks <- plant_trajectories |>
  arrange(plant_no, Day) |>
  group_by(plant_no) |>
  summarise(
    N_days = n(), Nondecreasing = all(diff(Cumulative_germination) >= 0),
    Final_count = last(Cumulative_germination), .groups = "drop"
  )
if (nrow(trajectory_checks) != nrow(germination_input$assignments) || any(!trajectory_checks$Nondecreasing) ||
  any(trajectory_checks$Final_count > 100) || any(trajectory_checks$N_days != 34L)) {
  stop("Expected one valid trajectory per plant, 100 seeds per plant (four replicates of 25), Day 0 and 34 subsequent observation days.")
}
calculate_indices <- function(data) {
  data <- arrange(data, Day)
  cumulative <- data$Cumulative_germination
  days <- data$Day
  if (!any(cumulative > 0)) {
    return(tibble(
      t0 = NA_real_, tg = NA_real_, Time_spread = NA_real_,
      MGR = NA_real_, Final_germination = 0, GP = 0
    ))
  }
  tibble(
    t0 = germinationmetrics::FirstGermTime(cumulative, days, partial = FALSE),
    tg = germinationmetrics::LastGermTime(cumulative, days, partial = FALSE),
    Time_spread = germinationmetrics::TimeSpreadGerm(cumulative, days,
      partial = FALSE
    ),
    MGR = germinationmetrics::MeanGermRate(cumulative, days, partial = FALSE),
    Final_germination = max(cumulative),
    GP = 100 * max(cumulative) / 100
  )
}
indices <- plant_trajectories |>
  group_by(plant_no) |>
  group_modify(~ calculate_indices(.x)) |>
  ungroup()

# 7. Plant assignments and 100-seed mass -------------------------------------

elevation_plots <- unique(germination_input$assignments$Elevation)

# Each of the 27 measured plot elevations receives one weight.
elevation_plot_records <- dplyr::distinct(germination_input$assignments, Source_plot_ID, Elevation)
elevation_mean <- mean(elevation_plot_records$Elevation)
elevation_sd <- sd(elevation_plot_records$Elevation)
assignments <- germination_input$assignments |>
  transmute(
    plant_no = as.integer(plant_no), Source_plot_ID,
    Block = factor(Block), Plot = as.integer(Plot), Elevation = as.numeric(Elevation),
    Vegetation_zone = factor(VegetationZone),
    Temperature_light_C = 25, Temperature_dark_C = 10, ERH_percent = 60,
    Light_hours = 8, Dark_hours = 16
  )

# Directly measured mass (mg) of 100 seeds selected from each maternal plant

# for germination testing; model this directly measured 100-seed mass per mg.
plant_seed_mass <- read_excel(trait_file, sheet = "Sheet2") |>
  transmute(plant_no = as.integer(plant_no), Seed_mass_100_mg = as.numeric(`100_seed_mass`))
germ <- assignments |>
  left_join(indices, by = "plant_no") |>
  left_join(plant_seed_mass, by = "plant_no") |>
  mutate(
    Seed_mass_100_centered_mg = Seed_mass_100_mg -
      mean(Seed_mass_100_mg, na.rm = TRUE),
    Elevation_z = (Elevation - elevation_mean) / elevation_sd
  )
required_analysis_variables <- c(
  "t0", "Time_spread", "MGR", "Seed_mass_100_mg",
  "Seed_mass_100_centered_mg", "Elevation_z"
)
if (nrow(germ) != nrow(germination_input$assignments) || anyNA(germ[, required_analysis_variables])) {
  stop("Expected complete plant records after joining seed mass.")
}
write.csv(trajectory_checks,
  file.path(table_dir_smg, "Table_germination_trajectory_checks.csv"),
  row.names = FALSE
)
write.csv(germ,
  file.path(table_dir_smg, "Table_H8_plant_level_analysis_data.csv"),
  row.names = FALSE
)
write.csv(germ |> count(Temperature_light_C, Temperature_dark_C, ERH_percent, Light_hours, Dark_hours, name = "N_plants"),
  file.path(table_dir_smg, "Table_H8_shared_condition_sample_sizes.csv"),
  row.names = FALSE
)

# 8. H8a-H8c Gaussian linear models ------------------------------------------

model_specifications <- tibble(
  Hypothesis = c("H8a", "H8_spread", "H8b", "H8c"),
  Response = c("t0", "Time_spread", "MGR", "GP"),
  Expected_dir_smgection = c("negative", "not specified", "positive", "positive")
)
fit_h8_models <- function(response) {
  list(
    unadjusted = fit_plant_model(
      reformulate("Seed_mass_100_centered_mg", response = response),
      data = germ, na.action = na.fail
    ),
    elevation_adjusted = fit_plant_model(
      reformulate(c("Seed_mass_100_centered_mg", "Elevation_z"),
        response = response
      ),
      data = germ, na.action = na.fail
    )
  )
}
models <- setNames(
  lapply(model_specifications$Response, fit_h8_models),
  model_specifications$Hypothesis
)
extract_seed_mass <- function(model, hypothesis, response, model_name,
                              expected_dir_smgection) {
  term <- "Seed_mass_100_centered_mg"
  coefficient_table <- plant_coef_table(model)
  ci <- confint(model, parm = term, method = "Wald", level = .95)
  estimate <- unname(lme4::fixef(model)[term])
  p_value <- coefficient_table[term, "Pr(>|t|)"]
  direction_consistent <- if (expected_dir_smgection == "not specified") {
    NA
  } else if (expected_dir_smgection == "negative") {
    estimate < 0
  } else {
    estimate > 0
  }
  data.frame(
    Hypothesis = hypothesis, Response = response, Model = model_name,
    Term = term, Estimate_per_mg = estimate,
    SE = coefficient_table[term, "Std. Error"],
    CI_low = ci[1], CI_high = ci[2], P_value = p_value,
    Expected_dir_smgection = expected_dir_smgection,
    Direction_consistent = direction_consistent,
    Marginal_R2 = model_r2(model)[["Marginal"]], Conditional_R2 = model_r2(model)[["Conditional"]],
    AIC = AIC(model), N = nobs(model), Residual_df = df.residual(model),
    Singular = lme4::isSingular(model),
    Converged = length(setdiff(model@optinfo$conv$lme4$messages, "boundary (singular) fit: see help('isSingular')")) == 0L,
    stringsAsFactors = FALSE
  )
}
model_names <- c(
  unadjusted = "100-seed mass only",
  elevation_adjusted = "100-seed mass + elevation"
)
model_results <- bind_rows(lapply(seq_len(nrow(model_specifications)), function(i) {
  spec <- model_specifications[i, ]
  bind_rows(lapply(names(model_names), function(model_key) {
    extract_seed_mass(
      models[[spec$Hypothesis]][[model_key]], spec$Hypothesis,
      spec$Response, unname(model_names[model_key]), spec$Expected_dir_smgection
    )
  }))
}))
write.csv(model_results, file.path(table_dir_smg, "Table_H8_model_results.csv"),
  row.names = FALSE
)
saveRDS(models, file.path(model_dir_smg, "Models_H8_seed_mass_germination.rds"))

# 9. Residual diagnostics for seed-mass-only and elevation-adjusted models ----

save_lm_diagnostics <- function(model, stem) {
  png(file.path(diagnostic_dir_smg, paste0(stem, ".png")),
    width = 2200,
    height = 1900, res = 300, type = "cairo"
  )
  old <- par(
    mfrow = c(2, 2), family = "Arial",
    mar = c(4.2, 4.2, 2.0, 1.0)
  )
  on.exit(
    {
      par(old)
      dev.off()
    },
    add = TRUE
  )
  plant_diagnostics(model)
}
for (hypothesis in names(models)) {
  for (model_key in names(models[[hypothesis]])) {
    save_lm_diagnostics(
      models[[hypothesis]][[model_key]],
      paste0("Diagnostics_", hypothesis, "_", model_key)
    )
  }
}

# 10. Primary H8 association figures -----------------------------------------

annotation_data <- function(result, x_limits, y_limits,
                            side = c("right", "left")) {
  side <- match.arg(side)
  p_label <- if (result$P_value < .001) {
    "italic(P) < 0.001"
  } else {
    sprintf("italic(P) == %.3f", result$P_value)
  }
  data.frame(
    x = if (side == "left") {
      x_limits[1] + .04 * diff(x_limits)
    } else {
      x_limits[2] - .04 * diff(x_limits)
    },
    y = y_limits[2] - diff(y_limits) * c(.07, .15, .23, .31, .39),
    hjust = if (side == "left") 0 else 1,
    label = c(
      sprintf("beta == %.6f", result$Estimate_per_mg),
      sprintf("\"95%% CI: [%.6f, %.6f]\"", result$CI_low, result$CI_high),
      p_label,
      r2_plotmath(result$Marginal_R2, result$Conditional_R2)
    )
  )
}
association_panel <- function(response, hypothesis, subtitle, y_label,
                              annotation_side = "right") {
  model <- models[[hypothesis]]$unadjusted
  result <- filter(
    model_results, Hypothesis == hypothesis,
    Model == "100-seed mass only"
  )
  prediction_data <- data.frame(
    Seed_mass_100_mg = seq(min(germ$Seed_mass_100_mg),
      max(germ$Seed_mass_100_mg),
      length.out = 200
    )
  ) |>
    mutate(Seed_mass_100_centered_mg = Seed_mass_100_mg -
      mean(germ$Seed_mass_100_mg))
  prediction <- plant_prediction(model,
    newdata = prediction_data,
    interval = "confidence"
  )
  prediction_data <- bind_cols(prediction_data, as.data.frame(prediction))
  y_limits <- range(c(
    germ[[response]], prediction_data$lwr,
    prediction_data$upr
  ), finite = TRUE)
  annotations <- annotation_data(
    result, range(germ$Seed_mass_100_mg), y_limits, annotation_side
  )
  ggplot(germ, aes(x = Seed_mass_100_mg, y = .data[[response]])) +
    geom_ribbon(
      data = prediction_data,
      aes(x = Seed_mass_100_mg, ymin = lwr, ymax = upr),
      inherit.aes = FALSE, fill = "#E5E5E5"
    ) +
    geom_line(
      data = prediction_data,
      aes(x = Seed_mass_100_mg, y = fit), inherit.aes = FALSE,
      colour = "black", linewidth = .8,
      linetype = ifelse(result$P_value < .05, "solid", "dashed")
    ) +
    geom_point(
      shape = 21, fill = "#ECCBAE", colour = "black",
      size = 1.6, stroke = .25, alpha = .6
    ) +
    geom_text(
      data = annotations,
      aes(x = x, y = y, label = label, hjust = hjust),
      inherit.aes = FALSE, parse = TRUE, vjust = .5,
      family = "Arial", size = 4
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 5), expand = expansion(mult = .04)
    ) +
    labs(
      subtitle = subtitle, x = "100-seed mass (mg)",
      y = y_label
    ) +
    theme_manuscript() +
    theme(
      text = element_text(family = "Arial"),
      axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
      axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
      axis.title = element_text(colour = "black", size = 13, face = "bold"),
      plot.subtitle = element_text(colour = "black", size = 14, face = "bold")
    )
}
p_h8a <- association_panel(
  "t0", "H8a",
  "(e) Time to first germination",
  expression(bold("Time to first germination" ~ bolditalic(T)[0] ~ "(days)")),
  annotation_side = "right"
)
p_h8b <- association_panel(
  "Time_spread", "H8_spread",
  "(f) Germination time spread",
  expression(bold("Germination time spread" ~ (bolditalic(T)[g] - bolditalic(T)[0]) ~ "(days)")),
  annotation_side = "right"
)
p_h8c <- association_panel(
  "MGR", "H8b",
  "(g) Mean germination rate",
  expression(bold("Mean germination rate" ~ (day^{
    -1
  }))),
  annotation_side = "left"
)
p_h8d <- association_panel("GP", "H8c", "(h) Germination percentage",
  "Germination percentage (%)",
  annotation_side = "right"
)
save_figure(p_h8d, "Figure_H8c_seed_mass_GP")
y_title_spacing <- theme(
  axis.title.y = element_text(margin = margin(r = 12))
)
p_h8a <- p_h8a + y_title_spacing
p_h8b <- p_h8b + y_title_spacing
p_h8c <- p_h8c + y_title_spacing
save_figure(p_h8a, "Figure_H8a_seed_mass_first_germination")
save_figure(p_h8b, "Figure_H8b_seed_mass_time_spread")
save_figure(p_h8c, "Figure_H8c_seed_mass_MGR")

# 11. Comparison of primary and adjusted seed-mass coefficients --------------

significance_code <- function(p) {
  cut(p,
    breaks = c(-Inf, .001, .01, .05, Inf),
    labels = c("***", "**", "*", "ns"), right = FALSE
  ) |>
    as.character()
}
sensitivity_plot_data <- model_results |>
  mutate(
    Model = factor(Model, levels = unname(model_names)),
    Response_y = case_when(
      Response == "MGR" ~ 1,
      Response == "Time_spread" ~ 4,
      Response == "t0" ~ 7
    ),
    Point_y = Response_y + case_when(
      Model == "100-seed mass only" ~ .30,
      Model == "100-seed mass + elevation" ~ 0,
      TRUE ~ -.30
    ),
    Model_order = as.integer(Model),
    Significance = significance_code(P_value),
    Beta_annotation = sprintf("beta == %.6f", Estimate_per_mg),
    Fit_annotation = sprintf(
      "atop(R[m]^2 == \"%.3f\", R[c]^2 == \"%.3f\"~~'%s')",
      Marginal_R2, Conditional_R2, Significance
    ),

    # For T0 and time spread, place both annotation rows beneath the

    # lowest model point/error bar. MGR annotations remain level with points.
    Annotation_y = case_when(
      Response == "t0" ~ 6.30 - (Model_order - 1) * .42,
      Response == "Time_spread" ~ 3.30 - (Model_order - 1) * .42,
      TRUE ~ Point_y
    )
  )

# 12. H8 coefficient figure: layout and annotations --------------------------

# Reference panel-b layout using current plant-level H8 estimates.
build_seed_mass_sensitivity <- function(model_results) {
  plot_data <- model_results |>
    dplyr::mutate(
      Model_label = factor(
        dplyr::recode(as.character(Model),
          "100-seed mass only" = "Seed mass only",
          "100-seed mass + elevation" = "Seed mass + elevation"
        ),
        levels = c("Seed mass only", "Seed mass + elevation")
      ),
      Row_y = dplyr::case_when(
        Response == "t0" ~ 13,
        Response == "Time_spread" ~ 9, Response == "MGR" ~ 5,
        Response == "GP" ~ 1
      ),
      Model_order = as.integer(Model_label),
      Point_y = Row_y + ifelse(Model_order == 1, .20, -.20),
      Annotation_y = Row_y - .85 - (Model_order - 1) * .95,
      Significance = ifelse(P_value < .001, "***",
        ifelse(P_value < .01, "**", ifelse(P_value < .05, "*", "ns"))
      ),
      Beta_label = paste0("beta == ", ifelse(abs(Estimate_per_mg) < .0001,
        formatC(Estimate_per_mg, format = "e", digits = 2),
        formatC(Estimate_per_mg, format = "f", digits = 4)
      )),
      Fit_label = sprintf("atop(R[m]^2 == '%.3f', R[c]^2 == '%.3f'~~'%s')", Marginal_R2, Conditional_R2, Significance)
    )
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(Estimate_per_mg, Point_y, colour = Model_label, shape = Model_label)
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", colour = "grey60") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = CI_low, xmax = CI_high),
      orientation = "y", width = .14, linewidth = .45
    ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_text(ggplot2::aes(x = -.13, y = Annotation_y, label = Beta_label),
      hjust = 0, size = 3.3, family = "Arial", parse = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_text(ggplot2::aes(x = .20, y = Annotation_y, label = Fit_label),
      hjust = 1, size = 3.3, family = "Arial", parse = TRUE, show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c(
      "Seed mass only" = "#02401B",
      "Seed mass + elevation" = "#D55E00"
    )) +
    ggplot2::scale_shape_manual(values = c(
      "Seed mass only" = 16,
      "Seed mass + elevation" = 18
    )) +
    ggplot2::scale_x_continuous(
      limits = c(-.14, .21),
      breaks = seq(-.10, .20, .05), expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(13, 9, 5, 1),
      labels = expression(italic(T)[0], italic(T)[g] - italic(T)[0], MGR, GP), limits = c(-1.3, 13.65),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::labs(
      subtitle = "(b) Germination ~ seed mass",
      x = "Seed-mass coefficient", y = NULL, colour = NULL, shape = NULL
    ) +
    ggplot2::theme_bw(base_size = 11, base_family = "Arial") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey92", linewidth = .3),
      panel.border = ggplot2::element_rect(colour = "black", linewidth = .6),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.text.y = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "bottom", legend.direction = "vertical",
      legend.justification = "left", legend.text = ggplot2::element_text(size = 10),
      legend.key.height = grid::unit(.4, "cm")
    )
}
# End of h8 coefficient figure: layout and annotations.

p_h8_sensitivity <- build_seed_mass_sensitivity(model_results)
save_figure(
  p_h8_sensitivity, "Figure_H8_seed_mass_model_sensitivity",
  150, 120
)
message("H8a-H8c maternal seed-mass analysis complete. Outputs: ", output_dir_smg)

saveRDS(list(p_h8a = p_h8a, p_h8b = p_h8b, p_h8c = p_h8c, p_h8d = p_h8d, p_h8_sensitivity = p_h8_sensitivity), file.path(model_dir_smg, "Figure_objects_H8.rds"))

# Complete germination reporting after script 08 and the H8 models above.
# This section produces the shared H7/H8 figures and tables, H8 bootstraps,
# correlations and supplementary exports without a separate combined script.
local({
  # Retain full precision for these exports, independently of the H8 display rounding.
  write.csv <- utils::write.csv
  if (!file.exists("outputs/germination_elevation/models/Figure_objects_H7.rds")) {
    stop("Run script 08 before script 09 to generate the H7 model and figure inputs.")
  }
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(lme4)
  })
  cowplot::set_null_device("cairo")
  # Definitions above are reused in this section.
  # Definitions above are reused in this section.
  # Publication tables: formatting, R-squared notation and exports -------------------------------
  # Publication-ready gt tables ---------------------------------------------
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
        "% change per 1-mg seed-mass decrease"
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

  output_dir <- file.path("outputs", "germination_elevation")
  table_dir <- file.path(output_dir, "tables")
  figure_dir <- file.path(output_dir, "figures")
  mass_dir <- file.path("outputs", "maternal_seed_mass_germination_responses")
  germ <- read.csv(file.path(table_dir, "Table_plant_level_germination_indices.csv"))
  germ$Block <- factor(germ$Block)
  germ$Source_plot_ID <- factor(germ$Source_plot_ID)
  for (group in c("H7", "H8")) {
    folder <- if (group == "H7") output_dir else mass_dir
    plots <- readRDS(file.path(folder, "models", paste0("Figure_objects_", group, ".rds")))
    for (nm in names(plots)) assign(nm, plots[[nm]])
  }
  association <- patchwork::wrap_plots(lapply(list(p_h7a, p_h7b, p_h7c, p_h7d, p_h8a, p_h8b, p_h8c, p_h8d), function(p) p + labs(caption = NULL)), ncol = 4, nrow = 2)
  ggsave(file.path(figure_dir, "Figure_H7_H8_germination_elevation_seed_mass.png"), association, width = 400, height = 220, units = "mm", dpi = 300, bg = "white")
  # Merge Figure_H7c_sensitivity (a) and Figure_H8_seed_mass_model_sensitivity (b).
  # Combined H7-H8 coefficient figure: layout and annotations -------------------------------
  # Shared response ordering and typography for the combined H7/H8 figure.
  build_combined_sensitivity <- function(h7_results, h8_results) {
    legend_labels <- c("Elevation only", "Seed mass only", "Seed mass + elevation")
    legend_colours <- c(
      "Elevation only" = "#2B8CBE", "Seed mass only" = "#02401B",
      "Seed mass + elevation" = "#D55E00"
    )
    legend_shapes <- c(
      "Elevation only" = 21, "Seed mass only" = 22,
      "Seed mass + elevation" = 23
    )
    point_size <- 4.5
    shared_guides <- function() {
      ggplot2::guides(
        colour = ggplot2::guide_legend(nrow = 1), shape = ggplot2::guide_legend(nrow = 1),
        fill = ggplot2::guide_legend(nrow = 1)
      )
    }
    row_position <- function(response) {
      unname(c(t0 = 37, Time_spread = 25, MGR = 13, GP = 1)[response])
    }
    annotate <- function(beta, rm, rc, p) {
      significance <- ifelse(p < .001, "***", ifelse(p < .01, "**",
        ifelse(p < .05, "*", "ns")
      ))
      number <- vapply(beta, function(value) {
        if (is.finite(value) && value != 0 && abs(value) < .0001) {
          parts <- strsplit(formatC(value, format = "e", digits = 2), "e", fixed = TRUE)[[1]]
          paste0(parts[1], " %*% 10^{", as.integer(parts[2]), "}")
        } else {
          formatC(value, format = "f", digits = 4)
        }
      }, character(1))
      sprintf("beta == %s*','~~R[m]^2 == '%.3f'*','~~R[c]^2 == '%.3f'~~'%s'", number, rm, rc, significance)
    }
    shared_theme <- ggplot2::theme_bw(base_size = 11, base_family = "Arial") +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(colour = "grey92", linewidth = .3),
        panel.border = ggplot2::element_rect(colour = "black", linewidth = .6),
        axis.text = ggplot2::element_text(size = 12, colour = "black"),
        axis.title = ggplot2::element_text(size = 13, face = "bold"),
        strip.background = ggplot2::element_rect(fill = "grey85", colour = "black", linewidth = .6),
        strip.text = ggplot2::element_text(size = 13, face = "bold", hjust = .5),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.justification = "center", legend.text = ggplot2::element_text(size = 14),
        legend.key.height = grid::unit(.5, "cm")
      )
    y_scale <- function() {
      ggplot2::scale_y_continuous(
        breaks = c(37, 25, 13, 1), labels = expression(bold(bolditalic(T)[0]), bold(bolditalic(T)[g] - bolditalic(T)[0]), bold(MGR), bold(GP)),
        limits = c(-10, 40), expand = ggplot2::expansion(mult = 0)
      )
    }
    h7 <- h7_results
    h7$Row_y <- row_position(h7$Response)
    h7$Label_y <- h7$Row_y - 3.5
    h7_p <- suppressWarnings(as.numeric(h7$P))
    h7_p[grepl("<", h7$P, fixed = TRUE)] <- .0005
    h7$Label <- annotate(h7$Estimate_per_100_m, h7$Marginal_R2, h7$Conditional_R2, h7_p)
    h7$Model_label <- "Elevation only"
    h7$Panel_title <- "(a) Germination ~ elevation"
    a <- ggplot2::ggplot(h7, ggplot2::aes(Estimate_per_100_m, Row_y,
      colour = Model_label, shape = Model_label
    )) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = CI_low_per_100_m, xmax = CI_high_per_100_m),
        orientation = "y", width = .14, linewidth = .7, show.legend = TRUE
      ) +
      ggplot2::geom_point(ggplot2::aes(fill = Model_label),
        size = point_size,
        stroke = .8, show.legend = TRUE
      ) +
      ggplot2::geom_text(ggplot2::aes(y = Label_y, label = Label),
        parse = TRUE, hjust = .5, size = 4.6, family = "Arial", colour = "black", show.legend = FALSE
      ) +
      ggplot2::scale_colour_manual(values = legend_colours, limits = legend_labels, drop = FALSE) +
      ggplot2::scale_fill_manual(values = legend_colours, limits = legend_labels, drop = FALSE) +
      ggplot2::scale_shape_manual(values = legend_shapes, limits = legend_labels, drop = FALSE) +
      shared_guides() +
      ggplot2::scale_x_continuous(limits = c(-10, 5.5), breaks = seq(-8, 4, 2)) +
      y_scale() +
      ggplot2::facet_wrap(~Panel_title, nrow = 1) +
      ggplot2::labs(
        x = "Elevation coefficient\n", y = NULL, colour = NULL, shape = NULL, fill = NULL
      ) +
      shared_theme
    h8 <- h8_results
    h8$Model_label <- factor(
      ifelse(h8$Model == "100-seed mass only",
        "Seed mass only", "Seed mass + elevation"
      ),
      levels = c("Seed mass only", "Seed mass + elevation")
    )
    h8$Row_y <- row_position(h8$Response)
    h8$Point_y <- h8$Row_y + ifelse(as.integer(h8$Model_label) == 1, .8, -.8)
    h8$Label_y <- h8$Row_y - 3.8 - (as.integer(h8$Model_label) - 1) * 4.0
    h8$Label <- annotate(h8$Estimate_per_mg, h8$Marginal_R2, h8$Conditional_R2, h8$P_value)
    h8$Panel_title <- "(b) Germination ~ seed mass"
    b <- ggplot2::ggplot(h8, ggplot2::aes(Estimate_per_mg, Point_y,
      colour = Model_label, shape = Model_label
    )) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = CI_low, xmax = CI_high),
        orientation = "y", width = .14, linewidth = .7, show.legend = TRUE
      ) +
      ggplot2::geom_point(ggplot2::aes(fill = Model_label),
        size = point_size,
        stroke = .8, show.legend = TRUE
      ) +
      ggplot2::geom_text(ggplot2::aes(y = Label_y, label = Label),
        parse = TRUE, hjust = .5, size = 4.6, family = "Arial", show.legend = FALSE
      ) +
      ggplot2::scale_colour_manual(values = legend_colours, limits = legend_labels, drop = FALSE) +
      ggplot2::scale_fill_manual(values = legend_colours, limits = legend_labels, drop = FALSE) +
      ggplot2::scale_shape_manual(values = legend_shapes, limits = legend_labels, drop = FALSE) +
      shared_guides() +
      ggplot2::scale_x_continuous(
        limits = c(-.15, .20),
        breaks = round(seq(-.15, .20, .05), 2), labels = function(x) sprintf("%.2f", x),
        expand = ggplot2::expansion(mult = 0)
      ) +
      y_scale() +
      ggplot2::facet_wrap(~Panel_title, nrow = 1) +
      ggplot2::labs(
        x = "Seed-mass coefficient\n", y = NULL, colour = NULL, shape = NULL, fill = NULL
      ) +
      shared_theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
    patchwork::wrap_plots(a, b, ncol = 2, widths = c(1, 1), guides = "collect") &
      ggplot2::theme(
        legend.position = "bottom", legend.direction = "horizontal",
        legend.justification = "center"
      )
  }
  # End of combined h7-h8 coefficient figure: layout and annotations.

  sensitivity <- build_combined_sensitivity(
    read.csv(file.path(table_dir, "Table_H7_model_results.csv")),
    read.csv(file.path(mass_dir, "tables", "Table_H8_model_results.csv"))
  )
  ggsave(file.path(mass_dir, "figures", "Figure_H7_H8_sensitivity.png"), sensitivity,
    width = 300, height = 150, units = "mm", dpi = 600, bg = "white"
  )

  ggsave(file.path(mass_dir, "figures", "Figure_H7_H8_sensitivity.pdf"), sensitivity,
    width = 300, height = 150, units = "mm", device = cairo_pdf, bg = "white"
  )

  h7_model_results_file <- file.path(table_dir, "Table_H7_model_results.csv")
  h8_model_results_file <- file.path(mass_dir, "tables", "Table_H8_model_results.csv")
  h7_model_results <- read.csv(h7_model_results_file, check.names = FALSE) |>
    transmute(
      Research_question = "H7: Elevation and germination",
      Hypothesis, Response, Model,
      Effect_scale = "Per 100-m elevation increase",
      Estimate = Estimate_per_100_m,
      SE = SE_per_100_m,
      CI_low = CI_low_per_100_m,
      CI_high = CI_high_per_100_m,
      P_value = as.character(P),
      Expected_direction = Expected_dir_geection,
      Direction_consistent, Marginal_R2, Conditional_R2,
      AIC, N, Residual_df
    )
  h8_model_results <- read.csv(h8_model_results_file, check.names = FALSE) |>
    transmute(
      Research_question = "H8: 100-seed mass and germination",
      Hypothesis, Response, Model,
      Effect_scale = "Per 1-mg increase in 100-seed mass",
      Estimate = Estimate_per_mg,
      SE, CI_low, CI_high, P_value = as.character(P_value),
      Expected_direction = Expected_dir_smgection,
      Direction_consistent, Marginal_R2, Conditional_R2,
      AIC, N, Residual_df
    )
  Table_2 <- bind_rows(h7_model_results, h8_model_results) |>
    mutate(P = ifelse(grepl("<", P_value, fixed = TRUE), "< 0.001", sprintf("%.3f", suppressWarnings(as.numeric(P_value))))) |>
    select(-P_value)
  write.csv(Table_2, file.path(table_dir, "Table_2_H7_H8_model_results.csv"), row.names = FALSE)
  openxlsx::write.xlsx(list("Table 2" = Table_2), file.path(table_dir, "Table_2_H7_H8_model_results.xlsx"), overwrite = TRUE)
  # Definitions above are reused in this section.
  # Table 2: combine H7 and H8 model results -------------------------------
  # Table 2 uses the schema and publication styling of maternal allocation Table 1.
  build_combined_germination_table <- function(h7_file, h8_file, output_dir) {
    h7 <- utils::read.csv(h7_file, check.names = FALSE)
    h8 <- utils::read.csv(h8_file, check.names = FALSE)
    elevation <- dplyr::transmute(h7,
      Table_section = "Elevation and germination", Hypothesis, Response, Model, Term,
      Estimate = Estimate_per_100_m, SE = SE_per_100_m,
      CI_low = CI_low_per_100_m, CI_high = CI_high_per_100_m, P,
      Coefficient_unit = "per 100-m elevation increase",
      Marginal_R2, Conditional_R2, N, Singular, Converged, AIC, Residual_df,
      Expected_direction = Expected_dir_geection, Direction_consistent
    )
    mass <- dplyr::transmute(h8,
      Table_section = "Maternal seed mass and germination", Hypothesis, Response, Model, Term,
      Estimate = Estimate_per_mg, SE, CI_low, CI_high,
      P = ifelse(P_value < 0.001, "< 0.001", sprintf("%.3f", P_value)),
      Coefficient_unit = "per 1-mg increase in measured 100-seed mass",
      Marginal_R2, Conditional_R2, N, Singular, Converged, AIC, Residual_df,
      Expected_direction = Expected_dir_smgection, Direction_consistent
    )
    results <- dplyr::bind_rows(elevation, mass)
    # Follow the figure order: T0, Tg-T0, MGR, GP; pair H8 models by response.
    results <- results[order(
      match(
        results$Table_section,
        c("Elevation and germination", "Maternal seed mass and germination")
      ),
      match(results$Response, c("t0", "Time_spread", "MGR", "GP")),
      match(results$Model, c(
        "Elevation only", "100-seed mass only",
        "100-seed mass + elevation"
      ))
    ), ]
    stopifnot(
      nrow(results) == nrow(h7) + nrow(h8),
      !anyDuplicated(results[c("Hypothesis", "Response", "Model")]),
      all(results$CI_low <= results$Estimate),
      all(results$Estimate <= results$CI_high)
    )
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    export_results <- results
    decimal_columns <- names(results)[vapply(results, is.numeric, logical(1))]
    decimal_columns <- setdiff(decimal_columns, c("N", "Residual_df"))
    export_results[decimal_columns] <- lapply(results[decimal_columns], function(x) {
      ifelse(is.na(x), NA_character_, sprintf("%.3f", round(x, 3)))
    })
    effect_columns <- intersect(c("Estimate", "SE", "CI_low", "CI_high"), names(results))
    export_results[effect_columns] <- lapply(results[effect_columns], format_table_2_effect)
    utils::write.csv(export_results, file.path(output_dir, "Table_2.csv"), row.names = FALSE)
    table <- build_publication_gt(results, "2", "Germination responses") |>
      gt::tab_header(
        title = gt::md("**Table 2. Elevation, maternal seed mass and germination responses**"),
        subtitle = "H7 elevation models and H8 seed-mass-only and elevation-adjusted models"
      ) |>
      gt::fmt(columns = c(Estimate, SE, CI_low, CI_high), fns = format_table_2_effect) |>
      gt::tab_source_note(gt::md(paste0(
        "T0: time to first germination (days); Time spread: Tg - T0 (days); ",
        "MGR: mean germination rate (per day); GP: germination percentage (percentage points). ",
        "H8_spread is exploratory. Marginal and conditional R-squared describe fixed effects and all effects, respectively. ",
        "Singular flags are retained from the source models."
      )))
    gt_dir <- file.path(output_dir, "gt")
    dir.create(gt_dir, showWarnings = FALSE)
    gt::gtsave(table, file.path(gt_dir, "Table_2.html"), inline_css = TRUE)
    save_publication_table(table, file.path(gt_dir, "Table_2.rtf"))
    message("Table 2: ", nrow(results), " model rows saved in ", output_dir)
    invisible(results)
  }
  # End of table 2: combine h7 and h8 model results.

  build_combined_germination_table(
    h7_model_results_file, h8_model_results_file, table_dir_smg
  )
  combined_germ_table_dir <- file.path("outputs", "germination_elevation", "tables")
  dir.create(combined_germ_table_dir, recursive = TRUE, showWarnings = FALSE)
  h8_models <- readRDS(file.path(
    "outputs", "maternal_seed_mass_germination_responses",
    "models", "Models_H8_seed_mass_germination.rds"
  ))
  h7_primary <- readRDS(file.path(
    "outputs", "germination_elevation",
    "models", "Models_H7_germination_elevation.rds"
  ))
  h7_models <- lapply(h7_primary, function(set) {
    fit <- set$primary
    list(
      null = fit_plant_model(as.formula(paste(names(model.frame(fit))[1], "~ 1")), germ),
      elevation = fit
    )
  })
  coefficient_rows <- list()
  candidate_rows <- list()
  for (family in list(h7_models, h8_models)) {
    for (h in names(family)) {
      for (key in names(family[[h]])) {
        fit <- family[[h]][[key]]
        predictor <- names(model.frame(fit))[2]
        has_predictor <- predictor %in% rownames(plant_coef_table(fit))
        cf <- if (has_predictor) plant_coef_table(fit)[predictor, ] else rep(NA_real_, 4)
        ci <- if (has_predictor) confint(fit, parm = predictor, method = "Wald") else c(NA_real_, NA_real_)
        report_scale <- if (grepl("^H7", h)) 100 / sd(distinct(germ, Source_plot_ID, Elevation)$Elevation) else 1
        cf[1:2] <- cf[1:2] * report_scale
        ci <- ci * report_scale
        coefficient_rows[[length(coefficient_rows) + 1L]] <- data.frame(
          Hypothesis = h, Model = key, Term = if (grepl("^H7", h)) "Elevation (per 100 m)" else "100-seed mass (per mg)", Estimate = cf[1], SE = cf[2],
          CI_low = ci[1], CI_high = ci[2], P_value = if (has_predictor) cf["Pr(>|t|)"] else NA_real_,
          Marginal_R2 = model_r2(fit)[["Marginal"]], Conditional_R2 = model_r2(fit)[["Conditional"]], N = nobs(fit), N_plots = dplyr::n_distinct(model.frame(fit)$Source_plot_ID),
          Full_rank = is.null(attr(lme4::getME(fit, "X"), "col.dropped"))
        )
        ll <- logLik(fit)
        k <- attr(ll, "df")
        n <- nobs(fit)
        candidate_rows[[length(candidate_rows) + 1L]] <- data.frame(
          Hypothesis = h,
          Response = names(model.frame(fit))[1], Model = key, N_observations = n,
          N_plots = dplyr::n_distinct(model.frame(fit)$Source_plot_ID), K = k, logLik = as.numeric(ll), AIC = AIC(fit),
          AICc = AIC(fit) + 2 * k * (k + 1) / (n - k - 1), Singular = lme4::isSingular(fit), Converged = length(setdiff(fit@optinfo$conv$lme4$messages, "boundary (singular) fit: see help('isSingular')")) == 0L
        )
      }
    }
  }
  coefficients <- bind_rows(coefficient_rows)
  candidates <- bind_rows(candidate_rows) |>
    group_by(Hypothesis) |>
    mutate(
      Delta_AICc = AICc - min(AICc),
      Akaike_weight = exp(-.5 * Delta_AICc) / sum(exp(-.5 * Delta_AICc)),
      Supported = Delta_AICc <= 2, Rank = rank(AICc)
    ) |>
    ungroup()
  coefficients <- filter(coefficients, !is.na(Estimate))
  utils::write.csv(coefficients, file.path(combined_germ_table_dir, "H7_H8_coefficients.csv"), row.names = FALSE)
  utils::write.csv(candidates, file.path(combined_germ_table_dir, "H7_H8_candidate_models.csv"), row.names = FALSE)
  utils::write.csv(filter(coefficients, grepl("^H8", Hypothesis)),
    file.path(file.path("outputs", "maternal_seed_mass_germination_responses", "tables"), "H8_coefficients.csv"),
    row.names = FALSE
  )
  utils::write.csv(filter(candidates, grepl("^H8", Hypothesis)),
    file.path(file.path("outputs", "maternal_seed_mass_germination_responses", "tables"), "H8_candidate_models.csv"),
    row.names = FALSE
  )

  # Draws must correspond to the current models and the directly measured 100-seed mass.
  bootstrap_file <- file.path(mass_dir, "tables", "H8_bootstrap_draws.csv")
  cache_signature <- lapply(h8_models, function(set) list(frame = model.frame(set$elevation_adjusted), beta = fixef(set$elevation_adjusted)))
  cache_signature_file <- file.path(mass_dir, "models", "H8_bootstrap_signature.rds")
  verified_cache <- file.exists(cache_signature_file) && file.exists(bootstrap_file) && identical(readRDS(cache_signature_file), cache_signature)
  if (verified_cache) {
    message("H8: reusing 1,000 bootstrap draws after verifying the current models")
    bootstrap_data <- read.csv(bootstrap_file)
  } else {
    set.seed(61006)
    rows <- list()
    for (j in seq_along(h8_models)) {
      fit <- h8_models[[j]]$elevation_adjusted
      predictor <- names(model.frame(fit))[2]
      draws <- bootMer(fit,
        FUN = function(m) fixef(m)[predictor], nsim = 1000,
        type = "parametric", use.u = FALSE
      )$t[, 1]
      rows[[j]] <- data.frame(
        Panel = paste0(
          "(", letters[j], ") 100-seed mass: ",
          c("first germination", "germination spread", "mean rate", "percentage")[j]
        ),
        Estimate = draws, Observed = unname(fixef(fit)[predictor])
      )
    }
    bootstrap_data <- bind_rows(rows)
    write.csv(bootstrap_data, bootstrap_file, row.names = FALSE)
    saveRDS(cache_signature, cache_signature_file)
  }
  annotation <- bootstrap_data |>
    group_by(Panel) |>
    summarise(Observed = first(Observed), .groups = "drop")
  p_bootstrap <- ggplot(bootstrap_data, aes(Estimate)) +
    geom_histogram(bins = 35, fill = "#A9B6C8", colour = "white") +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_vline(data = annotation, aes(xintercept = Observed), colour = "#963B35") +
    facet_wrap(~Panel, scales = "free", ncol = 4) +
    labs(x = "Elevation-adjusted coefficient per 1 mg", y = "Bootstrap frequency") +
    theme_bw(base_size = 12, base_family = "Arial") +
    theme(strip.background = element_blank(), strip.text = element_text(face = "bold"))
  ggsave(file.path(mass_dir, "figures", "Figure_seed_mass_bootstrap.png"), p_bootstrap, width = 270, height = 95, units = "mm", dpi = 300)

  # Refresh the cross-stage overview from the current primary analyses.
  summary_dir <- file.path("outputs", "summary_all_associations")
  summary_file <- file.path(summary_dir, "tables", "Table_all_primary_associations.csv")
  if (file.exists(summary_file)) {
    flowering <- utils::read.csv(file.path("outputs", "flowering_phenology", "tables", "Table_H1_H3_primary_coefficients.csv"))
    allocation <- utils::read.csv(file.path("outputs", "maternal_seed_allocation", "tables", "Table_H4_H6_primary_coefficients.csv"))
    germ_overview <- utils::read.csv(file.path(combined_germ_table_dir, "H7_H8_coefficients.csv")) |>
      filter(Model %in% c("elevation", "unadjusted"))
    overview <- bind_rows(
      mutate(flowering, Stage = "Flowering"),
      mutate(allocation, Stage = "Seed allocation"), mutate(germ_overview, Stage = "Germination")
    ) |>
      mutate(
        Response = ifelse(Hypothesis == "H3", "Final flowering date", NA_character_),
        Null_value = ifelse(Hypothesis == "H3", 1, 0),
        P_zero = P_value,
        P_value = ifelse(Hypothesis == "H3", 2 * pnorm(-abs((Estimate - 1) / SE)), P_value)
      ) |>
      select(Hypothesis, Stage, Response, Estimate, SE, CI_low, CI_high, Null_value, P_zero, P_value)
    overview <- overview |> mutate(
      Evidence_z = (Estimate - Null_value) / SE, Display_z = pmax(-12, pmin(12, Evidence_z)),
      Significance = case_when(P_value < .001 ~ "***", P_value < .01 ~ "**", P_value < .05 ~ "*", TRUE ~ "ns"),
      Support = ifelse((CI_low - Null_value) * (CI_high - Null_value) > 0, "Interval excludes zero", "Uncertain"),
      Fontface = ifelse((CI_low - Null_value) * (CI_high - Null_value) > 0, "bold", "plain"), Direction = ifelse(Estimate > 0, "positive", "negative"),
      Result = sprintf("%.6g [%.6g, %.6g] %s", Estimate, CI_low, CI_high, Significance)
    )
    utils::write.csv(overview, summary_file, row.names = FALSE)
    p_overview <- ggplot(overview, aes(x = Evidence_z, y = factor(Hypothesis, levels = rev(Hypothesis)), colour = Stage)) +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
      geom_point(size = 3) +
      labs(
        x = "Coefficient minus hypothesis null, divided by SE", y = "Hypothesis",
        caption = "Coefficients have different units. This overview does not test reproductive coordination."
      ) +
      theme_minimal(base_size = 12, base_family = "Arial") +
      theme(panel.grid.minor = element_blank())
    for (ext in c("png", "pdf", "tiff")) {
      ggsave(file.path(summary_dir, "figures", paste0("Figure_12_summary_all_associations.", ext)),
        p_overview,
        width = 220, height = 170, units = "mm", dpi = 300, device = if (ext == "pdf") grDevices::cairo_pdf else NULL
      )
    }
    writeLines(
      "Figure 12. Descriptive overview of primary association estimates across reproductive stages. Points show each coefficient divided by its standard error; zero indicates the hypothesis null (one for H3 final-date slopes, zero otherwise). The underlying coefficients and confidence intervals are supplied in Table_all_primary_associations.csv. All germination plants experienced 25/10\u00b0C (8/16 h), 60% eRH and 8-hour light/16-hour darkness. This overview does not formally test reproductive coordination or differences among coefficients.",
      file.path(summary_dir, "Figure_12_caption.txt")
    )
  }

  # Table S1: precisely the candidate sets specified in the revised methods.
  flowering_sets <- read.csv(file.path("outputs", "flowering_phenology", "tables", "Table_H1_H3_AICc_model_comparison.csv"))
  allocation_sets <- read.csv(file.path("outputs", "maternal_seed_allocation", "tables", "Table_H4_H6_AICc_model_comparison.csv"))
  current_candidates <- bind_rows(
    flowering_sets, allocation_sets,
    candidates |> rename(N = N_observations)
  )
  current_candidates <- current_candidates |> filter(!grepl("^H7", Hypothesis))
  write.csv(format_candidate_table_export(current_candidates),
    file.path(table_dir, "Table_S1_all_candidate_models.csv"),
    row.names = FALSE
  )

  # Refresh exports after all coefficient and bootstrap tables have been updated.
  # Table S2: H7 results and H8 candidate-model comparisons -------------------------------
  build_germination_table_s2 <- function(h7_file, h8_file, output_dir) {
    h7 <- utils::read.csv(h7_file, check.names = FALSE)
    h8 <- utils::read.csv(h8_file, check.names = FALSE)
    combined <- dplyr::bind_rows(
      dplyr::transmute(h7,
        Table_section = "H7: elevation model coefficients",
        Hypothesis, Response, Model, Term, N, Estimate = Estimate_per_100_m,
        SE = SE_per_100_m, CI_low = CI_low_per_100_m, CI_high = CI_high_per_100_m,
        P, Coefficient_unit = "per 100-m elevation increase", Marginal_R2, Conditional_R2,
        AIC, Residual_df, Singular, Converged,
        Expected_direction = Expected_dir_geection, Direction_consistent
      ),
      h8 |> dplyr::rename(N = N_observations) |>
        dplyr::mutate(Table_section = "H8: seed-mass candidate-model comparisons")
    ) |>
      dplyr::arrange(Table_section, match(Response, c("t0", "Time_spread", "MGR", "GP")), Rank) |>
      dplyr::select(Table_section, Hypothesis, Response, Model, dplyr::everything())
    stopifnot(
      nrow(combined) == nrow(h7) + nrow(h8),
      !anyDuplicated(combined[c("Table_section", "Hypothesis", "Response", "Model")])
    )
    formatted <- format_candidate_table_export(combined)
    effects <- c("Estimate", "SE", "CI_low", "CI_high")
    formatted[effects] <- lapply(combined[effects], format_table_2_effect)
    utils::write.csv(formatted, file.path(output_dir, "Table_S2.csv"), row.names = FALSE)
    table <- build_publication_gt(combined, "S2", "H7 model results and H8 candidate models") |>
      gt::tab_source_note(gt::md(paste0(
        "H7 coefficients are per 100-m elevation increase. ",
        "H8 rows report candidate-model fit statistics, not coefficient estimates. ",
        "Dashes indicate statistics absent from the corresponding source table."
      )))
    gt_dir <- file.path(output_dir, "gt")
    dir.create(gt_dir, showWarnings = FALSE)
    gt::gtsave(table, file.path(gt_dir, "Table_S2.html"), inline_css = TRUE)
    save_publication_table(table, file.path(gt_dir, "Table_S2.rtf"))
    message("Table S2 saved: ", nrow(h7), " H7 rows and ", nrow(h8), " H8 rows.")
    invisible(combined)
  }
  # End of table s2: h7 results and h8 candidate-model comparisons.

  build_germination_table_s2(
    h7_model_results_file,
    file.path(table_dir_smg, "H8_candidate_models.csv"),
    table_dir_smg
  )
  # Align the final Table 2 labels before writing any germination workbooks.

  publication_table_2()
  openxlsx::write.xlsx(list("Table 2" = utils::read.csv(file.path(table_dir, "Table_2_H7_H8_model_results.csv"), check.names = FALSE)),
    file.path(table_dir, "Table_2_H7_H8_model_results.xlsx"),
    overwrite = TRUE
  )
  for (analysis_folder in c("germination_elevation", "maternal_seed_mass_germination_responses")) {
    refreshed_dir <- file.path("outputs", analysis_folder, "tables")
    refreshed_csv <- list.files(refreshed_dir, pattern = "\\.csv$", full.names = TRUE)
    refreshed_names <- make.unique(substr(tools::file_path_sans_ext(basename(refreshed_csv)), 1, 27))
    workbook_name <- switch(analysis_folder,
      germination_elevation = "summary_tables_H7_germination_elevation.xlsx",
      maternal_seed_mass_germination_responses = "summary_tables_H8_seed_mass_germination.xlsx"
    )
    openxlsx::write.xlsx(setNames(lapply(refreshed_csv, read.csv, check.names = FALSE), refreshed_names),
      file.path(refreshed_dir, workbook_name),
      overwrite = TRUE
    )
    render_gt_tables(refreshed_dir, "Plant-level germination analyses; directly measured 100-seed mass for H8")
  }
})

# 13. Synchronise publication tables and numbered figures --------------------

# Keep Table S1 response scales, candidate sets and numbered figures aligned.

publication_table_2()
publication_candidates()
publication_render_tables()
