# Confirmed protocol for every plant: 25/10 C (8/16 h), 60% eRH, 8 h light / 16 h dark.

# Constant conditions cannot estimate temperature or moisture responses.

# ============================================================================

# Script 08: GERMINATION TIMING AND SPEED ACROSS ELEVATION (H7a-H7c)

# Implements manuscript Research Question 3. Run from the "arabis analysis"

# project root.


# ============================================================================

# 1. Marginal and conditional R-squared: calculation and figure notation -----

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

# 2. Packages and output directories -----------------------------------------

required <- c(
  "lme4", "performance", "reformulas", "readxl", "dplyr", "tidyr", "ggplot2", "germinationmetrics",
  "patchwork", "ggtext"
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
input_file <- file.path("data", "raw", "Datasets", "germination_counts.xlsx")
output_dir_ge <- file.path("outputs", "germination_elevation")
figure_dir_ge <- file.path(output_dir_ge, "figures")
table_dir_ge <- file.path(output_dir_ge, "tables")
model_dir_ge <- file.path(output_dir_ge, "models")
diagnostic_dir_ge <- file.path(output_dir_ge, "diagnostics")
invisible(lapply(c(figure_dir_ge, table_dir_ge, model_dir_ge, diagnostic_dir_ge), dir.create,
  recursive = TRUE, showWarnings = FALSE
))

# Standardise reported precision without reducing precision used in analyses.
write.csv <- function(x, file = "", ..., row.names = TRUE) {
  x <- as.data.frame(x)
  x[] <- Map(function(v, nm) {
    if (!is.numeric(v)) {
      return(v)
    }
    round(v, if (grepl("^SE($|_)|Estimate|CI_low|CI_high", nm)) 6 else 3)
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
save_figure <- function(plot, stem, width = 240, height = 95) {
  ggsave(file.path(figure_dir_ge, paste0(stem, ".png")), plot,
    width = width,
    height = height, units = "mm", dpi = 600, bg = "white"
  )
  ggsave(file.path(figure_dir_ge, paste0(stem, ".pdf")), plot,
    width = width,
    height = height, units = "mm", device = cairo_pdf, bg = "white"
  )
}

# 3. Plant-level germination indices -----------------------------------------

# The workbook stores four cumulative replicate trajectories per plant.

# Sum the four replicate trajectories within each maternal plant.

# Each maternal plant contributes its own observed germination trajectory.

# 4. Germination count import and protocol validation ------------------------

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

# 5. Plant-level germination model fitting and predictions -------------------

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
  input_file, file.path("data", "raw", "Datasets", "trait_data.xlsx")
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
germ <- assignments |>
  left_join(indices, by = "plant_no") |>
  mutate(Elevation_z = (Elevation - elevation_mean) / elevation_sd)
if (nrow(germ) != nrow(germination_input$assignments) || anyNA(germ[, c(
  "t0", "Time_spread", "MGR",
  "Elevation"
)])) {
  stop("Expected complete plant records after calculating indices.")
}
write.csv(trajectory_checks,
  file.path(table_dir_ge, "Table_germination_trajectory_checks.csv"),
  row.names = FALSE
)
write.csv(germ,
  file.path(table_dir_ge, "Table_plant_level_germination_indices.csv"),
  row.names = FALSE
)

# 6. Distribution and skewness checks ----------------------------------------

sample_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3 || sd(x) == 0) {
    return(NA_real_)
  }
  n / ((n - 1) * (n - 2)) * sum(((x - mean(x)) / sd(x))^3)
}
response_labels <- c(
  t0 = "Time to first germination (days)",
  Time_spread = "Germination time spread (days)",
  MGR = expression(bold("Mean germination rate (day"^
    {
      -1
    } * ")"))
)
response_names <- c(
  t0 = "Time to first germination",
  Time_spread = "Germination time spread",
  MGR = "Mean germination rate", GP = "Germination percentage"
)
distribution_summary <- bind_rows(lapply(names(response_names), function(v) {
  x <- germ[[v]]
  data.frame(
    Variable = v, N = sum(is.finite(x)), Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE), Median = median(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE), Maximum = max(x, na.rm = TRUE),
    Skewness = sample_skewness(x)
  )
}))
write.csv(distribution_summary,
  file.path(table_dir_ge, "Table_germination_index_distributions.csv"),
  row.names = FALSE
)
distribution_long <- germ |>
  select(t0, Time_spread, MGR, GP) |>
  pivot_longer(everything(), names_to = "Response", values_to = "Value") |>
  mutate(Response = factor(Response,
    levels = names(response_names),
    labels = unname(response_names)
  ))
p_distribution <- ggplot(distribution_long, aes(Value)) +
  geom_histogram(aes(y = after_stat(density)),
    bins = 8,
    fill = "#7EC488", colour = "black", linewidth = .35
  ) +
  geom_density(colour = "black", linewidth = .7, adjust = 1.1) +
  facet_wrap(~Response, scales = "free", nrow = 1) +
  labs(x = "Observed value", y = "Density") +
  theme_manuscript()
save_figure(p_distribution, "Figure_germination_index_distributions", 235, 80)

# 7. H7a-H7c Gaussian linear models ------------------------------------------

model_specifications <- tibble(
  Hypothesis = c("H7a", "H7b", "H7c", "H7d"),
  Response = c("t0", "Time_spread", "MGR", "GP"),
  Expected_dir_geection = c("positive", "positive", "negative", "negative")
)
fit_model_pair <- function(response) {
  list(primary = fit_plant_model(reformulate("Elevation_z", response),
    data = germ, na.action = na.fail
  ))
}
models <- setNames(
  lapply(model_specifications$Response, fit_model_pair),
  model_specifications$Hypothesis
)
extract_elevation <- function(model, hypothesis, response, model_name,
                              expected_dir_geection) {
  coefficient_table <- plant_coef_table(model)
  term <- "Elevation_z"
  ci <- confint(model, parm = term, method = "Wald", level = .95)
  estimate_z <- unname(lme4::fixef(model)[term])
  estimate <- estimate_z / elevation_sd
  ci <- ci / elevation_sd
  p <- coefficient_table[term, "Pr(>|t|)"]
  expected_supported <- if (expected_dir_geection == "positive") estimate > 0 else estimate < 0
  data.frame(
    Hypothesis = hypothesis, Response = response, Model = model_name,
    Term = term, Elevation_SD_m = elevation_sd, Estimate_per_m = estimate,
    Estimate_per_100_m = estimate * 100,
    SE_per_m = coefficient_table[term, "Std. Error"] / elevation_sd,
    CI_low_per_m = ci[1], CI_high_per_m = ci[2],
    CI_low_per_100_m = ci[1] * 100, CI_high_per_100_m = ci[2] * 100,
    P_value = p, Expected_dir_geection = expected_dir_geection,
    Direction_consistent = expected_supported,
    Marginal_R2 = model_r2(model)[["Marginal"]], Conditional_R2 = model_r2(model)[["Conditional"]],
    AIC = AIC(model), N = nobs(model), Residual_df = df.residual(model),
    Singular = lme4::isSingular(model),
    Converged = length(setdiff(model@optinfo$conv$lme4$messages, "boundary (singular) fit: see help('isSingular')")) == 0L,
    stringsAsFactors = FALSE
  )
}
model_results <- bind_rows(lapply(seq_len(nrow(model_specifications)), function(i) {
  spec <- model_specifications[i, ]
  bind_rows(
    extract_elevation(
      models[[spec$Hypothesis]]$primary,
      spec$Hypothesis, spec$Response, "Elevation only",
      spec$Expected_dir_geection
    )
  )
}))
model_results_export <- model_results |>
  mutate(
    SE_per_100_m = SE_per_m * 100,
    P = if_else(P_value < 0.001, "< 0.001", sprintf("%.3f", P_value))
  ) |>
  select(-P_value) |>
  relocate(P, .after = CI_high_per_100_m)
write.csv(model_results_export,
  file.path(table_dir_ge, "Table_H7_model_results.csv"),
  row.names = FALSE
)
write.csv(
  germ |> count(Temperature_light_C, Temperature_dark_C, ERH_percent, Light_hours, Dark_hours,
    name = "N_plants"
  ),
  file.path(table_dir_ge, "Table_shared_condition_sample_sizes.csv"),
  row.names = FALSE
)
saveRDS(models, file.path(model_dir_ge, "Models_H7_germination_elevation.rds"))

# 8. Residual diagnostics for the elevation-only models ----------------------

save_lm_diagnostics <- function(model, stem) {
  png(file.path(diagnostic_dir_ge, paste0(stem, ".png")),
    width = 2200,
    height = 1900, res = 300, type = "cairo"
  )
  old <- par(mfrow = c(2, 2), family = "Arial", mar = c(4.2, 4.2, 2.0, 1.0))
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
  save_lm_diagnostics(
    models[[hypothesis]]$primary,
    paste0("Diagnostics_", hypothesis, "_primary")
  )
}

# 9. Primary association figure ----------------------------------------------

format_p <- function(p) ifelse(p < .001, "P < 0.001", sprintf("P = %.3f", p))
association_panel <- function(response, hypothesis, subtitle, y_label,
                              y_limits, y_breaks,
                              annotation_side = "left") {
  model <- models[[hypothesis]]$primary
  result <- filter(
    model_results, Hypothesis == hypothesis,
    Model == "Elevation only"
  )
  prediction_data <- data.frame(
    Elevation = seq(min(germ$Elevation), max(germ$Elevation), length.out = 200)
  ) |>
    mutate(Elevation_z = (Elevation - elevation_mean) / elevation_sd)
  prediction <- plant_prediction(model, newdata = prediction_data, interval = "confidence")
  prediction_data <- bind_cols(prediction_data, as.data.frame(prediction))
  y_limits <- range(c(
    y_limits, germ[[response]], prediction_data$lwr,
    prediction_data$upr
  ), finite = TRUE)
  line_type <- ifelse(result$P_value < .05, "solid", "dashed")
  p_label <- if (result$P_value < .001) {
    "italic(P) < 0.001"
  } else {
    sprintf("italic(P) == %.3f", result$P_value)
  }
  x_range <- range(germ$Elevation)
  annotation_x <- if (annotation_side == "right") {
    x_range[2] - .04 * diff(x_range)
  } else {
    x_range[1] + .04 * diff(x_range)
  }
  annotation_hjust <- if (annotation_side == "right") 1 else 0
  y_span <- diff(y_limits)
  annotation_data <- data.frame(
    x = annotation_x,
    y = if (response == "GP") y_limits[1] + y_span * c(.39, .31, .23, .15, .07) else y_limits[2] - y_span * c(.07, .15, .23, .31, .39),
    label = c(
      sprintf("beta == %.3f", result$Estimate_per_100_m),
      sprintf(
        "\"95%% CI: [%.3f, %.3f]\"", result$CI_low_per_100_m,
        result$CI_high_per_100_m
      ),
      p_label,
      r2_plotmath(result$Marginal_R2, result$Conditional_R2)
    )
  )
  ggplot(germ, aes(x = Elevation, y = .data[[response]])) +
    geom_ribbon(
      data = prediction_data,
      aes(x = Elevation, ymin = lwr, ymax = upr),
      inherit.aes = FALSE, fill = "#E5E5E5"
    ) +
    geom_line(
      data = prediction_data, aes(x = Elevation, y = fit),
      inherit.aes = FALSE, colour = "black", linewidth = .8,
      linetype = line_type
    ) +
    geom_point(
      shape = 21, fill = "#7EC488", colour = "black",
      size = 1.6, stroke = .25, alpha = .6,
      position = position_jitter(width = 8, height = 0, seed = 12191)
    ) +
    geom_text(
      data = annotation_data, aes(x = x, y = y, label = label),
      inherit.aes = FALSE, parse = TRUE, hjust = annotation_hjust,
      vjust = .5, family = "Arial", size = 4
    ) +
    scale_y_continuous(breaks = y_breaks, expand = expansion(mult = 0)) +
    coord_cartesian(ylim = y_limits) +
    labs(
      subtitle = subtitle, x = "Elevation (m)", y = y_label,
      caption = "Each point is one maternal plant (n = 581); horizontal jitter separates overlapping points."
    ) +
    theme_manuscript() +
    theme(
      text = element_text(family = "Arial"),
      axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
      axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
      axis.title = element_text(colour = "black", size = 13, face = "bold"),
      plot.subtitle = element_text(colour = "black", size = 13, face = "bold"),
      plot.margin = margin(t = 1, r = 4, b = 1, l = 2, unit = "mm")
    )
}
p_h7a <- association_panel(
  "t0", "H7a", "(a) Time to first germination",
  expression(bold("Time to first germination" ~ bolditalic(T)[0] ~ "(days)")),
  c(3, 12), seq(3, 12, 3)
)
p_h7b <- association_panel(
  "Time_spread", "H7b",
  "(b) Germination time spread",
  expression(bold("Germination time spread" ~ bolditalic(T)[g] - bolditalic(T)[0] ~ "(days)")),
  c(8, 24), seq(8, 24, 4)
)
p_h7c <- association_panel("MGR", "H7c", "(c) Mean germination rate",
  expression(bold("Mean germination rate" ~ "(day"^
    {
      -1
    } * ")")),
  c(.04, .12), seq(.04, .12, .02),
  annotation_side = "right"
)
p_h7d <- association_panel(
  "GP", "H7d", "(d) Germination percentage",
  "Germination percentage (%)", c(0, 100), seq(0, 100, 20)
)
save_figure(p_h7d, "Figure_H7d_GP_elevation", 150, 120)
y_title_spacing <- theme(
  axis.title.y = element_text(margin = margin(r = 12))
)
p_h7a <- p_h7a + y_title_spacing
p_h7b <- p_h7b + y_title_spacing
p_h7c <- p_h7c + y_title_spacing

# 10. Elevation coefficients under the common germination condition ----------

significance_code <- function(p) {
  ifelse(p < .001, "***",
    ifelse(p < .01, "**", ifelse(p < .05, "*", "ns"))
  )
}
h7_sensitivity_plot_data <- model_results |>
  mutate(
    Model = factor(Model,
      levels = c("Elevation only")
    ),
    Response_y = case_when(
      Response == "GP" ~ 10, Response == "MGR" ~ 1,
      Response == "Time_spread" ~ 4,
      Response == "t0" ~ 7
    ),
    Point_y = Response_y,
    Label_y = Point_y + .48, Label_hjust = ifelse(Estimate_per_100_m > -1, 1, 0),
    Label = sprintf(
      "atop(beta == %.3f*','~~R[m]^2 == \"%.3f\", R[c]^2 == \"%.3f\"~~'%s')",
      Estimate_per_100_m, Marginal_R2, Conditional_R2,
      significance_code(P_value)
    )
  )
p_H7_sensitivity <- ggplot(
  h7_sensitivity_plot_data,
  aes(
    x = Estimate_per_100_m, y = Point_y, colour = Model,
    fill = Model, shape = Model
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbar(
    data = h7_sensitivity_plot_data,
    aes(
      x = Estimate_per_100_m, y = Point_y,
      xmin = CI_low_per_100_m, xmax = CI_high_per_100_m,
      colour = Model
    ),
    inherit.aes = FALSE, width = .14, linewidth = .7, orientation = "y"
  ) +
  geom_point(
    data = h7_sensitivity_plot_data,
    aes(
      x = Estimate_per_100_m, y = Point_y, colour = Model,
      fill = Model, shape = Model
    ),
    inherit.aes = FALSE, size = 3.5, stroke = .7
  ) +
  geom_text(
    data = h7_sensitivity_plot_data,
    aes(
      x = Estimate_per_100_m, y = Label_y, label = Label,
      colour = Model, hjust = Label_hjust
    ), inherit.aes = FALSE, parse = TRUE,
    vjust = .5, family = "Arial", size = 4,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c("Elevation only" = "#02401B")) +
  scale_fill_manual(values = c("Elevation only" = "#02401B")) +
  scale_shape_manual(values = c("Elevation only" = 19)) +
  scale_y_continuous(
    breaks = c(1, 4, 7, 10),
    labels = expression(bold(MGR), bold(bolditalic(T)[g] - bolditalic(T)[0]), bold(bolditalic(T)[0]), bold(GP)),
    limits = c(-.20, 11.20),
    expand = expansion(mult = 0)
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 7)
  ) +
  labs(
    subtitle = "(a) Germination ~ elevation",
    x = "Elevation coefficient\n", y = NULL,
    colour = NULL, fill = NULL, shape = NULL
  ) +
  theme_manuscript() +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
    axis.title = element_text(colour = "black", size = 13, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 14, face = "bold"),
    legend.text = element_text(colour = "black", size = 12, face = "plain"),
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    legend.direction = "vertical",
    legend.background = element_rect(fill = scales::alpha("white", .82)),
    legend.key = element_blank()
  )

save_figure(p_h7a, "Figure_H7a_time_first_germination_elevation", 150, 120)
save_figure(p_h7b, "Figure_H7b_time_spread_elevation", 150, 120)
save_figure(p_h7c, "Figure_H7c_MGR_elevation", 150, 120)
save_figure(p_H7_sensitivity, "Figure_H7c_sensitivity", 150, 120)

# Combine all CSV tables from this analysis into one multi-sheet workbook.
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Install the openxlsx package to export the combined table workbook.")
}

# Export only the tables regenerated by this script; exclude older pooled outputs.
table_csv_files <- file.path(table_dir_ge, c(
  "Table_germination_trajectory_checks.csv", "Table_plant_level_germination_indices.csv",
  "Table_germination_index_distributions.csv", "Table_H7_model_results.csv",
  "Table_shared_condition_sample_sizes.csv"
))
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
    file = file.path(table_dir_ge, "summary_tables_H7_germination_elevation.xlsx"),
    overwrite = TRUE
  )
  message(
    "Combined table workbook written: ",
    file.path(table_dir_ge, "summary_tables_H7_germination_elevation.xlsx")
  )
}

# 11. Publication-ready gt tables --------------------------------------------

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

render_gt_tables(table_dir_ge, "Germination responses across elevation (H7a-H7d)")
saveRDS(list(p_h7a = p_h7a, p_h7b = p_h7b, p_h7c = p_h7c, p_h7d = p_h7d, p_H7_sensitivity = p_H7_sensitivity), file.path(model_dir_ge, "Figure_objects_H7.rds"))

# Run script 09 next to assemble the shared elevation/100-seed-mass figures and tables.

# 12. H7 sensitivity to response distribution --------------------------------

# This section can also be evaluated alone after the plant-index table exists.
# Keep Gaussian publication estimates as benchmarks; compare alternative links
# by direction and uncertainty, not by the numerical magnitude of coefficients.
for (package in c("lme4", "glmmTMB", "ordinal", "DHARMa")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Missing package: ", package)
}
h7_output <- file.path("outputs", "germination_elevation")
h7_data <- read.csv(file.path(h7_output, "tables", "Table_plant_level_germination_indices.csv"))
h7_data$Block <- factor(h7_data$Block)
h7_data$Source_plot_ID <- factor(h7_data$Source_plot_ID)
# Publication CSVs can contain rounded values. Recover exact responses and
# predictor values from the saved Gaussian model frames before sensitivity fits.
h7_benchmarks <- readRDS(file.path(h7_output, "models", "Models_H7_germination_elevation.rds"))
h7_benchmarks <- setNames(lapply(h7_benchmarks, function(x) x$primary),
  vapply(h7_benchmarks, function(x) as.character(formula(x$primary)[[2]]), character(1)))
for (response in c("t0", "Time_spread", "MGR", "GP")) {
  frame <- model.frame(h7_benchmarks[[response]])
  stopifnot(nrow(frame) == nrow(h7_data),
    identical(as.character(frame$Source_plot_ID), as.character(h7_data$Source_plot_ID)),
    identical(as.character(frame$Block), as.character(h7_data$Block)))
  h7_data[[response]] <- model.response(frame)
  h7_data$Elevation_z <- frame$Elevation_z
}
stopifnot(nrow(h7_data) == 581L, !anyNA(h7_data),
  all(h7_data$Final_germination %in% 0:100),
  all(h7_data$Time_spread %in% 0:34), all(h7_data$t0 %in% 1:34), all(h7_data$MGR > 0))
h7_data$t0_ordered <- ordered(h7_data$t0)
h7_data$spread_ordered <- ordered(h7_data$Time_spread)
h7_sensitivity_models <- list()
h7_sensitivity_warnings <- list()
h7_capture_fit <- function(name, expression) {
  messages <- character()
  fit <- withCallingHandlers(expression, warning = function(w) {
    messages <<- c(messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  h7_sensitivity_warnings[[name]] <<- messages
  h7_sensitivity_models[[name]] <<- fit
  message("Fitted H7 sensitivity model: ", name)
  invisible(fit)
}
for (response in c("t0", "Time_spread", "MGR", "GP")) {
  h7_capture_fit(paste0(response, "_Gaussian"), h7_benchmarks[[response]])
}
h7_capture_fit("GP_binomial", glmmTMB::glmmTMB(
  cbind(Final_germination, 100 - Final_germination) ~ Elevation_z +
    (1 | Block) + (1 | Source_plot_ID), data = h7_data, family = binomial()))
h7_capture_fit("GP_beta_binomial", glmmTMB::glmmTMB(
  cbind(Final_germination, 100 - Final_germination) ~ Elevation_z +
    (1 | Block) + (1 | Source_plot_ID), data = h7_data,
  family = glmmTMB::betabinomial(link = "logit")))
h7_capture_fit("MGR_Gamma", glmmTMB::glmmTMB(
  MGR ~ Elevation_z + (1 | Block) + (1 | Source_plot_ID),
  data = h7_data, family = Gamma(link = "log")))
for (response in c("t0", "Time_spread")) {
  ordered_response <- if (response == "t0") "t0_ordered" else "spread_ordered"
  h7_capture_fit(paste0(response, "_ordinal"), ordinal::clmm(
    as.formula(paste(ordered_response,
      "~ Elevation_z + (1 | Block) + (1 | Source_plot_ID)")),
    data = h7_data, link = "logit", Hess = TRUE, nAGQ = 1,
    control = ordinal::clmm.control(method = "nlminb", maxIter = 500,
      maxLineIter = 100, gradTol = 1e-6, iter.max = 2000,
      eval.max = 4000, rel.tol = 1e-10, x.tol = 1e-8)))
}
saveRDS(h7_sensitivity_models,
  file.path(h7_output, "models", "H7_distribution_sensitivity_models.rds"))
h7_positive_hessian <- function(hessian) {
  if (is.null(hessian) || !is.matrix(hessian) || any(!is.finite(hessian))) return(FALSE)
  all(eigen(hessian, symmetric = TRUE, only.values = TRUE)$values > 0)
}
h7_diagnostic_rows <- list()
h7_rows <- lapply(names(h7_sensitivity_models), function(name) {
  model <- h7_sensitivity_models[[name]]
  response <- if (startsWith(name, "Time_spread")) "Time_spread" else sub("_.*", "", name)
  hypothesis <- c(t0 = "H7a", MGR = "H7b", GP = "H7c", Time_spread = "H7d")[[response]]
  ordinal_fit <- inherits(model, "clmm")
  gaussian_fit <- inherits(model, "lmerMod")
  coefficients <- if (ordinal_fit || gaussian_fit) coef(summary(model)) else summary(model)$coefficients$cond
  estimate <- coefficients["Elevation_z", "Estimate"]
  se <- coefficients["Elevation_z", "Std. Error"]
  hessian_ok <- if (gaussian_fit) {
    h7_positive_hessian(model@optinfo$derivs$Hessian)
  } else if (ordinal_fit) {
    h7_positive_hessian(model$Hessian)
  } else model$sdr$pdHess
  gradient <- if (ordinal_fit) max(abs(model$gradient)) else NA_real_
  converged <- if (gaussian_fit) {
    model@optinfo$conv$opt == 0 && !length(setdiff(model@optinfo$conv$lme4$messages,
      "boundary (singular) fit: see help('isSingular')"))
  } else if (ordinal_fit) {
    is.finite(gradient) && gradient < 1e-4
  } else model$fit$convergence == 0
  variances <- if (gaussian_fit) as.data.frame(lme4::VarCorr(model))$vcov else if (ordinal_fit) {
    vapply(ordinal::VarCorr(model), function(x) as.numeric(x[1, 1]), numeric(1))
  } else vapply(glmmTMB::VarCorr(model)$cond, function(x) as.numeric(x[1, 1]), numeric(1))
  singular <- if (gaussian_fit) lme4::isSingular(model) else any(variances < 1e-8)
  if (!ordinal_fit && !gaussian_fit) {
    simulation <- DHARMa::simulateResiduals(model, n = 1000, plot = FALSE, seed = 12191)
    h7_diagnostic_rows[[name]] <<- data.frame(Model = name,
      Uniformity_P = DHARMa::testUniformity(simulation, plot = FALSE)$p.value,
      Dispersion_P = DHARMa::testDispersion(simulation, plot = FALSE)$p.value,
      Outlier_P = DHARMa::testOutliers(simulation, plot = FALSE)$p.value)
    grDevices::png(file.path(h7_output, "diagnostics", paste0("Sensitivity_", name, ".png")),
      width = 1800, height = 1200, res = 180)
    plot(simulation)
    grDevices::dev.off()
  }
  data.frame(Hypothesis = hypothesis, Response = response, Model = name,
    Scale = if (gaussian_fit) "Identity" else if (ordinal_fit) "Cumulative log odds of a later category" else if (response == "GP") "Log odds" else "Log mean",
    Estimate_per_elevation_SD = estimate, SE = se,
    CI_low = estimate - qnorm(.975) * se, CI_high = estimate + qnorm(.975) * se,
    P = 2 * pnorm(-abs(estimate / se)), N = nrow(h7_data), AIC = AIC(model),
    Converged = converged, Positive_definite_Hessian = hessian_ok,
    Singular = singular, Max_gradient = gradient,
    Warnings = paste(h7_sensitivity_warnings[[name]], collapse = " | "))
})
h7_sensitivity_results <- do.call(rbind, h7_rows)
write.csv(h7_sensitivity_results,
  file.path(h7_output, "tables", "Table_H7_distribution_sensitivity.csv"), row.names = FALSE)
write.csv(do.call(rbind, h7_diagnostic_rows),
  file.path(h7_output, "tables", "Table_H7_distribution_sensitivity_diagnostics.csv"), row.names = FALSE)
saveRDS(h7_sensitivity_models,
  file.path(h7_output, "models", "H7_distribution_sensitivity_models.rds"))
writeLines(c(
  "Gaussian models are benchmarks; sensitivity fits do not overwrite publication estimates.",
  "All models use 581 accessions, the same elevation scaling, and block/source-plot random intercepts.",
  "Ordinal cumulative-logit fits respect the observed ordered daily categories and avoid continuous normal errors; proportional odds is an additional assumption.",
  "Positive ordinal coefficients indicate a shift towards later first germination or wider time spread.",
  "GP uses germinated/ungerminated counts out of 100; beta-binomial allows extra-binomial variation.",
  "MGR uses a Gamma distribution with a log link; confidence intervals and P values are two-sided Wald approximations.",
  "Compare AIC only between binomial and beta-binomial GP fits, not across continuous and ordinal likelihoods.",
  "Report convergence, Hessian and boundary variance flags; singularity is not evidence that a response family is unsuitable.",
  "DHARMa checks use 1000 simulations with seed 12191; P < 0.05 flags a diagnostic departure.",
  "Ordinal models have no DHARMa checks here; converged ordinal fits alone do not establish proportional-odds adequacy."
), file.path(h7_output, "diagnostics", "README_H7_distribution_sensitivity.txt"))
print(h7_sensitivity_results)

# 13. Formatted supporting results for distribution sensitivity --------------

export_h7_distribution_audit <- function() {
  output <- file.path("outputs", "germination_elevation")
  models <- readRDS(file.path(output, "models", "H7_distribution_sensitivity_models.rds"))
  simulations <- utils::read.csv(file.path(output, "tables", "Table_H7_distribution_sensitivity_diagnostics.csv"))
  warning_records <- utils::read.csv(file.path(output, "tables", "Table_H7_distribution_sensitivity.csv"))
  plant_records <- utils::read.csv(file.path(output, "tables",
    "Table_plant_level_germination_indices.csv"))
  plot_records <- unique(plant_records[c("Source_plot_ID", "Elevation")])
  elevation_sd_m <- sd(plot_records$Elevation)
  stopifnot(is.finite(elevation_sd_m), elevation_sd_m > 0)
  elevation_multiplier <- 100 / elevation_sd_m
  alternatives <- names(models)[!grepl("Gaussian$", names(models))]
  rows <- lapply(alternatives, function(name) {
    model <- models[[name]]
    ordinal_fit <- inherits(model, "clmm")
    coefficients <- if (ordinal_fit) coef(summary(model)) else summary(model)$coefficients$cond
    gradient <- if (ordinal_fit) max(abs(model$gradient)) else NA_real_
    converged <- if (ordinal_fit) is.finite(gradient) && gradient < 1e-4 else model$fit$convergence == 0
    hessian <- if (ordinal_fit) all(eigen(model$Hessian, symmetric = TRUE, only.values = TRUE)$values > 0) else model$sdr$pdHess
    finite_se <- all(is.finite(coefficients[, "Std. Error"]))
    diagnostic <- simulations[simulations$Model == name, , drop = FALSE]
    simulation_pass <- if (nrow(diagnostic)) all(unlist(diagnostic[, -1, drop = FALSE]) >= .05) else NA
    eligible <- converged && hessian && finite_se && (ordinal_fit || simulation_pass)
    estimate <- coefficients["Elevation_z", "Estimate"] * elevation_multiplier
    se <- coefficients["Elevation_z", "Std. Error"] * elevation_multiplier
    response <- if (startsWith(name, "Time_spread")) "Time spread" else sub("_.*", "", name)
    variances <- if (ordinal_fit) unlist(lapply(ordinal::VarCorr(model), diag)) else
      unlist(lapply(glmmTMB::VarCorr(model)$cond, diag))
    data.frame(Model = name, Response = response,
      Package_function = if (ordinal_fit) "ordinal::clmm" else "glmmTMB::glmmTMB",
      Estimate_per_100_m = estimate, SE = se,
      CI_low = if (eligible) estimate - qnorm(.975) * se else NA_real_,
      CI_high = if (eligible) estimate + qnorm(.975) * se else NA_real_,
      P_zero = if (eligible) 2 * pnorm(-abs(estimate / se)) else NA_real_,
      N = nrow(model.frame(model)), Converged = converged,
      Positive_definite_Hessian = hessian, Finite_fixed_effect_SE = finite_se,
      Boundary = any(variances < 1e-8), Maximum_absolute_gradient = gradient,
      DHARMa_uniformity_P = if (nrow(diagnostic)) diagnostic$Uniformity_P else NA_real_,
      DHARMa_dispersion_P = if (nrow(diagnostic)) diagnostic$Dispersion_P else NA_real_,
      DHARMa_outlier_P = if (nrow(diagnostic)) diagnostic$Outlier_P else NA_real_,
      Accepted_sensitivity_fit = eligible,
      Warnings = warning_records$Warnings[match(name, warning_records$Model)])
  })
  result <- do.call(rbind, rows)
  # Format display values only; retain unrounded results in memory.
  publication <- result
  numeric_columns <- names(result)[vapply(result, is.numeric, logical(1))]
  numeric_columns <- setdiff(numeric_columns, "N")
  p_columns <- c("P_zero", "DHARMa_uniformity_P",
    "DHARMa_dispersion_P", "DHARMa_outlier_P")
  for (column in setdiff(numeric_columns, p_columns)) {
    values <- result[[column]]
    publication[[column]] <- ifelse(is.finite(values), sprintf("%.3f", values), "NA")
  }
  for (column in p_columns) {
    values <- result[[column]]
    publication[[column]] <- ifelse(!is.finite(values), "NA",
      ifelse(values < 0.001, "<0.001",
        ifelse(values < 0.01, "<0.01",
          ifelse(values < 0.05, "<0.05", sprintf("%.3f", values)))))
  }
  model_labels <- c(
    GP_binomial = "Binomial (logit)",
    GP_beta_binomial = "Beta-binomial (logit)",
    MGR_Gamma = "Gamma (log)",
    t0_ordinal = "Ordinal (logit)",
    Time_spread_ordinal = "Ordinal (logit)"
  )
  publication$Model <- unname(model_labels[result$Model])
  publication <- publication[c("Response", "Model",
    setdiff(names(publication), c("Response", "Model")))]
  for (column in names(publication)) {
    values <- publication[[column]]
    if (is.logical(values)) {
      publication[[column]] <- ifelse(is.na(values), "-", ifelse(values, "Yes", "No"))
    } else if (is.character(values)) {
      values[is.na(values) | values == "NA" | values == ""] <- "-"
      publication[[column]] <- values
    }
  }
  names(publication) <- c(
    "Response", "Model/link", "Package/function",
    "Elevation coefficient per 100 m", "Standard error per 100 m",
    "95% CI lower limit", "95% CI upper limit", "P value (coefficient = 0)",
    "Number of accessions", "Converged", "Positive-definite Hessian",
    "Finite fixed-effect standard errors", "Boundary random-effect variance",
    "Maximum absolute gradient", "DHARMa uniformity P value",
    "DHARMa dispersion P value", "DHARMa outlier P value",
    "Accepted sensitivity fit", "Warnings"
  )
  utils::write.csv(publication, file.path(output, "tables",
    "Table S3.csv"),
    row.names = FALSE, fileEncoding = "UTF-8")
  result
}
h7_distribution_audit <- export_h7_distribution_audit()
