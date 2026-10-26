# 1. Local publication support functions -------------------------------------

# Local definitions: this script never sources another R file.
publication_root <- "outputs"
publication_captions <- c(
  Figure_S6 = "Figure S6. Environmental variation across 27 source plots. Panels show (a) June 2021 mean temperature, (b) total precipitation and (c) standardised soil PC1 against elevation, and (d) soil-variable PC1 loadings. Points in a-c represent plots; lines show separate ordinary linear regressions and shading shows 95% confidence intervals. C, N, P and EC denote organic carbon, total nitrogen, extractable phosphorus and electrical conductivity. Equation x is elevation in metres."
)
publication_response <- function(h) {
  unname(c(
    H1 = "Flowering onset", H2 = "Flowering duration", H3 = "Final flowering date",
    H4 = "ln(MISM)", H5 = "Total seed number", H6 = "Total seed number"
  )[h])
}
publication_r2 <- function(m) {
  vf <- var(as.numeric(lme4::getME(m, "X") %*% lme4::fixef(m)))
  vr <- sum(vapply(lme4::VarCorr(m), function(v) as.numeric(v[1, 1]), numeric(1)))
  c(Marginal_R2 = vf / (vf + vr + sigma(m)^2), Conditional_R2 = (vf + vr) / (vf + vr + sigma(m)^2))
}
publication_flowering_tables <- function() {
  td <- file.path(publication_root, "flowering_phenology", "tables")
  primary_file <- file.path(td, "Table_H1_H3_primary_coefficients.csv")
  if (!file.exists(primary_file)) {
    return(invisible(NULL))
  }
  d <- utils::read.csv(primary_file, check.names = FALSE)
  models <- readRDS(file.path(publication_root, "flowering_phenology", "models", "Models_H1_H3_ML.rds"))
  completion <- readRDS(file.path(publication_root, "flowering_phenology", "models", "final_flowering_date_onset_models.rds"))
  env <- utils::read.csv(file.path(td, "Table_plot_environment.csv"))
  for (h in c("H1", "H2", "H3")) {
    m <- switch(h,
      H1 = models$H1$elevation,
      H2 = models$H2$elevation,
      H3 = completion$onset
    )
    term <- if (h == "H3") "Onset_centered" else "Elevation_z"
    cf <- coef(summary(m))[term, ]
    factor <- if (h == "H3") 1 else 100 / sd(env$Elevation)
    est <- unname(cf["Estimate"]) * factor
    se <- unname(cf["Std. Error"]) * factor
    j <- which(d$Hypothesis == h)
    stopifnot(length(j) == 1)
    d$Estimate[j] <- est
    d$SE[j] <- se
    d$CI_low[j] <- est - qnorm(.975) * se
    d$CI_high[j] <- est + qnorm(.975) * se
    d$z_approximation[j] <- est / se
    d$P_value[j] <- 2 * pnorm(-abs(est / se))
    r2 <- publication_r2(m)
    d$Marginal_R2[j] <- r2[1]
    d$Conditional_R2[j] <- r2[2]
  }
  d$Response <- publication_response(d$Hypothesis)
  d$P_zero <- d$P_value
  d$P_duration <- ifelse(d$Hypothesis == "H3", 2 * pnorm(-abs((d$Estimate - 1) / d$SE)), NA_real_)
  d$Null_for_duration <- ifelse(d$Hypothesis == "H3", 1, NA_real_)
  d$Implied_duration_slope <- ifelse(d$Hypothesis == "H3", d$Estimate - 1, NA_real_)
  d$Implied_duration_CI_low <- ifelse(d$Hypothesis == "H3", d$CI_low - 1, NA_real_)
  d$Implied_duration_CI_high <- ifelse(d$Hypothesis == "H3", d$CI_high - 1, NA_real_)
  utils::write.csv(d, primary_file, row.names = FALSE)
  within_file <- file.path(td, "Table_H3_within_among_coefficients.csv")
  w <- utils::read.csv(within_file)
  # Derive both tests from the unrounded saved decomposition, not rounded CSV SEs.
  wm <- models$H3_decomposition
  for (j in seq_len(nrow(w))) {
    cf <- coef(summary(wm))[w$Term[j], ]
    est <- unname(cf["Estimate"])
    se <- unname(cf["Std. Error"])
    w$Estimate[j] <- est
    w$SE[j] <- se
    w$CI_low[j] <- est - qnorm(.975) * se
    w$CI_high[j] <- est + qnorm(.975) * se
    w$z_approximation[j] <- est / se
    w$P_value[j] <- 2 * pnorm(-abs(est / se))
    r2 <- publication_r2(wm)
    w$Marginal_R2[j] <- r2[1]
    w$Conditional_R2[j] <- r2[2]
  }
  w$Response <- "Final flowering date"
  w$P_zero <- w$P_value
  w$P_duration <- 2 * pnorm(-abs((w$Estimate - 1) / w$SE))
  w$Null_for_duration <- 1
  w$Implied_duration_slope <- w$Estimate - 1
  w$Implied_duration_CI_low <- w$CI_low - 1
  w$Implied_duration_CI_high <- w$CI_high - 1
  utils::write.csv(w, within_file, row.names = FALSE)
  invisible(d)
}
publication_figure_exports <- function() {
  from <- file.path(publication_root, "flowering_phenology/figures/Figure1_environment_gradients_and_soil_PC1")
  to <- file.path(dirname(from), "Figure_S6_environment_gradients_and_soil_PC1")
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
      records[[length(records) + 1L]] <- data.frame(Number = "Figure S6", Format = ext, File = output)
    }
  }
  if (!length(records)) {
    return(invisible(NULL))
  }
  writeLines(publication_captions[["Figure_S6"]], paste0(to, "_caption.txt"))
  writeLines(publication_captions[["Figure_S6"]], paste0(from, "_caption.txt"))
  manifest_file <- file.path(publication_root, "Publication_figure_manifest.csv")
  updated <- do.call(rbind, records)
  if (file.exists(manifest_file)) {
    previous <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)
    previous <- previous[previous$Number != "Figure S6", , drop = FALSE]
    updated <- rbind(previous[, names(updated), drop = FALSE], updated)
  }
  utils::write.csv(updated, manifest_file, row.names = FALSE)
  invisible(updated)
}
# END INLINE PUBLICATION SUPPORT

# Flowering calendar: 1 June 2021 = Day 1; date = 2021-06-01 + day - 1.
# Duration = final flowering day minus onset day (elapsed days).
# ============================================================================

# Script 06: FLOWERING PHENOLOGY (H1-H3)

# Implements the statistical analysis described in manuscript Section 2.8.

# Run from the "arabis analysis" project root.

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

# 3. Reproductive-trait figure: grey facet-strip formatting ------------------

# A single facet strip for each reproductive-trait panel.
reproductive_panel_strip <- function(plot) {
  heading <- plot$labels$subtitle
  if (is.null(heading)) stop("Panel subtitle is missing.")
  wrapped_heading <- paste(strwrap(heading, width = 32), collapse = "\n")
  if (!grepl("\n", wrapped_heading, fixed = TRUE)) wrapped_heading <- paste0(wrapped_heading, "\n")
  plot$data$.panel_heading <- wrapped_heading
  plot + facet_wrap(~.panel_heading, ncol = 1) + labs(subtitle = NULL) +
    theme(
      strip.background = element_rect(fill = "#D9D9D9", colour = "grey30"),
      strip.text = element_text(
        family = "Arial", colour = "black", face = "bold",
        size = 13, hjust = .5, lineheight = 1.05,
        margin = margin(5, 4, 5, 4)
      )
    )
}
# End of reproductive-trait figure: grey facet-strip formatting.

# 4. Packages and output directories -----------------------------------------

required <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "lme4", "performance",
  "patchwork", "ggtext"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lme4)
  library(patchwork)
})
if (!file.exists("arabis analysis.Rproj")) {
  stop("Run this script from the 'arabis analysis' project root.")
}
set.seed(12191)

# Derive significance symbols from model P-values; never enter them manually.
significance_code <- function(p) {
  cut(p,
    breaks = c(-Inf, .001, .01, .05, Inf),
    labels = c("***", "**", "*", "ns"), right = FALSE
  ) |>
    as.character()
}
input_dir_fp <- file.path("data", "raw", "Datasets")
output_dir_fp <- file.path("outputs", "flowering_phenology")
figure_dir_fp <- file.path(output_dir_fp, "figures")
table_dir_fp <- file.path(output_dir_fp, "tables")
model_dir_fp <- file.path(output_dir_fp, "models")
diagnostic_dir_fp <- file.path(output_dir_fp, "diagnostics")
invisible(lapply(c(figure_dir_fp, table_dir_fp, model_dir_fp, diagnostic_dir_fp), dir.create,
  recursive = TRUE, showWarnings = FALSE
))

# Standardise reported precision without reducing precision used in analyses.
write.csv <- function(x, file = "", ..., row.names = TRUE) {
  x <- as.data.frame(x)
  x[] <- lapply(x, function(v) if (is.numeric(v)) round(v, 3) else v)
  utils::write.csv(x, file = file, ..., row.names = row.names)
}
theme_manuscript <- function(base_size = 11) {
  theme_bw(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(family = "Arial", colour = "black"),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(colour = "black", size = 11),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.subtitle = element_text(face = "bold", hjust = 0, colour = "black", size = 12),
      legend.position = "bottom"
    )
}
save_figure <- function(plot, stem, width = 190, height = 130) {
  ggsave(file.path(figure_dir_fp, paste0(stem, ".png")), plot,
    width = width,
    height = height, units = "mm", dpi = 600, bg = "white"
  )
  ggsave(file.path(figure_dir_fp, paste0(stem, ".pdf")), plot,
    width = width,
    height = height, units = "mm", device = cairo_pdf, bg = "white"
  )
}

# 5. Data preparation --------------------------------------------------------

raw_traits <- read_excel(
  file.path(input_dir_fp, "trait_data.xlsx"),
  sheet = "Sheet2",
  .name_repair = function(x) make.unique(x, sep = "_duplicate")
)
individual <- raw_traits |>
  transmute(
    Plant = plant_no,
    Block = factor(Block),
    Plot = as.integer(Plot),
    Elevation = as.numeric(Elevation),
    PlotID = paste(Block, Plot, Elevation, sep = "_"),
    Onset = as.numeric(FloweringOnset),
    FinalDate = as.numeric(Final_flowering_date),
    Duration = as.numeric(Final_flowering_date) - as.numeric(FloweringOnset),
    MISM = as.numeric(MISM),
    SeedNumber = as.numeric(TotalSeedNumber),
    EC = as.numeric(Electrical.Conductivity),
    pH = as.numeric(pH),
    Carbon = as.numeric(Carbon),
    Nitrogen = as.numeric(Nitrogen),
    Phosphorus = as.numeric(Phosphorus)
  )
# Both dates must use the same calendar origin; duration is elapsed days.
if (any(!is.na(individual$FinalDate) & !is.finite(individual$FinalDate)) ||
  any(!is.na(individual$Onset) & !is.finite(individual$Onset)) ||
  any(individual$Duration < 0, na.rm = TRUE)) {
  stop("Invalid flowering dates: verify numeric day offsets and final date >= onset.")
}
climate <- read_excel(
  file.path(input_dir_fp, "plot_abiotic_factors.xlsx"),
  sheet = "June summary",
  skip = 3, .name_repair = function(x) make.unique(x, sep = "_duplicate")
) |>
  transmute(
    PlotID = as.character(Plot_ID),
    Temperature = as.numeric(June_mean_temperature_C),
    Precipitation = as.numeric(June_total_precipitation_mm)
  )

# 6. Environmental predictors and soil PCA preparation -----------------------

# Environmental measurements are plot-level covariates; trait responses remain plants.
prepare_plot_environment <- function(individual, climate) {
  variables <- c("PlotID", "Elevation", "Carbon", "pH", "EC", "Nitrogen", "Phosphorus")
  plots <- individual |>
    dplyr::group_by(PlotID) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(setdiff(variables, "PlotID")), mean), .groups = "drop")
  if (anyDuplicated(plots$PlotID) || nrow(plots) != 27L) {
    stop("Expected 27 source plots with consistent soil measurements.")
  }
  plots <- dplyr::left_join(plots, climate, by = "PlotID")
  soil <- dplyr::select(plots, Carbon, pH, EC, Nitrogen, Phosphorus)
  if (any(!is.finite(as.matrix(soil)))) stop("Invalid soil measurements.")
  pca <- prcomp(soil, center = TRUE, scale. = TRUE)
  if (mean(pca$rotation[c("Carbon", "pH", "Nitrogen", "Phosphorus"), "PC1"]) < 0) {
    pca$rotation[, "PC1"] <- -pca$rotation[, "PC1"]
    pca$x[, "PC1"] <- -pca$x[, "PC1"]
  }
  plots$SoilPC1 <- pca$x[, "PC1"]
  for (variable in c("Elevation", "Temperature", "Precipitation", "SoilPC1")) {
    plots[[paste0(variable, "_z")]] <- as.numeric(scale(plots[[variable]]))
  }
  joined <- dplyr::left_join(individual, dplyr::select(
    plots, PlotID,
    Temperature, Precipitation, SoilPC1, dplyr::ends_with("_z")
  ), by = "PlotID")
  list(plots = plots, plants = joined, pca = pca)
}
# End of environmental predictors and soil pca preparation.

prepared_environment <- prepare_plot_environment(individual, climate)
plant_environment <- prepared_environment$plots
soil_pca <- prepared_environment$pca
individual <- prepared_environment$plants |>
  mutate(Onset_centered = Onset - mean(Onset, na.rm = TRUE))

# 7. Soil PCA and plant-level Spearman correlations --------------------------

pca_variance <- soil_pca$sdev^2
pca_table <- data.frame(
  Component = paste0("PC", seq_along(pca_variance)),
  Eigenvalue = pca_variance,
  Variance_explained_percent = 100 * pca_variance / sum(pca_variance),
  Cumulative_percent = 100 * cumsum(pca_variance) / sum(pca_variance)
)
pca_loadings <- data.frame(
  Variable = rownames(soil_pca$rotation),
  soil_pca$rotation, row.names = NULL
)
write.csv(pca_table, file.path(table_dir_fp, "Table_soil_PCA_variance.csv"), row.names = FALSE)
write.csv(pca_loadings, file.path(table_dir_fp, "Table_soil_PCA_loadings.csv"), row.names = FALSE)

# 8. Environmental gradients and soil-PC1 composition ------------------------

# Regressions are descriptive plant-level summaries used to visualise how the

# environmental predictors change along the sampled elevation gradient.
lm_panel_annotation <- function(model, slope_digits = 5,
                                observed_values = NULL, range_unit = NULL) {
  sm <- summary(model)
  co <- coef(model)
  slope_format <- paste0("%.", as.integer(slope_digits), "f")
  sign_text <- if (co[[2]] >= 0) "+" else "-"
  equation <- paste0(
    "<i>y</i> = ", sprintf("%.3f", co[[1]]), " ", sign_text, " ",
    sprintf(slope_format, abs(co[[2]])), "<i>x</i>"
  )
  p <- coef(sm)[2, "Pr(>|t|)"]
  p_text <- if (p < .001) {
    "<i>P</i> &lt; 0.001"
  } else {
    sprintf("<i>P</i> = %.3f", p)
  }
  range_text <- if (!is.null(observed_values) && !is.null(range_unit)) {
    paste0(
      "<br>(", sprintf("%.1f", min(observed_values, na.rm = TRUE)),
      "&ndash;", sprintf("%.1f", max(observed_values, na.rm = TRUE)),
      " ", range_unit, ")"
    )
  } else {
    ""
  }
  paste0(
    equation, range_text, "<br>R<sup>2</sup> = ",
    sprintf("%.3f", sm$r.squared), "<br>", p_text
  )
}
environment_scatter_panel <- function(response, y_label, colour, tag,
                                      slope_digits = 5,
                                      annotation_position = "top-right",
                                      y_limits = NULL, y_breaks = waiver(),
                                      range_unit = NULL, annotation_y = NULL) {
  fit <- lm(reformulate("Elevation", response = response),
    data = plant_environment
  )
  annotation_data <- if (annotation_position == "bottom-right") {
    data.frame(x = Inf, y = -Inf, hjust = 1.08, vjust = -.70)
  } else {
    data.frame(x = Inf, y = Inf, hjust = 1.08, vjust = 2.05)
  }
  if (!is.null(annotation_y)) {
    annotation_data$y <- annotation_y
    annotation_data$vjust <- 1
  }
  ggplot(plant_environment, aes(x = Elevation, y = .data[[response]])) +
    geom_smooth(
      method = "lm", formula = y ~ x, se = TRUE,
      colour = "grey40", fill = "#E5E5E5",
      linewidth = .9
    ) +
    geom_point(
      shape = 21, fill = scales::alpha(colour, .72),
      colour = "grey20", stroke = .65, size = 2.6
    ) +
    ggtext::geom_richtext(
      data = annotation_data,
      aes(x = x, y = y, hjust = hjust, vjust = vjust),
      label = lm_panel_annotation(
        fit, slope_digits,
        observed_values = plant_environment[[response]],
        range_unit = range_unit
      ),
      inherit.aes = FALSE, family = "Arial", size = 3.5,
      lineheight = 1.02, fill = NA, label.colour = NA,
      label.padding = grid::unit(0, "pt")
    ) +
    annotate("text",
      x = -Inf, y = Inf, label = tag,
      hjust = -.35, vjust = 1.5, family = "Arial",
      fontface = "bold", size = 4.5
    ) +
    scale_y_continuous(breaks = y_breaks) +
    coord_cartesian(ylim = y_limits) +
    labs(x = "Elevation (m)", y = y_label) +
    theme_bw(base_size = 11, base_family = "Arial") +
    theme(
      text = element_text(family = "Arial"),
      axis.text.x = element_text(colour = "black", size = 11, face = "plain"),
      axis.text.y = element_text(colour = "black", size = 11, face = "plain"),
      axis.title = element_text(colour = "black", size = 12, face = "bold"),
      plot.subtitle = element_text(colour = "black", size = 12, face = "bold"),
      plot.margin = margin(t = 1.5, r = 3, b = .5, l = 1, unit = "mm")
    )
}
p_environment_temperature <- environment_scatter_panel(
  "Temperature",
  expression(bold("Mean June temperature (" * degree * "C)")),
  "#FFFF6D", "(a)", 4.5,
  y_limits = c(2.5, 6.5),
  y_breaks = seq(2.5, 6.5, 1),
  range_unit = "&deg;C", annotation_y = 6
)
p_environment_precipitation <- environment_scatter_panel(
  "Precipitation", "Total June precipitation (mm)", "#99F9FF", "(b)", 4.5,
  y_limits = c(22, 30), y_breaks = seq(22, 30, 2), range_unit = "mm",
  annotation_y = 29
)
p_environment_soil_pc1 <- environment_scatter_panel(
  "SoilPC1_z", "Standardized soil PC1 score", "#924900", "(c)", 4.5,
  "bottom-left",
  y_limits = c(-2.5, 2.5),
  y_breaks = seq(-2.5, 2.5, 1.25), annotation_y = 1.5
)
loading_labels <- c(
  pH = "pH", Phosphorus = "P", Carbon = "C",
  Nitrogen = "N", EC = "EC"
)
loading_plot_data <- pca_loadings |>
  transmute(
    Variable = factor(unname(loading_labels[Variable]),
      levels = c("EC", "N", "C", "P", "pH")
    ),
    Loading = PC1
  )
p_environment_loadings <- ggplot(
  loading_plot_data, aes(x = Loading, y = Variable, fill = Loading > 0)
) +
  geom_vline(xintercept = 0, colour = "black", linewidth = .55) +
  geom_col(width = .68, colour = "black", linewidth = .55) +
  geom_text(
    aes(
      label = sprintf("%+.3f", Loading),
      hjust = ifelse(Loading >= 0, 1.08, -.08)
    ),
    family = "Arial", size = 3.7, colour = "black"
  ) +
  annotate("text",
    x = -Inf, y = Inf, label = "(d)",
    hjust = -.35, vjust = 1.35, family = "Arial",
    fontface = "bold", size = 4.5
  ) +
  scale_fill_manual(
    values = c(`TRUE` = "#DE77AE", `FALSE` = "#B8E186"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(-.52, .52), breaks = seq(-.5, .5, .25),
    expand = expansion(mult = 0)
  ) +
  labs(x = "PC1 loading", y = "Soil properties") +
  theme_bw(base_size = 11, base_family = "Arial") +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 11, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 11, face = "plain"),
    axis.title = element_text(colour = "black", size = 12, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 12, face = "bold"),
    plot.margin = margin(t = 1, r = 3, b = .2, l = 1, unit = "mm")
  )
environment_column_gap_mm <- 0.2
environment_gradient_figure <- wrap_plots(
  p_environment_temperature, plot_spacer(), p_environment_precipitation,
  p_environment_soil_pc1, plot_spacer(), p_environment_loadings,
  ncol = 3,
  widths = grid::unit(c(1, environment_column_gap_mm, 1), c("null", "mm", "null")),
  guides = "collect"
)
save_figure(
  environment_gradient_figure,
  "Figure1_environment_gradients_and_soil_PC1", 200, 150
)

# 9. Analysis sample and model helpers ---------------------------------------

# Shared complete-case data ensure identical observations within each AICc set.
model_data <- individual |>
  filter(complete.cases(
    Onset, Duration, Elevation_z, Temperature_z,
    Precipitation_z, SoilPC1_z, Block, PlotID
  ))
fit_ml <- function(formula, data = model_data) {
  lmer(formula,
    data = data, REML = FALSE,
    control = lmerControl(
      optimizer = "bobyqa",
      check.conv.singular = "warning"
    )
  )
}
aicc_table <- function(models) {
  out <- bind_rows(lapply(names(models), function(name) {
    model <- models[[name]]
    k <- attr(logLik(model), "df")
    n <- nobs(model)
    aic <- AIC(model)
    data.frame(
      Model = name, N = n, K = k, logLik = as.numeric(logLik(model)),
      AIC = aic, AICc = aic + (2 * k * (k + 1)) / (n - k - 1)
    )
  })) |>
    arrange(AICc) |>
    mutate(
      Delta_AICc = AICc - min(AICc),
      Akaike_weight = exp(-0.5 * Delta_AICc) / sum(exp(-0.5 * Delta_AICc))
    )
  if (length(unique(out$N)) != 1L) stop("Candidate models use different observations.")
  out
}
extract_term <- function(model, term, hypothesis, model_name) {
  tab <- coef(summary(model))
  estimate <- tab[term, "Estimate"]
  se <- tab[term, "Std. Error"]
  statistic <- estimate / se
  ci <- estimate + qnorm(c(.025, .975)) * se
  if (term == "Elevation_z") {
    conversion <- 100 / sd(plant_environment$Elevation)
    estimate <- estimate * conversion
    se <- se * conversion
    ci <- ci * conversion
  }
  data.frame(
    Hypothesis = hypothesis, Model = model_name, Term = term,
    Coefficient_unit = if (term == "Elevation_z") "per 100-m elevation increase" else "per one-day onset increase",
    Estimate = estimate, SE = se, CI_low = ci[1], CI_high = ci[2],
    z_approximation = statistic, P_value = 2 * pnorm(abs(statistic), lower.tail = FALSE),
    Marginal_R2 = model_r2(model)[["Marginal"]], Conditional_R2 = model_r2(model)[["Conditional"]],
    N = nobs(model), Singular = isSingular(model, tol = 1e-4)
  )
}

# 10. H1: flowering onset ----------------------------------------------------

h1_models <- list(
  elevation = fit_ml(Onset ~ Elevation_z + (1 | Block) + (1 | PlotID)),
  elevation_climate = fit_ml(Onset ~ Elevation_z + Temperature_z + Precipitation_z +
    (1 | Block) + (1 | PlotID)),
  elevation_soil = fit_ml(Onset ~ Elevation_z + SoilPC1_z +
    (1 | Block) + (1 | PlotID)),
  elevation_climate_soil = fit_ml(Onset ~ Elevation_z + Temperature_z +
    Precipitation_z + SoilPC1_z +
    (1 | Block) + (1 | PlotID))
)

# 11. H2: flowering duration -------------------------------------------------

h2_models <- list(
  elevation = fit_ml(Duration ~ Elevation_z + (1 | Block) + (1 | PlotID)),
  elevation_climate = fit_ml(Duration ~ Elevation_z + Temperature_z + Precipitation_z +
    (1 | Block) + (1 | PlotID)),
  elevation_soil = fit_ml(Duration ~ Elevation_z + SoilPC1_z +
    (1 | Block) + (1 | PlotID)),
  elevation_climate_soil = fit_ml(Duration ~ Elevation_z + Temperature_z +
    Precipitation_z + SoilPC1_z +
    (1 | Block) + (1 | PlotID))
)

# 12. H3: flowering duration versus onset ------------------------------------

h3_models <- list(
  onset = fit_ml(Duration ~ Onset_centered + (1 | Block) + (1 | PlotID)),
  onset_elevation = fit_ml(Duration ~ Onset_centered + Elevation_z +
    (1 | Block) + (1 | PlotID)),
  onset_elevation_climate = fit_ml(Duration ~ Onset_centered + Elevation_z +
    Temperature_z + Precipitation_z +
    (1 | Block) + (1 | PlotID)),
  onset_elevation_soil = fit_ml(Duration ~ Onset_centered + Elevation_z + SoilPC1_z +
    (1 | Block) + (1 | PlotID)),
  onset_elevation_climate_soil = fit_ml(Duration ~ Onset_centered + Elevation_z +
    Temperature_z + Precipitation_z + SoilPC1_z +
    (1 | Block) + (1 | PlotID))
)
selection <- bind_rows(
  mutate(aicc_table(h1_models), Hypothesis = "H1"),
  mutate(aicc_table(h2_models), Hypothesis = "H2"),
  mutate(aicc_table(h3_models), Hypothesis = "H3")
) |>
  select(Hypothesis, everything())
write.csv(selection, file.path(table_dir_fp, "Table_H1_H3_AICc_model_comparison.csv"),
  row.names = FALSE
)

# Decompose onset while retaining every individual plant response.
model_data <- model_data |>
  group_by(PlotID) |>
  mutate(Onset_plot_mean = mean(Onset), Onset_within = Onset - Onset_plot_mean) |>
  ungroup() |>
  mutate(Onset_among = Onset_plot_mean - mean(Onset))
h3_decomposition <- fit_ml(FinalDate ~ Onset_within + Onset_among +
  (1 | Block) + (1 | PlotID))
h3_decomposition_results <- bind_rows(
  extract_term(h3_decomposition, "Onset_within", "H3", "within_plot"),
  extract_term(h3_decomposition, "Onset_among", "H3", "among_plot")
)
write.csv(h3_decomposition_results, file.path(
  table_dir_fp,
  "Table_H3_within_among_coefficients.csv"
), row.names = FALSE)
coefficients <- bind_rows(
  extract_term(h1_models$elevation, "Elevation_z", "H1", "elevation"),
  extract_term(h2_models$elevation, "Elevation_z", "H2", "elevation"),
  extract_term(h3_models$onset, "Onset_centered", "H3", "onset")
)
write.csv(coefficients, file.path(table_dir_fp, "Table_H1_H3_primary_coefficients.csv"),
  row.names = FALSE
)
saveRDS(
  list(
    H1 = h1_models, H2 = h2_models, H3 = h3_models,
    H3_decomposition = h3_decomposition, Soil_PCA = soil_pca
  ),
  file.path(model_dir_fp, "Models_H1_H3_ML.rds")
)

# 13. Residual diagnostics ---------------------------------------------------

diagnostic_plot <- function(model, label) {
  diagnostic_data <- data.frame(
    Fitted = fitted(model), Residual = resid(model),
    Standardized_residual = as.numeric(scale(resid(model)))
  )
  residual_plot <- ggplot(diagnostic_data, aes(Fitted, Standardized_residual)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(shape = 21, fill = "#2CB89A", alpha = .65) +
    geom_smooth(method = "loess", se = FALSE, colour = "#B2182B") +
    labs(
      subtitle = paste0(label, ": residuals versus fitted"),
      y = "Standardized residual"
    )
  qq_plot <- ggplot(diagnostic_data, aes(sample = Standardized_residual)) +
    stat_qq(shape = 21, fill = "#2CB89A") +
    stat_qq_line(colour = "#B2182B") +
    labs(subtitle = paste0(label, ": normal Q-Q"))
  (residual_plot + qq_plot) & theme_manuscript()
}
diagnostics <- diagnostic_plot(h1_models$elevation, "H1") /
  diagnostic_plot(h2_models$elevation, "H2") /
  diagnostic_plot(h3_models$onset, "H3")
ggsave(file.path(diagnostic_dir_fp, "Figure_H1_H3_residual_diagnostics.png"), diagnostics,
  width = 300, height = 300, units = "mm", dpi = 400, bg = "white"
)

# Place annotation text exactly 1 mm inside the panel border at draw time.
geom_text_edge <- function(...) {
  annotation_layer <- ggplot2::geom_text(...)
  annotation_layer$geom <- ggplot2::ggproto(NULL, ggplot2::GeomText,
    draw_panel = function(data, panel_params, coord, parse = TRUE, na.rm = FALSE, check_overlap = FALSE, ...) {
      text <- ggplot2::GeomText$draw_panel(data, panel_params, coord, parse = TRUE, na.rm = na.rm, check_overlap = check_overlap, ...)
      text$x <- if (all(data$hjust == 0)) {
        grid::unit(1, "mm")
      } else {
        grid::unit(1, "npc") - grid::unit(1, "mm")
      }
      text
    }
  )
  annotation_layer
}

# 14. Publication figures ----------------------------------------------------

plant_points <- model_data
model_annotation_data <- function(row, x_limits, y_limits,
                                  side = c("left", "right"),
                                  vertical = c("top", "bottom")) {
  side <- match.arg(side)
  vertical <- match.arg(vertical)
  p_label <- if (row$P_value < .001) {
    "italic(P) < 0.001"
  } else {
    sprintf("italic(P) == %.3f", row$P_value)
  }
  y_positions <- if (vertical == "top") {
    y_limits[2] - diff(y_limits) * c(.10, .21, .32, .43, .54)
  } else {
    y_limits[1] + diff(y_limits) * c(.54, .43, .32, .21, .10)
  }
  data.frame(
    x = if (side == "left") {
      x_limits[1] + .04 * diff(x_limits)
    } else {
      x_limits[2] - .04 * diff(x_limits)
    },
    y = y_positions,
    hjust = if (side == "left") 0 else 1,
    label = c(
      sprintf(if (row$Hypothesis %in% c("H1", "H2")) "beta[\"100 m\"] == %.3f" else "beta == %.3f", row$Estimate),
      sprintf("\"CI: [%.3f, %.3f]\"", row$CI_low, row$CI_high),
      p_label,
      r2_plotmath(row$Marginal_R2, row$Conditional_R2)
    )
  )
}
fixed_prediction <- function(model, newdata) {
  fixed_formula <- reformulas::nobars(formula(model))
  design <- model.matrix(delete.response(terms(fixed_formula)), newdata)
  fit <- as.numeric(design %*% fixef(model))
  se <- sqrt(diag(design %*% vcov(model) %*% t(design)))
  transform(newdata,
    Fitted = fit, CI_low = fit - 1.96 * se,
    CI_high = fit + 1.96 * se
  )
}
h1_row <- coefficients |> filter(Hypothesis == "H1")
h2_row <- coefficients |> filter(Hypothesis == "H2")
h3_row <- coefficients |> filter(Hypothesis == "H3", Model == "onset")
elevation_sequence <- seq(min(model_data$Elevation), max(model_data$Elevation),
  length.out = 200
)
elevation_mean <- mean(plant_environment$Elevation)
elevation_sd <- sd(plant_environment$Elevation)
h1_prediction <- fixed_prediction(
  h1_models$elevation,
  data.frame(
    Elevation = elevation_sequence,
    Elevation_z = (elevation_sequence - elevation_mean) / elevation_sd
  )
)
h2_prediction <- fixed_prediction(
  h2_models$elevation,
  data.frame(
    Elevation = elevation_sequence,
    Elevation_z = (elevation_sequence - elevation_mean) / elevation_sd
  )
)
onset_sequence <- seq(min(model_data$Onset), max(model_data$Onset), length.out = 200)
h3_prediction <- fixed_prediction(
  h3_models$onset,
  data.frame(
    Onset = onset_sequence,
    Onset_centered = onset_sequence - mean(model_data$Onset)
  )
)
h1_annotation <- model_annotation_data(
  h1_row, range(model_data$Elevation), c(8, 24), "left", "bottom"
)
h2_annotation <- model_annotation_data(
  h2_row, range(model_data$Elevation), c(0, 32), "left"
)
h3_annotation <- model_annotation_data(
  h3_row, range(model_data$Onset), c(5, 25), "right"
)
# Match panel b: successive annotation lines occupy 10% of panel height.
h3_annotation$y <- h3_annotation$y[1] - seq(0, 4) * (25 - 10) * .10
p_h1 <- ggplot(model_data, aes(Elevation, Onset)) +
  geom_ribbon(
    data = h1_prediction, aes(x = Elevation, ymin = CI_low, ymax = CI_high),
    inherit.aes = FALSE, fill = "#E5E5E5", alpha = .6
  ) +
  geom_line(
    data = h1_prediction, aes(x = Elevation, y = Fitted), linewidth = .8,
    inherit.aes = FALSE, colour = "black",
    linetype = ifelse(h1_row$P_value < 0.05, "solid", "dashed")
  ) +
  geom_point(
    shape = 21, fill = "#2CB89A", colour = "black", size = 1.5,
    alpha = 0.55, stroke = 0.25,
    position = position_jitter(width = 8, height = 0.24, seed = 12191)
  ) +
  scale_y_continuous(breaks = seq(8, 24, by = 4)) +
  coord_cartesian(ylim = c(8, 24)) +
  geom_text_edge(
    data = h1_annotation,
    aes(x = x, y = y, label = label, hjust = hjust),
    inherit.aes = FALSE, parse = TRUE, vjust = .5,
    size = 4.8, family = "Arial", colour = "black"
  ) +
  labs(
    subtitle = "(a) Flowering onset ~ elevation",
    x = "Elevation (m)\n",
    y = "Flowering onset (days)\n"
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
p_h1 <- reproductive_panel_strip(p_h1)
save_figure(p_h1, "Figure_H1_flowering_onset_elevation", 150, 120)
p_h2 <- ggplot(model_data, aes(Elevation, Duration)) +
  geom_ribbon(
    data = h2_prediction, aes(x = Elevation, ymin = CI_low, ymax = CI_high),
    inherit.aes = FALSE, fill = "#E5E5E5", alpha = .6
  ) +
  geom_line(
    data = h2_prediction, aes(x = Elevation, y = Fitted), linewidth = .8,
    inherit.aes = FALSE, colour = "black",
    linetype = ifelse(h2_row$P_value < 0.05, "solid", "dashed")
  ) +
  geom_point(
    shape = 21, fill = "#2CB89A", colour = "black", size = 1.5,
    alpha = 0.55, stroke = 0.25,
    position = position_jitter(width = 8, height = 0.24, seed = 12192)
  ) +
  scale_y_continuous(breaks = seq(0, 30, by = 10)) +
  coord_cartesian(ylim = c(0, 32)) +
  geom_text_edge(
    data = h2_annotation,
    aes(x = x, y = y, label = label, hjust = hjust),
    inherit.aes = FALSE, parse = TRUE, vjust = .5,
    size = 4.8, family = "Arial", colour = "black"
  ) +
  labs(
    subtitle = "(b) Flowering duration ~ elevation", x = "Elevation (m)\n",
    y = "Flowering duration (days)\n"
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
p_h2 <- reproductive_panel_strip(p_h2)
save_figure(p_h2, "Figure_H2_flowering_duration_elevation", 150, 120)
p_h3 <- ggplot(model_data, aes(Onset, Duration)) +
  geom_ribbon(
    data = h3_prediction, aes(x = Onset, ymin = CI_low, ymax = CI_high),
    inherit.aes = FALSE, fill = "#E5E5E5", alpha = .6
  ) +
  geom_line(
    data = h3_prediction, aes(x = Onset, y = Fitted), linewidth = .8,
    inherit.aes = FALSE, colour = "black",
    linetype = ifelse(h3_row$P_value < 0.05, "solid", "dashed")
  ) +
  geom_point(
    shape = 21, fill = "#2CB89A", colour = "black", size = 1.5,
    alpha = 0.55, stroke = 0.25,
    position = position_jitter(width = 0.24, height = 0.24, seed = 12193)
  ) +
  scale_y_continuous(breaks = seq(10, 25, by = 5)) +
  coord_cartesian(ylim = c(10, 25), expand = TRUE) +
  geom_text_edge(
    data = h3_annotation,
    aes(x = x, y = y, label = label, hjust = hjust),
    inherit.aes = FALSE, parse = TRUE, vjust = .5,
    size = 4.8, family = "Arial", colour = "black"
  ) +
  labs(
    subtitle = "(c) Duration ~ onset",
    x = "Flowering onset (days)\n", y = "Flowering duration (days)\n"
  ) +
  theme_manuscript(12) +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
    axis.title = element_text(colour = "black", size = 13, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 13, face = "bold"),
    plot.margin = margin(t = 1, r = 4, b = 1, l = 2, unit = "mm")
  )
p_h3 <- reproductive_panel_strip(p_h3)
save_figure(p_h3, "Figure_H3_duration_onset", 150, 120)

# Plant-level H3 coefficient, with block/plot random intercepts.
h3_coefficient_labels <- h3_decomposition_results |>
  mutate(
    Component = factor(Model,
      levels = c("among_plot", "within_plot"),
      labels = c("Among plots", "Within plot")
    ),
    Annotation = paste0(
      "\u03b2 = ", sprintf("%.3f", Estimate),
      if_else(P_value < .001, " ***", if_else(P_value < .01, " **", if_else(P_value < .05, " *", " ns"))),
      "\nCI: [", sprintf("%.3f", CI_low), ", ",
      sprintf("%.3f", CI_high), "]"
    )
  )
p_h3_coefficient <- ggplot(h3_coefficient_labels, aes(Estimate, as.numeric(Component))) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high),
    orientation = "y", width = .20, linewidth = 1,
    colour = "#1ABC9C"
  ) +
  geom_point(size = 4, colour = "#1ABC9C") +
  geom_text(aes(x = Estimate, y = as.numeric(Component) + .22, label = Annotation),
    vjust = 0, size = 4.8, family = "Arial",
    colour = "black", lineheight = 1.15
  ) +
  scale_y_continuous(
    breaks = c(1, 2), labels = c("Among plots", "Within plot"),
    limits = c(.5, 3.1), expand = expansion(mult = 0)
  ) +
  scale_x_continuous(expand = expansion(mult = c(.75, .75))) +
  labs(
    subtitle = "(d) Final flowering date ~ onset (within and among plots)",
    x = "Final-date onset coefficient (days/day)\n", y = NULL
  ) +
  theme_manuscript() +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 13, face = "bold"),
    axis.title = element_text(colour = "black", size = 13, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 13, face = "bold"),
    plot.margin = margin(t = 1, r = 4, b = 1, l = 3, unit = "mm")
  )
p_h3_coefficient <- reproductive_panel_strip(p_h3_coefficient)
p_h3_coefficient$data$.panel_heading <- "(d) Final flowering date\n~ onset (within/among plots)"
save_figure(p_h3_coefficient, "Figure_H3_plant_level_coefficient", 150, 120)
saveRDS(
  list(p_h1 = p_h1, p_h2 = p_h2, p_h3 = p_h3, p_h3_coefficient = p_h3_coefficient),
  file.path(model_dir_fp, "Figure_objects_H1_H3.rds")
)
message("H1-H3 flowering-phenology analysis complete. Outputs: ", output_dir_fp)

# 15. Publication-ready gt tables --------------------------------------------

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
  paste0(analysis_label, " â€” ", type)
}

publication_column_labels <- function(columns) {
  labels <- c(
    logLik = "Log-likelihood", AIC = "AIC", AICc = "AICc",
    Delta_AICc = "Î”AICc", Akaike_weight = "Akaike weight",
    Positive_definite_Hessian = "Positive-definite Hessian",
    CI_low = "Lower 95% CI", CI_high = "Upper 95% CI",
    P_value = "P value", P_Value = "P value", p_value = "P value",
    Dispersion_P = "Dispersion P", Zero_inflation_P = "Zero-inflation P",
    Outlier_P = "Outlier P", Marginal_R2 = "RÂ²m",
    Conditional_R2 = "RÂ²c", R_squared = "RÂ²",
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
      ifelse(is.na(x), "â€”", ifelse(x < .001, "<0.001", sprintf("%.3f", x)))
    }
    table <- gt::fmt(table,
      columns = tidyselect::all_of(p_columns),
      fns = p_formatter
    )
  }

  table <- table |>
    gt::sub_missing(columns = gt::everything(), missing_text = "â€”") |>
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
            "*Note:* logLik, AIC, AICc and Î”AICc are displayed to one decimal place; Akaike weights to three decimal places. "
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
        footnote = "Lower Î”AICc indicates greater relative support within the candidate set.",
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

# Check the onsetâ€“duration measurement relationship without changing model data.
flowering_calendar_start <- as.Date("2021-06-01")
flowering_calendar_date <- function(day) flowering_calendar_start + day - 1
flowering_date_check_data <- individual |>
  transmute(PlotID, Onset, Duration, FinalDate,
    Onset_calendar_date = flowering_calendar_date(Onset),
    Final_flowering_calendar_date = flowering_calendar_date(FinalDate)
  )
flowering_date_checks <- data.frame(
  N = nrow(individual), Day_1_calendar_date = as.character(flowering_calendar_start),
  Final_day_min = min(individual$FinalDate, na.rm = TRUE),
  Final_day_max = max(individual$FinalDate, na.rm = TRUE),
  Earliest_final_flowering_date = as.character(min(flowering_date_check_data$Final_flowering_calendar_date, na.rm = TRUE)),
  Latest_final_flowering_date = as.character(max(flowering_date_check_data$Final_flowering_calendar_date, na.rm = TRUE))
)
utils::write.csv(flowering_date_checks, file.path(
  table_dir_fp,
  "Table_H3_flowering_date_checks.csv"
), row.names = FALSE)
# Latest observed flowering date does not independently establish monitoring end.

# Reproducible environmental summaries use the 27 measured plot records.
write.csv(plant_environment, file.path(table_dir_fp, "Table_plot_environment.csv"), row.names = FALSE)
write.csv(bind_rows(lapply(c("Temperature", "Precipitation", "SoilPC1_z"), function(response) {
  fit <- lm(reformulate("Elevation", response), plant_environment)
  tab <- coef(summary(fit))
  ci <- confint(fit)[2, ]
  test <- cor.test(plant_environment$Elevation, plant_environment[[response]], method = "spearman", exact = FALSE)
  data.frame(
    Response = response, N_plots = nrow(plant_environment),
    Minimum = min(plant_environment[[response]]), Maximum = max(plant_environment[[response]]),
    Estimate_per_100m = coef(fit)[2] * 100, CI_low = ci[1] * 100, CI_high = ci[2] * 100,
    P_value = tab[2, 4], R_squared = summary(fit)$r.squared,
    Spearman_rho = unname(test$estimate), Spearman_P = test$p.value
  )
})), file.path(table_dir_fp, "Table_environment_gradient_statistics.csv"), row.names = FALSE)

# Complementary completion-timing analysis; use the same covariates as H3.
# For identical linear model designs, beta_duration = beta_final_date - 1.
# This re-expression cannot establish causality or remove onset measurement error.
completion_models <- lapply(h3_models, function(m) {
  f <- formula(m)
  f[[2]] <- as.name("FinalDate")
  fit_ml(f)
})
completion_results <- bind_rows(lapply(names(completion_models), function(nm) {
  out <- extract_term(
    completion_models[[nm]], "Onset_centered",
    "Completion timing", nm
  )
  out$P_slope_equals_1 <- 2 * pnorm(abs((out$Estimate - 1) / out$SE), lower.tail = FALSE)
  out$Implied_duration_slope <- out$Estimate - 1
  out$Implied_duration_CI_low <- out$CI_low - 1
  out$Implied_duration_CI_high <- out$CI_high - 1
  out
}))
write.csv(completion_results, file.path(
  table_dir_fp,
  "Table_final_flowering_date_onset_models.csv"
), row.names = FALSE)
write.csv(
  individual |> select(Plant, PlotID, Onset, FinalDate, Duration) |>
    mutate(
      Onset_calendar_date = flowering_calendar_date(Onset),
      Final_flowering_calendar_date = flowering_calendar_date(FinalDate)
    ),
  file.path(table_dir_fp, "Table_plant_flowering_dates_and_derived_duration.csv"),
  row.names = FALSE
)
saveRDS(completion_models, file.path(model_dir_fp, "final_flowering_date_onset_models.rds"))
# Match the H1 panel: fixed-effect confidence band, black line, green
# jittered points, parsed model statistics, grey strip and export dimensions.
completion_grid <- fixed_prediction(
  completion_models$onset,
  data.frame(
    Onset = seq(min(model_data$Onset), max(model_data$Onset), length.out = 200),
    Onset_centered = seq(min(model_data$Onset), max(model_data$Onset), length.out = 200) -
      mean(individual$Onset, na.rm = TRUE)
  )
)
completion_row <- completion_results |> filter(Model == "onset")
completion_y_limits <- range(c(
  model_data$FinalDate, completion_grid$CI_low,
  completion_grid$CI_high
), na.rm = TRUE)
completion_y_limits <- c(
  floor(completion_y_limits[1]) - 1,
  ceiling(completion_y_limits[2]) + 1
)
completion_annotation <- model_annotation_data(
  completion_row,
  range(model_data$Onset), completion_y_limits, "right", "bottom"
)
# Keep statistics below the observed points, with space above the lower border.
completion_annotation$y <- completion_y_limits[1] + diff(completion_y_limits) *
  c(.44, .35, .26, .17, .08)
p_completion <- ggplot(model_data, aes(Onset, FinalDate)) +
  geom_ribbon(
    data = completion_grid, aes(x = Onset, ymin = CI_low, ymax = CI_high),
    inherit.aes = FALSE, fill = "#E5E5E5", alpha = .6
  ) +
  geom_line(
    data = completion_grid, aes(x = Onset, y = Fitted), linewidth = .8,
    inherit.aes = FALSE, colour = "black",
    linetype = ifelse(completion_row$P_value < .05, "solid", "dashed")
  ) +
  geom_point(
    shape = 21, fill = "#2CB89A", colour = "black", size = 1.5,
    alpha = .55, stroke = .25,
    position = position_jitter(width = .15, height = .24, seed = 12191)
  ) +
  scale_y_continuous(breaks = pretty(completion_y_limits, n = 5)) +
  scale_x_continuous(breaks = seq(5, 25, by = 5)) +
  coord_cartesian(xlim = c(5, 25), ylim = completion_y_limits) +
  geom_text_edge(
    data = completion_annotation,
    aes(x = x, y = y, label = label, hjust = hjust),
    inherit.aes = FALSE, parse = TRUE, vjust = .5,
    size = 4.8, family = "Arial", colour = "black"
  ) +
  labs(
    subtitle = "Final flowering date ~ onset",
    x = "Flowering onset (days)\n",
    y = "Final flowering day (days)\n"
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
p_completion <- reproductive_panel_strip(p_completion)
save_figure(p_completion, "Figure_final_flowering_date_vs_onset", 150, 120)
writeLines(
  c(
    "Line and grey 95% confidence band: mixed-model fixed effects, with block and plot intercepts set to zero.",
    "Points show individual plants with display jitter. P tests the onset slope against zero.",
    "Day 1 = 1 June 2021. The slope-equals-one test is reported in the completion-model table."
  ),
  file.path(figure_dir_fp, "Figure_final_flowering_date_vs_onset_caption.txt")
)

# Completion timing is panel (c) in the combined H1-H6 figure.
p_h3 <- p_completion
p_h3$data$.panel_heading <- "(c) Final flowering date ~ onset\n"
saveRDS(
  list(
    p_h1 = p_h1, p_h2 = p_h2, p_h3 = p_h3,
    p_completion = p_completion, p_h3_coefficient = p_h3_coefficient
  ),
  file.path(model_dir_fp, "Figure_objects_H1_H3.rds")
)

# Primary H3 publication tables use final date; duration estimates remain derivable.

publication_flowering_tables()

# Combine all CSV tables from this analysis into one multi-sheet workbook.
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Install the openxlsx package to export the combined table workbook.")
}
table_csv_files <- list.files(table_dir_fp, pattern = "\\.csv$", full.names = TRUE)
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
    file = file.path(table_dir_fp, "summary_tables_H1_H3_flowering_phenology.xlsx"),
    overwrite = TRUE
  )
  message(
    "Combined table workbook written: ",
    file.path(table_dir_fp, "summary_tables_H1_H3_flowering_phenology.xlsx")
  )
}

render_gt_tables(table_dir_fp, "Flowering phenology (H1-H3)")

# Canonical manuscript figure numbers and caption sidecars.
publication_figure_exports()
