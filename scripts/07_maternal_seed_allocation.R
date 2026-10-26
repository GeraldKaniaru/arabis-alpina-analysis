# 1. Local publication support functions -------------------------------------

# Local definitions: this script never sources another R file.
publication_root <- "outputs"
publication_captions <- c(
  Table_1 = "Table 1. Mixed-effects estimates for H1-H6 and within-plot and among-plot associations (581 maternal plants). Elevation effects are per 100 m. All H3 estimates, intervals and R-squared values describe final flowering date. Subtract one from H3 coefficients and confidence limits for the corresponding duration association. For H3, P_zero tests a coefficient against zero and P_duration tests it against one; the P column tests zero for all rows. The primary H3 final-date slope is 0.947 (95% CI 0.892-1.002); P_duration = 0.059. H4 uses natural-log MISM; H5-H6 use a log link. MISM is mean individual seed mass (mg per seed). Marginal_R2 and Conditional_R2 denote marginal and conditional R-squared.",
  Table_S1 = "Table S1. Candidate-model comparisons for H1-H6 and seed-mass-germination models. Each comparison uses the same 581 maternal plants and random-effects structure. H1, H2, H4 and H5 each compare elevation alone, elevation + climate, elevation + soil, and elevation + climate + soil. Climate includes temperature and precipitation jointly; soil is soil PC1. H3 compares onset alone, onset + elevation, onset + elevation + climate, onset + elevation + soil, and onset + elevation + climate + soil. H3 AICc values were calculated from duration models and are equivalent for final flowering date with the same observations, onset predictor, covariates, random effects, maximum-likelihood fitting and residual covariance model. H3 coefficient tables and Figure 4 use final flowering date. H6 has one MISM candidate, not an environmental covariate comparison. H8 and germination time spread compare unadjusted and elevation-adjusted 100-seed-mass models within response. H7 is omitted. K is the parameter count; AICc is small-sample-corrected AIC; Delta_AICc and Akaike_weight quantify relative support within a candidate set.",
  Figure_4 = "Figure 4. Elevational associations and trait relationships in Arabis alpina. Panels: (a) elevation and flowering onset; (b) elevation and duration; (c) onset and final flowering date; (d) within-plot and among-plot onset coefficients for final flowering date; (e) elevation and natural-log MISM; (f) elevation and seed number; (g) MISM and seed number; (h) within-plot and among-plot mass-number coefficients. In c and d, coefficients describe final flowering date. Tests against zero describe completion timing; comparison with one addresses duration. The P value in c tests against zero; the primary H3 test against one gives P = 0.059. Lines and confidence bands show fixed-effect predictions; points in regression panels represent plants. Bars in d and h show 95% confidence intervals; dashed vertical lines mark zero. MISM is in mg per seed; f-h use a log link. Flowering day 1 is 1 June 2021. R2m and R2c denote marginal and conditional R-squared."
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
publication_table_1 <- function() {
  td <- file.path(publication_root, "maternal_seed_allocation", "tables")
  f <- file.path(td, "Table_1.csv")
  if (!file.exists(f)) {
    return(invisible(NULL))
  }
  t <- utils::read.csv(f, check.names = FALSE)
  fp <- file.path(publication_root, "flowering_phenology", "tables")
  source <- dplyr::bind_rows(
    utils::read.csv(file.path(fp, "Table_H1_H3_primary_coefficients.csv")),
    utils::read.csv(file.path(fp, "Table_H3_within_among_coefficients.csv"))
  )
  for (j in seq_len(nrow(source))) {
    k <- which(t$Hypothesis == source$Hypothesis[j] & t$Model == source$Model[j])
    stopifnot(length(k) == 1)
    for (nm in intersect(names(t), names(source))) t[k, nm] <- source[j, nm]
    pv <- source$P_value[j]
    t$P[k] <- if (pv < .001) "< 0.001" else sprintf("%.3f", pv)
  }
  t$Response <- publication_response(t$Hypothesis)
  if (!"P_zero" %in% names(t)) t$P_zero <- NA_real_
  if (!"P_duration" %in% names(t)) t$P_duration <- NA_real_
  for (j in seq_len(nrow(source))) {
    k <- which(t$Hypothesis == source$Hypothesis[j] & t$Model == source$Model[j])
    for (nm in c("P_zero", "P_duration", "Null_for_duration", "Implied_duration_slope", "Implied_duration_CI_low", "Implied_duration_CI_high")) {
      if (!nm %in% names(t)) t[[nm]] <- NA_real_
      t[k, nm] <- source[j, nm]
    }
  }
  # Existing P remains a test against zero; H3 duration tests are explicitly separate.
  t <- t[, c("Table_section", "Hypothesis", "Response", setdiff(names(t), c("Table_section", "Hypothesis", "Response")))]
  utils::write.csv(t, f, row.names = FALSE)
  writeLines(publication_captions[["Table_1"]], file.path(td, "Table_1_caption.txt"))
  invisible(t)
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
publication_render_tables <- function() {
  fp <- file.path(publication_root, "flowering_phenology", "tables")
  msa <- file.path(publication_root, "maternal_seed_allocation", "tables")
  ge <- file.path(publication_root, "germination_elevation", "tables")
  files <- file.path(publication_root, c("maternal_seed_allocation/tables/Table_1.csv", "maternal_seed_allocation/tables/Table_S1.csv"))
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
  from <- file.path(publication_root, "maternal_seed_allocation/figures/Figure_H1_H6_flowering_seed_allocation")
  to <- file.path(dirname(from), "Figure_4_flowering_seed_allocation")
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
      records[[length(records) + 1L]] <- data.frame(Number = "Figure 4", Format = ext, File = output)
    }
  }
  if (!length(records)) {
    return(invisible(NULL))
  }
  writeLines(publication_captions[["Figure_4"]], paste0(to, "_caption.txt"))
  writeLines(publication_captions[["Figure_4"]], paste0(from, "_caption.txt"))
  manifest_file <- file.path(publication_root, "Publication_figure_manifest.csv")
  updated <- do.call(rbind, records)
  if (file.exists(manifest_file)) {
    previous <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)
    previous <- previous[previous$Number != "Figure 4", , drop = FALSE]
    updated <- rbind(previous[, names(updated), drop = FALSE], updated)
  }
  utils::write.csv(updated, manifest_file, row.names = FALSE)
  invisible(updated)
}
# END INLINE PUBLICATION SUPPORT

# ============================================================================

# Script 07: MATERNAL SEED ALLOCATION ACROSS ELEVATION (H4-H6)

# Implements manuscript Section 2.8. Run from the "arabis analysis" root.

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
  "readxl", "dplyr", "tidyr", "ggplot2", "ggpubr", "lme4", "glmmTMB",
  "DHARMa", "performance", "MuMIn", "patchwork", "ggtext", "cowplot", "gridExtra"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggpubr)
  library(lme4)
  library(glmmTMB)
  library(patchwork)
})
if (!file.exists("arabis analysis.Rproj")) {
  stop("Run this script from the 'arabis analysis' project root.")
}
set.seed(12191)
input_dir_msa <- file.path("data", "raw", "Datasets")
output_dir_msa <- file.path("outputs", "maternal_seed_allocation")
figure_dir_msa <- file.path(output_dir_msa, "figures")
table_dir_msa <- file.path(output_dir_msa, "tables")
model_dir_msa <- file.path(output_dir_msa, "models")
diagnostic_dir_msa <- file.path(output_dir_msa, "diagnostics")
invisible(lapply(c(figure_dir_msa, table_dir_msa, model_dir_msa, diagnostic_dir_msa), dir.create,
  recursive = TRUE, showWarnings = FALSE
))

# Standardise reported precision without reducing precision used in analyses.
write.csv <- function(x, file = "", ..., row.names = TRUE) {
  x <- as.data.frame(x)
  x[] <- Map(function(v, nm) if (is.numeric(v)) round(v, if (grepl("Estimate|^SE$|CI_low|CI_high", nm)) 6 else 3) else v, x, names(x))
  utils::write.csv(x, file = file, ..., row.names = row.names)
}
theme_manuscript <- function(base_size = 11) {
  theme_bw(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(family = "Arial", colour = "black"),
      axis.title = element_text(face = "bold", size = 13),
      axis.text = element_text(colour = "black", size = 12),
      plot.subtitle = element_text(face = "bold", hjust = 0, size = 13),
      panel.grid.minor = element_blank(), legend.position = "bottom"
    )
}
save_figure <- function(plot, stem, width = 160, height = 125,
                        directory = figure_dir_msa) {
  ggsave(file.path(directory, paste0(stem, ".png")), plot,
    width = width,
    height = height, units = "mm", dpi = 600, bg = "white"
  )
  ggsave(file.path(directory, paste0(stem, ".pdf")), plot,
    width = width,
    height = height, units = "mm", device = cairo_pdf, bg = "white"
  )
}

# 5. Data preparation --------------------------------------------------------

raw_traits <- read_excel(
  file.path(input_dir_msa, "trait_data.xlsx"),
  sheet = "Sheet2",
  .name_repair = function(x) make.unique(x, sep = "_duplicate")
)
individual <- raw_traits |>
  transmute(
    Plant = plant_no, Block = factor(Block), Plot = as.integer(Plot),
    Elevation = as.numeric(Elevation),
    PlotID = paste(Block, Plot, Elevation, sep = "_"),
    MISM = as.numeric(MISM),
    SeedNumber = as.numeric(TotalSeedNumber),
    EC = as.numeric(Electrical.Conductivity), pH = as.numeric(pH),
    Carbon = as.numeric(Carbon), Nitrogen = as.numeric(Nitrogen),
    Phosphorus = as.numeric(Phosphorus)
  )
climate <- read_excel(
  file.path(input_dir_msa, "plot_abiotic_factors.xlsx"),
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
  mutate(
    LogMISM = log(MISM),
    MISM_centered = MISM - mean(MISM, na.rm = TRUE)
  )
if (any(individual$MISM <= 0, na.rm = TRUE)) {
  stop("MISM contains zero or negative values and cannot be log transformed.")
}
if (any(individual$SeedNumber < 0, na.rm = TRUE) ||
  any(individual$SeedNumber %% 1 != 0, na.rm = TRUE)) {
  stop("SeedNumber must contain non-negative integer counts.")
}

# 7. Seed-mass distribution and skewness (diagnostic figure) -----------------

adjusted_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L || sd(x) == 0) {
    return(NA_real_)
  }
  z <- (x - mean(x)) / sd(x)
  n / ((n - 1) * (n - 2)) * sum(z^3)
}
mass_distribution <- bind_rows(
  data.frame(Scale = "Original MISM", Value = individual$MISM),
  data.frame(Scale = "Log-transformed MISM", Value = individual$LogMISM)
) |> filter(is.finite(Value))
mass_skewness <- mass_distribution |>
  group_by(Scale) |>
  summarise(
    N = n(), Mean = mean(Value), SD = sd(Value),
    Median = median(Value), Skewness = adjusted_skewness(Value),
    Shapiro_Wilk_P = shapiro.test(Value)$p.value, .groups = "drop"
  )
write.csv(mass_skewness, file.path(table_dir_msa, "Table_H4_seed_mass_skewness.csv"),
  row.names = FALSE
)
mass_distribution_plot <- ggplot(mass_distribution, aes(Value)) +
  geom_histogram(aes(y = after_stat(density)),
    bins = 25, fill = "#CEAB07",
    colour = "white", linewidth = .3
  ) +
  geom_density(colour = "black", linewidth = .7) +
  facet_wrap(~Scale, scales = "free", ncol = 2) +
  labs(x = NULL, y = "Density", subtitle = "MISM distributions") +
  theme_manuscript() +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
    axis.title = element_text(colour = "black", size = 13, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 13, face = "bold"),
    plot.margin = margin(t = 1, r = 4, b = 1, l = 2, unit = "mm")
  )
save_figure(mass_distribution_plot, "Figure_seed_mass_distribution", 210, 100)

# 8. Helpers -----------------------------------------------------------------

aicc_table <- function(models) {
  out <- bind_rows(lapply(names(models), function(name) {
    model <- models[[name]]
    k <- attr(logLik(model), "df")
    n <- nobs(model)
    aic <- AIC(model)
    is_tmb <- inherits(model, "glmmTMB")
    converged <- if (is_tmb) {
      isTRUE(model$fit$convergence == 0)
    } else {
      isTRUE(model@optinfo$conv$opt == 0)
    }
    pd_hessian <- if (is_tmb) isTRUE(model$sdr$pdHess) else TRUE
    singular <- if (is_tmb) NA else lme4::isSingular(model, tol = 1e-4)
    data.frame(
      Model = name, N = n, K = k, logLik = as.numeric(logLik(model)),
      AIC = aic, AICc = aic + 2 * k * (k + 1) / (n - k - 1),
      Converged = converged, Positive_definite_Hessian = pd_hessian,
      Singular = singular
    )
  })) |>
    arrange(AICc) |>
    mutate(
      Delta_AICc = AICc - min(AICc, na.rm = TRUE),
      Akaike_weight = exp(-.5 * Delta_AICc) / sum(exp(-.5 * Delta_AICc), na.rm = TRUE)
    )
  if (length(unique(out$N)) != 1L) stop("Candidate models use different observations.")
  out
}
extract_lmer <- function(model, term, hypothesis, model_name) {
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
    Coefficient_unit = if (term == "Elevation_z") "per 100-m elevation increase" else "per 1-mg MISM increase",
    Estimate = estimate, SE = se, CI_low = ci[1], CI_high = ci[2],
    z_approximation = statistic,
    P_value = 2 * pnorm(abs(statistic), lower.tail = FALSE),
    IRR = NA_real_, IRR_low = NA_real_, IRR_high = NA_real_,
    Marginal_R2 = model_r2(model)[["Marginal"]], Conditional_R2 = model_r2(model)[["Conditional"]], N = nobs(model),
    Singular = isSingular(model, tol = 1e-4)
  )
}
extract_glmm <- function(model, term, hypothesis, model_name) {
  tab <- summary(model)$coefficients$cond
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
    Coefficient_unit = if (term == "Elevation_z") "per 100-m elevation increase" else "per 1-mg MISM increase",
    Estimate = estimate, SE = se, CI_low = ci[1], CI_high = ci[2],
    z_value = statistic, P_value = 2 * pnorm(abs(statistic), lower.tail = FALSE),
    IRR = exp(estimate), IRR_low = exp(ci[1]), IRR_high = exp(ci[2]),
    Marginal_R2 = model_r2(model)[["Marginal"]], Conditional_R2 = model_r2(model)[["Conditional"]], N = nobs(model)
  )
}
family_object <- function(name) {
  switch(name,
    Poisson = poisson(link = "log"),
    nbinom1 = glmmTMB::nbinom1(link = "log"),
    nbinom2 = glmmTMB::nbinom2(link = "log"),
    stop("Unknown count family: ", name)
  )
}
fit_distribution_set <- function(formula, data, hypothesis) {
  message(hypothesis, ": fitting count distributions")
  models <- if (hypothesis == "H5" && !is.null(h5_cache)) {
    h5_cache$H5_distribution_models
  } else {
    list(
      Poisson = glmmTMB(formula, data = data, family = poisson(link = "log")),
      nbinom1 = glmmTMB(formula, data = data, family = nbinom1(link = "log")),
      nbinom2 = glmmTMB(formula, data = data, family = nbinom2(link = "log"))
    )
  }
  selection <- aicc_table(models)
  diagnostics <- bind_rows(lapply(names(models), function(name) {
    message(hypothesis, ": residual diagnostics for ", name)
    simulation <- DHARMa::simulateResiduals(models[[name]],
      n = 1000,
      plot = FALSE, seed = 12191
    )
    data.frame(
      Hypothesis = hypothesis, Distribution = name,
      Dispersion_statistic = DHARMa::testDispersion(simulation, plot = FALSE)$statistic,
      Dispersion_P = DHARMa::testDispersion(simulation, plot = FALSE)$p.value,
      Zero_inflation_P = DHARMa::testZeroInflation(simulation, plot = FALSE)$p.value,
      Outlier_P = DHARMa::testOutliers(simulation, type = "bootstrap", plot = FALSE)$p.value
    )
  }))
  ranked <- selection |>
    mutate(Finite_fixed_effect_SE = vapply(models[Model], function(m) {
      all(is.finite(summary(m)$coefficients$cond[, "Std. Error"]))
    }, logical(1))) |>
    left_join(diagnostics, by = c("Model" = "Distribution")) |>
    arrange(AICc)
  acceptable <- ranked |>
    filter(Converged, Positive_definite_Hessian, Finite_fixed_effect_SE, Dispersion_P >= .05)
  if (!nrow(acceptable)) {
    stop(hypothesis, ": no count distribution converged with an acceptable Hessian and dispersion test.")
  }
  selected <- acceptable$Model[1]
  list(
    models = models, table = ranked, selected = selected,
    simulation = DHARMa::simulateResiduals(models[[selected]],
      n = 1000,
      plot = FALSE, seed = 12191
    )
  )
}

# 9. H4: log seed mass versus elevation --------------------------------------

h4_data <- individual |>
  filter(complete.cases(
    LogMISM, Elevation_z, Temperature_z,
    Precipitation_z, SoilPC1_z, Block, PlotID
  ))
h4_models <- list(
  elevation = lmer(LogMISM ~ Elevation_z + (1 | Block) + (1 | PlotID),
    data = h4_data, REML = FALSE
  ),
  elevation_climate = lmer(
    LogMISM ~ Elevation_z + Temperature_z +
      Precipitation_z + (1 | Block) + (1 | PlotID),
    data = h4_data, REML = FALSE
  ),
  elevation_soil = lmer(LogMISM ~ Elevation_z + SoilPC1_z +
    (1 | Block) + (1 | PlotID), data = h4_data, REML = FALSE),
  elevation_climate_soil = lmer(
    LogMISM ~ Elevation_z + Temperature_z +
      Precipitation_z + SoilPC1_z +
      (1 | Block) + (1 | PlotID),
    data = h4_data, REML = FALSE
  )
)
h4_selection <- aicc_table(h4_models)
message("H4: extracting model estimates")
h4_result <- extract_lmer(h4_models$elevation, "Elevation_z", "H4", "elevation")
message("H4: estimates extracted")

# 10. H5: total seed number versus elevation ---------------------------------

h5_data <- individual |>
  filter(complete.cases(
    SeedNumber, Elevation_z, Temperature_z,
    Precipitation_z, SoilPC1_z, Block, PlotID
  ))

# Optional reuse is restricted to unchanged seed-number model frames.
h5_cache <- NULL
h5_cache_file <- Sys.getenv("H5_MODEL_CACHE", "")
if (nzchar(h5_cache_file)) {
  candidate_cache <- readRDS(h5_cache_file)
  canonical_frame <- function(x) {
    x <- as.data.frame(x)
    for (k in names(x)) if (is.factor(x[[k]])) x[[k]] <- as.character(x[[k]])
    x <- x[do.call(order, x), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  for (fit in c(candidate_cache$H5, candidate_cache$H5_distribution_models)) {
    frame <- model.frame(fit)
    if (!isTRUE(all.equal(canonical_frame(frame), canonical_frame(h5_data[, names(frame)]),
      tolerance = 1e-12, check.attributes = FALSE
    ))) {
      stop("H5 cache does not match current seed-number data and predictors.")
    }
  }
  h5_cache <- candidate_cache
  message("H5: verified identical model data; reusing saved fits and rerunning diagnostics")
}
h5_distribution <- fit_distribution_set(
  SeedNumber ~ Elevation_z + (1 | Block) + (1 | PlotID), h5_data, "H5"
)
h5_family <- family_object(h5_distribution$selected)
h5_models <- if (!is.null(h5_cache)) {
  h5_cache$H5
} else {
  list(
    elevation = glmmTMB(SeedNumber ~ Elevation_z + (1 | Block) + (1 | PlotID),
      data = h5_data, family = h5_family
    ),
    elevation_climate = glmmTMB(
      SeedNumber ~ Elevation_z + Temperature_z +
        Precipitation_z + (1 | Block) + (1 | PlotID),
      data = h5_data, family = h5_family
    ),
    elevation_soil = glmmTMB(
      SeedNumber ~ Elevation_z + SoilPC1_z +
        (1 | Block) + (1 | PlotID),
      data = h5_data, family = h5_family
    ),
    elevation_climate_soil = glmmTMB(
      SeedNumber ~ Elevation_z + Temperature_z +
        Precipitation_z + SoilPC1_z +
        (1 | Block) + (1 | PlotID),
      data = h5_data, family = h5_family
    )
  )
}
h5_selection <- aicc_table(h5_models)
h5_result <- extract_glmm(h5_models$elevation, "Elevation_z", "H5", "elevation")

# 11. H6: total seed number versus mean-centred seed mass --------------------

h6_data <- individual |>
  filter(complete.cases(
    SeedNumber, MISM_centered, Elevation_z,
    Temperature_z, Precipitation_z, SoilPC1_z, Block, PlotID
  ))
# MISM is measured in mg per seed; fit and report coefficients per 1 mg.
h6_primary_fit_data <- h6_data
h6_distribution <- fit_distribution_set(
  SeedNumber ~ MISM_centered + (1 | Block) + (1 | PlotID), h6_primary_fit_data, "H6"
)
h6_family <- family_object(h6_distribution$selected)
h6_models <- list(seed_mass = glmmTMB(SeedNumber ~ MISM_centered +
  (1 | Block) + (1 | PlotID), data = h6_primary_fit_data, family = h6_family))
if (!isTRUE(h6_models$seed_mass$sdr$pdHess) || h6_models$seed_mass$fit$convergence != 0 ||
  any(!is.finite(summary(h6_models$seed_mass)$coefficients$cond[, "Std. Error"]))) {
  stop("H6 primary model failed convergence or Hessian checks; inference is unavailable.")
}

# Within/among components remain covariates in a plant-level count model.
h6_data <- h6_data |>
  group_by(PlotID) |>
  mutate(
    MISM_plot_mean = mean(MISM),
    MISM_within = MISM - MISM_plot_mean
  ) |>
  ungroup() |>
  mutate(MISM_among = MISM_plot_mean - mean(MISM))
# Fit within- and among-plot MISM effects directly in mg per seed.
h6_fit_data <- h6_data
h6_decomposition <- glmmTMB(SeedNumber ~ MISM_within + MISM_among +
  (1 | Block) + (1 | PlotID), data = h6_fit_data, family = h6_family)
if (!isTRUE(h6_decomposition$sdr$pdHess) || h6_decomposition$fit$convergence != 0) {
  stop("H6 decomposition failed convergence or Hessian checks.")
}
h6_decomposition_results <- bind_rows(
  extract_glmm(h6_decomposition, "MISM_within", "H6", "within_plot"),
  extract_glmm(h6_decomposition, "MISM_among", "H6", "among_plot")
) |>
  mutate(IRR = exp(Estimate), IRR_low = exp(CI_low), IRR_high = exp(CI_high))
write.csv(h6_decomposition_results, file.path(
  table_dir_msa,
  "Table_H6_within_among_coefficients.csv"
), row.names = FALSE)
message("H6: primary candidate models fitted")
h6_selection <- aicc_table(h6_models)
h6_result <- extract_glmm(h6_models$seed_mass, "MISM_centered", "H6", "seed_mass") |>
  mutate(IRR = exp(Estimate), IRR_low = exp(CI_low), IRR_high = exp(CI_high))

# Calculate Nakagawa marginal and conditional R2. Explicit null models are

# supplied for glmmTMB fits because their stored calls refer to family objects

# created in this script and cannot otherwise be reconstructed reliably.
extract_nakagawa_r2 <- function(model, null_model = NULL) {
  values <- suppressWarnings(MuMIn::r.squaredGLMM(model, null = null_model))
  if (is.matrix(values)) {
    row <- if (!is.null(rownames(values)) && "delta" %in% rownames(values)) "delta" else 1L
    return(c(Marginal = unname(values[row, "R2m"]), Conditional = unname(values[row, "R2c"])))
  }
  c(Marginal = unname(values[1]), Conditional = unname(values[2]))
}
h4_r2 <- extract_nakagawa_r2(h4_models$elevation)
message("H5-H6: calculating model R-squared")
h5_null <- glmmTMB(
  SeedNumber ~ 1 + (1 | Block) + (1 | PlotID),
  data = h5_data, family = h5_family
)
h6_null <- glmmTMB(
  SeedNumber ~ 1 + (1 | Block) + (1 | PlotID),
  data = h6_data, family = h6_family
)
h5_r2 <- extract_nakagawa_r2(h5_models$elevation, h5_null)
h6_r2 <- extract_nakagawa_r2(h6_models$seed_mass, h6_null)
h4_result$Marginal_R2 <- h4_r2[["Marginal"]]
h4_result$Conditional_R2 <- h4_r2[["Conditional"]]
h5_result$Marginal_R2 <- h5_r2[["Marginal"]]
h5_result$Conditional_R2 <- h5_r2[["Conditional"]]
h6_result$Marginal_R2 <- h6_r2[["Marginal"]]
h6_result$Conditional_R2 <- h6_r2[["Conditional"]]

# Convert log-scale coefficients to percentage changes on interpretable

# predictor increments. Elevation_z is based on the plant-level elevation SD.
elevation_sd_m <- sd(plant_environment$Elevation, na.rm = TRUE)
primary_results <- bind_rows(h4_result, h5_result, h6_result) |>
  mutate(
    Percent_change_per_100m_elevation_increase = case_when(
      Hypothesis %in% c("H4", "H5") ~
        100 * expm1(Estimate),
      TRUE ~ NA_real_
    ),
    Percent_change_in_seed_number_per_1mg_seed_mass_decrease = case_when(
      Hypothesis == "H6" ~ 100 * (exp(-Estimate) - 1),
      TRUE ~ NA_real_
    )
  )

# 12. Tables and saved models ------------------------------------------------

write.csv(
  bind_rows(
    mutate(h4_selection, Hypothesis = "H4"),
    mutate(h5_selection, Hypothesis = "H5"),
    mutate(h6_selection, Hypothesis = "H6")
  ) |>
    select(Hypothesis, everything()),
  file.path(table_dir_msa, "Table_H4_H6_AICc_model_comparison.csv"),
  row.names = FALSE
)
write.csv(bind_rows(h5_distribution$table, h6_distribution$table),
  file.path(table_dir_msa, "Table_H5_H6_count_distribution_comparison.csv"),
  row.names = FALSE
)
write.csv(primary_results,
  file.path(table_dir_msa, "Table_H4_H6_primary_coefficients.csv"),
  row.names = FALSE
)

# 13. Publication-ready gt tables --------------------------------------------

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
      ifelse(is.na(x), "NA", ifelse(x < .001, "<0.001", sprintf("%.3f", x)))
    }
    table <- gt::fmt(table,
      columns = tidyselect::all_of(p_columns),
      fns = p_formatter
    )
  }

  table <- table |>
    gt::sub_missing(columns = gt::everything(), missing_text = "NA") |>
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

# 14. Table S1: combine H1-H6 candidate-model comparisons --------------------

# Combine the original candidate sets without recalculating model support.
build_allocation_candidate_table_s1 <- function(flowering_file, allocation_file, output_dir) {
  if (!file.exists(flowering_file)) stop("Run script 06 first: ", flowering_file, " is missing.")
  flowering <- utils::read.csv(flowering_file, check.names = FALSE)
  allocation <- utils::read.csv(allocation_file, check.names = FALSE)
  combined <- dplyr::bind_rows(
    dplyr::mutate(flowering, Table_section = "Flowering phenology"),
    dplyr::mutate(allocation, Table_section = "Maternal seed allocation")
  ) |>
    dplyr::arrange(match(Hypothesis, paste0("H", 1:6)), Delta_AICc) |>
    dplyr::select(
      Table_section, Hypothesis, Model, N, K, logLik, AIC, AICc,
      Delta_AICc, Akaike_weight, dplyr::everything()
    )
  stopifnot(
    nrow(combined) == nrow(flowering) + nrow(allocation),
    !anyDuplicated(combined[c("Hypothesis", "Model")])
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(format_candidate_table_export(combined),
    file.path(output_dir, "Table_S1.csv"),
    row.names = FALSE
  )
  table <- build_publication_gt(combined, "S1", "Flowering phenology and maternal seed allocation (H1-H6)")
  gt_dir <- file.path(output_dir, "gt")
  dir.create(gt_dir, showWarnings = FALSE)
  gt::gtsave(table, file.path(gt_dir, "Table_S1.html"), inline_css = TRUE)
  gt::gtsave(table, file.path(gt_dir, "Table_S1.rtf"))
  message("Table S1 saved with ", nrow(combined), " H1-H6 candidate models.")
  invisible(combined)
}
# End of table s1: combine h1-h6 candidate-model comparisons.

table_s1 <- build_allocation_candidate_table_s1(
  file.path(
    "outputs", "flowering_phenology", "tables",
    "Table_H1_H3_AICc_model_comparison.csv"
  ),
  file.path(table_dir_msa, "Table_H4_H6_AICc_model_comparison.csv"),
  table_dir_msa
)

# 15. Table 1 ----------------------------------------------------------------

# H3 is reported on final-date scale, with tests against zero and one separated.

publication_flowering_tables()

# Combine the primary plant-level H1-H6 coefficients. Script 06 creates

# the flowering-phenology component of this table.
h1_h3_primary_file <- file.path(
  "outputs", "flowering_phenology", "tables",
  "Table_H1_H3_primary_coefficients.csv"
)
if (!file.exists(h1_h3_primary_file)) {
  stop(
    "Table_H1_H3_primary_coefficients.csv was not found. ",
    "Run 06_flowering_phenology.R before 07_maternal_seed_allocation.R."
  )
}
h1_h3_table_1 <- read.csv(h1_h3_primary_file, check.names = FALSE) |>
  mutate(Table_section = "Flowering phenology")
h4_h6_table_1 <- primary_results |>
  mutate(Table_section = "Maternal seed allocation")
table_1 <- bind_rows(
  h1_h3_table_1,
  h4_h6_table_1,
  mutate(read.csv(file.path("outputs", "flowering_phenology", "tables", "Table_H3_within_among_coefficients.csv")), Table_section = "Flowering phenology"),
  mutate(h6_decomposition_results, Table_section = "Maternal seed allocation")
) |>
  mutate(
    P = case_when(
      is.na(P_value) ~ NA_character_,
      P_value < 0.001 ~ "< 0.001",
      TRUE ~ sprintf("%.3f", P_value)
    ),
    Hypothesis_order = match(Hypothesis, paste0("H", 1:6)),
    Model_order = case_when(
      Hypothesis == "H6" & Model == "seed_mass" ~ 1L,
      TRUE ~ 1L
    )
  ) |>
  arrange(Hypothesis_order, Model_order) |>
  select(
    Table_section, Hypothesis, Model, Term, Estimate, SE, CI_low, CI_high,
    P, everything(), -P_value, -Hypothesis_order, -Model_order
  )
write.csv(
  table_1,
  file.path(table_dir_msa, "Table_1.csv"),
  row.names = FALSE
)
write.csv(
  data.frame(
    Hypothesis = c("H5", "H6"),
    Selected_distribution = c(
      h5_distribution$selected,
      h6_distribution$selected
    )
  ),
  file.path(table_dir_msa, "Table_H5_H6_selected_count_distributions.csv"),
  row.names = FALSE
)
saveRDS(
  list(
    H4 = h4_models, H5 = h5_models, H6 = h6_models,
    H5_distribution_models = h5_distribution$models,
    H6_distribution_models = h6_distribution$models, H6_decomposition = h6_decomposition,
    Soil_PCA = soil_pca
  ),
  file.path(model_dir_msa, "Models_H4_H6.rds")
)

# 16. Residual diagnostics ---------------------------------------------------

h4_diag_data <- data.frame(
  Fitted = fitted(h4_models$elevation),
  Residual = resid(h4_models$elevation),
  Standardized = as.numeric(scale(resid(h4_models$elevation)))
)
h4_resid <- ggplot(h4_diag_data, aes(Fitted, Standardized)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(shape = 21, fill = "#CEAB07", alpha = .6) +
  geom_smooth(method = "loess", se = FALSE, colour = "#B2182B") +
  labs(
    subtitle = "(a) H4 residuals versus fitted",
    x = "Fitted value", y = "Standardized residual"
  ) +
  theme_manuscript()
h4_qq <- ggplot(h4_diag_data, aes(sample = Standardized)) +
  stat_qq(shape = 21, fill = "#CEAB07") +
  stat_qq_line(colour = "#B2182B") +
  labs(
    subtitle = "(b) H4 normal Q-Q", x = "Theoretical quantile",
    y = "Observed quantile"
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
save_figure(
  h4_resid + h4_qq, "Figure_H4_residual_diagnostics", 220, 105,
  diagnostic_dir_msa
)
png(file.path(diagnostic_dir_msa, "Figure_H5_H6_DHARMa_diagnostics.png"),
  width = 3200, height = 1800, res = 300, family = "Arial"
)
par(mfrow = c(1, 2), family = "Arial")
plot(h5_distribution$simulation, main = paste("H5:", h5_distribution$selected))
plot(h6_distribution$simulation, main = paste("H6:", h6_distribution$selected))
dev.off()
cairo_pdf(file.path(diagnostic_dir_msa, "Figure_H5_H6_DHARMa_diagnostics.pdf"),
  width = 11, height = 6, family = "Arial"
)
par(mfrow = c(1, 2), family = "Arial")
plot(h5_distribution$simulation, main = paste("H5:", h5_distribution$selected))
plot(h6_distribution$simulation, main = paste("H6:", h6_distribution$selected))
dev.off()

# 17. Association figures ----------------------------------------------------

fixed_prediction_lmer <- function(model, newdata) {
  f <- reformulas::nobars(formula(model))
  X <- model.matrix(delete.response(terms(f)), newdata)
  fit <- as.numeric(X %*% fixef(model))
  se <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  transform(newdata, Fitted = fit, CI_low = fit - 1.96 * se, CI_high = fit + 1.96 * se)
}
fixed_prediction_glmm <- function(model, newdata) {
  f <- reformulas::nobars(formula(model))
  X <- model.matrix(delete.response(terms(f)), newdata)
  beta <- fixef(model)$cond
  V <- vcov(model)$cond
  link <- as.numeric(X %*% beta)
  se <- sqrt(diag(X %*% V %*% t(X)))
  transform(newdata,
    Fitted = exp(link), CI_low = exp(link - 1.96 * se),
    CI_high = exp(link + 1.96 * se)
  )
}
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

model_annotation_data <- function(result, x_limits, y_limits,
                                  side = c("left", "right"),
                                  vertical = c("top", "bottom")) {
  side <- match.arg(side)
  vertical <- match.arg(vertical)
  p_label <- if (!is.finite(result$P_value)) {
    warning(
      "Model P-value unavailable for ", result$Hypothesis,
      "; check convergence and Hessian before interpreting this model."
    )
    "italic(P) == \"unavailable\""
  } else if (result$P_value < .001) {
    "italic(P) < 0.001"
  } else {
    sprintf("italic(P) == %.3f", result$P_value)
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
      sprintf(if (result$Hypothesis == "H6") "beta == %.3f" else "beta[\"100 m\"] == %.3f", result$Estimate),
      sprintf("\"CI: [%.3f, %.3f]\"", result$CI_low, result$CI_high),
      p_label,
      r2_plotmath(result$Marginal_R2, result$Conditional_R2)
    )
  )
}
elevation_sequence <- seq(min(individual$Elevation), max(individual$Elevation), length.out = 200)
elevation_mean <- mean(plant_environment$Elevation)
elevation_sd <- sd(plant_environment$Elevation)
h4_prediction <- fixed_prediction_lmer(
  h4_models$elevation,
  data.frame(
    Elevation = elevation_sequence,
    Elevation_z = (elevation_sequence - elevation_mean) / elevation_sd
  )
)
h5_prediction <- fixed_prediction_glmm(
  h5_models$elevation,
  data.frame(
    Elevation = elevation_sequence,
    Elevation_z = (elevation_sequence - elevation_mean) / elevation_sd
  )
)
mass_sequence <- seq(min(h6_data$MISM), max(h6_data$MISM), length.out = 200)
h6_prediction <- fixed_prediction_glmm(
  h6_models$seed_mass,
  data.frame(
    MISM = mass_sequence,
    MISM_centered = (mass_sequence - mean(individual$MISM))
  )
)
h4_y_limits <- range(c(
  h4_data$LogMISM, h4_prediction$CI_low,
  h4_prediction$CI_high
), finite = TRUE)
h5_y_limits <- range(c(
  h5_data$SeedNumber, h5_prediction$CI_low,
  h5_prediction$CI_high
), finite = TRUE)
h6_y_limits <- range(c(
  h6_data$SeedNumber, h6_prediction$CI_low,
  h6_prediction$CI_high
), finite = TRUE)
h4_annotation <- model_annotation_data(
  h4_result, range(h4_data$Elevation), h4_y_limits, "left", "bottom"
)
# Keep panel (e) annotations below its fitted regression line.
h4_annotation$y <- h4_y_limits[1] + diff(h4_y_limits) * c(.45, .35, .25, .15, .05)
h5_annotation <- model_annotation_data(
  h5_result, range(h5_data$Elevation), h5_y_limits, "left"
)
h6_annotation <- model_annotation_data(
  h6_result, range(h6_data$MISM), h6_y_limits, "left"
)
h6_annotation$x <- min(h6_data$MISM) + .08 * diff(range(h6_data$MISM))
p_h4 <- ggplot(h4_data, aes(Elevation, LogMISM)) +
  geom_ribbon(
    data = h4_prediction, aes(x = Elevation, ymin = CI_low, ymax = CI_high),
    inherit.aes = FALSE, fill = "#E5E5E5"
  ) +
  geom_line(
    data = h4_prediction, aes(Elevation, Fitted), inherit.aes = FALSE,
    colour = "black", linewidth = .8,
    linetype = if (isTRUE(h4_result$P_value < .05)) "solid" else "dashed"
  ) +
  geom_point(
    shape = 21, fill = "#CEAB07", alpha = .35,
    position = position_jitter(width = 4, height = 0, seed = 12191)
  ) +
  geom_text_edge(
    data = h4_annotation,
    aes(x = x, y = y, label = label, hjust = hjust),
    inherit.aes = FALSE, parse = TRUE, vjust = .5,
    family = "Arial", size = 4.8, colour = "black"
  ) +
  labs(
    subtitle = "(e) MISM ~ elevation", x = "Elevation (m)\n",
    y = "MISM (log-scale)\n"
  ) +
  theme_manuscript() +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
    axis.title = element_text(colour = "black", size = 13, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 13, face = "bold"),
    plot.margin = margin(t = 3, r = 4, b = 1, l = 2, unit = "mm")
  )
p_h4 <- reproductive_panel_strip(p_h4)
save_figure(p_h4, "Figure_H4_seed_mass_elevation", 150, 120)
p_h5 <- ggplot(h5_data, aes(Elevation, SeedNumber)) +
  geom_ribbon(
    data = h5_prediction, aes(x = Elevation, ymin = CI_low, ymax = CI_high),
    inherit.aes = FALSE, fill = "#E5E5E5"
  ) +
  geom_line(
    data = h5_prediction, aes(Elevation, Fitted), inherit.aes = FALSE,
    colour = "black", linewidth = .8,
    linetype = if (isTRUE(h5_result$P_value < .05)) "solid" else "dashed"
  ) +
  geom_point(
    shape = 21, fill = "#CEAB07", alpha = .35,
    position = position_jitter(width = 4, height = 0, seed = 12191)
  ) +
  geom_text_edge(
    data = h5_annotation,
    aes(x = x, y = y, label = label, hjust = hjust),
    inherit.aes = FALSE, parse = TRUE, vjust = .5,
    family = "Arial", size = 4.8, colour = "black"
  ) +
  labs(
    subtitle = "(f) Seed number ~ elevation", x = "Elevation (m)\n",
    y = "Total seed number per plant\n"
  ) +
  theme_manuscript() +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
    axis.title = element_text(colour = "black", size = 13, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 13, face = "bold"),
    plot.margin = margin(t = 3, r = 4, b = 1, l = 2, unit = "mm")
  )
p_h5 <- reproductive_panel_strip(p_h5)
save_figure(p_h5, "Figure_H5_seed_number_elevation", 150, 120)
p_h6 <- ggplot(h6_data, aes(MISM, SeedNumber)) +
  geom_ribbon(
    data = h6_prediction, aes(x = MISM, ymin = CI_low, ymax = CI_high),
    inherit.aes = FALSE, fill = "#E5E5E5"
  ) +
  geom_line(
    data = h6_prediction, aes(MISM, Fitted), inherit.aes = FALSE,
    colour = "black", linewidth = .8,
    linetype = if (isTRUE(h6_result$P_value < .05)) "solid" else "dashed"
  ) +
  geom_point(shape = 21, fill = "#CEAB07", alpha = .35) +
  geom_text_edge(
    data = h6_annotation,
    aes(x = x, y = y, label = label, hjust = hjust),
    inherit.aes = FALSE, parse = TRUE, vjust = .5,
    family = "Arial", size = 4.8, colour = "black"
  ) +
  labs(
    subtitle = "(g) Seed number ~ MISM",
    x = "MISM (mg per seed)\n", y = "Total seed number per plant\n"
  ) +
  theme_manuscript() +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
    axis.title = element_text(colour = "black", size = 13, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 13, face = "bold"),
    plot.margin = margin(t = 3, r = 4, b = 1, l = 2, unit = "mm")
  )
p_h6 <- reproductive_panel_strip(p_h6)
save_figure(p_h6, "Figure_H6_seed_mass_seed_number", 150, 120)

# Plant-level H6 coefficient from the primary mixed model.
h6_coefficient_labels <- h6_decomposition_results |>
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
p_h6_coefficient <- ggplot(h6_coefficient_labels, aes(Estimate, as.numeric(Component))) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high),
    orientation = "y", width = .20, linewidth = 1,
    colour = "#D9AE00"
  ) +
  geom_point(size = 4, colour = "#D9AE00") +
  geom_text(aes(x = Estimate, y = as.numeric(Component) + .22, label = Annotation),
    vjust = 0,
    size = 4.8, family = "Arial",
    colour = "black", lineheight = 1.15
  ) +
  scale_y_continuous(
    breaks = c(1, 2), labels = c("Among plots", "Within plot"),
    limits = c(.5, 3.1), expand = expansion(mult = 0)
  ) +
  scale_x_continuous(expand = expansion(mult = c(.75, .75))) +
  labs(
    subtitle = "(h) Seed number ~ MISM (within and among plots)",
    x = expression(bold("MISM coefficient (log link)" ~ bgroup("[", mg ~ seed^{
      -1
    }, "]")^{
      -1
    })), y = NULL
  ) +
  theme_manuscript() +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 13, face = "bold"),
    axis.title = element_text(colour = "black", size = 13, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 13, face = "bold"),
    plot.margin = margin(t = 1, r = 4, b = 1, l = 2, unit = "mm")
  )
p_h6_coefficient <- reproductive_panel_strip(p_h6_coefficient)
save_figure(p_h6_coefficient, "Figure_H6_plant_level_coefficient", 175, 125)
message("H4-H6 maternal-seed analysis complete. Outputs: ", output_dir_msa)

# Align response labels and candidate descriptions before workbook export.
publication_table_1()
publication_candidates()

# Combine all CSV tables from this analysis into one multi-sheet workbook.
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Install the openxlsx package to export the combined table workbook.")
}
table_csv_files <- list.files(table_dir_msa, pattern = "\\.csv$", full.names = TRUE)
if (length(table_csv_files)) {
  table_sheet_names <- tools::file_path_sans_ext(basename(table_csv_files)) |>
    sub("^Table_", "", x = _) |>
    gsub("[^A-Za-z0-9 _-]", "_", x = _) |>
    substr(1, 27) |>
    make.unique(sep = "_")
  table_sheet_names[basename(table_csv_files) == "Table_1.csv"] <- "Table 1"
  table_sheet_names[basename(table_csv_files) == "Table_S1.csv"] <- "Table S1"
  table_workbook <- setNames(
    lapply(table_csv_files, read.csv, check.names = FALSE),
    table_sheet_names
  )
  openxlsx::write.xlsx(
    table_workbook,
    file = file.path(table_dir_msa, "summary_tables_H4_H6_maternal_seed_allocation.xlsx"),
    overwrite = TRUE
  )
  message(
    "Combined table workbook written: ",
    file.path(table_dir_msa, "summary_tables_H4_H6_maternal_seed_allocation.xlsx")
  )
}

# Save plot objects so formatting edits can reuse the fitted analyses.
saveRDS(
  list(p_h4 = p_h4, p_h5 = p_h5, p_h6 = p_h6, p_h6_coefficient = p_h6_coefficient),
  file.path(model_dir_msa, "Figure_objects_H4_H6.rds")
)

# Merge flowering phenology and seed allocation plots
flowering_panels <- readRDS(file.path(
  "outputs", "flowering_phenology",
  "models", "Figure_objects_H1_H3.rds"
))
p_h1 <- flowering_panels$p_h1
p_h2 <- flowering_panels$p_h2
p_h3 <- flowering_panels$p_h3
p_h3_coefficient <- flowering_panels$p_h3_coefficient

# Enlarge the grey-strip subtitles consistently across all eight combined panels.
combined_panel_subtitle_size <- 16
# Keep the longest subtitle inside panel (h) at the enlarged font size.
p_h6_coefficient$data$.panel_heading <- paste(
  "(h) Seed number ~ MISM", "(within and among plots)",
  sep = "\n"
)
combined_panel_theme <- ggplot2::theme(
  strip.text = ggplot2::element_text(size = combined_panel_subtitle_size),
  plot.subtitle = ggplot2::element_text(size = combined_panel_subtitle_size)
)
p_h1 <- p_h1 + combined_panel_theme
p_h2 <- p_h2 + combined_panel_theme
p_h3 <- p_h3 + combined_panel_theme
p_h3_coefficient <- p_h3_coefficient + combined_panel_theme
p_h4 <- p_h4 + combined_panel_theme
p_h5 <- p_h5 + combined_panel_theme
p_h6 <- p_h6 + combined_panel_theme
p_h6_coefficient <- p_h6_coefficient + combined_panel_theme
# Centre panels (g) and (h) as a pair, retaining the width of the upper panels.
flowering_seed_row_1 <- ggpubr::ggarrange(p_h1, p_h2, p_h3, ncol = 3)
flowering_seed_row_2 <- ggpubr::ggarrange(p_h3_coefficient, p_h4, p_h5, ncol = 3)
flowering_seed_row_3 <- ggpubr::ggarrange(
  NULL, p_h6, p_h6_coefficient, NULL,
  ncol = 4, widths = c(.5, 1, 1, .5)
)
combined_figure_flowering_seed_allocation <- ggpubr::ggarrange(
  flowering_seed_row_1, flowering_seed_row_2, flowering_seed_row_3,
  ncol = 1, nrow = 3, heights = c(1, 1, 1)
)

ggplot2::ggsave(
  filename = file.path(
    figure_dir_msa,
    "Figure_H1_H6_flowering_seed_allocation.png"
  ),
  plot = combined_figure_flowering_seed_allocation,
  width = 350, height = 270, units = "mm", dpi = 600, bg = "white"
)

# Definitions above are reused in this section.
render_gt_tables(table_dir_msa, "Maternal seed allocation (H4-H6)")

# Final publication captions and numbered figures override generic table notes.
publication_figure_exports()
publication_render_tables()

# 18. Focal coefficients for environmental sensitivity comparisons ------------

# Manuscript supporting Table S2 separates coefficient sensitivity from AICc
# support. Invalid adjusted H5 intervals remain missing, rather than inferred.
export_environmental_focal_coefficients <- function() {
  flowering_path <- file.path("outputs", "flowering_phenology", "models")
  allocation_path <- file.path("outputs", "maternal_seed_allocation", "models")
  flowering <- readRDS(file.path(flowering_path, "Models_H1_H3_ML.rds"))
  completion <- readRDS(file.path(flowering_path, "final_flowering_date_onset_models.rds"))
  allocation <- readRDS(file.path(allocation_path, "Models_H4_H6.rds"))
  plots <- utils::read.csv(file.path("outputs", "flowering_phenology", "tables", "Table_plot_environment.csv"))
  elevation_sd <- sd(plots$Elevation)
  results <- list()
  for (hypothesis in c("H1", "H2", "H3", "H4", "H5")) {
    candidates <- if (hypothesis == "H3") completion else if (hypothesis %in% names(flowering)) {
      flowering[[hypothesis]]
    } else allocation[[hypothesis]]
    for (candidate in names(candidates)) {
      model <- candidates[[candidate]]
      count_model <- inherits(model, "glmmTMB")
      coefficients <- if (count_model) summary(model)$coefficients$cond else coef(summary(model))
      term <- if (hypothesis == "H3") "Onset_centered" else "Elevation_z"
      multiplier <- if (hypothesis == "H3") 1 else 100 / elevation_sd
      estimate <- coefficients[term, "Estimate"] * multiplier
      se <- coefficients[term, "Std. Error"] * multiplier
      finite_se <- all(is.finite(coefficients[, "Std. Error"]))
      messages <- if (count_model) model$fit$message else model@optinfo$conv$lme4$messages
      converged <- if (count_model) isTRUE(model$fit$convergence == 0) else
        !length(setdiff(messages, "boundary (singular) fit: see help('isSingular')"))
      hessian <- if (count_model) isTRUE(model$sdr$pdHess) else NA
      eligible <- converged && finite_se && (!count_model || hessian)
      boundary <- if (count_model) {
        any(unlist(lapply(glmmTMB::VarCorr(model)$cond, diag)) < 1e-8)
      } else lme4::isSingular(model)
      results[[length(results) + 1L]] <- data.frame(
        Hypothesis = hypothesis, Model = candidate,
        Response = c(H1 = "Flowering onset", H2 = "Flowering duration", H3 = "Final flowering date",
          H4 = "ln(MISM)", H5 = "Seed number")[[hypothesis]],
        Term = term, Predictor_increment = if (hypothesis == "H3") "1 onset day" else "100 m elevation",
        Estimate = estimate, SE = se,
        CI_low = if (eligible) estimate - qnorm(.975) * se else NA_real_,
        CI_high = if (eligible) estimate + qnorm(.975) * se else NA_real_,
        P_zero = if (eligible) 2 * pnorm(-abs(estimate / se)) else NA_real_,
        P_duration = if (eligible && hypothesis == "H3") 2 * pnorm(-abs((estimate - 1) / se)) else NA_real_,
        N = nobs(model), Converged = converged, Positive_definite_Hessian = hessian,
        Finite_fixed_effect_SE = finite_se, Boundary = boundary, Eligible = eligible,
        stringsAsFactors = FALSE)
    }
  }
  result <- do.call(rbind, results)
  # Keep full precision in memory and export only the formatted Table S2.
  publication <- result
  model_labels <- c(
    elevation = "Elevation",
    elevation_climate = "Elevation + climate",
    elevation_soil = "Elevation + soil",
    elevation_climate_soil = "Elevation + climate + soil",
    onset = "Onset",
    onset_elevation = "Onset + elevation",
    onset_elevation_climate = "Onset + elevation + climate",
    onset_elevation_soil = "Onset + elevation + soil",
    onset_elevation_climate_soil = "Onset + elevation + climate + soil"
  )
  publication$Model <- unname(model_labels[result$Model])
  format_number <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
  format_p <- function(x) {
    ifelse(!is.finite(x), "NA",
      ifelse(x < 0.001, "<0.001",
        ifelse(x < 0.01, "<0.01",
          ifelse(x < 0.05, "<0.05", sprintf("%.3f", x)))))
  }
  for (column in c("Estimate", "SE", "CI_low", "CI_high")) {
    publication[[column]] <- format_number(result[[column]])
  }
  for (column in c("P_zero", "P_duration")) {
    publication[[column]] <- format_p(result[[column]])
  }
  utils::write.csv(
    publication,
    file.path("outputs", "maternal_seed_allocation", "tables", "Table S2.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  result
}
environmental_focal_coefficients <- export_environmental_focal_coefficients()
