# Assess Gaussian mixed-model residuals before selecting response transformations.
# Run from the analysis root; missing saved models are fitted here for diagnostics.
# Uses identical observations, fixed effects and random effects for raw/log comparisons.

# 1. Packages and output directories -----------------------------------------

required <- c("lme4", "glmmTMB")
for (p in required) if (!requireNamespace(p, quietly = TRUE)) stop("Missing package: ", p)
suppressPackageStartupMessages(library(lme4))
root <- "outputs"
out <- file.path(root, "diagnostics", "model_residuals")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# 2. Discover and collect saved Gaussian models ------------------------------

files <- list.files(root, pattern = "^(Models_.*|final_flowering_date_onset_models)\\.rds$", recursive = TRUE, full.names = TRUE)
models <- list()
collect <- function(x, id) {
  if (inherits(x, "lmerMod")) {
    models[[id]] <<- x
  } else if (inherits(x, "glmmTMB")) {
    if (family(x)$family == "gaussian") stop("Gaussian glmmTMB model requires implementation: ", id)
  } else if (is.list(x)) {
    nm <- names(x)
    if (is.null(nm)) nm <- seq_along(x)
    for (i in seq_along(x)) collect(x[[i]], paste(id, nm[i], sep = "__"))
  }
}
for (f in files) collect(readRDS(f), tools::file_path_sans_ext(basename(f)))

# 3. Fit diagnostic candidates when saved models are unavailable -------------

model_source <- "Saved Gaussian mixed models from scripts 06-10."
if (!length(models)) {
  message("No saved Gaussian models found; fitting diagnostic candidates directly from data/raw/Datasets.")
  for (p in c("readxl", "dplyr", "germinationmetrics")) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Missing package: ", p)
  }
  suppressPackageStartupMessages(library(dplyr))
  input <- file.path("data", "raw", "Datasets")
  traits <- readxl::read_excel(file.path(input, "trait_data.xlsx"), sheet = "Sheet2")
  plants <- traits |> transmute(
    plant_no = as.integer(plant_no), Block = factor(Block), Plot = as.integer(Plot),
    Elevation = as.numeric(Elevation), PlotID = paste(Block, Plot, Elevation, sep = "_"),
    Onset = as.numeric(FloweringOnset), FinalDate = as.numeric(Final_flowering_date),
    Duration = FinalDate - Onset, LogMISM = log(as.numeric(MISM)),
    Seed_mass_100_mg = as.numeric(`100_seed_mass`),
    Carbon = as.numeric(Carbon), pH = as.numeric(pH),
    EC = as.numeric(Electrical.Conductivity), Nitrogen = as.numeric(Nitrogen),
    Phosphorus = as.numeric(Phosphorus)
  )
  climate <- readxl::read_excel(file.path(input, "plot_abiotic_factors.xlsx"),
    sheet = "June summary", skip = 3
  ) |> transmute(
    PlotID = as.character(Plot_ID),
    Temperature = as.numeric(June_mean_temperature_C),
    Precipitation = as.numeric(June_total_precipitation_mm)
  )
  plots <- plants |>
    group_by(PlotID) |>
    summarise(across(c(Elevation, Carbon, pH, EC, Nitrogen, Phosphorus), mean), .groups = "drop") |>
    left_join(climate, by = "PlotID")
  stopifnot(nrow(plots) == 27L, !anyDuplicated(climate$PlotID))
  soil <- prcomp(select(plots, Carbon, pH, EC, Nitrogen, Phosphorus), center = TRUE, scale. = TRUE)
  direction <- if (mean(soil$rotation[c("Carbon", "pH", "Nitrogen", "Phosphorus"), "PC1"]) < 0) -1 else 1
  plots$SoilPC1 <- soil$x[, "PC1"] * direction
  for (v in c("Elevation", "Temperature", "Precipitation", "SoilPC1")) {
    plots[[paste0(v, "_z")]] <- as.numeric(scale(plots[[v]]))
  }
  plants <- left_join(plants, select(plots, PlotID, ends_with("_z")), by = "PlotID") |>
    mutate(Onset_centered = Onset - mean(Onset, na.rm = TRUE))
  env <- list(
    elevation = "Elevation_z",
    elevation_climate = c("Elevation_z", "Temperature_z", "Precipitation_z"),
    elevation_soil = c("Elevation_z", "SoilPC1_z"),
    elevation_climate_soil = c("Elevation_z", "Temperature_z", "Precipitation_z", "SoilPC1_z")
  )
  flowering <- plants |> filter(complete.cases(
    Onset, Duration, Elevation_z,
    Temperature_z, Precipitation_z, SoilPC1_z, Block, PlotID
  ))
  fit_diagnostic <- function(response, terms, data, id, flowering_fit = FALSE) {
    fm <- as.formula(paste(response, "~", paste(c(terms, "(1 | Block)", "(1 | PlotID)"), collapse = " + ")))
    if (flowering_fit) {
      models[[id]] <<- lmer(fm,
        data = data, REML = FALSE,
        control = lmerControl(optimizer = "bobyqa", check.conv.singular = "warning")
      )
    } else {
      models[[id]] <<- lmer(fm, data = data, REML = FALSE)
    }
  }
  for (response in c("Onset", "Duration", "LogMISM")) {
    d <- if (response == "LogMISM") {
      plants |> filter(complete.cases(
        LogMISM,
        Elevation_z, Temperature_z, Precipitation_z, SoilPC1_z, Block, PlotID
      ))
    } else {
      flowering
    }
    for (s in names(env)) {
      fit_diagnostic(
        response, env[[s]], d,
        paste(response, s, sep = "__"), response != "LogMISM"
      )
    }
  }
  for (response in c("Duration", "FinalDate")) {
    onset_sets <- c(list(onset = character()), env)
    for (s in names(onset_sets)) {
      fit_diagnostic(
        response,
        c("Onset_centered", onset_sets[[s]]), flowering, paste("H3", response, s, sep = "__"), TRUE
      )
    }
  }
  flowering <- flowering |>
    group_by(PlotID) |>
    mutate(Onset_plot_mean = mean(Onset), Onset_within = Onset - Onset_plot_mean) |>
    ungroup() |>
    mutate(Onset_among = Onset_plot_mean - mean(Onset))
  fit_diagnostic("FinalDate", c("Onset_within", "Onset_among"), flowering, "H3__within_among", TRUE)
  counts <- readxl::read_excel(file.path(input, "germination_counts.xlsx"), sheet = "Sheet 1") |>
    transmute(
      plant_no = as.integer(Plant_no), Replicate = as.integer(Replicate),
      Day = as.numeric(Day), Count = as.numeric(Germination_records)
    )
  checks <- counts |>
    arrange(plant_no, Replicate, Day) |>
    group_by(plant_no, Replicate) |>
    summarise(valid = identical(Day, as.numeric(0:34)) && first(Count) == 0 &&
      all(is.finite(Count) & Count >= 0 & Count <= 25 & Count == floor(Count)) &&
      all(diff(Count) >= 0), .groups = "drop")
  stopifnot(
    all(checks$valid), all(checks$Replicate %in% 1:4),
    all(table(checks$plant_no) == 4L), setequal(checks$plant_no, plants$plant_no)
  )
  indices <- counts |>
    group_by(plant_no, Day) |>
    summarise(Count = sum(Count), .groups = "drop") |>
    filter(Day > 0) |>
    group_by(plant_no) |>
    group_modify(function(.x, .y) {
      x <- arrange(.x, Day)
      if (!any(x$Count > 0)) {
        return(tibble(t0 = NA_real_, Time_spread = NA_real_, MGR = NA_real_, GP = 0))
      }
      tibble(
        t0 = germinationmetrics::FirstGermTime(x$Count, x$Day, partial = FALSE),
        Time_spread = germinationmetrics::TimeSpreadGerm(x$Count, x$Day, partial = FALSE),
        MGR = germinationmetrics::MeanGermRate(x$Count, x$Day, partial = FALSE), GP = max(x$Count)
      )
    }) |>
    ungroup()
  germ <- left_join(plants, indices, by = "plant_no") |>
    mutate(Seed_mass_100_centered_mg = Seed_mass_100_mg - mean(Seed_mass_100_mg, na.rm = TRUE))
  for (response in c("t0", "Time_spread", "MGR", "GP")) {
    for (s in c("H7_elevation", "H8_unadjusted", "H8_elevation_adjusted")) {
      terms <- switch(s,
        H7_elevation = "Elevation_z",
        H8_unadjusted = "Seed_mass_100_centered_mg",
        H8_elevation_adjusted = c("Seed_mass_100_centered_mg", "Elevation_z")
      )
      fit_diagnostic(response, terms, germ, paste(response, s, sep = "__"))
    }
  }
  model_source <- paste(
    "Diagnostic-only fits made directly from current workbooks:",
    "H1-H4 environmental candidates, H3 duration/final-date and within/among models, H7 and H8.",
    "Production model files are not created; run scripts 06-10 for publication analyses."
  )
}

# 4. Residual assessment and diagnostic plots --------------------------------

rows <- list()
assess <- function(m, id, scale) {
  r <- as.numeric(resid(m))
  z <- r / sigma(m)
  f <- as.numeric(fitted(m))
  sw <- if (length(z) >= 3 && length(z) <= 5000) shapiro.test(z)$p.value else NA_real_
  # Descriptive screen for variance changing with fitted values; not an independent-error test.
  trend <- suppressWarnings(cor(abs(z), f, method = "spearman"))
  skew <- mean((z - mean(z))^3) / sd(z)^3
  tail_fraction <- mean(abs(z) > 3)
  convergence <- m@optinfo$conv$lme4$messages
  convergence <- setdiff(convergence, "boundary (singular) fit: see help('isSingular')")
  flagged <- abs(skew) > 1 || abs(trend) > .3 || tail_fraction > .01
  rows[[length(rows) + 1L]] <<- data.frame(
    Model = id, Scale = scale, Formula = paste(deparse(formula(m)), collapse = " "),
    N = nobs(m), Residual_SD = sigma(m), Residual_skewness = skew,
    Shapiro_Wilk_P = sw, Abs_residual_fitted_Spearman = trend,
    Fraction_abs_standardized_residual_above_3 = tail_fraction,
    Singular = isSingular(m), Converged = !length(convergence),
    Assessment = if (flagged) "Review QQ, variance pattern and influential observations before choosing a transformation" else "No large descriptive residual flag; confirm visually",
    stringsAsFactors = FALSE
  )
  stem <- paste0(gsub("[^A-Za-z0-9_-]", "_", id), "__", scale)
  draw <- function() {
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), family = "sans")
    plot(f, z, xlab = "Conditional fitted values", ylab = "Residual / residual SD", main = "Residuals versus fitted", pch = 16, col = "darkgreen")
    abline(h = 0, lty = 2)
    lines(lowess(f, z), col = "red")
    qqnorm(z, main = "Conditional residual normal Q-Q", pch = 16, col = "darkgreen")
    qqline(z, col = "red")
    hist(z, breaks = 30, main = "Conditional residual distribution", xlab = "Residual / residual SD", col = "#D9AE00")
    plot(f, sqrt(abs(z)), xlab = "Conditional fitted values", ylab = "Square root |standardized residual|", main = "Scale-location", pch = 16, col = "darkgreen")
    lines(lowess(f, sqrt(abs(z))), col = "red")
  }
  png(file.path(out, paste0(stem, ".png")), width = 2400, height = 1800, res = 250)
  draw()
  dev.off()
  pdf(file.path(out, paste0(stem, ".pdf")), width = 9.6, height = 7.2)
  draw()
  dev.off()
}

# 5. Compare original and log response models --------------------------------

raw_refits <- list()
for (id in names(models)) {
  m <- models[[id]]
  response <- as.character(formula(m)[[2]])
  logged <- response %in% c("LogMISM", "Log_MISM") || grepl("^log\\(", response)
  if (logged) {
    d <- model.frame(m)
    d$.raw_response <- exp(model.response(d))
    fm <- formula(m)
    fm[[2]] <- as.name(".raw_response")
    raw <- lmer(fm,
      data = d, REML = isREML(m),
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
    )
    raw_refits[[id]] <- raw
    assess(raw, id, "original_response")
    assess(m, id, "existing_log_response")
  } else {
    assess(m, id, "original_response")
  }
}

# 6. Export assessments and interpretation notes -----------------------------

utils::write.csv(do.call(rbind, rows), file.path(out, "Table_Gaussian_model_residual_assessment.csv"), row.names = FALSE)
saveRDS(raw_refits, file.path(out, "Raw_response_comparison_models.rds"))
writeLines(c(
  model_source,
  "Raw-response comparisons reverse the existing natural-log response and refit the same model on the same plants.",
  "Conditional residuals assess within-plant response errors after the fitted random effects; histogram shape alone does not justify transforming the response.",
  "Shapiro-Wilk P values are descriptive: estimated mixed-model residuals are not independent observations and large samples detect small deviations.",
  "Flags use |skewness| > 1, |Spearman correlation of absolute residual with fitted| > 0.3, or >1% residuals beyond 3 residual SD. These are review screens, not automatic decision rules.",
  "Review Q-Q tails, scale-location, influential records, convergence and singularity together. Do not compare raw/log AIC without accounting for the transformation Jacobian.",
  "No production model, manuscript or existing figure is changed by this script. Gaussian glmmTMB models cause a stop rather than silent omission; non-Gaussian models are outside its scope."
), file.path(out, "README_residual_assessment.txt"))
message("Assessed ", length(models), " Gaussian models; outputs: ", out)
