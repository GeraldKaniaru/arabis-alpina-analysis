# 1. Local publication support functions -------------------------------------

# Local definitions: this script never sources another R file.
publication_root <- "outputs"
publication_captions <- c(
  Figure_S3 = "Figure S3. Distributions of environmental variables and reproductive traits. Histograms show density and black curves show smoothed density estimates. Yellow bars show original scales and grey bars natural-log scales. MISM is mean individual seed mass in mg per seed."
)
publication_figure_exports <- function() {
  from <- file.path(publication_root, "diagnostics/data_distributions/distributions_all_variables")
  to <- file.path(dirname(from), "Figure_S3_distributions")
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
      records[[length(records) + 1L]] <- data.frame(Number = "Figure S3", Format = ext, File = output)
    }
  }
  if (!length(records)) {
    return(invisible(NULL))
  }
  writeLines(publication_captions[["Figure_S3"]], paste0(to, "_caption.txt"))
  writeLines(publication_captions[["Figure_S3"]], paste0(from, "_caption.txt"))
  manifest_file <- file.path(publication_root, "Publication_figure_manifest.csv")
  updated <- do.call(rbind, records)
  if (file.exists(manifest_file)) {
    previous <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)
    previous <- previous[previous$Number != "Figure S3", , drop = FALSE]
    updated <- rbind(previous[, names(updated), drop = FALSE], updated)
  }
  utils::write.csv(updated, manifest_file, row.names = FALSE)
  invisible(updated)
}
# END INLINE PUBLICATION SUPPORT

# Flowering calendar: 1 June 2021 = Day 1; date = 2021-06-01 + day - 1.
# Duration = final flowering day minus onset day (elapsed days).
# ============================================================================

# Script 05a: DISTRIBUTION AND SKEWNESS AUDIT

# Examines the climate, phenology, soil and seed variables used in the models.

# Run from the "arabis analysis" project root.

# ============================================================================

# 2. Packages and output directories -----------------------------------------

required <- c("readxl", "dplyr", "tidyr", "ggplot2", "germinationmetrics")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install the required packages: ", paste(missing, collapse = ", "))
}
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})
if (!file.exists("arabis analysis.Rproj")) {
  stop("Run this script from the 'arabis analysis' project root.")
}
input_dir <- file.path("data", "raw", "Datasets")
output_dir <- file.path("outputs", "diagnostics", "data_distributions")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 3. Read and select the requested variables ---------------------------------

plot_abiotic_factors <- read_excel(
  file.path(input_dir, "plot_abiotic_factors.xlsx"),
  sheet = "June summary",
  skip = 3,
  .name_repair = "unique"
) |>
  transmute(
    PlotID = as.character(Plot_ID),
    June_mean_temperature_C = as.numeric(June_mean_temperature_C),
    June_total_precipitation_mm = as.numeric(June_total_precipitation_mm)
  )
trait_data <- read_excel(
  file.path(input_dir, "trait_data.xlsx"),
  sheet = "Sheet2",
  .name_repair = "unique"
) |>
  transmute(
    Plant = plant_no, PlotID = paste(Block, Plot, Elevation, sep = "_"),
    FloweringOnset = as.numeric(FloweringOnset),
    FloweringDuration = as.numeric(Final_flowering_date) - as.numeric(FloweringOnset),
    Electrical.Conductivity = as.numeric(Electrical.Conductivity),
    pH = as.numeric(pH),
    Carbon = as.numeric(Carbon),
    Nitrogen = as.numeric(Nitrogen),
    Phosphorus = as.numeric(Phosphorus),
    TotalSeedNumber = as.numeric(TotalSeedNumber),
    MISM = as.numeric(MISM),
    `100_seed_mass` = as.numeric(`100_seed_mass`)
  )
plant_climate <- trait_data |>
  select(Plant, PlotID) |>
  left_join(plot_abiotic_factors, by = "PlotID")
# Plant-level germination indices from the four replicate dishes per maternal plant.

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

germination_input <- read_germination_counts(
  file.path(input_dir, "germination_counts.xlsx"), file.path(input_dir, "trait_data.xlsx")
)
plant_trajectories <- germination_input$trajectories
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
germination_data <- indices |> select(plant_no, t0, Time_spread, MGR, GP)
if (nrow(germination_data) != nrow(trait_data) || anyDuplicated(germination_data$plant_no)) {
  stop("Expected one germination-index record per maternal plant.")
}
germination_variables <- c("t0", "Time_spread", "MGR", "GP")

distribution_data <- bind_rows(
  plant_climate |>
    select(-Plant, -PlotID) |>
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
    mutate(Dataset = "plot_abiotic_factors", Observation_level = "plant record"),
  trait_data |>
    select(-Plant, -PlotID) |>
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
    mutate(Dataset = "trait_data", Observation_level = "plant record")
) |>
  bind_rows(
    trait_data |>
      transmute(LogMISM = log(if_else(MISM > 0, MISM, NA_real_))) |>
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
      mutate(Dataset = "trait_data", Observation_level = "plant record"),
    germination_data |>
      select(-plant_no) |>
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
      mutate(Dataset = "germination_counts", Observation_level = "plant record")
  ) |>
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "June_mean_temperature_C", "June_total_precipitation_mm",
        "FloweringOnset", "FloweringDuration", "Electrical.Conductivity",
        "pH", "Carbon", "Nitrogen", "Phosphorus", "TotalSeedNumber",
        "MISM", "100_seed_mass", "LogMISM",
        "t0", "Time_spread", "MGR", "GP"
      )
    )
  )

# Adjusted Fisher-Pearson sample skewness. Positive values indicate a longer

# right tail; negative values indicate a longer left tail.
adjusted_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L || stats::sd(x) == 0) {
    return(NA_real_)
  }
  z <- (x - mean(x)) / stats::sd(x)
  n / ((n - 1) * (n - 2)) * sum(z^3)
}
classify_skewness <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    abs(x) < 0.5 ~ "approximately symmetric",
    abs(x) < 1 ~ if_else(x > 0, "moderately right-skewed", "moderately left-skewed"),
    x >= 1 ~ "strongly right-skewed",
    TRUE ~ "strongly left-skewed"
  )
}
safe_shapiro_p <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3L || length(unique(x)) < 3L) {
    return(NA_real_)
  }

  # shapiro.test accepts at most 5,000 observations. The fixed seed makes any

  # necessary subsampling reproducible.
  if (length(x) > 5000L) {
    set.seed(12191)
    x <- sample(x, 5000L)
  }
  stats::shapiro.test(x)$p.value
}
distribution_summary <- distribution_data |>
  group_by(Dataset, Observation_level, Variable) |>
  summarise(
    N_total = n(),
    N_non_missing = sum(is.finite(Value)),
    N_missing = sum(!is.finite(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Minimum = min(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE, names = FALSE),
    Median = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE, names = FALSE),
    Maximum = max(Value, na.rm = TRUE),
    IQR = IQR(Value, na.rm = TRUE),
    Adjusted_Fisher_Pearson_skewness = adjusted_skewness(Value),
    Shapiro_Wilk_P = safe_shapiro_p(Value),
    Potential_outliers_below_1.5_IQR = sum(
      Value < quantile(Value, 0.25, na.rm = TRUE) - 1.5 * IQR(Value, na.rm = TRUE),
      na.rm = TRUE
    ),
    Potential_outliers_above_1.5_IQR = sum(
      Value > quantile(Value, 0.75, na.rm = TRUE) + 1.5 * IQR(Value, na.rm = TRUE),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    Skewness_interpretation = classify_skewness(Adjusted_Fisher_Pearson_skewness)
  )
write.csv(distribution_summary,
  file.path(output_dir, "Table_distribution_summary_all_variables.csv"),
  row.names = FALSE
)

write.csv(filter(distribution_summary, as.character(Variable) %in% germination_variables),
  file.path(output_dir, "Table_distribution_summary_germination_indices.csv"),
  row.names = FALSE
)

# 5. Diagnostic figures ------------------------------------------------------

plot_data <- distribution_data |> filter(is.finite(Value))

# 6. Publication-style distribution figure -----------------------------------

# Retain every requested raw variable and add the log-transformed seed-mass

# distribution used in the seed-mass models. The transformation is shown as a

# separate panel so the original measurement scale remains visible.
distribution_labels <- c(
  June_mean_temperature_C = "Mean June temperature\n(°C)",
  June_total_precipitation_mm = "Total June precipitation\n(mm)",
  FloweringOnset = "Flowering onset (days)",
  FloweringDuration = "Flowering duration (days)",
  Electrical.Conductivity = "Electrical conductivity",
  pH = "Soil pH",
  Carbon = "Soil organic carbon",
  Nitrogen = "Total nitrogen",
  Phosphorus = "Extractable phosphorus",
  TotalSeedNumber = "Total seed number\nper plant",
  MISM = "Mean individual seed mass\n(MISM; mg)",
  `100_seed_mass` = "100-seed mass (mg)",
  LogMISM = "ln(MISM)",
  t0 = "Time to first germination (days)",
  Time_spread = "Germination-time spread (days)",
  MGR = "Mean germination rate (day⁻¹)",
  GP = "Germination percentage (%)"
)
publication_distribution_data <- plot_data |>
  mutate(
    Variable = as.character(Variable),
    Transformation = if_else(Variable %in% c("LogMISM"),
      "Natural-log transformed", "Untransformed"
    ),
    Display_variable = factor(unname(distribution_labels[Variable]),
      levels = unname(distribution_labels)
    )
  )
publication_distribution_plot <- ggplot(
  publication_distribution_data, aes(x = Value)
) +
  geom_histogram(
    aes(y = after_stat(density), fill = Transformation),
    bins = 24, colour = "black", linewidth = .35
  ) +
  geom_density(
    colour = "black", linewidth = .75, adjust = 1,
    na.rm = TRUE
  ) +
  scale_fill_manual(values = c(
    "Untransformed" = "#D9AE00",
    "Natural-log transformed" = "#E5E5E5"
  ), guide = "none") +
  facet_wrap(~Display_variable,
    scales = "free", ncol = 4, nrow = 5,
    labeller = label_wrap_gen(width = 28)
  ) +
  labs(
    x = NULL, y = "Density"
  ) +
  theme_bw(base_size = 11, base_family = "Arial") +
  theme(
    text = element_text(family = "Arial", colour = "black"),
    strip.background = element_rect(
      fill = "#D9D9D9", colour = "grey30",
      linewidth = .55
    ),
    strip.text = element_text(
      face = "plain", colour = "black", size = 16,
      margin = margin(4, 3, 4, 3)
    ),
    axis.text.x = element_text(colour = "black", size = 13, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 13, face = "plain"),
    panel.grid.minor = element_blank(),
    panel.spacing = grid::unit(5, "mm"),
    plot.margin = margin(6, 6, 5, 6, unit = "mm")
  )
ggsave(
  file.path(output_dir, "distributions_all_variables.png"),
  publication_distribution_plot,
  width = 400, height = 450,
  units = "mm", dpi = 600, bg = "white"
)
ggsave(
  file.path(output_dir, "distributions_all_variables.pdf"),
  publication_distribution_plot,
  width = 400, height = 450,
  units = "mm", device = cairo_pdf, bg = "white"
)
ggsave(
  file.path(output_dir, "distributions_all_variables.tiff"),
  publication_distribution_plot,
  width = 400, height = 450,
  units = "mm", dpi = 600, compression = "lzw", bg = "white"
)
qq_plot <- ggplot(filter(plot_data, !Variable %in% germination_variables), aes(sample = Value)) +
  stat_qq(shape = 21, fill = "#69B3A2", colour = "black", size = 1.4) +
  stat_qq_line(colour = "#B2182B", linewidth = 0.55) +
  facet_wrap(~Variable, scales = "free", ncol = 4, nrow = 4) +
  labs(x = "Theoretical quantiles", y = "Observed quantiles") +
  theme_bw(base_size = 10, base_family = "Arial") +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())
ggsave(
  file.path(output_dir, "QQ_plots_all_variables.png"),
  qq_plot,
  width = 300, height = 300, units = "mm", dpi = 400, bg = "white"
)
# Matching four-panel diagnostics for the four germination responses.
germination_qq_plot <- qq_plot %+%
  filter(publication_distribution_data, Variable %in% germination_variables) +
  facet_wrap(~Display_variable, scales = "free", ncol = 2, nrow = 2)
ggsave(file.path(output_dir, "QQ_plots_germination_indices.png"),
  germination_qq_plot,
  width = 300, height = 300,
  units = "mm", dpi = 400, bg = "white"
)

message("Distribution audit complete. Outputs written to: ", output_dir)
print(distribution_summary, n = Inf)

# 7. Numbered publication exports --------------------------------------------

# Numbered publication exports preserve the descriptive working filenames.

publication_figure_exports()
