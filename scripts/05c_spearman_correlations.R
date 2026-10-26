# 1. Local publication support functions -------------------------------------

# Local definitions: this script never sources another R file.
publication_root <- "outputs"
publication_captions <- c(
  Figure_S5 = "Figure S5. Spearman rank correlations among environmental variables and reproductive traits. Correlations involving 100-seed mass are restricted to the four germination components. Blank cells indicate untested pairs. Asterisks indicate Benjamini-Hochberg-adjusted P values: * <0.05, ** <0.01, *** <0.001."
)
publication_figure_exports <- function() {
  from <- file.path(publication_root, "diagnostics/figures/spearman_correlations")
  to <- file.path(dirname(from), "Figure_S5_spearman_correlations")
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
      records[[length(records) + 1L]] <- data.frame(Number = "Figure S5", Format = ext, File = output)
    }
  }
  if (!length(records)) {
    return(invisible(NULL))
  }
  writeLines(publication_captions[["Figure_S5"]], paste0(to, "_caption.txt"))
  writeLines(publication_captions[["Figure_S5"]], paste0(from, "_caption.txt"))
  manifest_file <- file.path(publication_root, "Publication_figure_manifest.csv")
  updated <- do.call(rbind, records)
  if (file.exists(manifest_file)) {
    previous <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)
    previous <- previous[previous$Number != "Figure S5", , drop = FALSE]
    updated <- rbind(previous[, names(updated), drop = FALSE], updated)
  }
  utils::write.csv(updated, manifest_file, row.names = FALSE)
  invisible(updated)
}
# END INLINE PUBLICATION SUPPORT

# Flowering calendar: 1 June 2021 = Day 1; date = 2021-06-01 + day - 1.
# Duration = final flowering day minus onset day (elapsed days).
# Script 05c: PLANT-LEVEL CORRELATIONS
# Run from the arabis analysis project root, after script 05.
# The existing analysis uses Spearman rank correlations; the requested script
# and image filenames are retained without changing the statistical method.

# 2. Packages and output directories -----------------------------------------

required <- c("readxl", "dplyr", "ggplot2", "germinationmetrics")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})
if (!file.exists("arabis analysis.Rproj")) stop("Run from the arabis analysis project root.")
input_dir_reporting <- file.path("data", "raw", "Datasets")
trait_file <- file.path(input_dir_reporting, "trait_data.xlsx")
source_diagnostic_table_dir <- file.path("outputs", "diagnostics", "tables")
source_diagnostic_figure_dir <- file.path("outputs", "diagnostics", "figures")
for (folder in c(source_diagnostic_table_dir, source_diagnostic_figure_dir)) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

# Updated plant-level correlations, including the recorded 100_seed_mass.

# 3. Read plant traits and prepare environmental predictors ------------------

raw_for_correlations <- readxl::read_excel(trait_file, sheet = "Sheet2", .name_repair = function(x) make.unique(x, sep = "_duplicate")) |>
  transmute(
    plant_no = as.integer(plant_no), Source_plot_ID = paste(Block, Plot, Elevation, sep = "_"),
    Elevation = as.numeric(Elevation), Onset = as.numeric(FloweringOnset),
    Duration = as.numeric(Final_flowering_date) - as.numeric(FloweringOnset), MISM = as.numeric(MISM), Seed_mass_100_mg = as.numeric(`100_seed_mass`),
    Seed_number = as.numeric(TotalSeedNumber), EC = as.numeric(Electrical.Conductivity),
    pH = as.numeric(pH), Carbon = as.numeric(Carbon), Nitrogen = as.numeric(Nitrogen),
    Phosphorus = as.numeric(Phosphorus)
  )
soil_records <- raw_for_correlations |>
  group_by(Source_plot_ID) |>
  summarise(across(c(Carbon, pH, EC, Nitrogen, Phosphorus), mean), .groups = "drop")
soil_correlations <- prcomp(select(soil_records, Carbon, pH, EC, Nitrogen, Phosphorus),
  center = TRUE, scale. = TRUE
)
soil_direction <- if (mean(soil_correlations$rotation[c("Carbon", "pH", "Nitrogen", "Phosphorus"), 1]) < 0) -1 else 1
soil_records$SoilPC1 <- soil_correlations$x[, 1] * soil_direction
raw_for_correlations <- left_join(raw_for_correlations, select(soil_records, Source_plot_ID, SoilPC1), by = "Source_plot_ID")
climate_correlations <- readxl::read_excel(file.path(input_dir_reporting, "plot_abiotic_factors.xlsx"),
  sheet = "June summary", skip = 3
) |>
  transmute(
    Source_plot_ID = as.character(Plot_ID), Temperature = as.numeric(June_mean_temperature_C),
    Precipitation = as.numeric(June_total_precipitation_mm)
  )
# Calculate the same plant-level indices used in script 08 directly from raw counts.

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
  file.path(input_dir_reporting, "germination_counts.xlsx"), trait_file
)
plant_trajectories <- germination_input$trajectories

# 5. Calculate germination indices -------------------------------------------

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
germ_correlations <- indices |> select(plant_no, t0, Time_spread, MGR, GP)

# 6. Assemble correlation data -----------------------------------------------

correlation_data <- raw_for_correlations |>
  left_join(climate_correlations, by = "Source_plot_ID") |>
  left_join(germ_correlations, by = "plant_no") |>
  select(
    Elevation, Temperature, Precipitation, SoilPC1, Onset, Duration,
    MISM, Seed_mass_100_mg, Seed_number, t0, Time_spread, MGR, GP
  )
if (nrow(correlation_data) != nrow(germination_input$assignments) || anyNA(correlation_data)) stop("Incomplete correlation data.")

# 7. Define eligible pairs and calculate correlations ------------------------

# Restrict 100-seed mass to the four germination components before testing
# and BH adjustment; excluded pairs are absent from both table and heatmap.
germination_components <- c("t0", "Time_spread", "MGR", "GP")
n_variables <- ncol(correlation_data)
correlation_rows <- list()
for (i in 2:n_variables) {
  for (j in 1:(i - 1)) {
    # Omit these pairs from both tests and displays.
    pair_names <- c(names(correlation_data)[i], names(correlation_data)[j])
    if ("Seed_mass_100_mg" %in% pair_names &&
      !any(germination_components %in% pair_names)) {
      next
    }
    if (any(c("Seed_number", "MISM") %in% pair_names) && any(c("t0", "Time_spread", "MGR", "GP") %in% pair_names)) next
    test <- cor.test(correlation_data[[i]], correlation_data[[j]], method = "spearman", exact = FALSE)
    correlation_rows[[length(correlation_rows) + 1L]] <- data.frame(
      Row = names(correlation_data)[i], Column = names(correlation_data)[j],
      Rho = unname(test$estimate), P = test$p.value
    )
  }
}
correlation_results <- bind_rows(correlation_rows) |>
  mutate(
    Adjusted_P = p.adjust(P, "BH"),
    Stars = case_when(
      Adjusted_P < .001 ~ "***", Adjusted_P < .01 ~ "**",
      Adjusted_P < .05 ~ "*", TRUE ~ ""
    ),
    Label = paste0(sprintf("%.2f", Rho), Stars),
    Row = factor(Row, levels = rev(names(correlation_data))),
    Column = factor(Column, levels = names(correlation_data))
  )

# 8. Draw and export correlation heatmap -------------------------------------

correlation_axis_labels <- c(
  MISM = "MISM",
  Seed_mass_100_mg = "100-seed mass",
  `100_seed_mass_mg` = "100-seed mass",
  Seed_number = "Seed number",
  Time_spread = "Time spread"
)
label_correlations <- function(x) {
  labels <- ifelse(x %in% names(correlation_axis_labels), unname(correlation_axis_labels[x]), x)
  as.expression(lapply(seq_along(x), function(i) {
    if (x[i] == "t0") quote(italic(T)[0]) else labels[i]
  }))
}
p_correlations <- ggplot(correlation_results, aes(Column, Row, fill = Rho)) +
  geom_tile(colour = "grey40") +
  geom_text(aes(label = Label), size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#79402E", mid = "white", high = "#235B38", limits = c(-1, 1)) +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = "Spearman's Rho") +
  scale_x_discrete(labels = label_correlations) +
  scale_y_discrete(labels = label_correlations) +
  theme_bw(base_size = 11, base_family = "Arial") +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "inside", legend.position.inside = c(.94, .88),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.title = element_text(
      size = 12, face = "bold", color = "black",
      margin = margin(b = 4, unit = "mm")
    ),
    legend.text = element_text(size = 11, face = "plain", color = "black"),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
    axis.title = element_text(colour = "black", size = 12, face = "bold"),
    plot.subtitle = element_text(colour = "black", size = 12, face = "bold"),
    plot.margin = margin(t = 1, r = 4, b = 1, l = 2, unit = "mm")
  )
ggsave(file.path(source_diagnostic_figure_dir, "spearman_correlations.png"), p_correlations,
  width = 300, height = 200, units = "mm", dpi = 300
)
utils::write.csv(correlation_results, file.path(source_diagnostic_table_dir, "Source_trait_correlations.csv"), row.names = FALSE)

message("Plant-level correlation figure and table written to outputs/diagnostics.")

# 9. Numbered publication exports --------------------------------------------

# Numbered publication exports preserve the descriptive working filenames.

publication_figure_exports()
