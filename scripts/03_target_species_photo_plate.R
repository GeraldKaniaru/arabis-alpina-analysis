# ============================================================================
# TARGET-SPECIES PHOTOGRAPHIC PLATE
# ============================================================================
# Purpose: combine four photographs of Arabis alpina into a two-column by
# two-row plate progressing from whole-plant habit to flowers, fruiting habit
# and an opened silique.

# 1. Packages and project setup ----------------------------------------------

options(stringsAsFactors = FALSE)

required_packages <- "magick"
if (!requireNamespace(required_packages, quietly = TRUE)) {
  stop("Install the required package: magick")
}

library(magick)

project_marker <- "arabis analysis.Rproj"
if (!file.exists(project_marker)) {
  stop("Run this script from the 'arabis analysis' project directory.")
}

dir.create("outputs/target_species", recursive = TRUE, showWarnings = FALSE)

# 2. Directories and photograph selection ------------------------------------

input_dir <- file.path("images", "target_species_photos")
output_dir <- file.path("outputs/target_species")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Retain original panels a, f, b and h, in that order, and relabel them a-d.
# The order within each row is left to right.
photo_order <- c(
  "4.jpeg", # (a) whole-plant habit
  "6.jpeg", # (b) close view of the flower cluster (formerly panel f)
  "7.jpeg", # (c) fruiting growth form (formerly panel b)
  "5.jpeg" # (d) opened silique exposing seeds (formerly panel h)
)

photo_paths <- file.path(input_dir, photo_order)
missing_photos <- photo_paths[!file.exists(photo_paths)]
if (length(missing_photos)) {
  stop("Missing photographs: ", paste(missing_photos, collapse = ", "))
}

# 3. Panel dimensions and image preparation ----------------------------------

panel_labels <- paste0("(", letters[1:4], ")")
panel_width <- 2000
panel_height <- 1250
panel_gap <- 8
outer_border <- 15

prepare_panel <- function(path, label) {
  image_read(path) |>
    image_resize(paste0(panel_width, "x", panel_height, "^")) |>
    image_extent(
      geometry = paste0(panel_width, "x", panel_height),
      gravity = "center",
      color = "white"
    ) |>
    image_border(color = "white", geometry = paste0(outer_border, "x", outer_border)) |>
    image_annotate(
      text = label,
      gravity = "northwest",
      location = "+42+33",
      font = "Arial",
      size = 120,
      weight = 700,
      color = "black",
      boxcolor = "rgba(255,255,255,0.72)"
    )
}

# 4. Assemble photographic plate ---------------------------------------------

panels <- Map(prepare_panel, photo_paths, panel_labels)

rows <- lapply(seq(1, 4, by = 2), function(i) {
  spacer <- image_blank(
    width = panel_gap, height = panel_height + 2 * outer_border,
    color = "white"
  )
  image_append(c(panels[[i]], spacer, panels[[i + 1]]), stack = FALSE)
})

row_spacer <- image_blank(
  width = panel_width * 2 + panel_gap + outer_border * 4,
  height = panel_gap,
  color = "white"
)

plate_parts <- list()
for (i in seq_along(rows)) {
  plate_parts[[length(plate_parts) + 1]] <- rows[[i]]
  if (i < length(rows)) plate_parts[[length(plate_parts) + 1]] <- row_spacer
}

plate <- image_append(do.call(c, plate_parts), stack = TRUE)
plate <- image_strip(plate)

# 5. Publication dimensions and exports --------------------------------------

# Requested publication dimensions; raster rounding is at most half a pixel.
figure_width_cm <- 16
figure_height_cm <- 10
figure_dpi <- 600
figure_width_px <- round(figure_width_cm / 2.54 * figure_dpi)
figure_height_px <- round(figure_height_cm / 2.54 * figure_dpi)
figure_geometry <- paste0(figure_width_px, "x", figure_height_px)
plate <- image_resize(plate, paste0(figure_geometry, "^")) |>
  image_extent(figure_geometry, gravity = "center", color = "white")

output_base <- file.path(output_dir, "Figure_2_target_species_arabis_alpina")
image_write(image_resize(plate, "1600x"), paste0(output_base, "_preview.jpg"),
  format = "jpeg", quality = 92
)
# Preserve ImageMagick's pixel ordering, then record PNG physical resolution.
png_intermediate <- tempfile(fileext = ".png")
image_write(plate, png_intermediate, format = "png")
png::writePNG(png::readPNG(png_intermediate), paste0(output_base, ".png"),
  dpi = figure_dpi
)
unlink(png_intermediate)
image_write(plate, paste0(output_base, ".tiff"),
  format = "tiff",
  compression = "lzw", density = "600x600"
)

message("Created target-species photographic plate: ", paste0(output_base, ".png"))
