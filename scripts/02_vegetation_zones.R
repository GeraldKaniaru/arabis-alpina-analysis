# ============================================================================
# Script 02: VEGETATION-ZONE PHOTOGRAPHIC PLATE
# ============================================================================
# Purpose: create Supplementary Figure S1 from the vegetation photographs.
#
# This standalone script converts the eight source photographs to archival TIFF,
# prepares consistently cropped and labelled TIFF panels, and assembles a
# publication-resolution 3 + 3 + 2 photographic plate. It does not source any
# other project script.

# 1. Global options ----------------------------------------------------------

options(stringsAsFactors = FALSE)

if (!requireNamespace("magick", quietly = TRUE)) {
  stop("Install the 'magick' package before running this script.")
}

project_marker <- "arabis analysis.Rproj"
if (!file.exists(project_marker)) {
  stop("Run this script from the 'arabis analysis' project directory.")
}

dir.create("outputs/vegetation_zones", recursive = TRUE, showWarnings = FALSE)

# 2. Input and output directories --------------------------------------------

image_dir <- file.path("images", "vegetation_zones")
output_dir <- file.path("outputs/vegetation_zones")
converted_dir <- file.path(output_dir, "individual_tiff")
panel_dir <- file.path(output_dir, "labelled_panel_tiff")
summary_dir <- file.path(
  "outputs/vegetation_zones"
)
dir.create(converted_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

# 3. Photograph manifest -----------------------------------------------------

# The zone assignments follow the visible habitat structure in the photographs.
# Several images deliberately represent different expressions of the same broad
# Afro-alpine vegetation zone.
photo_manifest <- data.frame(
  panel = letters[1:8],
  source_file = file.path(image_dir, c(
    "1.jpg", "2.jpg", "3.jpg", "4.jpg", "5.png", "6.jpg", "8.jpg", "7.jpg"
  )),
  zone = c(
    "Lower montane wet forest",
    "Upper montane forest-bamboo transition",
    "Afro-alpine zone",
    "Afro-alpine zone",
    "Afro-alpine zone",
    "Afro-alpine zone",
    "Upper Afro-alpine-nival ecotone",
    "Nival zone"
  ),
  habitat = c(
    "Closed, humid montane forest beside a perennial stream and waterfall",
    "Upper forest edge with bamboo and tall giant-groundsel vegetation",
    "Tussock grassland with a flowering giant rosette plant",
    "Wet tussock grassland containing abundant silver-leaved rosettes",
    "Alpine wetland and tarn bordered by tussocks and giant rosette plants",
    "Open giant-rosette grassland dominated by tussocks and giant groundsels",
    "Rocky upper-alpine habitat with giant groundsels below nival cliffs",
    "Sparsely vegetated high-elevation scree and unstable rocky substrate"
  ),
  stringsAsFactors = FALSE
)

missing_photos <- photo_manifest$source_file[!file.exists(photo_manifest$source_file)]
if (length(missing_photos) > 0L) {
  stop("Missing source photographs: ", paste(missing_photos, collapse = ", "))
}

# 4. Image preparation helpers -----------------------------------------------

safe_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^a-zA-Z0-9]+", "_", x))
  gsub("(^_+|_+$)", "", x)
}

read_photo <- function(path) {
  magick::image_read(path) |>
    magick::image_orient() |>
    magick::image_convert(colorspace = "sRGB")
}

fit_panel <- function(image, width, height) {
  image |>
    magick::image_resize(sprintf("%dx%d^", width, height)) |>
    magick::image_crop(sprintf("%dx%d+0+0", width, height), gravity = "center") |>
    magick::image_extent(sprintf("%dx%d", width, height), gravity = "center")
}

label_panel <- function(image, panel, zone, pointsize) {
  label <- sprintf("(%s)", panel)
  magick::image_annotate(
    image,
    text = label,
    gravity = "northwest",
    location = "+28+22",
    size = pointsize,
    font = "Arial",
    weight = 700,
    color = "black",
    boxcolor = "#FFFFFFD9"
  )
}

# 5. Convert source photographs ----------------------------------------------

# First create full-resolution TIFF conversions of every original photograph.
for (i in seq_len(nrow(photo_manifest))) {
  original <- read_photo(photo_manifest$source_file[i])
  converted_name <- sprintf(
    "panel_%s_%s_original.tiff",
    photo_manifest$panel[i], safe_name(photo_manifest$zone[i])
  )
  magick::image_write(
    original,
    path = file.path(converted_dir, converted_name),
    format = "tiff",
    density = "600x600",
    compression = "lzw",
    quality = 100
  )
}

# 6. Assemble photographic plate ---------------------------------------------

# Construct two three-panel rows and a two-panel bottom row. The wider bottom
# panels use all available plate width and reproduce the balanced reference layout.
gutter <- 10L
small_width <- 1400L
wide_width <- 2105L
panel_height <- 1000L

panels <- vector("list", nrow(photo_manifest))
for (i in seq_len(nrow(photo_manifest))) {
  this_width <- if (i <= 6L) small_width else wide_width
  panel_image <- read_photo(photo_manifest$source_file[i]) |>
    fit_panel(width = this_width, height = panel_height) |>
    label_panel(
      panel = photo_manifest$panel[i],
      zone = photo_manifest$zone[i],
      pointsize = 90
    )

  panel_name <- sprintf(
    "panel_%s_%s_labelled.tiff",
    photo_manifest$panel[i], safe_name(photo_manifest$zone[i])
  )
  magick::image_write(
    panel_image,
    path = file.path(panel_dir, panel_name),
    format = "tiff",
    density = "600x600",
    compression = "lzw",
    quality = 100
  )
  panels[[i]] <- panel_image
}

make_row <- function(images) {
  magick::image_append(do.call(c, images), stack = FALSE)
}

row_1 <- make_row(panels[1:3])
row_2 <- make_row(panels[4:6])
row_3 <- make_row(panels[7:8])

white_gutter <- magick::image_blank(
  width = magick::image_info(row_1)$width,
  height = gutter, color = "white"
)
plate <- magick::image_append(c(row_1, white_gutter, row_2, white_gutter, row_3),
  stack = TRUE
)
plate <- magick::image_border(plate, color = "white", geometry = "8x8")

# 7. Export plate and metadata -----------------------------------------------

plate_base <- file.path(output_dir, "Figure_vegetation_zones_R")
magick::image_write(plate, paste0(plate_base, ".tiff"),
  format = "tiff",
  density = "600x600", compression = "lzw", quality = 100
)
magick::image_write(plate, paste0(plate_base, ".png"),
  format = "png",
  density = "600x600", quality = 100
)
magick::image_write(plate, paste0(plate_base, ".pdf"),
  format = "pdf",
  density = "600x600", quality = 100
)

summary_png <- file.path(
  summary_dir, "supp_figure_s01_vegetation_zone_photographs.png"
)
summary_tiff <- file.path(
  summary_dir, "supp_figure_s01_vegetation_zone_photographs.tiff"
)
file.copy(paste0(plate_base, ".png"), summary_png, overwrite = TRUE)
file.copy(paste0(plate_base, ".tiff"), summary_tiff, overwrite = TRUE)

message("Created Supplementary Figure S1 and eight converted TIFF photographs.")
message("Combined TIFF: ", paste0(plate_base, ".tiff"))
message("Caption: ", file.path(output_dir, "Figure_S1_caption.txt"))
