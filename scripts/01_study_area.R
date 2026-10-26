# ============================================================================
# Script 01: STUDY AREA AND SAMPLING LOCATIONS
# ============================================================================
# Purpose: create the multiscale study-area map and the accompanying table of
# independent source plots. Run this script from the arabis analysis root.

# 1. Project setup -----------------------------------------------------------

if (!file.exists("arabis analysis.Rproj")) {
  stop("Run this script from the arabis analysis project root.")
}

# 2. Output directories ------------------------------------------------------

dir.create("outputs/study_area", recursive = TRUE, showWarnings = FALSE)

# BEGIN INLINED MODULE: scripts/figures/13_figure_study_area_map.R
# Figure 1: study area on Mount Kenya, arranged to match the reference map.
# Unlabelled Africa inset within (a); (a) Kenya counties and the Mount Kenya
# region; (b) six surrounding counties and the vegetation-zone extent;
# (c) Mount Kenya vegetation zones; (d) the 27 source plots.
# Run from the "arabis analysis" directory.

# 3. Packages and map configuration ------------------------------------------

needed <- c(
  "sf", "terra", "png", "ggplot2", "ggrepel", "patchwork", "ggspatial",
  "rnaturalearth", "cowplot", "magick"
)
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

library(sf)
library(terra)
library(ggplot2)
library(patchwork)
library(ggspatial)

# Use Arial for every map element, including coordinates, legends,
# scale labels, contour elevations, annotations and panel labels.
figure_font <- "Arial"
if (.Platform$OS.type == "windows") {
  windowsFonts(`Arial` = windowsFont("Arial"))
}
update_geom_defaults("text", list(family = figure_font, colour = "black"))
update_geom_defaults("label", list(family = figure_font, colour = "black"))

shape_dir <- "data/shapefiles"
county_file <- "data/admin_boundaries/gadm/gadm41_KEN_1_pk.rds"
raw_file <- "data/raw/Datasets/trait_data.xlsx"
raw_sheet <- "Sheet2"
coordinate_file <- file.path("data", "raw", "Datasets", "new_occurrence_data_all_arabis_alpina.csv")
out <- "outputs/study_area"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

zone_files <- c(
  "Bamboo Zone" = "Bamboo Zone.shp",
  "Nival Zone" = "Nival Zone.shp",
  "Lower Montane Wet Forest" = "Lower Montane Wet Forest.shp",
  "Lower Montane Dry Forest" = "Lower Montane Dry Forest.shp",
  "Afro-alpine Zone" = "Afro-alpine Zone.shp",
  "Heath Zone" = "Heath Zone.shp",
  "Upper Montane Forest" = "Upper Montane Forest.shp"
)
required_files <- c(
  county_file, raw_file, coordinate_file,
  file.path(shape_dir, unname(zone_files))
)
if (!all(file.exists(required_files))) {
  stop("Missing map data: ", paste(required_files[!file.exists(required_files)], collapse = ", "))
}

# 4. Read and validate spatial data ------------------------------------------

# These legacy vegetation polygons contain ring crossings that are repaired and
# handled reliably by GEOS. Keep spherical s2 processing disabled throughout
# both direct execution and execution from the consolidated study-area script.
sf_use_s2(FALSE)

counties <- st_as_sf(terra::unwrap(readRDS(county_file))) |>
  st_transform(4326) |>
  st_make_valid()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(4326) |>
  st_make_valid()
africa <- world[world$continent == "Africa", ]
kenya_country <- st_union(counties) |> st_as_sf()
africa_box <- st_bbox(africa)

focus_names <- c("Laikipia", "Meru", "Tharaka-Nithi", "Embu", "Kirinyaga", "Nyeri")
focus <- counties[counties$NAME_1 %in% focus_names, ]
if (nrow(focus) != length(focus_names)) {
  stop("Not all six Mount Kenya counties were found in the county dataset.")
}

zones <- do.call(rbind, lapply(names(zone_files), function(zone_name) {
  z <- st_read(file.path(shape_dir, zone_files[[zone_name]]), quiet = TRUE)
  if (is.na(st_crs(z))) stop("Missing CRS in ", zone_files[[zone_name]])
  z <- st_transform(st_make_valid(z), 4326)
  z$zone <- zone_name
  z[, "zone", drop = FALSE]
}))
zones$zone <- factor(zones$zone, levels = names(zone_files))
# Draw outer polygons before nested high-elevation polygons.
zones <- zones[order(as.numeric(st_area(zones)), decreasing = TRUE), ]

# Polygon-coordinate table used to add environmental context behind the
# sampling locations in panel (d) without introducing another fill scale.
zone_plot_df <- do.call(rbind, lapply(seq_len(nrow(zones)), function(i) {
  xy <- st_coordinates(zones[i, ])
  level_cols <- grep("^L", colnames(xy), value = TRUE)
  data.frame(
    long = xy[, "X"], lat = xy[, "Y"],
    group = paste(i, apply(xy[, level_cols, drop = FALSE], 1, paste,
      collapse = "_"
    ), sep = "_"),
    zone = as.character(zones$zone[i]), stringsAsFactors = FALSE
  )
}))

# Match the 27 sampling locations to coordinates by unique elevation.
# The occurrence file contains 28 records because two records share the same
# 4,438-m source-plot coordinate; retaining one unique elevation/coordinate
# yields the 27 source plots containing the individual plants analysed.
if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install the readxl package to import data/raw/Datasets/trait_data.xlsx.")
}

main_data <- as.data.frame(
  readxl::read_excel(raw_file, sheet = raw_sheet, .name_repair = "unique")
)

# readxl appends positional suffixes only when duplicate headers are present.
# Accept the single Elevation column and validate any duplicate elevation copies.
elevation_columns <- grep("^Elevation([.]{3}[0-9]+)?$", names(main_data), value = TRUE)
if (!length(elevation_columns)) stop("Missing required Elevation column in ", raw_file)
if (length(elevation_columns) > 1L &&
  !all(vapply(
    elevation_columns[-1], function(column) {
      isTRUE(all.equal(main_data[[elevation_columns[1]]], main_data[[column]]))
    },
    logical(1)
  ))) {
  stop("The elevation columns in trait_data.xlsx are inconsistent.")
}
required_columns <- c("plant_no", "Block", "Plot", "VegetationZone")
missing_columns <- setdiff(required_columns, names(main_data))
if (length(missing_columns)) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

individual_data <- data.frame(
  plant_no = main_data[["plant_no"]],
  Block = main_data[["Block"]],
  Plot = main_data[["Plot"]],
  Elevation = main_data[[elevation_columns[1]]],
  Vegetation.zone = main_data[["VegetationZone"]],
  stringsAsFactors = FALSE
)
individual_data$plot_id <- interaction(
  individual_data$Block, individual_data$Plot, individual_data$Elevation,
  drop = TRUE, sep = "_"
)
plot_data <- unique(individual_data[, c("plot_id", "Elevation", "Vegetation.zone")])
coordinates <- read.csv(coordinate_file)
coordinates <- coordinates[
  !duplicated(coordinates$alt),
  c("long", "lat", "alt")
]
sampled_plots <- merge(
  plot_data[, c("plot_id", "Elevation", "Vegetation.zone")],
  coordinates,
  by.x = "Elevation", by.y = "alt", all.x = TRUE, sort = FALSE
)
if (nrow(sampled_plots) != 27L || anyNA(sampled_plots[, c("long", "lat")])) {
  stop("The sampling-coordinate join did not produce 27 complete source plots.")
}
expected_zone_n <- c("Lower alpine" = 2L, "Upper alpine" = 22L, "Nival" = 3L)
observed_zone_n <- table(sampled_plots$Vegetation.zone)
if (!all(observed_zone_n[names(expected_zone_n)] == expected_zone_n)) {
  stop(
    "Unexpected vegetation-zone sample sizes: ",
    paste(names(observed_zone_n), observed_zone_n, collapse = ", ")
  )
}
sampled_plots_sf <- st_as_sf(sampled_plots,
  coords = c("long", "lat"),
  crs = 4326, remove = FALSE
)

kenya_box <- st_bbox(counties)
focus_box <- st_bbox(focus)
zone_box <- st_bbox(zones)
focus_labels <- suppressWarnings(st_point_on_surface(focus))
county_label_positions <- data.frame(
  NAME_1 = c("Laikipia", "Meru", "Tharaka-Nithi", "Embu", "Kirinyaga", "Nyeri"),
  longitude = c(36.70, 37.80, 38.23, 37.78, 37.24, 36.72),
  latitude = c(.39, .39, -.29, -.69, -.67, -.43)
)

# 5. Satellite imagery for the vegetation-zone panel -------------------------

# Esri World Imagery is used as an openly accessible Google-Earth-like
# basemap. The downloaded image is cached, so subsequent runs are reproducible
# without another network request. Provider attribution is printed on panel c.
cache_dir <- file.path(out, "map_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
satellite_file <- file.path(cache_dir, "mount_kenya_esri_world_imagery.png")
sat_pad_x <- as.numeric(zone_box["xmax"] - zone_box["xmin"]) * .05
sat_pad_y <- as.numeric(zone_box["ymax"] - zone_box["ymin"]) * .05
sat_extent <- c(
  xmin = as.numeric(zone_box["xmin"] - sat_pad_x),
  ymin = as.numeric(zone_box["ymin"] - sat_pad_y),
  xmax = as.numeric(zone_box["xmax"] + sat_pad_x),
  ymax = as.numeric(zone_box["ymax"] + sat_pad_y)
)
if (!file.exists(satellite_file)) {
  imagery_url <- paste0(
    "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/export?",
    "bbox=", paste(sat_extent, collapse = ","),
    "&bboxSR=4326&imageSR=4326&size=1800,1800&format=png&transparent=false&f=image"
  )
  tryCatch(
    download.file(imagery_url, satellite_file, mode = "wb", quiet = TRUE),
    error = function(e) {
      stop(
        "Satellite imagery could not be downloaded. Check the internet connection, then rerun. ",
        "The requested basemap was Esri World Imagery. Original error: ", conditionMessage(e)
      )
    }
  )
}
satellite <- png::readPNG(satellite_file)

# Copernicus GLO-30 elevation data for labelled 100-m contours in panel (d).
# The cached Cloud-Optimized GeoTIFF covers 1 S to 0 N and 37 E to 38 E.
dem_file <- file.path(cache_dir, "Copernicus_DSM_30m_S01_E037.tif")
dem_url <- paste0(
  "https://copernicus-dem-30m.s3.amazonaws.com/",
  "Copernicus_DSM_COG_10_S01_00_E037_00_DEM/",
  "Copernicus_DSM_COG_10_S01_00_E037_00_DEM.tif"
)
dem_is_valid <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 1e7) {
    return(FALSE)
  }
  isTRUE(tryCatch(
    {
      probe <- terra::rast(path)
      probe_extent <- terra::ext(probe)
      dx <- (probe_extent$xmax - probe_extent$xmin) / 100
      dy <- (probe_extent$ymax - probe_extent$ymin) / 100
      probe_crop <- terra::crop(probe, terra::ext(
        probe_extent$xmin,
        probe_extent$xmin + dx, probe_extent$ymax - dy, probe_extent$ymax
      ))
      if (!length(terra::values(probe_crop, mat = FALSE))) stop("DEM contains no readable cells")
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  ))
}
if (!dem_is_valid(dem_file)) {
  download_dem <- function(url, destination, attempts = 3L) {
    previous_timeout <- getOption("timeout")
    on.exit(options(timeout = previous_timeout), add = TRUE)
    options(timeout = max(600, previous_timeout))
    temporary <- paste0(destination, ".download")
    last_error <- "Downloaded DEM was incomplete or unreadable."
    for (attempt in seq_len(attempts)) {
      if (file.exists(temporary)) unlink(temporary)
      message(
        "Downloading Copernicus DEM (attempt ", attempt, "/", attempts,
        "; timeout ", getOption("timeout"), " seconds)."
      )
      status <- tryCatch(
        download.file(url, temporary,
          mode = "wb", method = "libcurl",
          quiet = FALSE
        ),
        error = function(e) {
          last_error <<- conditionMessage(e)
          return(1L)
        }
      )
      if (identical(as.integer(status), 0L) && dem_is_valid(temporary)) {
        if (file.exists(destination) && unlink(destination) != 0L) {
          stop("Could not replace invalid DEM cache: ", destination)
        }
        if (!file.rename(temporary, destination)) {
          stop("Could not install the validated Copernicus DEM cache file.")
        }
        return(invisible(destination))
      }
      if (file.exists(temporary)) unlink(temporary)
    }
    stop(
      "Copernicus DEM download failed after ", attempts,
      " attempts with a ", getOption("timeout"),
      "-second timeout. Check the connection and rerun. Original error: ",
      last_error
    )
  }
  download_dem(dem_url, dem_file)
}

# Geographic extent used to display the 27 measured source-plot coordinates.
sample_extent <- c(
  xmin = min(sampled_plots$long[
    sampled_plots$Vegetation.zone == "Lower alpine"
  ]) - .0015,
  ymin = floor((min(sampled_plots$lat) - .005) * 100) / 100,
  # One upper-alpine point lies 0.00294 degrees east of the last nival point;
  # retain it rather than clipping a genuine source-plot coordinate.
  xmax = max(sampled_plots$long) + .003,
  ymax = ceiling((max(sampled_plots$lat) + .005) * 100) / 100
)
sample_x_breaks <- seq(
  sample_extent["xmin"], max(sampled_plots$long) + .001,
  length.out = 5
)
sample_y_breaks <- seq(sample_extent["ymin"], sample_extent["ymax"], by = .01)

dem <- terra::rast(dem_file)
dem_crop <- terra::crop(
  dem, terra::ext(sample_extent[c("xmin", "xmax", "ymin", "ymax")])
)
contour_levels <- seq(
  floor(terra::minmax(dem_crop)[1] / 100) * 100,
  ceiling(terra::minmax(dem_crop)[2] / 100) * 100,
  by = 100
)
elevation_contours <- terra::as.contour(dem_crop, levels = contour_levels) |>
  st_as_sf() |>
  st_transform(4326)
contour_plot_df <- do.call(rbind, lapply(seq_len(nrow(elevation_contours)), function(i) {
  xy <- st_coordinates(elevation_contours[i, ])
  level_cols <- grep("^L", colnames(xy), value = TRUE)
  data.frame(
    long = xy[, "X"], lat = xy[, "Y"],
    group = paste(i, apply(xy[, level_cols, drop = FALSE], 1, paste,
      collapse = "_"
    ), sep = "_")
  )
}))
# Place one altitude label on each contour where it is farthest from all
# sampling symbols. This prevents elevation labels from concealing observations.
placed_contour_labels <- matrix(numeric(0),
  ncol = 2,
  dimnames = list(NULL, c("X", "Y"))
)
contour_labels <- do.call(rbind, lapply(seq_len(nrow(elevation_contours)), function(i) {
  line <- st_cast(elevation_contours[i, ], "LINESTRING", warn = FALSE)
  if (!nrow(line)) {
    return(NULL)
  }
  line_xy <- st_coordinates(line)
  keep <- line_xy[, "X"] > sample_extent["xmin"] + .0045 &
    line_xy[, "X"] < sample_extent["xmax"] - .0045 &
    line_xy[, "Y"] > sample_extent["ymin"] + .0018 &
    line_xy[, "Y"] < sample_extent["ymax"] - .0018 &
    !(line_xy[, "X"] < sample_extent["xmin"] + .013 &
      line_xy[, "Y"] < sample_extent["ymin"] + .007)
  candidates <- line_xy[keep, c("X", "Y"), drop = FALSE]
  if (!nrow(candidates)) candidates <- line_xy[, c("X", "Y"), drop = FALSE]
  point_separation <- vapply(seq_len(nrow(candidates)), function(k) {
    # Longitude is rescaled to the narrower latitude range for balanced distance.
    min(sqrt(((candidates[k, "X"] - sampled_plots$long) / 2)^2 +
      (candidates[k, "Y"] - sampled_plots$lat)^2))
  }, numeric(1))
  label_separation <- if (nrow(placed_contour_labels)) {
    vapply(seq_len(nrow(candidates)), function(k) {
      min(sqrt(((candidates[k, "X"] - placed_contour_labels[, "X"]) / 2)^2 +
        (candidates[k, "Y"] - placed_contour_labels[, "Y"])^2))
    }, numeric(1))
  } else {
    rep(Inf, nrow(candidates))
  }
  separation <- pmin(point_separation, label_separation)
  xy <- candidates[which.max(separation), ]
  placed_contour_labels <<- rbind(placed_contour_labels, xy)
  data.frame(
    long = xy["X"], lat = xy["Y"],
    altitude = paste0(elevation_contours$level[i], " m")
  )
}))
# Keep every altitude annotation fully inside the coordinate panel.
contour_labels$long <- pmax(
  sample_extent["xmin"] + .0045,
  pmin(
    sample_extent["xmax"] - .0045,
    contour_labels$long
  )
)
contour_labels$lat <- pmax(
  sample_extent["ymin"] + .0015,
  pmin(
    sample_extent["ymax"] - .0015,
    contour_labels$lat
  )
)
contour_repel_df <- rbind(
  contour_labels,
  data.frame(long = sampled_plots$long, lat = sampled_plots$lat, altitude = "")
)

zone_colours <- c(
  "Bamboo Zone" = "#A6D854",
  "Nival Zone" = "#F2F2F2",
  "Lower Montane Wet Forest" = "#005A32",
  "Lower Montane Dry Forest" = "#00A6A6",
  "Afro-alpine Zone" = "#E78AC3",
  "Heath Zone" = "#8DA0CB",
  "Upper Montane Forest" = "#FC8D62"
)
zone_plot_df$zone_fill <- unname(zone_colours[zone_plot_df$zone])
plot_colours <- c(
  "Lower alpine" = "#56B4E9",
  "Upper alpine" = "#009E73",
  "Nival" = "#E69F00"
)
plot_shapes <- c("Lower alpine" = 21, "Upper alpine" = 22, "Nival" = 24)

legend_zone_order <- c(
  "Lower Montane Dry Forest", "Lower Montane Wet Forest",
  "Upper Montane Forest", "Bamboo Zone", "Heath Zone",
  "Afro-alpine Zone", "Nival Zone"
)
longitude_label <- function(x) {
  paste0(
    formatC(abs(x), format = "f", digits = 1), "\u00b0 ",
    ifelse(x < 0, "W", "E")
  )
}
latitude_label <- function(y) {
  ifelse(
    abs(y) < 1e-10, "0.0\u00b0",
    paste0(
      formatC(abs(y), format = "f", digits = 1), "\u00b0 ",
      ifelse(y < 0, "S", "N")
    )
  )
}

base_map_theme <- theme_bw(base_size = 9, base_family = figure_font) +
  theme(
    panel.grid.major = element_line(colour = "grey88", linewidth = .25),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = .5),
    axis.title = element_blank(),
    axis.text = element_text(colour = "black", size = 8),
    axis.ticks = element_line(colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(.5, -2, .5, .5, unit = "mm"),
    legend.title = element_text(face = "bold", colour = "black", size = 9),
    legend.text = element_text(colour = "black", size = 8),
    legend.key.height = grid::unit(3.7, "mm"),
    legend.key.width = grid::unit(4.5, "mm")
  )

north_arrow <- annotation_north_arrow(
  location = "tr", which_north = "true",
  height = grid::unit(6.5, "mm"), width = grid::unit(6.5, "mm"),
  pad_x = grid::unit(2.5, "mm"), pad_y = grid::unit(31, "mm"),
  style = north_arrow_orienteering(text_size = 5.5, line_width = .45)
)

# 6. (a) Africa overview -----------------------------------------------------

p_africa <- ggplot() +
  geom_sf(data = africa, fill = "grey96", colour = "black", linewidth = .18) +
  geom_sf(
    data = kenya_country, fill = "#74C476", colour = "black",
    linewidth = .45
  ) +
  coord_sf(xlim = c(-19, 53), ylim = c(-36, 38), expand = FALSE) +
  scale_x_continuous(breaks = c(-10, 10, 30, 50), labels = longitude_label) +
  scale_y_continuous(breaks = c(-30, -10, 10, 30), labels = latitude_label) +
  base_map_theme +
  theme(
    axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "darkgrey", fill = NA,
      linewidth = .5
    ),
    plot.tag = element_text(
      family = figure_font, face = "bold",
      size = 12, colour = "black", hjust = 1,
      vjust = 1
    ),
    plot.tag.position = c(.975, .975), plot.tag.location = "panel",
    plot.margin = margin(.5, -2, .5, .5, unit = "mm")
  )

# 7. (b) Kenya overview ------------------------------------------------------

box_pad_x <- as.numeric(focus_box["xmax"] - focus_box["xmin"]) * .07
box_pad_y <- as.numeric(focus_box["ymax"] - focus_box["ymin"]) * .07
p_a <- ggplot() +
  geom_sf(data = counties, fill = "grey96", colour = "black", linewidth = .25) +
  geom_sf(data = focus, fill = "#B8E39A", colour = "black", linewidth = .3) +
  annotate("rect",
    xmin = focus_box["xmin"] - box_pad_x, xmax = focus_box["xmax"] + box_pad_x,
    ymin = focus_box["ymin"] - box_pad_y, ymax = focus_box["ymax"] + box_pad_y,
    fill = NA, colour = "#238B45", linewidth = .8
  ) +
  labs(tag = "(a)") +
  coord_sf(
    xlim = c(kenya_box["xmin"] - .2, kenya_box["xmax"] + .2),
    ylim = c(kenya_box["ymin"] - .2, kenya_box["ymax"] + .2), expand = FALSE
  ) +
  scale_x_continuous(
    breaks = c(35, 37.5, 40, 42.5),
    labels = longitude_label
  ) +
  scale_y_continuous(
    breaks = c(-2.5, 0, 2.5),
    labels = latitude_label
  ) +
  annotation_scale(location = "bl", width_hint = .28, text_cex = .55) +
  base_map_theme +
  theme(
    plot.tag = element_text(
      family = figure_font, face = "bold",
      size = 12, colour = "black", hjust = 1,
      vjust = 1
    ),
    plot.tag.position = c(.975, .975), plot.tag.location = "panel",
    plot.margin = margin(.5, -2, .5, .5, unit = "mm")
  )

# 8. (c) Mount Kenya counties ------------------------------------------------

focus_pad_x <- as.numeric(focus_box["xmax"] - focus_box["xmin"]) * .15
focus_pad_y <- as.numeric(focus_box["ymax"] - focus_box["ymin"]) * .10
p_b <- ggplot() +
  geom_sf(data = focus, fill = "#B8E39A", colour = "black", linewidth = .2) +
  geom_text(
    data = county_label_positions,
    aes(longitude, latitude, label = NAME_1),
    colour = "black", size = 2.5
  ) +
  annotate("rect",
    xmin = zone_box["xmin"], xmax = zone_box["xmax"],
    ymin = zone_box["ymin"], ymax = zone_box["ymax"],
    fill = NA, colour = "black", linewidth = .45
  ) +
  labs(tag = "(b)") +
  coord_sf(
    xlim = c(
      focus_box["xmin"] - focus_pad_x,
      focus_box["xmax"] + focus_pad_x
    ),
    ylim = c(
      focus_box["ymin"] - focus_pad_y,
      focus_box["ymax"] + focus_pad_y
    ), expand = FALSE,
    clip = "off"
  ) +
  scale_x_continuous(
    breaks = c(36, 37, 38),
    labels = longitude_label
  ) +
  scale_y_continuous(
    breaks = c(-.5, 0, .5),
    labels = latitude_label
  ) +
  annotation_scale(location = "bl", width_hint = .25, text_cex = .55) +
  base_map_theme +
  theme(
    plot.tag = element_text(
      family = figure_font, face = "bold",
      size = 12, colour = "black", hjust = 1,
      vjust = 1
    ),
    plot.tag.position = c(.975, .975), plot.tag.location = "panel",
    plot.margin = margin(.5, -2, 4, .5, unit = "mm")
  )

# 9. (c) Mount Kenya vegetation zones ----------------------------------------

zone_pad_x <- sat_pad_x
zone_pad_y <- sat_pad_y
p_c <- ggplot() +
  annotation_raster(satellite,
    xmin = sat_extent["xmin"], xmax = sat_extent["xmax"],
    ymin = sat_extent["ymin"], ymax = sat_extent["ymax"]
  ) +
  geom_sf(
    data = zones, aes(fill = zone), colour = "black", linewidth = .20,
    alpha = .24
  ) +
  annotate("rect",
    xmin = sample_extent["xmin"], xmax = sample_extent["xmax"],
    ymin = sample_extent["ymin"], ymax = sample_extent["ymax"],
    fill = NA, colour = "black", linewidth = 1.05
  ) +
  annotate("rect",
    xmin = sample_extent["xmin"], xmax = sample_extent["xmax"],
    ymin = sample_extent["ymin"], ymax = sample_extent["ymax"],
    fill = NA, colour = "white", linewidth = .55, linetype = 2
  ) +
  geom_sf(
    data = sampled_plots_sf, aes(shape = Vegetation.zone),
    alpha = 0, show.legend = TRUE
  ) +
  scale_fill_manual(
    values = zone_colours, breaks = legend_zone_order,
    drop = FALSE
  ) +
  scale_shape_manual(
    name = "Sampling plots", values = plot_shapes,
    breaks = names(plot_shapes), drop = FALSE
  ) +
  annotate("label",
    x = sat_extent["xmin"] + .012, y = sat_extent["ymax"] - .012,
    label = "(c)", fontface = "bold", hjust = 0, vjust = 1, size = 4.5,
    fill = NA, linewidth = 0,
    label.padding = grid::unit(.35, "mm")
  ) +
  coord_sf(
    xlim = c(zone_box["xmin"] - zone_pad_x, zone_box["xmax"] + zone_pad_x),
    ylim = c(zone_box["ymin"] - zone_pad_y, zone_box["ymax"] + zone_pad_y),
    expand = FALSE
  ) +
  annotation_scale(
    location = "br", width_hint = .22, text_cex = .90,
    pad_x = grid::unit(2.5, "mm"),
    pad_y = grid::unit(2.5, "mm")
  ) +
  north_arrow +
  guides(
    fill = guide_legend(
      title = "Vegetation zones", ncol = 1, byrow = TRUE,
      override.aes = list(colour = "black", linewidth = .35, alpha = .65),
      order = 1
    ),
    shape = guide_legend(
      order = 2,
      override.aes = list(
        size = 2.5, fill = unname(plot_colours),
        colour = "black", stroke = .4, alpha = 1
      )
    )
  ) +
  base_map_theme +
  theme(
    axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid = element_blank(), legend.position = "bottom",
    legend.justification = "left", legend.box.just = "left",
    legend.location = "plot", legend.title.position = "top",
    legend.margin = margin(0, 0, 0, 0, unit = "mm"),
    legend.key.height = grid::unit(3, "mm"),
    legend.key.width = grid::unit(3.8, "mm"),
    legend.title = element_text(size = 7, face = "bold", colour = "black"),
    legend.text = element_text(size = 5, colour = "black"),
    legend.box = "vertical",
    plot.margin = margin(.5, .5, .5, -3, unit = "mm")
  )

# 10. Panel d: sampled plots -------------------------------------------------

# Panel (d): the 27 actual sampling coordinates, without downloaded imagery.
p_sampling_inset <- ggplot() +
  geom_polygon(
    data = zone_plot_df,
    aes(x = long, y = lat, group = group),
    fill = zone_plot_df$zone_fill, colour = "grey35",
    linewidth = .18, alpha = .16, show.legend = FALSE
  ) +
  geom_path(
    data = contour_plot_df,
    aes(x = long, y = lat, group = group), inherit.aes = FALSE,
    colour = "grey30", linewidth = .28, alpha = .75,
    show.legend = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = contour_repel_df,
    aes(x = long, y = lat, label = altitude),
    inherit.aes = FALSE, family = figure_font, size = 2.8,
    colour = "black",
    box.padding = grid::unit(.7, "mm"),
    point.padding = grid::unit(1.2, "mm"),
    min.segment.length = 0, segment.colour = "grey35",
    segment.size = .20, seed = 12191, max.time = 3,
    max.iter = 20000, max.overlaps = Inf, show.legend = FALSE
  ) +
  geom_point(
    data = sampled_plots,
    aes(x = long, y = lat),
    shape = 24, fill = "#D1B659", colour = "black",
    size = 2.1, stroke = .4, show.legend = FALSE
  ) +
  scale_shape_manual(
    name = NULL, values = plot_shapes,
    breaks = names(plot_shapes),
    labels = c(
      "Lower alpine (n = 2)",
      "Upper alpine (n = 22)",
      "Nival (n = 3)"
    ), drop = FALSE
  ) +
  scale_fill_manual(values = plot_colours, guide = "none") +
  coord_fixed(
    ratio = .72, clip = "on",
    xlim = sample_extent[c("xmin", "xmax")],
    ylim = sample_extent[c("ymin", "ymax")], expand = FALSE
  ) +
  scale_x_continuous(
    breaks = sample_x_breaks,
    labels = function(x) paste0(formatC(x, format = "f", digits = 2), "\u00b0 E")
  ) +
  scale_y_continuous(
    breaks = sample_y_breaks,
    labels = function(y) {
      ifelse(
        abs(y) < 1e-10, "0.00\u00b0",
        paste0(
          formatC(abs(y), format = "f", digits = 2), "\u00b0 ",
          ifelse(y < 0, "S", "N")
        )
      )
    }
  ) +
  annotate("text",
    x = sample_extent["xmin"] + .001,
    y = sample_extent["ymax"] - .001,
    label = "(d)", hjust = 0, vjust = 1,
    colour = "black", fontface = "bold", size = 4.5
  ) +
  annotate("rect",
    xmin = sample_extent["xmin"] + .002,
    xmax = sample_extent["xmin"] + .0065,
    ymin = sample_extent["ymin"] + .0140,
    ymax = sample_extent["ymin"] + .0150,
    fill = "black", colour = "black", linewidth = .45
  ) +
  annotate("rect",
    xmin = sample_extent["xmin"] + .0065,
    xmax = sample_extent["xmin"] + .011,
    ymin = sample_extent["ymin"] + .0140,
    ymax = sample_extent["ymin"] + .0150,
    fill = "white", colour = "black", linewidth = .45
  ) +
  annotate("text",
    x = sample_extent["xmin"] + .0065,
    y = sample_extent["ymin"] + .01535, label = "1 km",
    colour = "black", size = 3.2, vjust = 0
  ) +
  theme_bw(base_size = 9, base_family = figure_font) +
  theme(
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = .9),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey88", linewidth = .25),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 8, colour = "black"),
    axis.ticks = element_line(colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(.5, 4, .5, 0, unit = "mm"),
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(size = 10, face = "bold", colour = "black"),
    legend.text = element_text(size = 10, colour = "black"),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.box.spacing = grid::unit(0, "mm"),
    legend.margin = margin(.2, 1.5, .2, 1.5, unit = "mm")
  ) +
  guides(shape = guide_legend(
    nrow = 1, byrow = TRUE,
    override.aes = list(
      size = 3.7, fill = unname(plot_colours),
      colour = "black", stroke = .45
    )
  ))

# 11. Legends and figure assembly --------------------------------------------

# Extract the two legends so they can occupy the unused area below panel (c).
legend_grob <- cowplot::get_legend(
  p_c + guides(shape = "none") +
    theme(
      legend.position = "bottom", legend.box.just = "center",
      legend.justification = "center",
      legend.margin = margin(2, 2, 2, 1, unit = "mm"),
      legend.title = element_text(
        size = 10.5, face = "bold",
        colour = "black", hjust = .5,
        margin = margin(b = 1.4, unit = "mm")
      ),
      legend.text = element_text(size = 10, colour = "black")
    )
)
legend_panel <- wrap_elements(full = legend_grob, clip = FALSE) +
  theme(plot.margin = margin(-8, 0, 0, 0, unit = "mm"))
p_d <- p_c + theme(
  legend.position = "none",
  plot.margin = margin(.5, .5, .5, -3, unit = "mm")
)
p_e <- p_sampling_inset + theme(legend.position = "none")

# Panel (a) is an upper-left inset within panel (b).
p_b_with_africa <- p_a + inset_element(
  p_africa,
  left = -.018, bottom = .670, right = .392, top = .995,
  align_to = "panel", on_top = TRUE, clip = FALSE
)

# Two-column composition: nested regional context and legends at left;
# vegetation map and sampling-location panel at right.
left_column <- p_b_with_africa / p_b / legend_panel +
  plot_layout(heights = c(.40, .25, .35))
right_column <- p_d / p_e + plot_layout(heights = c(.65, .35))
study_area_map <- left_column | right_column
study_area_map <- study_area_map +
  plot_layout(widths = c(.62, 1.38)) +
  plot_annotation(theme = theme(plot.margin = margin(0, 0, .5, 0,
    unit = "mm"
  )))
study_area_map

ggsave(file.path(out, "Figure_1_study_area_map_R.png"), study_area_map,
  width = 180, height = 150, units = "mm", dpi = 600, bg = "white"
)
ggsave(file.path(out, "Figure_1_study_area_map_R.tiff"), study_area_map,
  width = 180, height = 150, units = "mm", dpi = 600,
  compression = "lzw", bg = "white"
)

# 12. Publication exports ----------------------------------------------------

# Cairo cannot embed the Windows Arial CID font reliably. Build the
# PDF from the 600-dpi Arial raster export to preserve typography.
magick::image_read(file.path(out, "Figure_1_study_area_map_R.png")) |>
  magick::image_write(file.path(out, "Figure_1_study_area_map_R.pdf"),
    format = "pdf", density = 600
  )

message("Created reference-style Figure 1 study-area map.")
