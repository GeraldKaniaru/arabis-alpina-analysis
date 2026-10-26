# ============================================================================
# SAMPLING-DESIGN SCHEMATIC
# ============================================================================
# Purpose: illustrate the route-based belt transect and 100-m elevation bands.
# The transect drawing is conceptual and is not a geographic map.

# 1. Packages and project setup ----------------------------------------------

options(stringsAsFactors = FALSE)

required_packages <- c("ggplot2", "patchwork", "readxl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install required packages: ", paste(missing_packages, collapse = ", "))
}

library(ggplot2)
library(patchwork)

project_marker <- "arabis analysis.Rproj"
if (!file.exists(project_marker)) {
  stop("Run this script from the 'arabis analysis' project directory.")
}

dir.create("outputs/sampling_design", recursive = TRUE, showWarnings = FALSE)

figure_font <- "Arial"
data_file <- file.path("data", "raw", "Datasets", "trait_data.xlsx")
output_dir <- file.path("outputs/sampling_design")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Validate observed sampling units ----------------------------------------

# Verify the analytical sampling structure from the current primary dataset.
raw <- readxl::read_excel(data_file, sheet = "Sheet2")
names(raw) <- make.unique(names(raw))
elevation_name <- grep("^Elevation(\\.\\.\\.[0-9]+)?$", names(raw), value = TRUE)[1]
if (is.na(elevation_name)) stop("No elevation column was found in trait_data.xlsx.")

plot_data <- unique(data.frame(
  Block = raw$Block,
  Plot = raw$Plot,
  Elevation = raw[[elevation_name]],
  ElevationRange = raw$ElevationRange,
  VegetationZone = raw$VegetationZone
))
plot_data <- plot_data[order(plot_data$Elevation, plot_data$Block, plot_data$Plot), ]
plot_data$VegetationZone <- factor(
  plot_data$VegetationZone,
  levels = c("Lower alpine", "Upper alpine", "Nival")
)

stopifnot(nrow(plot_data) == 27L)
stopifnot(length(unique(plot_data$Block)) == 9L)

zone_colours <- c(
  "Lower alpine" = "#56B4E9",
  "Upper alpine" = "#009E73",
  "Nival" = "#E69F00"
)

# 3. Define complete surveyed hierarchy --------------------------------------

# Full surveyed hierarchy confirmed by the researcher on 23 October 2026.
elevation_limits <- c(3000, 4800)
band_height <- 100
n_bands <- diff(elevation_limits) / band_height
blocks_per_band <- 2L
plots_per_block <- 10L
n_blocks <- n_bands * blocks_per_band
n_plots <- n_blocks * plots_per_block
stopifnot(n_bands == 18L, n_blocks == 36L, n_plots == 360L)
survey_counts <- data.frame(
  Unit = c("Elevation bands", "Sampling blocks", "Plots"),
  Surveyed = c(n_bands, n_blocks, n_plots),
  Occupied = c(length(unique(floor((plot_data$Elevation - 3000) / 100))), 9L, 27L),
  Basis = "All units surveyed, confirmed by researcher 23 October 2026"
)
survey_counts$Unoccupied <- survey_counts$Surveyed - survey_counts$Occupied
write.csv(survey_counts, file.path(output_dir, "Surveyed_sampling_units.csv"), row.names = FALSE)

# 4. Panel a: conceptual mountain transect -----------------------------------

mountain <- data.frame(
  x = c(
    0.04, 0.14, 0.24, 0.33, 0.41, 0.47, 0.50,
    0.53, 0.56, 0.59, 0.65, 0.74, 0.84, 0.96
  ),
  y = c(
    3000, 3260, 3570, 3900, 4240, 4520, 4710,
    4800, 4715, 4770, 4520, 4150, 3650, 3000
  )
)

route <- data.frame(
  x = c(0.49, 0.495, 0.500, 0.505, 0.510, 0.520),
  y = c(3000, 3400, 3800, 4100, 4450, 4800)
)

# Horizontal offsets separate plots that occupy the same elevation block.
block_counts <- ave(plot_data$Elevation, plot_data$Block, FUN = length)
within_block_rank <- ave(plot_data$Elevation, plot_data$Block,
  FUN = rank,
  ties.method = "first"
)
plot_data$x <- 0.505 +
  (plot_data$Elevation - 3763) / (4800 - 3000) * 0.012 +
  (within_block_rank - (block_counts + 1) / 2) * 0.010

block_lines <- data.frame(y = seq(3000, 4800, by = 100))
block_lines$xmin <- 0.04 + (block_lines$y - 3000) / 1800 * 0.46
block_lines$xmax <- 0.96 - (block_lines$y - 3000) / 1800 * 0.43

panel_a <- ggplot() +
  geom_polygon(
    data = mountain, aes(x, y), fill = "grey94", colour = "grey35",
    linewidth = 0.7
  ) +
  geom_segment(
    data = block_lines,
    aes(x = xmin, xend = xmax, y = y, yend = y),
    colour = "grey60", linewidth = 0.40, linetype = "dashed"
  ) +
  geom_path(
    data = route, aes(x, y), colour = "black", linewidth = 1.0,
    lineend = "round"
  ) +
  geom_point(
    data = plot_data,
    aes(x, Elevation), shape = 24, fill = "#D1B659",
    size = 4.5, stroke = 0.65, colour = "black", show.legend = FALSE
  ) +
  annotate("text",
    x = 0.535, y = 3270, label = "Established route",
    family = figure_font, size = 6.0, angle = 88, hjust = 0, color = "black"
  ) +
  annotate("segment",
    x = 0.78, xend = 0.78, y = 3763, yend = 4567,
    linewidth = 0.8, colour = "grey25"
  ) +
  annotate("segment",
    x = 0.765, xend = 0.795, y = 3763, yend = 3763,
    linewidth = 0.8, colour = "grey25"
  ) +
  annotate("segment",
    x = 0.765, xend = 0.795, y = 4567, yend = 4567,
    linewidth = 0.8, colour = "grey25"
  ) +
  annotate("text",
    x = .71, y = 4880, hjust = 0, vjust = 1,
    label = "Surveyed: 18 bands\n36 blocks; 360 plots",
    family = figure_font, size = 5.5, colour = "black"
  ) +
  annotate("text",
    x = 0.80, y = 4165,
    label = paste0("27 occupied plots\n9 occupied blocks\n3,763\u20134,567 m"),
    family = figure_font, size = 5.5, hjust = 0
  ) +
  annotate("text",
    x = 0.04, y = 3530, label = "100-m\nelevation bands",
    family = figure_font, size = 6, hjust = 0, vjust = -0.3, color = "black"
  ) +
  scale_y_continuous(
    limits = c(2800, 5000), breaks = seq(0, 4800, 600),
    labels = function(x) format(x, big.mark = ","),
    expand = expansion(mult = c(0, 0.03))
  ) +
  coord_cartesian(xlim = c(0, 1.04), clip = "off") +
  annotate("text",
    x = .52, y = 2870, label = "200-m belt: 100 m on each side of route",
    family = figure_font, size = 6, color = "black"
  ) +
  labs(
    subtitle = "(a) Elevation bands and occupied plots", x = NULL, y = "Elevation (m)",
    fill = NULL, shape = NULL
  ) +
  theme_classic(base_family = figure_font, base_size = 14) +
  theme(
    axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.text.y = element_text(colour = "black", size = 15),
    axis.title.y = element_text(colour = "black", face = "bold", size = 16),
    plot.subtitle = element_text(face = "bold", size = 16, hjust = .5, color = "black"),
    legend.position = "none", legend.direction = "horizontal",
    legend.text = element_text(size = 14, face = "plain", colour = "black"),
    legend.key = element_blank(),
    legend.margin = margin(0, 0, 0, 0, unit = "mm"),
    legend.box.spacing = grid::unit(0.5, "mm"),
    plot.margin = margin(0.5, 5, 3, 4, unit = "mm")
  )

# 5. Panel b: sampling blocks and plot placement -----------------------------

# Panel b: pairs of 100 x 100 m blocks on opposite sides of the route.
# Two illustrative bands are expanded here; the full survey has 18 bands.
plot_size <- 5
min_spacing <- 10
edge_gap <- function(a, b) {
  dx <- max(a[1] - b[1] - plot_size, b[1] - a[1] - plot_size, 0)
  dy <- max(a[2] - b[2] - plot_size, b[2] - a[2] - plot_size, 0)
  sqrt(dx^2 + dy^2)
}
random_plots <- function(seed, xoffset, yoffset, label) {
  set.seed(seed)
  xy <- matrix(numeric(), ncol = 2)
  attempts <- 0L
  while (nrow(xy) < plots_per_block && attempts < 100000L) {
    attempts <- attempts + 1L
    point <- runif(2, 0, 100 - plot_size)
    if (!nrow(xy) || all(apply(xy, 1, function(z) edge_gap(point, z) >= min_spacing))) xy <- rbind(xy, point)
  }
  stopifnot(nrow(xy) == plots_per_block)
  data.frame(x = xy[, 1] + xoffset, y = xy[, 2] + yoffset, block = label)
}
block_outlines <- data.frame(
  xmin = c(-100, 0, -100, 0), xmax = c(0, 100, 0, 100),
  ymin = c(0, 0, 120, 120), ymax = c(100, 100, 220, 220),
  block = c("A", "B", "C", "D"), label = paste("Block", c("A", "B", "C", "D"))
)
plot_squares <- do.call(rbind, lapply(seq_len(nrow(block_outlines)), function(i) {
  random_plots(20211000 + i, block_outlines$xmin[i], block_outlines$ymin[i], block_outlines$block[i])
}))
stopifnot(
  all(table(plot_squares$block) == 10L),
  all(block_outlines$xmax - block_outlines$xmin == 100),
  all(block_outlines$ymax - block_outlines$ymin == 100)
)
for (b in unique(plot_squares$block)) {
  z <- as.matrix(plot_squares[plot_squares$block == b, c("x", "y")])
  stopifnot(min(combn(seq_len(nrow(z)), 2, function(i) edge_gap(z[i[1], ], z[i[2], ]))) >= min_spacing)
}
write.csv(plot_squares, file.path(output_dir, "Figure_S2_illustrative_coordinates.csv"), row.names = FALSE)
example <- subset(plot_squares, block == "A")
example <- example[which.min((example$x + 35)^2 + (example$y - 25)^2), ]
both <- grid::arrow(ends = "both", type = "closed", length = grid::unit(1.5, "mm"))
panel_b_diagram <- ggplot() +
  annotate("rect", xmin = -100, xmax = 100, ymin = 100, ymax = 120, fill = "grey95", colour = NA) +
  geom_rect(data = block_outlines, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#EEF5E8", colour = "grey35", linewidth = .7) +
  geom_rect(data = plot_squares, aes(xmin = x, xmax = x + plot_size, ymin = y, ymax = y + plot_size), fill = "#D1B659", colour = "black", linewidth = .5) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = 243, linewidth = .8) +
  geom_label(
    data = block_outlines, aes(x = (xmin + xmax) / 2, y = ymax + 5, label = label),
    family = figure_font, fontface = "bold", size = 5.0, fill = "white", linewidth = 0, label.padding = grid::unit(.25, "mm")
  ) +
  annotate("label", x = 0, y = 247, label = "Route", family = figure_font, size = 5.0, fill = "white", linewidth = 0, label.padding = grid::unit(.2, "mm")) +
  annotate("text", x = 0, y = 280, label = "Two blocks per elevation band\n(one on each side of the route)", family = figure_font, size = 5.5, lineheight = 1.1) +
  annotate("segment", x = -100, xend = example$x, y = 0, yend = 0, colour = "#276B9A", linewidth = .8, arrow = grid::arrow(length = grid::unit(1.5, "mm"))) +
  annotate("segment", x = example$x, xend = example$x, y = 0, yend = example$y, colour = "#276B9A", linetype = "dotted", linewidth = .6) +
  annotate("segment", x = -100, xend = -100, y = 0, yend = example$y, colour = "#276B9A", linewidth = .8, arrow = grid::arrow(length = grid::unit(1.5, "mm"))) +
  annotate("segment", x = -100, xend = example$x, y = example$y, yend = example$y, colour = "#276B9A", linetype = "dotted", linewidth = .5) +
  annotate("point", x = -100, y = 0, size = 2, colour = "#276B9A") +
  annotate("text", x = -103, y = -3, label = "O", hjust = 1, vjust = 1, family = figure_font, size = 5.0, fontface = "bold", colour = "#276B9A") +
  annotate("text", x = example$x + 3, y = example$y - 3, label = "(x, y)", hjust = 0, vjust = 1, family = figure_font, size = 5.0, colour = "#276B9A") +
  annotate("segment", x = -113, xend = -113, y = 0, yend = 100, arrow = both, linewidth = .6) +
  annotate("text", x = -123, y = 50, label = "100 m", angle = 90, family = figure_font, size = 5.0) +
  annotate("segment", x = -100, xend = 0, y = -13, yend = -13, arrow = both, linewidth = .6) +
  annotate("segment", x = 0, xend = 100, y = -13, yend = -13, arrow = both, linewidth = .6) +
  annotate("text", x = -50, y = -22, label = "100 m", family = figure_font, size = 5.0) +
  annotate("text", x = 50, y = -22, label = "100 m", family = figure_font, size = 5.0) +
  annotate("segment", x = -100, xend = 100, y = -39, yend = -39, arrow = both, linewidth = .6) +
  annotate("text", x = 0, y = -50, label = "200-m belt (ground)", family = figure_font, size = 5.0) +
  annotate("segment", x = 108, xend = 108, y = 100, yend = 120, arrow = both, linewidth = .6) +
  annotate("text",
    x = 116, y = 110, hjust = 0, label = "\u226520-m buffer\nGround distance\nbetween\nblock rows",
    family = figure_font, size = 5.2, lineheight = 1.15
  ) +
  coord_fixed(xlim = c(-130, 225), ylim = c(-65, 300), clip = "off") +
  labs(subtitle = "(b) Paired ground blocks and plot coordinates") +
  theme_void(base_family = figure_font) +
  theme(plot.subtitle = element_text(face = "bold", size = 16, hjust = .5, colour = "black"), plot.margin = margin(5, 0, 5, 0))

# Keep the notes in their own row, with clear space below the belt dimension.
panel_b_notes <- ggplot() +
  annotate("text",
    x = .175, y = .88, hjust = 0, vjust = 1,
    label = "Ten non-overlapping 5 \u00d7 5 m plots per block\nRandom coordinate pairs\nalong perpendicular axes\nfrom fixed reference corner O\n\u226510 m between plot boundaries",
    family = figure_font, size = 6, lineheight = 1.15, color = "black"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(2, 2, 0, 4, unit = "mm"))
panel_b <- (panel_b_diagram / panel_b_notes) + plot_layout(heights = c(3.6, 1))

# 6. Panel c: rosette sampling units -----------------------------------------

# Panel c: a plot-level sampling-unit detail, with different rosette sizes.
rosettes <- data.frame(x = c(1, 1.25, 3.35, 3.9), y = c(1.25, 3.55, 3.8, 1.55), radius = c(.30, .39, .26, .43), label = paste0("R", 1:4))
leaves <- do.call(rbind, lapply(1:nrow(rosettes), function(i) {
  do.call(rbind, lapply(0:7, function(k) {
    theta <- seq(0, 2 * pi, length.out = 31)
    angle <- k * pi / 4
    r <- rosettes$radius[i]
    u <- r * (.53 + .53 * cos(theta))
    v <- .19 * r * sin(theta)
    data.frame(x = rosettes$x[i] + u * cos(angle) - v * sin(angle), y = rosettes$y[i] + u * sin(angle) + v * cos(angle), leaf = paste(i, k))
  }))
}))
panel_c <- ggplot() +
  annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax = 5, fill = "#F5F1E5", colour = "grey35", linewidth = .7) +
  annotate("curve", x = 1.25, y = 3.55, xend = 3.35, yend = 3.8, curvature = -.18, colour = "grey35", linewidth = .7) +
  geom_polygon(data = leaves, aes(x, y, group = leaf), fill = "#438C57", colour = "#235C35", linewidth = .15) +
  geom_point(data = rosettes, aes(x, y), shape = 21, fill = "#7D4E2C", colour = "black", size = 1.7, stroke = .3) +
  geom_text(data = rosettes, aes(x = x, y = y - radius - .22, label = label), family = figure_font, size = 5.0, fontface = "bold") +
  annotate("text", x = 2.5, y = 4.65, label = "5 \u00d7 5 m plot detail", family = figure_font, size = 5.5, fontface = "bold") +
  annotate("text", x = 2.5, y = 3.0, label = "Stolon\nconnection", family = figure_font, size = 5.0, colour = "black") +
  annotate("segment", x = 0, xend = 5, y = -.3, yend = -.3, arrow = both, linewidth = .6) +
  annotate("text", x = 2.5, y = -.55, label = "5 m", family = figure_font, size = 5.0) +
  annotate("segment", x = 5.25, xend = 5.25, y = 0, yend = 5, arrow = both, linewidth = .6) +
  annotate("text", x = 5.55, y = 2.5, label = "5 m", angle = 90, family = figure_font, size = 5.0) +
  annotate("text",
    x = -.03, y = -.95, hjust = 0, vjust = 1,
    label = "\u2022 Select well-spaced rosettes\n   representing different\n   plant sizes.\n\n\u2022 Treat each separately rooted\n   rosette as a sampling unit.\n\n\u2022 Do not assume\n   stolon-connected rosettes\n   are distinct genetic\n   individuals.\n\n\u2022 581 sampled rosettes\n   across 27 occupied plots.",
    family = figure_font, size = 6.0, lineheight = 1.0, colour = "black"
  ) +
  coord_fixed(xlim = c(-.3, 5.9), ylim = c(-9.0, 5.7), clip = "off") +
  labs(subtitle = "(c) Rosette sampling within plots") +
  theme_void(base_family = figure_font) +
  theme(
    plot.subtitle = element_text(face = "bold", size = 16, hjust = .5, color = "black"),
    plot.margin = margin(5, 5, 5, 0)
  )

# 7. Assemble figure and export caption --------------------------------------

figure <- panel_a + panel_b + panel_c + plot_layout(widths = c(1.1, 1.7, 1.0))
output_base <- file.path(output_dir, "Figure_S2_sampling_design")
ggsave(paste0(output_base, ".png"), figure, width = 420, height = 210, units = "mm", dpi = 600, bg = "white")
ggsave(paste0(output_base, ".tiff"), figure, width = 420, height = 210, units = "mm", dpi = 600, compression = "lzw", bg = "white")
ggsave(paste0(output_base, ".pdf"), figure, width = 420, height = 210, units = "mm", device = cairo_pdf, family = figure_font, bg = "white")
caption <- paste0(
  "Figure S2. Sampling hierarchy for Arabis alpina on Mount Kenya. ",
  "(a) The 200-m-wide belt followed an established route from 3,000 to 4,800 m, extending 100 m perpendicular to each side. Eighteen contiguous 100-m elevation bands were surveyed. Two 100 \u00d7 100 m blocks per band and ten plots per block gave 36 surveyed blocks and 360 surveyed plots, including unoccupied units. Arabis alpina occurred in nine blocks and 27 plots (3,763\u20134,567 m); 27 blocks and 333 plots were unoccupied. Dashed horizontal lines mark elevation-band boundaries and triangles identify occupied plots. ",
  "(b) Two illustrative pairs show one block on each side of the route within each band. Blocks are 100 \u00d7 100 m; the full belt is 200 m wide. The illustrated ground buffer separates successive block rows, not the paired blocks across the route, and represents the minimum 20-m ground distance rather than a 20-m elevational interval. Within each block, ten non-overlapping 5 \u00d7 5 m plots were randomly positioned using coordinate pairs along perpendicular axes from a fixed reference corner O, with at least 10 m between plot boundaries (Bureau of Land Management, 1996). All ten plots are shown in each of the four illustrative blocks. Generated coordinates and block identities are schematic. ",
  "(c) Well-spaced rosettes representing different plant sizes were selected in occupied plots. Each separately rooted rosette was a sampling unit; stolon-connected rosettes were not assumed to be genetically distinct. The dataset contains 581 sampled rosettes across 27 occupied plots. The detail does not imply a fixed number of plants per plot. The mountain and allocation of ground blocks to elevation bands are conceptual; they are not a map or a physical slope profile."
)
writeLines(caption, paste0(output_base, "_caption.txt"), useBytes = TRUE)
message("Created updated Figure S2: ", output_base)
