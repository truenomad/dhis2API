# Hex logo, in the flat style of pubdeck's. Writes man/figures/logo.png.

hex <- function(r = 1) {
  angle <- (90 + 60 * 0:5) * pi / 180
  tibble::tibble(x = r * cos(angle), y = r * sin(angle))
}

circle <- function(x, y, r, n = 80) {
  t <- seq(0, 2 * pi, length.out = n)
  tibble::tibble(x = x + r * cos(t), y = y + r * sin(t))
}

ellipse <- function(x, y, rx, ry, from = 0, to = 2 * pi, n = 80) {
  t <- seq(from, to, length.out = n)
  tibble::tibble(x = x + rx * cos(t), y = y + ry * sin(t))
}

slate <- "#2D4052"
border <- "#3A8A8F"
edge <- "#8FA6BA"
leaf_cols <- c("#6FA287", "#D9773E", "#62A3B8", "#D4A64A")

# everything above the name moves up by this much
lift <- 0.05

# the store
db_x <- 0
db_top <- 0.05 + lift
db_bottom <- -0.2 + lift
db_rx <- 0.25
db_ry <- 0.065

# org unit hierarchy: country, two provinces, four districts. The districts
# fan out around the lid at mirrored angles, each placed so its line shows
# the same length above the rim.
leaf_r <- 0.052
visible <- 0.2
gap <- 0.015
angles <- c(165, 130, 50, 15) * pi / 180
# distance from the lid's centre to its rim along each direction
rim <- 1 / sqrt((cos(angles) / db_rx)^2 + (sin(angles) / db_ry)^2)
reach <- rim + visible + leaf_r + gap
leaves <- tibble::tibble(
  x = db_x + reach * cos(angles),
  y = db_top + reach * sin(angles)
)
provinces <- tibble::tibble(
  x = c(mean(leaves$x[1:2]), mean(leaves$x[3:4])),
  y = max(leaves$y) + 0.17
)
country <- tibble::tibble(x = 0, y = provinces$y[[1]] + 0.19)

nodes <- tibble::tibble(
  x = c(country$x, provinces$x, leaves$x),
  y = c(country$y, provinces$y, leaves$y),
  r = c(0.064, 0.05, 0.05, rep(leaf_r, 4)),
  fill = c("#FFFFFF", "#DCE3EA", "#DCE3EA", leaf_cols)
)
tree_edges <- tibble::tibble(
  x = c(country$x, country$x, rep(provinces$x, each = 2)),
  y = c(country$y, country$y, rep(provinces$y, each = 2)),
  xend = c(provinces$x, leaves$x),
  yend = c(provinces$y, leaves$y)
)

# each line leaves its district's edge, points at the lid's centre, and runs
# past the rim so the lid, drawn on top, covers the arrowhead
flows <- tibble::tibble(
  ux = -cos(angles), uy = -sin(angles), colour = leaf_cols
) |>
  dplyr::mutate(
    x = leaves$x + (leaf_r + gap) * ux,
    y = leaves$y + (leaf_r + gap) * uy,
    xend = db_x - rim * ux + 0.06 * ux,
    yend = db_top - rim * uy + 0.06 * uy
  )

# the body: across the top edge, then back along the bottom curve
body <- dplyr::bind_rows(
  ellipse(db_x, db_top, db_rx, db_ry, from = pi, to = 2 * pi),
  ellipse(db_x, db_bottom, db_rx, db_ry, from = 2 * pi, to = pi)
)
# two grooves following the lower curve of the cylinder
bands <- purrr::map(c(-0.04, -0.125) + lift, function(level) {
  ellipse(
    db_x, level, db_rx, db_ry, from = pi + 0.15, to = 2 * pi - 0.15,
    n = 300
  ) |>
    dplyr::mutate(band = as.character(level))
}) |>
  purrr::list_rbind()

node_polys <- purrr::pmap(nodes, function(x, y, r, fill) {
  key <- paste(x, y)
  circle(x, y, r) |> dplyr::mutate(id = key, fill = fill)
}) |>
  purrr::list_rbind()

logo <- ggplot2::ggplot() +
  ggplot2::geom_polygon(
    data = hex(1), ggplot2::aes(x, y), fill = border
  ) +
  ggplot2::geom_polygon(
    data = hex(0.93), ggplot2::aes(x, y), fill = slate
  ) +
  ggplot2::geom_segment(
    data = tree_edges, ggplot2::aes(x, y, xend = xend, yend = yend),
    colour = edge, linewidth = 1.05, lineend = "round"
  ) +
  ggplot2::geom_curve(
    data = flows,
    ggplot2::aes(x, y, xend = xend, yend = yend, colour = colour),
    curvature = 0, linewidth = 1.3, linetype = "22", lineend = "round",
    arrow = grid::arrow(length = grid::unit(0.05, "in"), type = "closed")
  ) +
  ggplot2::scale_colour_identity() +
  ggplot2::geom_polygon(
    data = node_polys, ggplot2::aes(x, y, group = id, fill = fill),
    colour = slate, linewidth = 0.8
  ) +
  ggplot2::scale_fill_identity() +
  # a soft shadow under the store
  ggplot2::geom_polygon(
    data = ellipse(db_x, db_bottom - 0.035, db_rx * 1.2, db_ry * 0.95),
    ggplot2::aes(x, y), fill = "#1F2D3A", alpha = 0.7
  ) +
  ggplot2::geom_polygon(
    data = body, ggplot2::aes(x, y), fill = "#EEF2F5"
  ) +
  ggplot2::geom_path(
    data = bands, ggplot2::aes(x, y, group = band),
    colour = "#9FB1C1", linewidth = 1.4, lineend = "butt"
  ) +
  ggplot2::geom_polygon(
    data = ellipse(db_x, db_top, db_rx, db_ry), ggplot2::aes(x, y),
    fill = "#DDEFEF", colour = "#7FB0B4", linewidth = 1.1
  ) +
  ggplot2::annotate(
    "text", x = 0, y = -0.42, label = "dhis2API", colour = "#FFFFFF",
    family = "Source Sans 3", fontface = "bold", size = 14
  ) +
  ggplot2::coord_fixed(xlim = c(-0.87, 0.87), ylim = c(-1, 1),
                       expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = NA, colour = NA),
    panel.background = ggplot2::element_rect(fill = NA, colour = NA)
  )

fs::dir_create("man/figures")
ragg::agg_png(
  "man/figures/logo.png", width = 2 * 0.866 * 600, height = 2 * 600,
  units = "px", res = 300, background = "transparent"
)
print(logo)
grDevices::dev.off()
