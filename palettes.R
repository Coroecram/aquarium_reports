colors <- c(
  `red`        = "#d11141",
  `green`      = "#00b159",
  `blue`       = "#00aedb",
  `orange`     = "#f37735",
  `yellow`     = "#ffc425",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c",
  `purple`     = "#E800E1",
  `magenta`    = "#E5008B",
  `deep blue`    = "#4C007E")

cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (colors)

  colors[cols]
}

palettes <- list(
  `main`  = cols("blue", "green", "yellow"),

  `cool`  = cols("blue", "green"),

  `hot`   = cols("yellow", "orange", "red"),

  `mixed` = cols("blue", "green", "yellow", "orange", "red"),

  `ph`    = cols("red", "magenta", "green", "yellow", "orange"),

  `temp`  = cols("deep blue", "blue", "purple", "magenta", "red"),

  `grey`  = cols("light grey", "dark grey")
)

pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

scale_color_custom <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
