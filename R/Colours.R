#' Colour Functions
#'
#' These function add colours to add to ggplot scales
#' @param Colours used to create ggplot scales
#' @keywords Colours Palettes
#' @export
#' @examples
#' Bede_colours()
#' Bede_cols()
#' Bede_Palettes()
#' Bede_pal()

Bede_colours <- c(
  `red`        = "#bd1c1c",
  `green`      = "#38ab07",
  `blue`       = "#0058db",
  `orange`     = "#e3950e",
  `yellow`     = "#fce517",
  `light grey` = "#cccccc",
  `dark grey`  = "#545454",
  `dark cyan`  = "#008b8b",
  `dark purple`= "#3e0269",
  `light blue` = "#b5f6ff",
  `dark blue`  = "#0e1296",
  `dark gold`  = "#e86a28",
  `dark sea blue` = "#0f3d64",
  `Mussels Rope` = "#002673",
  `Mussels Control` = "#a90000",
  `Orkney RN` = "#56bda2",
  `Orkney RS` = "#dbb13b",
  `Orkney WC` = "#2494a2"
  )

Bede_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (Bede_colours)

  Bede_colours[cols]
}

Bede_palettes <- list(
  `main`  = Bede_cols("dark blue","dark cyan", "yellow"),

  `cool`  = Bede_cols("blue", "green"),

  `hot`   = Bede_cols("yellow", "orange", "red"),

  `mixed` = Bede_cols("dark cyan", "green", "yellow", "orange", "red"),

  `grey`  = Bede_cols("light grey", "dark grey"),

  `Mussels`= Bede_cols("Mussels Rope","Mussels Control"),

  `bathy` = Bede_cols("light blue","dark cyan", "dark blue", "dark purple"),

  `PhD` = Bede_cols("dark cyan", "light grey", "dark gold"),

  `Orkney` = Bede_cols("Orkney RN", "Orkney RS", "Orkney WC")


)

Bede_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- Bede_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)

  }
