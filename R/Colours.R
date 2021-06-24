#' Colour Functions
#'
#' These function add colours to add to ggplot scales
#' @param Colours used to create ggplot scales
#' @keywords Colours Palettes
#' @export
#' @examples
#' Bede_cols()
#' Bede_Palettes()
#' Bede_pal()

Bede_colours <- c(
  `red`        = "#d11141",
  `green`      = "#00b159",
  `blue`       = "#00aedb",
  `orange`     = "#f37735",
  `yellow`     = "#ffc425",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c")

Bede_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (Bede_colours)

  Bede_colours[cols]
}

Bede_palettes <- list(
  `main`  = Bede_cols("blue", "green", "yellow"),

  `cool`  = Bede_cols("blue", "green"),

  `hot`   = Bede_cols("yellow", "orange", "red"),

  `mixed` = Bede_cols("blue", "green", "yellow", "orange", "red"),

  `grey`  = Bede_cols("light grey", "dark grey")
)

Bede_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- Bede_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)


  }
