#' Scale Colour Function
#'
#' This function add colours to add to ggplot scales
#' @param Colours used to create ggplot scales
#' @keywords Colours Palettes
#' @export
#' @examples
#' scale_colour_Bede()

scale_colour_Bede <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- Bede_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("Bede_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), ...)
  }
}
