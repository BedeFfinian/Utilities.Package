#' Scale Colour Function
#'
#' This function add colours to add to ggplot scales
#' @param palette Select one of the preset colour palettes within the package. "main" is default. other options include "cool", "hot","mixed", "grey", "mussels" "bathy", "PhD", "Orkney", "Bathy_Blues" and "SAS"
#' @param discrete Select whether colour scale is discrete or not. Automatic unless selected. TRUE is default.
#' @param reverse Select which direction a scale should be applied.
#' @keywords Colours Palettes
#' @export
#' @examples
#'
#' df<- data.frame(x=rep(seq(1,10,1),10),
#'                 y=rep(rnorm(10,4),10),
#'                 Treatment=rep(c("A","B"),50))
#'
#' ggplot(df,aes(x=x,y=y,colour=Treatment))+
#' geom_point()+
#' scale_colour_Bede("SAS")
#'

scale_colour_Bede <- function(palette = "main",  discrete = TRUE, reverse = FALSE, ...) {
  pal <- Bede_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("Bede_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), ...)
  }
}
