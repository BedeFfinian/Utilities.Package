#' Signif Down
#'
#' This function applies signif figure adjustment but rounds down
#' @param x data that you want to remove rows containing NAs in.
#' @param digits number of significant digits to change values to
#' @keywords Round down to signif figures
#' @export
#' @examples
#'
#' down_signif(3599, digits = 2)
#' 3500
#' down_signif(7890349, digits = 2)
#' 7800000
#'
#'


down_signif <- function(x, digits = 0) {
  m <- 10^(ceiling(log(x, 10)) - digits)
  (x %/% m)*m
}
