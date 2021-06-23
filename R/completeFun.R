#' Complete Fun
#'
#' This function remove NA rows
#' @param Remove NA rows
#' @keywords Remove NAs
#' @export
#' @examples
#' completeFun()


completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
