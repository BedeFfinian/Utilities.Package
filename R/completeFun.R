#' Complete Fun
#'
#' This function remove NA rows
#' @param data data that you want to remove rows containing NAs in.
#' @param desiredCols column(s) to check for NAs to remove rows where NAs are,
#' @keywords Remove NAs
#' @export
#' @examples
#' df<-data.frame(A=c(1,2,3,NA),B=c("a",NA,"c","d"),C=c("Z","Y",NA,"W"))
#' df_1<-completeFun(df,desiredCols=c("B","C"))
#' df_1


completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
