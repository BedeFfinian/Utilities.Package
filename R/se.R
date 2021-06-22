#' Standard Error Function
#'
#' This function calculates Standard Error
#' @param Standard_Error
#' @keywords se
#' @export
#' @examples
#' se()


se<-function(x)sd(x,na.rm=TRUE)/sqrt(length(x))
