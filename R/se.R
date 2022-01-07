#' Standard Error Function
#'
#' This function calculates Standard Error
#' @param x vector of values to calculate Standard error from
#' @keywords se
#' @export
#' @examples
#' vec<-rnorm(200,0)
#' vec_se<-se(vec)
#' vec_se


se<-function(x){
  sd(x,na.rm=TRUE)/sqrt(length(x))
}
