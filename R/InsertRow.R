#' Insert Row
#'
#' This function adds a row to a df
#' @param data the input dataframe
#' @param newrow a vector be added as a row, must be the same length as the number of coloumns in data.
#' @param r the row index where the new row will be added into the dataframe.
#' @keywords Insert
#' @export
#' @examples
#' df<-data.frame(a=c(1,3,4,5),b=c("a","c","d","e"),c=c("z","x","w","u"))
#' df_1<-insertRow(df,c(2,"b","y"),2)
#' df_1

insertRow <- function(data, newrow, r) {
  data[seq(r+1,nrow(data)+1),] <- data[seq(r,nrow(data)),]
  data[r,] <- newrow
  data
}
