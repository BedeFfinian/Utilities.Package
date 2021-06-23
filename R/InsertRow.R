#' Insert Row
#'
#' This function adds a row to a df
#' @param Insert rows
#' @keywords Insert
#' @export
#' @examples
#' insertRow()

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}
