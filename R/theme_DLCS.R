#' A ggplot Theme Function
#'
#' This function adds a custom theme to ggplots from a project on Deep Learning Convolutional Stacking
#' @keywords Theme
#' @export
#' @examples
#' df<- data.frame(x=rep(seq(1,10,1),10),y=rep(rnorm(10,4),10))
#' ggplot(df,aes(x=x,y=y))+geom_point()+theme_DLCS()


theme_DLCS <- function(...){

  ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(size=8, colour = "black"),
          axis.text.x = ggplot2::element_text(size=8, colour = "black"),
          axis.text.y = ggplot2::element_text(size=8, colour = "black"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          ...)
}
