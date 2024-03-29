#' A ggplot Theme Function
#'
#' This function adds a custom theme to ggplots
#' @keywords Theme
#' @export
#' @examples
#' df<- data.frame(x=rep(seq(1,10,1),10),y=rep(rnorm(10,4),10))
#' ggplot(df,aes(x=x,y=y))+geom_point()+theme_Bede()


theme_Bede <- function(...){

  ggplot2::theme_classic()+
    ggplot2::theme(panel.grid = ggplot2::element_line(color = "#b4aea9"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.title.position = "plot",
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "grey50"),
                   axis.title.y = ggplot2::element_text(face="bold", angle=90),
                   axis.title.x = ggplot2::element_text(face="bold"),
                   rect = ggplot2::element_blank(),
                   panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                   legend.text.align = 0.5,
                   legend.title.align = 0,
                   axis.title = ggplot2::element_text(size=11.5,vjust=0.5),
                   axis.text.x = ggplot2::element_text(size = 10.5,
                                                       colour = "black"),
                   axis.text.y = ggplot2::element_text(colour = "black",
                                                       size = 10.5),
                   ...)
}
