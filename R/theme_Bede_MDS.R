#' A ggplot Theme Function
#'
#' This function adds a custom theme to ggplots specifically for plotting AXIS less MDS plots
#' @keywords Theme
#' @export
#' @examples
#' df<- data.frame(x=rep(seq(1,10,1),10),y=rep(rnorm(10,4),10))
#' ggplot(df,aes(x=x,y=y))+geom_point()+theme_Bede_MDS()


theme_Bede_MDS <- function(...){

  ggplot2::theme_classic()+
    ggplot2::theme(panel.grid = ggplot2::element_line(color = "#b4aea9"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.title.position = "plot",
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "grey50"),
                   rect = ggplot2::element_blank(),
                   panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                   legend.text.align = 0.5,
                   legend.title.align = 0,
                   axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   ...)
}
