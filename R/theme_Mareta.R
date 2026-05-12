#' A ggplot Theme Function
#'
#' This function adds a custom theme to ggplots
#' @keywords Theme
#' @export theme_Mareta
#' @examples
#' df<- data.frame(x=rep(seq(1,10,1),10),y=rep(rnorm(10,4),10))
#' ggplot(df,aes(x=x,y=y))+geom_point()+theme_Mareta()

theme_Mareta <- function(...){

  ggplot2::theme_classic()+
    ggplot2::theme(plot.title.position = "plot",
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"),
                   rect = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(), # bg of the panel
                   panel.grid.major = ggplot2::element_line(linetype = "dotted",
                                                            colour="grey30",
                                                            linewidth=0.25),
                   panel.ontop = TRUE,
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.text.align = 0.5,
                   legend.title.align = 0,
                   plot.background=ggplot2::element_rect(fill=NA),
                   axis.title = ggplot2::element_text(colour='black',family='poppins'),
                   axis.text.y = ggplot2::element_text(colour='black',family='poppins'),
                   axis.text.x = ggplot2::element_text(colour='black',family='poppins'),
                   text = ggplot2::element_text(colour='black',family='poppins'),
                   panel.border = ggplot2::element_rect(colour='black'),
                   axis.ticks = ggplot2::element_line(colour='black'),
                   ...)
}

#' This function adds a custom theme to ggplots
#' @keywords Theme
#' @export theme_Mareta_Dark
#' @examples
#' df<- data.frame(x=rep(seq(1,10,1),10),y=rep(rnorm(10,4),10))
#' ggplot(df,aes(x=x,y=y))+geom_point()+theme_Mareta_Dark()

theme_Mareta_Dark <- function(...){

  ggplot2::theme_classic()+
    ggplot2::theme(plot.title.position = "plot",
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "grey50"),
                   rect = ggplot2::element_blank(),
                   panel.background = ggplot2::rect(fill='grey20'), # bg of the panel
                   panel.grid.major = ggplot2::element_line(linetype = "dotted",
                                                            colour="grey30",
                                                            linewidth=0.25),
                   panel.ontop = TRUE,
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.text.align = 0.5,
                   legend.title.align = 0,
                   plot.background=ggplot2::element_rect(fill='grey20'),
                   axis.title = ggplot2::element_text(colour='grey50',family='poppins'),
                   axis.text.y = ggplot2::element_text(colour='grey50',family='poppins'),
                   axis.text.x = ggplot2::element_text(colour='grey50',family='poppins'),
                   text = ggplot2::element_text(colour='grey50',family='poppins'),
                   panel.border = ggplot2::element_rect(colour='grey50'),
                   axis.ticks = ggplot2::element_line(colour='grey50'),
                   ...)
}
