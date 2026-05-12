#' A ggplot Theme Function
#'
#' This function adds a custom theme to ggplots
#' @keywords Theme
#' @export theme_Bede
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

#' This function adds a custom theme to ggplots
#' @keywords Theme
#' @export theme_Bede_Dark
#' @examples
#' df<- data.frame(x=rep(seq(1,10,1),10),y=rep(rnorm(10,4),10))
#' ggplot(df,aes(x=x,y=y))+geom_point()+theme_Bede_Dark()


theme_Bede_Dark <- function(...){

  ggplot2::theme_classic()+
    ggplot2::theme(panel.grid = ggplot2::element_line(color = "#b4aea9"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.title.position = "plot",
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "grey50"),
                   rect = ggplot2::element_blank(),
                   plot.background=ggplot2::element_rect(fill='grey20'),
                   axis.title = ggplot2::element_text(face="bold",colour='grey50',family='poppins'),
                   axis.text.y = ggplot2::element_text(colour='grey50',family='poppins'),
                   axis.text.x = ggplot2::element_text(colour='grey50',family='poppins'),
                   text = ggplot2::element_text(colour='grey50',family='poppins'),
                   panel.border = ggplot2::element_rect(colour='grey50', fill=NA, linewidth=1),
                   axis.ticks = ggplot2::element_line(colour='grey50'),
                   ...)
}

#' This function adds a custom theme to ggplots
#' @keywords Theme
#' @export theme_Bede_MDS
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
                   axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   ...)
}

#' This function adds a custom theme to ggplots
#' @keywords Theme
#' @export theme_Bede_Map
#' @examples
#' df<- data.frame(x=rep(seq(1,10,1),10),y=rep(rnorm(10,4),10))
#' ggplot(df,aes(x=x,y=y))+geom_point()+theme_Bede_Map()

theme_Bede_Map <- function(...){

  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.key = ggplot2::element_rect(fill = "white"),
    panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth=1),
    panel.background = ggplot2::element_blank(), # bg of the panel
    panel.grid.major = ggplot2::element_line(linetype = "dotted",
                                             colour="grey30",
                                             linewidth=0.25),
    panel.ontop = TRUE,
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 14,
                                        colour = "black"),
    axis.text.y = ggplot2::element_text(colour = "black",
                                        size = 12),
    axis.title = ggplot2::element_text(colour = "black",
                                       size = 12),
    strip.background = element_rect(fill = "white",
                                    colour = "black", linewidth = rel(2)),
    ...
  )
}
