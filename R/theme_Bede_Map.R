#' A ggplot Theme Function
#'
#' This function adds a custom theme to ggplots
#' @keywords Theme
#' @export
#' @examples
#' df<- data.frame(x=rep(seq(1,10,1),10),y=rep(rnorm(10,4),10))
#' ggplot(df,aes(x=x,y=y))+geom_point()+theme_Bede_Map()


theme_Bede_Map <- function(...){

  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          legend.key = ggplot2::element_rect(fill = "white"),
          panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1),
          legend.position=c(.99,.98),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.title = ggplot2::element_text(size=14),
          legend.text = ggplot2::element_text(size=12),
          panel.background = ggplot2::element_rect(fill = "lightblue"), # bg of the panel
          panel.grid.major = ggplot2::element_blank(), # get rid of major grid
          panel.grid.minor = ggplot2::element_blank(), # get rid of legend panel bg,
          axis.text.x = ggplot2::element_text(size = 14,
                                     colour = "black"),
          axis.text.y = ggplot2::element_text(colour = "black",
                                     size = 14),
          axis.title = ggplot2::element_text(colour = "black",
                                    size = 15)
    )
}
