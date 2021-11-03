#' Model Plot Function
#'
#' This Function creates a ggplot of Model Predictions using YearNum and Area
#' @param plot used to create ggplot scales
#' @keywords Model Predictions
#' @export
#' @examples
#' plot_model_Bede_Orkney()



plot_model_Bede_Orkney<-function(df_mean,df_pred,Metric,Formula){


  ggplot2::ggplot()+
    ggplot2::geom_errorbar(data=df_mean,
                           mapping=ggplot2::aes(x=YearNum,
                                                y=Mean,
                                                ymin=Mean-CI,
                                                ymax=Mean+CI,
                                                colour=Area),
                  size=1,width=0.1,
                  position = ggplot2::position_dodge(0.1))+
    ggplot2::geom_point(data=df_mean,
                        mapping=ggplot2::aes(x=YearNum,
                                             y=Mean,
                                             shape=Area,
                                             fill=Area),
                        colour="black",
               position = ggplot2::position_dodge(0.1), size=2)+
    ggplot2::geom_ribbon(data=df_pred,
                         mapping=ggplot2::aes(x=YearNum,
                                              y=fit_resp,
                                              ymin=lwr_resp,
                                              ymax=upr_resp,
                                              colour=Area,
                                              fill=Area),
                linetype='twodash',
                alpha=0.2,size=0.5)+
    ggplot2::geom_line(data=df_pred,
                       mapping=ggplot2::aes(x=YearNum,
                                            y=fit_resp,
                                            colour=Area,
                                            linetype=Area))+
    ggplot2::labs(y=paste0(stringr::str_to_title(Metric)),x="Year")+
    scale_colour_Bede("Orkney")+
    scale_fill_Bede("Orkney")+
    theme_Bede()+
    guides(linetype=ggplot2::guide_legend(override.aes=list(fill=NA)))+
    ggtitle(paste0(stringr::str_to_title(Metric),": ",deparse(Formula)))

}
