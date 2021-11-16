#' Model Plot Function
#'
#' This Function creates a ggplot of Model Predictions using a X variable (xvar) and treatment (colourvar)
#' @param plot used to create ggplot scales
#' @keywords Model Predictions
#' @export
#' @examples
#' plot_model_Bede()



plot_model_Bede<-function(df_mean,df_pred,Metric,Name,Formula,xvar,colourvar,pallette,xlabel,Categorical.x){

  if(Categorical.x==FALSE){

  ggplot2::ggplot()+
    ggplot2::geom_errorbar(data=df_mean,
                           mapping=ggplot2::aes(x=.data[[xvar]],
                                                y=Mean,
                                                ymin=Mean-CI,
                                                ymax=Mean+CI,
                                                colour=.data[[colourvar]]),
                  size=1,width=0.1,
                  position = ggplot2::position_dodge(0.1))+
    ggplot2::geom_point(data=df_mean,
                        mapping=ggplot2::aes(x=.data[[xvar]],
                                             y=Mean,
                                             shape=.data[[colourvar]],
                                             fill=.data[[colourvar]]),
                        colour="black",
               position = ggplot2::position_dodge(0.1), size=2)+
    ggplot2::geom_ribbon(data=df_pred,
                         mapping=ggplot2::aes(x=.data[[xvar]],
                                              y=fit_resp,
                                              ymin=lwr_resp,
                                              ymax=upr_resp,
                                              colour=.data[[colourvar]],
                                              fill=.data[[colourvar]]),
                linetype='twodash',
                alpha=0.2,size=0.5)+
    ggplot2::geom_line(data=df_pred,
                       mapping=ggplot2::aes(x=.data[[xvar]],
                                            y=fit_resp,
                                            colour=.data[[colourvar]],
                                            linetype=.data[[colourvar]]))+
    ggplot2::labs(y=paste0(stringr::str_to_title(Metric)),x=xlabel)+
    scale_colour_Bede(pallette)+
    scale_fill_Bede(pallette)+
    theme_Bede()+
    guides(linetype=ggplot2::guide_legend(override.aes=list(fill=NA)))+
    ggtitle(paste0(stringr::str_to_title(Metric),": ",deparse(Formula)))+
    scale_y_continuous(name=paste0(stringr::str_to_title(Name)),
                       limits = c((min(df_mean$Mean)-max(df_mean$CI)),
                                  (max(df_mean$Mean)+max(df_mean$CI))),
                       oob = scales::squish)}

  if(Categorical.x==TRUE){

    ggplot2::ggplot()+
      ggplot2::geom_errorbar(data=df_pred,
                             mapping=ggplot2::aes(x=.data[[xvar]],
                                                  y=fit_resp,
                                                  ymin=lwr_resp,
                                                  ymax=upr_resp,
                                                  colour=.data[[colourvar]]),
                             size=1,width=0.1,
                             position = ggplot2::position_dodge(0.1))+
      ggplot2::geom_point(data=df_pred,
                          mapping=ggplot2::aes(x=.data[[xvar]],
                                               y=fit_resp,
                                               shape=.data[[colourvar]],
                                               fill=.data[[colourvar]]),
                          colour="black",
                          position = ggplot2::position_dodge(0.1), size=2)+
      ggplot2::labs(y=paste0(stringr::str_to_title(Metric)),x=xlabel)+
      scale_colour_Bede(pallette)+
      scale_fill_Bede(pallette)+
      theme_Bede()+
      guides(linetype=ggplot2::guide_legend(override.aes=list(fill=NA)))+
      ggtitle(paste0(stringr::str_to_title(Metric),": ",deparse(Formula)))+
      scale_y_continuous(name=paste0(stringr::str_to_title(Name)),
                         limits = c((min(df_pred$lwr_resp)),
                                    (max(df_pred$upr_resp))),
                         oob = scales::squish)}


}
