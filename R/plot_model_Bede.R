#' Model Plot Function
#'
#' This Function creates a ggplot of Model Predictions using a X variable (xvar) and treatment (colourvar)
#' @param df_mean dataframe of raw data that contains columns explicitly named Mean and CI. and two columns xvar and colourvar (named in the item xvar and colourvar)
#' @param df_pred dataframe of prediction data contains columns explicitly named fit_resp, upr_resp and lwr_resp. and two columns xvar and colourvar (named in the item xvar and colourvar)
#' @param Metric name of metric being analysed
#' @param Formula Formula object that will be printed above each graph
#' @param xvar the name of the column in df_mean and df_pred to be plotted on x axis
#' @param colourvar the name of the column in df_mean and df_pred to be plotted as the colour
#' @param pallette the colour palette to select from Utilities.Package (default="main")
#' @param xlabel the label to be written below x axis
#' @param Categorical.x logical, TRUE if x is categorical, FALSE if x is not categorical. When categorical x, no raw data (data from df_mean) is plotted
#' @keywords Model Predictions
#' @export
#' @examples
#' library(purrr)
#' library(patchwork)
#'
#' df1<-data.frame(Response=rnorm(3000, mean=10, sd=2),
#'                 Var1=rep(c("A","B","C"),1000),
#'                 Var2=c(rep("X",1000),rep("Y",1000),rep("Z",1000)),
#'                 RandomVar1=rep(c("ID_1","ID_2","ID_3","ID_4"),750),
#'                 Metric=rep("Water_Temp",3000),
#'                 Family=rep("gaussian",3000))
#'
#' df2<-data.frame(Response=rbeta(3000,0.5,1),
#'                 Var1=rep(c("A","B","C"),1000),
#'                 Var2=c(rep("X",1000),rep("Y",1000),rep("Z",1000)),
#'                 RandomVar1=rep(c("ID_1","ID_2","ID_3","ID_4"),750),
#'                 Metric=rep("Infection_Prop",3000),
#'                 Family=rep("beta",3000))
#'
#' df3<-data.frame(Response=rgamma(3000, 30),
#'                 Var1=rep(c("A","B","C"),1000),
#'                 Var2=c(rep("X",1000),rep("Y",1000),rep("Z",1000)),
#'                 RandomVar1=rep(c("ID_1","ID_2","ID_3","ID_4"),750),
#'                 Metric=rep("Tree_Height",3000),
#'                 Family=rep("gamma",3000))
#'
#' Input_df<-rbind(df1,df2,df3) %>% dplyr::mutate(Var1=as.factor(Var1),Var2=as.factor(Var2),RandomVar1=as.factor(RandomVar1))
#'
#' Modelled<-model_Bede(Input_df=Input_df,Response="Response",
#'                      Var1="Var1",Var2="Var2",RandomVar1="RandomVar1",
#'                      Metric="Metric",Mixed=FALSE,Family="Family")
#'
#'
#'ModelPlots <- pmap(list(df_mean=Modelled$Raw,
#'                             df_pred=Modelled$Prediction,
#'                             Metric=Modelled$Metric,
#'                             Formula=Modelled$Formula,
#'                             xvar="Var1",
#'                             colourvar="Var2",
#'                             pallette="main",
#'                             xlabel="Variable 1",
#'                             Categorical.x=TRUE),
#'                        plot_model_Bede)
#'
#'wrap_plots(ModelPlots,ncol=2)+plot_layout(guides="collect")+plot_annotation(tag_levels = "a")
#'



plot_model_Bede<-function(df_mean,df_pred,Metric,
                          Formula,xvar,colourvar,
                          pallette,xlabel,Categorical.x){

df_mean<-as.data.frame(df_mean)
df_pred<-as.data.frame(df_pred)

  if(!isTRUE(Categorical.x))

    return({

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
        ggplot2::labs(y=paste0(Metric),x=xlabel)+
        scale_colour_Bede(pallette)+
        scale_fill_Bede(pallette)+
        theme_Bede()+
        guides(linetype=ggplot2::guide_legend(override.aes=list(fill=NA)))+
        ggtitle(paste0(Metric,": ",deparse(Formula)))+
        scale_y_continuous(name=paste0(Metric),
                           limits = c(min(df_pred$lwr_resp,(min(df_mean$Mean)-max(df_mean$CI))),
                                      max(df_pred$upr_resp,(max(df_mean$Mean)+max(df_mean$CI)))),
                           oob = scales::squish)

      })

  else
    return({

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
        ggplot2::labs(y=paste0(Metric),x=xlabel)+
        scale_colour_Bede(pallette)+
        scale_fill_Bede(pallette)+
        theme_Bede()+
        guides(linetype=ggplot2::guide_legend(override.aes=list(fill=NA)))+
        ggtitle(paste0(Metric,": ",deparse(Formula)))+
        scale_y_continuous(name=paste0(Metric),
                           limits = c(min(df_pred$lwr_resp),
                                      max(df_pred$upr_resp)),
                           oob = scales::squish)})


}
