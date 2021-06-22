#' A Model Diagnostic Function Plot
#'
#' This function Plots Model Diagnostics
#' @param Model used to diagnose model assumptions
#' @keywords Model Diagnostics
#' @export
#' @examples
#' Model_Check()

Model_Check <- function(Model){

  tmp<-data.frame(Fitted=fitted(Model),
                  Residuals=resid(Model))

  G1<-ggplot2::ggplot(tmp)+
    ggplot2::geom_point(ggplot2::aes(x=Fitted,y=Residuals))+
    theme_Bede()

  G2<-ggplot2::ggplot(tmp) +
    ggplot2::stat_qq(ggplot2::aes(sample=Residuals))+
    ggplot2::stat_qq_line(ggplot2::aes(sample=Residuals))+
    ggplot2::labs(y="Sample Quartiles",x="Theoretical Quartiles")+
    theme_Bede()

  G3<-ggplot2::ggplot(tmp,ggplot2::aes(x=Residuals))+
    ggplot2::geom_histogram()+
    ggplot2::labs(y="Count",x="Residuals")+
    theme_Bede()

  bacf <- acf(tmp$Residuals, plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))

  res_<-acf(tmp$Residuals, plot= F)

  lim1<- qnorm((1 + (1 - 0.05))/2)/sqrt(res_$n.used)
  lim0<- -lim1

   G4<- ggplot2::ggplot(data = bacfdf,
                        mapping = ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
    ggplot2::geom_segment(mapping = ggplot2::aes(xend = lag, yend = 0))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = lim1), linetype = 2, color = 'blue') +
    ggplot2::geom_hline(ggplot2::aes(yintercept = lim0), linetype = 2, color = 'blue')+
     ggplot2::labs(y="Auto Correlation Factor",x="Lag")+
     theme_Bede()

   cowplot::plot_grid(G1,G2,G3,G4)


}

