#' Model Function
#'
#' This Function applies a glm/glmm models applying model selection using AICc to input into plot_model_Bede()
#' It can handle beta, gamma and poisson models with ease, gaussian is currently in development
#' @param Input_df a wide dataframe of Species by Site.
#' @keywords Model
#' @export
#' @examples
#' SpeciesList<-paste0("Species",seq_along(1:15)) # 15 fake species
#'
#' SiteList<-paste0("Site",seq_along(1:50)) # 15 fake sites
#' SitexSpeciesdf<- expand_grid(Site=SiteList,Species=SpeciesList) %>% # create df of all unique combinations of these lists
#' dplyr::mutate(Abundance=rpois(n(),1.2)) %>% ## add fake abundance (can be count like here or continuous)
#' tidyr::pivot_wider(names_from = "Species",values_from = "Abundance") %>%
#' dplyr::mutate(Meta1=rep(c("Yay","Nay"),0.5*nrow(.)),
#' Meta2=rep(c("Nay","Yay"),0.5*nrow(.)),
#' .before=1)
#'
#' Metrics<-Diversity_Bede(SitexSpeciesdf,Meta=1:3,
#'                         Total.Abundance=TRUE,
#'                         Number.Taxa=TRUE,
#'                         Shannons=TRUE,
#'                         Pielou=TRUE,
#'                         Simpsons=TRUE,
#'                         Rare=TRUE,
#'                         NRare=50,
#'                         output="Long")
#' @import mgcv
#' @import tidyr
#' @import dplyr
#' @import MuMIn
#' @import vegan
#' @import lme4
#' @import broom
#' @import broom.mixed
#' @importFrom magrittr "%>%"
#' @importFrom merTools "predictInterval"


Diversity_Bede <- function(Input_df,Meta=1:3,
                           Total.Abundance=TRUE,
                           Number.Taxa=TRUE,
                           Shannons=TRUE,
                           Pielou=TRUE,
                           Simpsons=TRUE,
                           Rarefaction=TRUE,
                           NRare=50,
                           output="Long"){

  Meta_df<-Input_df[,Meta]

  Species<-Input_df[,-Meta]

  if(Total.Abundance){
  TotAb<-data.frame(TotAb=rowSums(Species))
  }
  if(!Total.Abundance){
    TotAb<-data.frame(TotAb=rep(-99999,nrow(Species)))
  }

  if(Number.Taxa){
    NTaxa<-data.frame(NTaxa=vegan::specnumber(Species))
  }
  if(!Number.Taxa){
    NTaxa<-data.frame(NTaxa=rep(-99999,nrow(Species)))
  }

  if(Shannons){
    Sh<-data.frame(Sh=vegan::diversity(Species,index="shannon"))
  }
  if(!Shannons){
    Sh<-data.frame(Sh=rep(-99999,nrow(Species)))
  }

  if(Pielou){
    PIE<-data.frame(PIE=vegan::diversity(Species,index="shannon")/log(vegan::specnumber(Species)))
  }
  if(!Pielou){
    PIE<-data.frame(PIE=rep(-99999,nrow(Species)))
  }

  if(Simpsons){
    Simp<-data.frame(Simp=vegan::diversity(Species,index="simpson"))
  }

  if(!Simpsons){
    Simp<-data.frame(Simp=rep(-99999,nrow(Species)))
  }

  if(Rarefaction){
    suppressWarnings(Rare<-data.frame(Rare=vegan::rarefy(Species,NRare)))
  }

  if(!Rarefaction){suppressWarnings(Rare<-data.frame(Rare=rep(-99999,nrow(Species))))}




  if(output=="Long"){
 Metrics_df<-dplyr::bind_cols(Meta_df,
                          TotAb,
                          NTaxa,
                          Sh,
                          PIE,
                          Simp,
                          Rare) %>%
tidyr::pivot_longer(-colnames(Meta_df),names_to = "Metric",values_to = "Value") %>%
  dplyr::filter(!Value==-99999) %>%
   dplyr::mutate(Metric=dplyr::case_when(Metric=="Rare"~paste0("Rare_",NRare),
                                  TRUE~Metric))
  }

  if(output=="Wide"){
    Metrics_df<-dplyr::bind_cols(Meta_df,
                                     TotAb,
                                     NTaxa,
                                     Sh,
                                     PIE,
                                     Simp,
                                 Rare
    ) %>%
      tidyr::pivot_longer(-colnames(Meta_df),names_to = "Metric",values_to = "Value") %>%
      dplyr::filter(!Value==-99999)  %>%
      dplyr::mutate(Metric=dplyr::case_when(Metric=="Rare"~paste0("Rare_",NRare),
                                     TRUE~Metric))%>%
      tidyr::pivot_wider(names_from = "Metric",values_from = "Value")
  }


return({Metrics_df})

}

