#' Model Function
#'
#' This Function applies a glm/glmm models applying model selection using AICc to input into plot_model_Bede()
#' It can handle beta, gamma and poisson models with ease, gaussian is currently in development
#' @param Model glm or glmm
#' @keywords Model
#' @export
#' @examples
#' model_Bede()
#' @import mgcv
#' @import tidyr
#' @import dplyr
#' @import MuMIn
#' @import broom
#' @import broom.mixed
#' @importFrom magrittr "%>%"



model_Bede <- function(Input_df,Response,Time_Element,Treatment_Element,
                       Random_Element,Metric_Column,Mixed,Family_Column){

  print("This might take a while, sorrrreyyyyy")

  if(isTRUE(Mixed)){


      BestModels<-list()

      Metrics<-unique(Input_df$Metric_Column)

      NewData<-NULL
      Good_dfs_min<-NULL
      tmp_df<-NULL
      AIC_Values<-NULL
      CombinedMod1_tmp<-NULL
      CombinedMod2_tmp<-NULL
      CombinedMod3_tmp<-NULL
      CombinedMod4_tmp<-NULL
      CombinedMod5_tmp<-NULL
      Good_dfs<-NULL
      Output<-NULL
      BestOutput<-NULL


      NewData<-data.frame(Time_Element=Input_df$Time_Element,
                          Treatment_Element=Input_df$Treatment_Element,
                          Random_Element=Input_df$Random_Element) %>%
        dplyr::distinct()


      if(length(Metrics)>1) { for (i in seq_along(Metrics)){
        print(paste("Assessing Metric:: ", Metrics[[i]]))

        tmp_df<-Input_df %>%
          dplyr::filter(Metric_Column==Metrics[[i]]& !Response%in%NA & !Time_Element%in%NA & !Treatment_Element%in%NA & !Random_Element%in%NA)

        if(!as.character(unique(tmp_df$Family_Column))=="gaussian"){
          CombinedMod1_tmp<-glmmADMB::glmmadmb(Response~Time_Element*Treatment_Element+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
          CombinedMod2_tmp<-glmmADMB::glmmadmb(Response~Time_Element+Treatment_Element+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
          CombinedMod3_tmp<-glmmADMB::glmmadmb(Response~Treatment_Element+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
          CombinedMod4_tmp<-glmmADMB::glmmadmb(Response~Time_Element+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
          CombinedMod5_tmp<-glmmADMB::glmmadmb(Response~1+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
        }

        if(as.character(unique(tmp_df$Family_Column))=="gaussian"){

          CombinedMod1_tmp<-lmer(Response~Time_Element*Treatment_Element+(1|Random_Element),data=tmp_df)
          CombinedMod2_tmp<-lmer(Response~Time_Element+Treatment_Element+(1|Random_Element),data=tmp_df)
          CombinedMod3_tmp<-lmer(Response~Treatment_Element+(1|Random_Element),data=tmp_df)
          CombinedMod4_tmp<-lmer(Response~Time_Element+(1|Random_Element),data=tmp_df)
          CombinedMod5_tmp<-lmer(Response~1+(1|Random_Element),data=tmp_df)

        }

        AIC_Values<-MuMIn::AICc(CombinedMod1_tmp,
                                CombinedMod2_tmp,
                                CombinedMod3_tmp,
                                CombinedMod4_tmp,
                                CombinedMod5_tmp) %>%
          dplyr::mutate(deltaAIC=AICc-min(AICc),
                        GoodBad=dplyr::case_when(deltaAIC<=2~"Good",
                                                 deltaAIC>2~"Bad"))

        Good_dfs<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good")

        Good_dfs_min<-min(Good_dfs$df)

        BestAICc<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])




        Output<-tibble::tibble(AIC=MuMIn::AICc(CombinedMod1_tmp,
                                               CombinedMod2_tmp,
                                               CombinedMod3_tmp,
                                               CombinedMod4_tmp,
                                               CombinedMod5_tmp ),
                               Formula=c(CombinedMod1_tmp$formula,
                                         CombinedMod2_tmp$formula,
                                         CombinedMod3_tmp$formula,
                                         CombinedMod4_tmp$formula,
                                         CombinedMod5_tmp$formula),
                               Link=c(family(CombinedMod1_tmp)$linkinv,
                                      family(CombinedMod2_tmp)$linkinv,
                                      family(CombinedMod3_tmp)$linkinv,
                                      family(CombinedMod4_tmp)$linkinv,
                                      family(CombinedMod5_tmp)$linkinv),
                               Model=list(broom.mixed::tidy(CombinedMod1_tmp,effects="fixed"),
                                          broom.mixed::tidy(CombinedMod2_tmp,effects="fixed"),
                                          broom.mixed::tidy(CombinedMod3_tmp,effects="fixed"),
                                          broom.mixed::tidy(CombinedMod4_tmp,effects="fixed"),
                                          broom.mixed::tidy(CombinedMod5_tmp,effects="fixed")),
                               Model_obj=list(CombinedMod1_tmp,
                                              CombinedMod2_tmp,
                                              CombinedMod3_tmp,
                                              CombinedMod4_tmp,
                                              CombinedMod5_tmp),
                               Prediction=list(dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod1_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod2_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod3_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod4_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod5_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))),
                               Raw=list(tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) )) %>%
          dplyr::mutate(Metric=Metrics[[i]])


        BestOutput<-Output %>%
          dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)


        ilink <- BestOutput$Link[[1]]


        BestOutput$Prediction[[1]][,6]<-ilink(BestOutput$Prediction[[1]][,4])

        BestOutput$Prediction[[1]][,7]<-ilink(BestOutput$Prediction[[1]][,5])

        BestOutput$Prediction[[1]][,8]<-ilink(BestOutput$Prediction[[1]][,4]+(2*BestOutput$Prediction[[1]][,5]))

        BestOutput$Prediction[[1]][,9]<-ilink(BestOutput$Prediction[[1]][,4]-(2*BestOutput$Prediction[[1]][,5]))

        names(BestOutput$Prediction[[1]])<-c("Time_Element","Treatment_Element","Random_Element",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")

        BestModels[[i]]<-BestOutput

        tmp_df<-NULL
        AIC_Values<-NULL
        CombinedMod1_tmp<-NULL
        CombinedMod2_tmp<-NULL
        CombinedMod3_tmp<-NULL
        CombinedMod4_tmp<-NULL
        CombinedMod5_tmp<-NULL
        Good_dfs<-NULL
        Good_dfs_min<-NULL
        Output<-NULL
        BestOutput<-NULL
        ilink<-NULL

      }}



      if(length(Metrics)==1) {{
        print(paste("Assessing Metric:: ", Metrics[[1]]))

        tmp_df<-Input_df %>%
          dplyr::filter(!Response%in%NA & !Time_Element%in%NA & !Treatment_Element%in%NA & !Random_Element%in%NA)

        if(!as.character(unique(tmp_df$Family_Column))=="gaussian"){
          CombinedMod1_tmp<-glmmADMB::glmmadmb(Response~Time_Element*Treatment_Element+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
          CombinedMod2_tmp<-glmmADMB::glmmadmb(Response~Time_Element+Treatment_Element+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
          CombinedMod3_tmp<-glmmADMB::glmmadmb(Response~Treatment_Element+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
          CombinedMod4_tmp<-glmmADMB::glmmadmb(Response~Time_Element+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
          CombinedMod5_tmp<-glmmADMB::glmmadmb(Response~1+(1|Random_Element),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family_Column)))
        }

        if(as.character(unique(tmp_df$Family_Column))=="gaussian"){

          CombinedMod1_tmp<-lmer(Response~Time_Element*Treatment_Element+(1|Random_Element),data=tmp_df)
          CombinedMod2_tmp<-lmer(Response~Time_Element+Treatment_Element+(1|Random_Element),data=tmp_df)
          CombinedMod3_tmp<-lmer(Response~Treatment_Element+(1|Random_Element),data=tmp_df)
          CombinedMod4_tmp<-lmer(Response~Time_Element+(1|Random_Element),data=tmp_df)
          CombinedMod5_tmp<-lmer(Response~1+(1|Random_Element),data=tmp_df)

        }

        AIC_Values<-MuMIn::AICc(CombinedMod1_tmp,
                                CombinedMod2_tmp,
                                CombinedMod3_tmp,
                                CombinedMod4_tmp,
                                CombinedMod5_tmp) %>%
          dplyr::mutate(deltaAIC=AICc-min(AICc),
                        GoodBad=dplyr::case_when(deltaAIC<=2~"Good",
                                                 deltaAIC>2~"Bad"))

        Good_dfs<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good")

        Good_dfs_min<-min(Good_dfs$df)

        BestAICc<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])




        Output<-tibble::tibble(AIC=MuMIn::AICc(CombinedMod1_tmp,
                                               CombinedMod2_tmp,
                                               CombinedMod3_tmp,
                                               CombinedMod4_tmp,
                                               CombinedMod5_tmp ),
                               Formula=c(CombinedMod1_tmp$formula,
                                         CombinedMod2_tmp$formula,
                                         CombinedMod3_tmp$formula,
                                         CombinedMod4_tmp$formula,
                                         CombinedMod5_tmp$formula),
                               Link=c(family(CombinedMod1_tmp)$linkinv,
                                      family(CombinedMod2_tmp)$linkinv,
                                      family(CombinedMod3_tmp)$linkinv,
                                      family(CombinedMod4_tmp)$linkinv,
                                      family(CombinedMod5_tmp)$linkinv),
                               Model=list(broom.mixed::tidy(CombinedMod1_tmp,effects="fixed"),
                                          broom.mixed::tidy(CombinedMod2_tmp,effects="fixed"),
                                          broom.mixed::tidy(CombinedMod3_tmp,effects="fixed"),
                                          broom.mixed::tidy(CombinedMod4_tmp,effects="fixed"),
                                          broom.mixed::tidy(CombinedMod5_tmp,effects="fixed")),
                               Model_obj=list(CombinedMod1_tmp,
                                              CombinedMod2_tmp,
                                              CombinedMod3_tmp,
                                              CombinedMod4_tmp,
                                              CombinedMod5_tmp),
                               Prediction=list(dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod1_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod2_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod3_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod4_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(CombinedMod5_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))),
                               Raw=list(tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) )) %>%
          dplyr::mutate(Metric=Metrics[[1]])


        BestOutput<-Output %>%
          dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)


        ilink <- BestOutput$Link[[1]]


        BestOutput$Prediction[[1]][,6]<-ilink(BestOutput$Prediction[[1]][,4])

        BestOutput$Prediction[[1]][,7]<-ilink(BestOutput$Prediction[[1]][,5])

        BestOutput$Prediction[[1]][,8]<-ilink(BestOutput$Prediction[[1]][,4]+(2*BestOutput$Prediction[[1]][,5]))

        BestOutput$Prediction[[1]][,9]<-ilink(BestOutput$Prediction[[1]][,4]-(2*BestOutput$Prediction[[1]][,5]))

        names(BestOutput$Prediction[[1]])<-c("Time_Element","Treatment_Element","Random_Element",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")

        BestModels1<-BestOutput

        tmp_df<-NULL
        AIC_Values<-NULL
        CombinedMod1_tmp<-NULL
        CombinedMod2_tmp<-NULL
        CombinedMod3_tmp<-NULL
        CombinedMod4_tmp<-NULL
        CombinedMod5_tmp<-NULL
        Good_dfs<-NULL
        Good_dfs_min<-NULL
        Output<-NULL
        BestOutput<-NULL
        ilink<-NULL

      }}

      BestModels_df<-dplyr::bind_rows(BestModels)
  }


  if(!isTRUE(Mixed)){

    BestModels<-list()
    Metrics<-NULL
    tmp_df<-NULL
    AIC_Values<-NULL
    Mod1_tmp<-NULL
    Mod2_tmp<-NULL
    Mod3_tmp<-NULL
    Mod4_tmp<-NULL
    Mod5_tmp<-NULL
    Good_dfs<-NULL
    Good_dfs_min<-NULL
    Output<-NULL
    BestOutput<-NULL
    NewData<-NULL

    NewData<-data.frame(Time_Element=Input_df$Time_Element,
                        Treatment_Element=Input_df$Treatment_Element) %>%
      dplyr::distinct()

        Metrics<-unique(as.character(Input_df$Metric_Column))

    if(length(Metrics)>1) {for (j in seq_along(Metrics)){

      print(paste0("Assessing Metric:: ", Metrics[[j]]))

      tmp_df<-Input_df %>%
        dplyr::filter(Metric_Column==Metrics[[j]]& !Response%in%NA & !Time_Element%in%NA & !Treatment_Element%in%NA & !Random_Element%in%NA)



      if(as.character(unique(tmp_df$Family_Column))%in%c("poisson")){
        Mod1_tmp<-glm(Response~Time_Element*Treatment_Element,data=tmp_df,family="poisson")
        Mod2_tmp<-glm(Response~Time_Element+Treatment_Element,data=tmp_df,family="poisson")
        Mod3_tmp<-glm(Response~Treatment_Element,data=tmp_df,family="poisson")
        Mod4_tmp<-glm(Response~Time_Element,data=tmp_df,family="poisson")
        Mod5_tmp<-glm(Response~1,data=tmp_df,family="poisson")
      }

      if(as.character(unique(tmp_df$Family_Column))=="beta"){
        Mod1_tmp<-mgcv::gam(Response~Time_Element*Treatment_Element,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod2_tmp<-mgcv::gam(Response~Time_Element+Treatment_Element,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod3_tmp<-mgcv::gam(Response~Treatment_Element,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod4_tmp<-mgcv::gam(Response~Time_Element,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod5_tmp<-mgcv::gam(Response~1,data=tmp_df, family=mgcv::betar(link="logit"))
      }

      if(as.character(unique(tmp_df$Family_Column))=="gamma"){
        Mod1_tmp<-glm(Response~Time_Element*Treatment_Element,data=tmp_df, family=Gamma())
        Mod2_tmp<-glm(Response~Time_Element+Treatment_Element,data=tmp_df, family=Gamma())
        Mod3_tmp<-glm(Response~Treatment_Element,data=tmp_df, family=Gamma())
        Mod4_tmp<-glm(Response~Time_Element,data=tmp_df, family=Gamma())
        Mod5_tmp<-glm(Response~1,data=tmp_df, family=Gamma())
      }

      if(as.character(unique(tmp_df$Family_Column))=="gaussian"){
        Mod1_tmp<-lm(Response~Time_Element*Treatment_Element,data=tmp_df)
        Mod2_tmp<-lm(Response~Time_Element+Treatment_Element,data=tmp_df)
        Mod3_tmp<-lm(Response~Treatment_Element,data=tmp_df)
        Mod4_tmp<-lm(Response~Time_Element,data=tmp_df)
        Mod5_tmp<-lm(Response~1,data=tmp_df)
      }

      AIC_Values<-MuMIn::AICc(Mod1_tmp,
                              Mod2_tmp,
                              Mod3_tmp,
                              Mod4_tmp,
                              Mod5_tmp) %>%
        dplyr::mutate(deltaAIC=AICc-min(AICc),
                      GoodBad=dplyr::case_when(deltaAIC<=2~"Good",
                                               deltaAIC>2~"Bad"))

      Good_dfs<-AIC_Values %>%
        dplyr::filter(GoodBad=="Good")

      Good_dfs_min<-min(Good_dfs$df)

      BestAICc<-AIC_Values %>%
        dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])



      if(!as.character(unique(tmp_df$Family_Column))%in%c("beta","gaussian")){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=c(Mod1_tmp$formula,
                                         Mod2_tmp$formula,
                                         Mod3_tmp$formula,
                                         Mod4_tmp$formula,
                                         Mod5_tmp$formula),
                               Link=c(family(Mod1_tmp)$linkinv,
                                      family(Mod2_tmp)$linkinv,
                                      family(Mod3_tmp)$linkinv,
                                      family(Mod4_tmp)$linkinv,
                                      family(Mod5_tmp)$linkinv),
                               Model=list(broom::tidy(Mod1_tmp,effects="fixed"),
                                          broom::tidy(Mod2_tmp,effects="fixed"),
                                          broom::tidy(Mod3_tmp,effects="fixed"),
                                          broom::tidy(Mod4_tmp,effects="fixed"),
                                          broom::tidy(Mod5_tmp,effects="fixed")),
                               Model_obj=list(Mod1_tmp,
                                              Mod2_tmp,
                                              Mod3_tmp,
                                              Mod4_tmp,
                                              Mod5_tmp),
                               Prediction=list(dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod1_tmp,
                                                                                                   NewData,se.fit=TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod2_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod3_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod4_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod5_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))),
                               Raw=list(tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) )) %>%
          dplyr::mutate(Metric=Metrics[[j]])

        BestOutput<-Output %>%
          dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)


        ilink <- BestOutput$Link[[1]]


        BestOutput$Prediction[[1]][,5]<-ilink(BestOutput$Prediction[[1]][,3])

        BestOutput$Prediction[[1]][,6]<-ilink(BestOutput$Prediction[[1]][,4])

        BestOutput$Prediction[[1]][,7]<-ilink(BestOutput$Prediction[[1]][,3]+(2*BestOutput$Prediction[[1]][,4]))

        BestOutput$Prediction[[1]][,8]<-ilink(BestOutput$Prediction[[1]][,3]-(2*BestOutput$Prediction[[1]][,4]))

        names(BestOutput$Prediction[[1]])<-c("Time_Element","Treatment_Element",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")
      }

      if(as.character(unique(tmp_df$Family_Column))=="beta"){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=c(Mod1_tmp$formula,
                                         Mod2_tmp$formula,
                                         Mod3_tmp$formula,
                                         Mod4_tmp$formula,
                                         Mod5_tmp$formula),
                               Link=c(family(Mod1_tmp)$linkinv,
                                      family(Mod1_tmp)$linkinv,
                                      family(Mod1_tmp)$linkinv,
                                      family(Mod1_tmp)$linkinv,
                                      family(Mod1_tmp)$linkinv),
                               Model=list(broom::tidy(Mod1_tmp,parametric=TRUE),
                                          broom::tidy(Mod2_tmp,parametric=TRUE),
                                          broom::tidy(Mod3_tmp,parametric=TRUE),
                                          broom::tidy(Mod4_tmp,parametric=TRUE),
                                          broom::tidy(Mod5_tmp,parametric=TRUE)),
                               Model_obj=list(Mod1_tmp,
                                              Mod2_tmp,
                                              Mod3_tmp,
                                              Mod4_tmp,
                                              Mod5_tmp),
                               Prediction=list(dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod1_tmp,
                                                                                                   NewData,se.fit=TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod2_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod3_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod4_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod5_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))),
                               Raw=list(tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) )) %>%
          dplyr::mutate(Metric=Metrics[[j]])
        BestOutput<-Output %>%
          dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)


        ilink <- BestOutput$Link[[1]]


        BestOutput$Prediction[[1]][,5]<-ilink(BestOutput$Prediction[[1]][,3])

        BestOutput$Prediction[[1]][,6]<-ilink(BestOutput$Prediction[[1]][,4])

        BestOutput$Prediction[[1]][,7]<-ilink(BestOutput$Prediction[[1]][,3]+(2*BestOutput$Prediction[[1]][,4]))

        BestOutput$Prediction[[1]][,8]<-ilink(BestOutput$Prediction[[1]][,3]-(2*BestOutput$Prediction[[1]][,4]))

        names(BestOutput$Prediction[[1]])<-c("Time_Element","Treatment_Element",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")

      }

      if(as.character(unique(tmp_df$Family_Column))=="gaussian"){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=c(paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod1_tmp$call$formula))),
                               Link=c("identity",
                                      "identity",
                                      "identity",
                                      "identity",
                                      "identity"),
                               Model=list(broom::tidy(Mod1_tmp,effects="fixed"),
                                          broom::tidy(Mod2_tmp,effects="fixed"),
                                          broom::tidy(Mod3_tmp,effects="fixed"),
                                          broom::tidy(Mod4_tmp,effects="fixed"),
                                          broom::tidy(Mod5_tmp,effects="fixed")),
                               Model_obj=list(Mod1_tmp,
                                              Mod2_tmp,
                                              Mod3_tmp,
                                              Mod4_tmp,
                                              Mod5_tmp),
                               Prediction=list(dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod1_tmp,
                                                                                                   NewData,se.fit=TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod2_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod3_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod4_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod5_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))),
                               Raw=list(tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response,na.rm=TRUE)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)),
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) )) %>%
          dplyr::mutate(Metric=Metrics[[j]])

        BestOutput<-Output %>%
          dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)




        BestOutput$Prediction[[1]][,5]<-BestOutput$Prediction[[1]][,3]

        BestOutput$Prediction[[1]][,6]<-BestOutput$Prediction[[1]][,4]

        BestOutput$Prediction[[1]][,7]<-BestOutput$Prediction[[1]][,3]+(2*BestOutput$Prediction[[1]][,4])

        BestOutput$Prediction[[1]][,8]<-BestOutput$Prediction[[1]][,3]-(2*BestOutput$Prediction[[1]][,4])

        names(BestOutput$Prediction[[1]])<-c("Time_Element","Treatment_Element",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")

      }



      BestModels[[j]]<-BestOutput

      tmp_df<-NULL
      AIC_Values<-NULL
      Mod1_tmp<-NULL
      Mod2_tmp<-NULL
      Mod3_tmp<-NULL
      Mod4_tmp<-NULL
      Mod5_tmp<-NULL
      Good_dfs<-NULL
      Good_dfs_min<-NULL
      Output<-NULL
      BestOutput<-NULL
      ilink<-NULL

    }}
    if(length(Metrics)==1) {

      print(paste0("Assessing Metric:: ", Metrics[[1]]))

      tmp_df<-Input_df %>%
        dplyr::filter(!Response%in%NA &
                        !Time_Element%in%NA &
                        !Treatment_Element%in%NA &
                        !Random_Element%in%NA)

      if(as.character(unique(tmp_df$Family_Column))=="poisson"){
        Mod1_tmp<-glm(Response~Time_Element*Treatment_Element,data=tmp_df,family="poisson")
        Mod2_tmp<-glm(Response~Time_Element+Treatment_Element,data=tmp_df,family="poisson")
        Mod3_tmp<-glm(Response~Treatment_Element,data=tmp_df,family="poisson")
        Mod4_tmp<-glm(Response~Time_Element,data=tmp_df,family="poisson")
        Mod5_tmp<-glm(Response~1,data=tmp_df,family="poisson")
      }
      if(as.character(unique(tmp_df$Family_Column))=="beta"){
        Mod1_tmp<-mgcv::gam(Response~Time_Element*Treatment_Element,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod2_tmp<-mgcv::gam(Response~Time_Element+Treatment_Element,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod3_tmp<-mgcv::gam(Response~Treatment_Element,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod4_tmp<-mgcv::gam(Response~Time_Element,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod5_tmp<-mgcv::gam(Response~1,data=tmp_df, family=mgcv::betar(link="logit"))
      }
      if(as.character(unique(tmp_df$Family_Column))=="gamma"){
        Mod1_tmp<-glm(Response~Time_Element*Treatment_Element,data=tmp_df, family=Gamma())
        Mod2_tmp<-glm(Response~Time_Element+Treatment_Element,data=tmp_df, family=Gamma())
        Mod3_tmp<-glm(Response~Treatment_Element,data=tmp_df, family=Gamma())
        Mod4_tmp<-glm(Response~Time_Element,data=tmp_df, family=Gamma())
        Mod5_tmp<-glm(Response~1,data=tmp_df, family=Gamma())
      }

      if(as.character(unique(tmp_df$Family_Column))=="gaussian"){
        Mod1_tmp<-lm(Response~Time_Element*Treatment_Element,data=tmp_df)
        Mod2_tmp<-lm(Response~Time_Element+Treatment_Element,data=tmp_df)
        Mod3_tmp<-lm(Response~Treatment_Element,data=tmp_df)
        Mod4_tmp<-lm(Response~Time_Element,data=tmp_df)
        Mod5_tmp<-lm(Response~1,data=tmp_df)
      }

      AIC_Values<-MuMIn::AICc(Mod1_tmp,
                              Mod2_tmp,
                              Mod3_tmp,
                              Mod4_tmp,
                              Mod5_tmp) %>%
        dplyr::mutate(deltaAIC=AICc-min(AICc),
                      GoodBad=dplyr::case_when(deltaAIC<=2~"Good",
                                               deltaAIC>2~"Bad"))

      Good_dfs<-AIC_Values %>%
        dplyr::filter(GoodBad=="Good")

      Good_dfs_min<-min(Good_dfs$df)

      BestAICc<-AIC_Values %>%
        dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])



      if(!as.character(unique(tmp_df$Family_Column))%in%c("beta","gaussian")){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=c(Mod1_tmp$formula,
                                         Mod2_tmp$formula,
                                         Mod3_tmp$formula,
                                         Mod4_tmp$formula,
                                         Mod5_tmp$formula),
                               Link=c(family(Mod1_tmp)$linkinv,
                                      family(Mod2_tmp)$linkinv,
                                      family(Mod3_tmp)$linkinv,
                                      family(Mod4_tmp)$linkinv,
                                      family(Mod5_tmp)$linkinv),
                               Model=list(broom::tidy(Mod1_tmp,effects="fixed"),
                                          broom::tidy(Mod2_tmp,effects="fixed"),
                                          broom::tidy(Mod3_tmp,effects="fixed"),
                                          broom::tidy(Mod4_tmp,effects="fixed"),
                                          broom::tidy(Mod5_tmp,effects="fixed")),
                               Model_obj=list(Mod1_tmp,
                                              Mod2_tmp,
                                              Mod3_tmp,
                                              Mod4_tmp,
                                              Mod5_tmp),
                               Prediction=list(dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod1_tmp,
                                                                                                   NewData,se.fit=TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod2_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod3_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod4_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod5_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))),
                               Raw=list(tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) )) %>%
          dplyr::mutate(Metric=Metrics[[1]])
        BestOutput<-Output %>%
          dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)


        ilink <- BestOutput$Link[[1]]


        BestOutput$Prediction[[1]][,5]<-ilink(BestOutput$Prediction[[1]][,3])

        BestOutput$Prediction[[1]][,6]<-ilink(BestOutput$Prediction[[1]][,4])

        BestOutput$Prediction[[1]][,7]<-ilink(BestOutput$Prediction[[1]][,3]+(2*BestOutput$Prediction[[1]][,4]))

        BestOutput$Prediction[[1]][,8]<-ilink(BestOutput$Prediction[[1]][,3]-(2*BestOutput$Prediction[[1]][,4]))

        names(BestOutput$Prediction[[1]])<-c("Time_Element","Treatment_Element",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")
        }

      if(as.character(unique(tmp_df$Family_Column))=="beta"){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=c(Mod1_tmp$formula,
                                         Mod2_tmp$formula,
                                         Mod3_tmp$formula,
                                         Mod4_tmp$formula,
                                         Mod5_tmp$formula),
                               Link=c(family(Mod1_tmp)$linkinv,
                                      family(Mod2_tmp)$linkinv,
                                      family(Mod3_tmp)$linkinv,
                                      family(Mod4_tmp)$linkinv,
                                      family(Mod5_tmp)$linkinv),
                               Model=list(broom::tidy(Mod1_tmp,parametric=TRUE),
                                          broom::tidy(Mod2_tmp,parametric=TRUE),
                                          broom::tidy(Mod3_tmp,parametric=TRUE),
                                          broom::tidy(Mod4_tmp,parametric=TRUE),
                                          broom::tidy(Mod5_tmp,parametric=TRUE)),
                               Model_obj=list(Mod1_tmp,
                                              Mod2_tmp,
                                              Mod3_tmp,
                                              Mod4_tmp,
                                              Mod5_tmp),
                               Prediction=list(dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod1_tmp,
                                                                                                   NewData,se.fit=TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod2_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod3_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod4_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod5_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))),
                               Raw=list(tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) )) %>%
          dplyr::mutate(Metric=Metrics[[1]])

        BestOutput<-Output %>%
          dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)


        ilink <- BestOutput$Link[[1]]


        BestOutput$Prediction[[1]][,5]<-ilink(BestOutput$Prediction[[1]][,3])

        BestOutput$Prediction[[1]][,6]<-ilink(BestOutput$Prediction[[1]][,4])

        BestOutput$Prediction[[1]][,7]<-ilink(BestOutput$Prediction[[1]][,3]+(2*BestOutput$Prediction[[1]][,4]))

        BestOutput$Prediction[[1]][,8]<-ilink(BestOutput$Prediction[[1]][,3]-(2*BestOutput$Prediction[[1]][,4]))

        names(BestOutput$Prediction[[1]])<-c("Time_Element","Treatment_Element",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")

        }

      if(as.character(unique(tmp_df$Family_Column))=="gaussian"){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=c(paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod2_tmp$call$formula)),
                                         paste0(deparse(Mod3_tmp$call$formula)),
                                         paste0(deparse(Mod4_tmp$call$formula)),
                                         paste0(deparse(Mod5_tmp$call$formula))),
                               Link=c("identity",
                                      "identity",
                                      "identity",
                                      "identity",
                                      "identity"),
                               Model=list(broom::tidy(Mod1_tmp,effects="fixed"),
                                          broom::tidy(Mod2_tmp,effects="fixed"),
                                          broom::tidy(Mod3_tmp,effects="fixed"),
                                          broom::tidy(Mod4_tmp,effects="fixed"),
                                          broom::tidy(Mod5_tmp,effects="fixed")),
                               Model_obj=list(Mod1_tmp,
                                              Mod2_tmp,
                                              Mod3_tmp,
                                              Mod4_tmp,
                                              Mod5_tmp),
                               Prediction=list(dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod1_tmp,
                                                                                                   NewData,se.fit=TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod2_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod3_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod4_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link'))),
                                               dplyr::bind_cols(NewData,
                                                                setNames(tibble::as_tibble(predict(Mod5_tmp,
                                                                                                   NewData, se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))),
                               Raw=list(tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response,na.rm=TRUE)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)),
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Time_Element,Treatment_Element) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) )) %>%
          dplyr::mutate(Metric=Metrics[[1]])

        BestOutput<-Output %>%
          dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)




        BestOutput$Prediction[[1]][,5]<-BestOutput$Prediction[[1]][,3]

        BestOutput$Prediction[[1]][,6]<-BestOutput$Prediction[[1]][,4]

        BestOutput$Prediction[[1]][,7]<-BestOutput$Prediction[[1]][,3]+(2*BestOutput$Prediction[[1]][,4])

        BestOutput$Prediction[[1]][,8]<-BestOutput$Prediction[[1]][,3]-(2*BestOutput$Prediction[[1]][,4])

        names(BestOutput$Prediction[[1]])<-c("Time_Element","Treatment_Element",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")
      }



      BestModels[[1]]<-BestOutput

      tmp_df<-NULL
      AIC_Values<-NULL
      Mod1_tmp<-NULL
      Mod2_tmp<-NULL
      Mod3_tmp<-NULL
      Mod4_tmp<-NULL
      Mod5_tmp<-NULL
      Good_dfs<-NULL
      Good_dfs_min<-NULL
      Output<-NULL
      BestOutput<-NULL
      ilink<-NULL

    }
        BestModels_df<-dplyr::bind_rows(BestModels)

}

return({BestModels_df})

}

