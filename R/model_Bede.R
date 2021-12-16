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



model_Bede<- function(Input_df,Response,Time_Element,Treatment_Element,
                      Random_Element,Metric_column,Mixed){
  if(isTRUE(Mixed))
    return({

  BestModels<-list()

  Metrics<-unique(Input_df$Metric_column)

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
    distinct()


  for (i in seq_along(Metrics)){

    tmp_df<-Input_df %>%
      dplyr::filter(Metric_column==Metrics[[i]]& !Response%in%NA)

    if(!as.character(unique(tmp_df$Family))=="gaussian"){
      CombinedMod1_tmp<-glmmADMB::glmmadmb(Response~Time_Element*Treatment_Element+(1|Random_Element),data=tmp_df,
                                 family=as.character(unique(tmp_df$Family)))
      CombinedMod2_tmp<-glmmADMB::glmmadmb(Response~Time_Element+Treatment_Element+(1|Random_Element),data=tmp_df,
                                 family=as.character(unique(tmp_df$Family)))
      CombinedMod3_tmp<-glmmADMB::glmmadmb(Response~Treatment_Element+(1|Random_Element),data=tmp_df,
                                 family=as.character(unique(tmp_df$Family)))
      CombinedMod4_tmp<-glmmADMB::glmmadmb(Response~Time_Element+(1|Random_Element),data=tmp_df,
                                 family=as.character(unique(tmp_df$Family)))
      CombinedMod5_tmp<-glmmADMB::glmmadmb(Response~1+(1|Random_Element),data=tmp_df,
                                 family=as.character(unique(tmp_df$Family)))
}

    if(as.character(unique(tmp_df$Family))=="gaussian"){

      CombinedMod1_tmp<-lmer(Response~Time_Element*Treatment_Element+(1|Random_Element),data=tmp_df)
      CombinedMod2_tmp<-lmer(Response~Time_Element+Treatment_Element+(1|Random_Element),data=tmp_df)
      CombinedMod3_tmp<-lmer(Response~Treatment_Element+(1|Random_Element),data=tmp_df)
      CombinedMod4_tmp<-lmer(Response~Time_Element+(1|Random_Element),data=tmp_df)
      CombinedMod5_tmp<-lmer(Response~1+(1|Random_Element),data=tmp_df)

    }

    AIC_Values<-AICc(CombinedMod1_tmp,
                     CombinedMod2_tmp,
                     CombinedMod3_tmp,
                     CombinedMod4_tmp,
                     CombinedMod5_tmp) %>%
      dplyr::mutate(deltaAIC=AICc-min(AICc),
                    GoodBad=case_when(deltaAIC<=2~"Good",
                                      deltaAIC>2~"Bad"))

    Good_dfs<-AIC_Values %>%
      dplyr::filter(GoodBad=="Good")

    Good_dfs_min<-min(Good_dfs$df)

    BestAICc<-AIC_Values %>%
      dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])




    Output<-tibble::tibble(AIC=AICc(CombinedMod1_tmp,
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
                                  family(CombinedMod1_tmp)$linkinv,
                                  family(CombinedMod1_tmp)$linkinv,
                                  family(CombinedMod1_tmp)$linkinv,
                                  family(CombinedMod1_tmp)$linkinv),
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
                           Prediction=list(bind_cols(NewData,
                                                     setNames(as_tibble(predict(CombinedMod1_tmp,
                                                                                NewData, se.fit = TRUE)[1:2]),
                                                              c('fit_link','se_link'))),
                                           bind_cols(NewData,
                                                     setNames(as_tibble(predict(CombinedMod2_tmp,
                                                                                NewData, se.fit = TRUE)[1:2]),
                                                              c('fit_link','se_link'))),
                                           bind_cols(NewData,
                                                     setNames(as_tibble(predict(CombinedMod3_tmp,
                                                                                NewData, se.fit = TRUE)[1:2]),
                                                              c('fit_link','se_link'))),
                                           bind_cols(NewData,
                                                     setNames(as_tibble(predict(CombinedMod4_tmp,
                                                                                NewData, se.fit = TRUE)[1:2]),
                                                              c('fit_link','se_link'))),
                                           bind_cols(NewData,
                                                     setNames(as_tibble(predict(CombinedMod5_tmp,
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

    BestModelsBaited[[i]]<-BestOutput

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

  }



  BestModels_df<-bind_rows(BestModelsBaited)
})



  if(!isTRUE(Mixed))

    return({

      BestModels<-list()

      Metrics<-unique(Input_df$Metric_column)

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


      NewData<-data.frame(Time_Element=Input_df$Time_Element,
                          Treatment_Element=Input_df$Treatment_Element) %>%
        distinct()


      for (i in seq_along(Metrics)){

        tmp_df<-Input_df %>%
          dplyr::filter(Metric_column==Metrics[[i]]& !Response%in%NA)

        if(!as.character(unique(tmp_df$Family))%in%c("beta","gamma")){
          Mod1_tmp<-glm(Response~Time_Element*Treatment_Element,data=tmp_df,family=as.character(unique(tmp_df$Family)))
          Mod2_tmp<-glm(Response~Time_Element+Treatment_Element,data=tmp_df,family=as.character(unique(tmp_df$Family)))
          Mod3_tmp<-glm(Response~Treatment_Element,data=tmp_df,family=as.character(unique(tmp_df$Family)))
          Mod4_tmp<-glm(Response~Time_Element,data=tmp_df,family=as.character(unique(tmp_df$Family)))
          Mod5_tmp<-glm(Response~1,data=tmp_df,family=as.character(unique(tmp_df$Family)))
        }

        if(as.character(unique(tmp_df$Family))=="beta"){
          Mod1_tmp<-gam(Response~Time_Element*Treatment_Element,data=tmp_df, family=betar(link="logit"))
          Mod2_tmp<-gam(Response~Time_Element+Treatment_Element,data=tmp_df, family=betar(link="logit"))
          Mod3_tmp<-gam(Response~Treatment_Element,data=tmp_df, family=betar(link="logit"))
          Mod4_tmp<-gam(Response~Time_Element,data=tmp_df, family=betar(link="logit"))
          Mod5_tmp<-gam(Response~1,data=tmp_df, family=betar(link="logit"))
        }

        if(as.character(unique(tmp_df$Family))=="gamma"){
          Mod1_tmp<-glm(Response~Time_Element*Treatment_Element,data=tmp_df, family=Gamma(link="identity"))
          Mod2_tmp<-glm(Response~Time_Element+Treatment_Element,data=tmp_df, family=Gamma(link="identity"))
          Mod3_tmp<-glm(Response~Treatment_Element,data=tmp_df, family=Gamma(link="identity"))
          Mod4_tmp<-glm(Response~Time_Element,data=tmp_df, family=Gamma(link="identity"))
          Mod5_tmp<-glm(Response~1,data=tmp_df, family=Gamma(link="identity"))
        }

        if(as.character(unique(tmp_df$Family))=="gaussian"){
          Mod1_tmp<-lm(Response~Time_Element*Treatment_Element,data=tmp_df)
          Mod2_tmp<-lm(Response~Time_Element+Treatment_Element,data=tmp_df)
          Mod3_tmp<-lm(Response~Treatment_Element,data=tmp_df)
          Mod4_tmp<-lm(Response~Time_Element,data=tmp_df)
          Mod5_tmp<-lm(Response~1,data=tmp_df)
        }

        AIC_Values<-AICc(Mod1_tmp,
                         Mod2_tmp,
                         Mod3_tmp,
                         Mod4_tmp,
                         Mod5_tmp) %>%
          dplyr::mutate(deltaAIC=AICc-min(AICc),
                        GoodBad=case_when(deltaAIC<=2~"Good",
                                          deltaAIC>2~"Bad"))

        Good_dfs<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good")

        Good_dfs_min<-min(Good_dfs$df)

        BestAICc<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])



        if(!as.character(unique(tmp_df$Family))=="beta"){
          Output<-tibble::tibble(AIC=AICc(Mod1_tmp,
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
                                 Prediction=list(bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod1_tmp,
                                                                                      NewData,se.fit=TRUE)[1:2]),
                                                                    c('fit_link','se_link'))),
                                                 bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod2_tmp,
                                                                                      NewData, se.fit = TRUE)[1:2]),
                                                                    c('fit_link','se_link'))),
                                                 bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod3_tmp,
                                                                                      NewData, se.fit = TRUE)[1:2]),
                                                                    c('fit_link','se_link'))),
                                                 bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod4_tmp,
                                                                                      NewData, se.fit = TRUE)[1:2]),
                                                                    c('fit_link','se_link'))),
                                                 bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod5_tmp,
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
        }

        if(as.character(unique(tmp_df$Family))=="beta"){
          Output<-tibble::tibble(AIC=AICc(Mod1_tmp,
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
                                 Prediction=list(bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod1_tmp,
                                                                                      NewData,se.fit=TRUE)[1:2]),
                                                                    c('fit_link','se_link'))),
                                                 bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod2_tmp,
                                                                                      NewData, se.fit = TRUE)[1:2]),
                                                                    c('fit_link','se_link'))),
                                                 bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod3_tmp,
                                                                                      NewData, se.fit = TRUE)[1:2]),
                                                                    c('fit_link','se_link'))),
                                                 bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod4_tmp,
                                                                                      NewData, se.fit = TRUE)[1:2]),
                                                                    c('fit_link','se_link'))),
                                                 bind_cols(NewData,
                                                           setNames(as_tibble(predict(Mod5_tmp,
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
        }

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

        BestModels[[i]]<-BestOutput

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



      BestModels_df<-bind_rows(BestModels)




    })




}
