#' Model Function
#'
#' This Function applies a glm/glmm models applying model selection using AICc to input into plot_model_Bede()
#' It can handle beta, gamma and poisson models with ease, gaussian is currently in development
#' @param Input_df a dataframe in tidy format.
#' @param Response column from Input_df that includes the response variable
#' @param Var1 column from Input_df that includes a fixed effect.
#' @param Var2 column from Input_df that includes a fixed effect.
#' @param RandomVar1 column from Input_df that includes a random effect. (must be a factor)
#' @param Metric column from Input_df that specifies each response variable metric.
#' @param Mixed logical. if TRUE RandomVar1 is required to carry out glmm. if FALSE RandomVar1 not needed and glm carried out.
#' @param Family column from Input_df that specifies the family/distribution to used for each metric. options: "gaussian", "poisson", "gamma" or "beta"
#' @keywords Model
#' @export
#' @examples
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
#'                      Metric="Metric",Mixed=TRUE,Family="Family")

#' @import mgcv
#' @import tidyr
#' @import dplyr
#' @import MuMIn
#' @import lme4
#' @import broom
#' @import broom.mixed
#' @importFrom magrittr "%>%"
#' @importFrom merTools "predictInterval"


model_Bede <- function(Input_df,Response,Var1,Var2,RandomVar1,Metric,Mixed,Family){

  fakedata <- data.frame(
    s = rep(c(4, 5, 10, 18, 19), each = 4),
    n = rep(20, 20))

 FakeMod<-glm(cbind(s, n - s) ~ 1, family = quasibinomial, data = fakedata)

  print("This might take a while, sorrrreyyyyy")

  if(isTRUE(Mixed)){


      BestModels<-list()

      Metrics<-unique(Input_df$Metric)

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


      NewData<-data.frame(Var1=Input_df$Var1,
                          Var2=Input_df$Var2,
                          RandomVar1=Input_df$RandomVar1) %>%
        dplyr::distinct()


      if(length(Metrics)>1) { for (i in seq_along(Metrics)){
        print(paste("Assessing Metric: ", Metrics[[i]]))

        tmp_df<-Input_df %>%
          dplyr::filter(Metric==Metrics[[i]]& !Response%in%NA & !Var1%in%NA & !Var2%in%NA & !RandomVar1%in%NA)

        if(!as.character(unique(tmp_df$Family))=="gaussian"){
            CombinedMod1_tmp<-glmmADMB::glmmadmb(Response~Var1*Var2+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))


          CombinedMod2_tmp<-glmmADMB::glmmadmb(Response~Var1+Var2+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))


          CombinedMod3_tmp<-glmmADMB::glmmadmb(Response~Var2+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))


          CombinedMod4_tmp<-glmmADMB::glmmadmb(Response~Var1+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))


          CombinedMod5_tmp<-glmmADMB::glmmadmb(Response~1+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))
        }

        if(as.character(unique(tmp_df$Family))=="gaussian"){

          CombinedMod1_tmp<-lmer(Response~Var1*Var2+(1|RandomVar1),data=tmp_df)
          CombinedMod2_tmp<-lmer(Response~Var1+Var2+(1|RandomVar1),data=tmp_df)
          CombinedMod3_tmp<-lmer(Response~Var2+(1|RandomVar1),data=tmp_df)
          CombinedMod4_tmp<-lmer(Response~Var1+(1|RandomVar1),data=tmp_df)
          CombinedMod5_tmp<-lmer(Response~1+(1|RandomVar1),data=tmp_df)

        }

        AIC_Values<-MuMIn::AICc(CombinedMod1_tmp,
                                CombinedMod2_tmp,
                                CombinedMod3_tmp,
                                CombinedMod4_tmp,
                                CombinedMod5_tmp) %>%
          dplyr::mutate(deltaAIC=AICc-min(AICc,na.rm = TRUE),
                        GoodBad=dplyr::case_when(deltaAIC<=2~"Good",
                                                 deltaAIC>2~"Bad"))

        Good_dfs<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good")

        Good_dfs_min<-min(Good_dfs$df)

        BestAICc<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])

        if(!as.character(unique(tmp_df$Family))=="gaussian"){
        Output<-tibble::tibble(AIC=MuMIn::AICc(CombinedMod1_tmp,
                                               CombinedMod2_tmp,
                                               CombinedMod3_tmp,
                                               CombinedMod4_tmp,
                                               CombinedMod5_tmp ),
                               Formula=list(CombinedMod1_tmp$formula,
                                         CombinedMod2_tmp$formula,
                                         CombinedMod3_tmp$formula,
                                         CombinedMod4_tmp$formula,
                                         CombinedMod5_tmp$formula),
                               Link=list(family(CombinedMod1_tmp)$linkinv,
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
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
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

        names(BestOutput$Prediction[[1]])<-c("Var1","Var2","RandomVar1",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")

        }

        if(as.character(unique(tmp_df$Family))=="gaussian"){

           Output<-tibble::tibble(AIC=MuMIn::AICc(CombinedMod1_tmp,
                                                 CombinedMod2_tmp,
                                                 CombinedMod3_tmp,
                                                 CombinedMod4_tmp,
                                                 CombinedMod5_tmp ),
                                 Formula=list(paste0(deparse(CombinedMod1_tmp@call$formula)),
                                           paste0(deparse(CombinedMod2_tmp@call$formula)),
                                           paste0(deparse(CombinedMod3_tmp@call$formula)),
                                           paste0(deparse(CombinedMod4_tmp@call$formula)),
                                           paste0(deparse(CombinedMod5_tmp@call$formula))),
                                 Link=list(family(CombinedMod1_tmp)$linkinv,
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
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod1_tmp,
                                                                                                     NewData, n.sims = 1000))),
                                                 dplyr::bind_cols(NewData,
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod2_tmp,
                                                                                                    NewData, n.sims = 1000))),
                                                 dplyr::bind_cols(NewData,
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod3_tmp,
                                                                                                    NewData, n.sims = 1000))),
                                                 dplyr::bind_cols(NewData,
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod4_tmp,
                                                                                                    NewData, n.sims = 1000))),
                                                 dplyr::bind_cols(NewData,
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod5_tmp,
                                                                                                    NewData, n.sims = 1000)))),
                                 Raw=list(tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) )) %>%
            dplyr::mutate(Metric=Metrics[[i]])

          BestOutput<-Output %>%
            dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)


          BestOutput$Prediction[[1]][,7]<-((BestOutput$Prediction[[1]][,5]-BestOutput$Prediction[[1]][,6])/3.92)

          names(BestOutput$Prediction[[1]])<-c("Var1",
                                               "Var2",
                                               "RandomVar1",
                                               "fit_resp",
                                               "upr_resp",
                                               "lwr_resp",
                                               "se_resp")

        }

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
        print(paste("Assessing Metric: ", Metrics[[1]]))

        tmp_df<-Input_df %>%
          dplyr::filter(!Response%in%NA & !Var1%in%NA & !Var2%in%NA & !RandomVar1%in%NA)

        if(!as.character(unique(tmp_df$Family))=="gaussian"){
          CombinedMod1_tmp<-glmmADMB::glmmadmb(Response~Var1*Var2+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))
          CombinedMod2_tmp<-glmmADMB::glmmadmb(Response~Var1+Var2+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))
          CombinedMod3_tmp<-glmmADMB::glmmadmb(Response~Var2+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))
          CombinedMod4_tmp<-glmmADMB::glmmadmb(Response~Var1+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))
          CombinedMod5_tmp<-glmmADMB::glmmadmb(Response~1+(1|RandomVar1),data=tmp_df,
                                               family=as.character(unique(tmp_df$Family)))
        }

        if(as.character(unique(tmp_df$Family))=="gaussian"){

          CombinedMod1_tmp<-lmer(Response~Var1*Var2+(1|RandomVar1),data=tmp_df)
          CombinedMod2_tmp<-lmer(Response~Var1+Var2+(1|RandomVar1),data=tmp_df)
          CombinedMod3_tmp<-lmer(Response~Var2+(1|RandomVar1),data=tmp_df)
          CombinedMod4_tmp<-lmer(Response~Var1+(1|RandomVar1),data=tmp_df)
          CombinedMod5_tmp<-lmer(Response~1+(1|RandomVar1),data=tmp_df)

        }

        AIC_Values<-MuMIn::AICc(CombinedMod1_tmp,
                                CombinedMod2_tmp,
                                CombinedMod3_tmp,
                                CombinedMod4_tmp,
                                CombinedMod5_tmp) %>%
          dplyr::mutate(deltaAIC=AICc-min(AICc,na.rm=TRUE),
                        GoodBad=dplyr::case_when(deltaAIC<=2~"Good",
                                                 deltaAIC>2~"Bad"))

        Good_dfs<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good")

        Good_dfs_min<-min(Good_dfs$df)

        BestAICc<-AIC_Values %>%
          dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])



        if(!as.character(unique(tmp_df$Family))=="gaussian"){
          Output<-tibble::tibble(AIC=MuMIn::AICc(CombinedMod1_tmp,
                                                 CombinedMod2_tmp,
                                                 CombinedMod3_tmp,
                                                 CombinedMod4_tmp,
                                                 CombinedMod5_tmp ),
                                 Formula=list(CombinedMod1_tmp$formula,
                                           CombinedMod2_tmp$formula,
                                           CombinedMod3_tmp$formula,
                                           CombinedMod4_tmp$formula,
                                           CombinedMod5_tmp$formula),
                                 Link=list(family(CombinedMod1_tmp)$linkinv,
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
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
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

          names(BestOutput$Prediction[[1]])<-c("Var1","Var2","RandomVar1",
                                               "fit_link","se_link",
                                               "fit_resp","se_resp",
                                               "upr_resp","lwr_resp")

        }

        if(as.character(unique(tmp_df$Family))=="gaussian"){

          Output<-tibble::tibble(AIC=MuMIn::AICc(CombinedMod1_tmp,
                                                 CombinedMod2_tmp,
                                                 CombinedMod3_tmp,
                                                 CombinedMod4_tmp,
                                                 CombinedMod5_tmp ),
                                 Formula=list(paste0(deparse(CombinedMod1_tmp@call$formula)),
                                           paste0(deparse(CombinedMod2_tmp@call$formula)),
                                           paste0(deparse(CombinedMod3_tmp@call$formula)),
                                           paste0(deparse(CombinedMod4_tmp@call$formula)),
                                           paste0(deparse(CombinedMod5_tmp@call$formula))),
                                 Link=list(family(CombinedMod1_tmp)$linkinv,
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
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod1_tmp,
                                                                                                              NewData, n.sims = 1000))),
                                                 dplyr::bind_cols(NewData,
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod2_tmp,
                                                                                                              NewData, n.sims = 1000))),
                                                 dplyr::bind_cols(NewData,
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod3_tmp,
                                                                                                              NewData, n.sims = 1000))),
                                                 dplyr::bind_cols(NewData,
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod4_tmp,
                                                                                                              NewData, n.sims = 1000))),
                                                 dplyr::bind_cols(NewData,
                                                                  tibble::as_tibble(merTools::predictInterval(CombinedMod5_tmp,
                                                                                                              NewData, n.sims = 1000)))),
                                 Raw=list(tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) ,
                                          tmp_df %>%
                                            dplyr::group_by(Var1,Var2) %>%
                                            dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                             Se=se(Response),
                                                             CI=(sd(Response)*1.96)/2) )) %>%
            dplyr::mutate(Metric=Metrics[[i]])

          BestOutput<-Output %>%
            dplyr::filter(AIC$df==Good_dfs_min[[1]] & AIC$AICc==BestAICc$AICc)


          BestOutput$Prediction[[1]][,7]<-((BestOutput$Prediction[[1]][,5]-BestOutput$Prediction[[1]][,6])/3.92)

          names(BestOutput$Prediction[[1]])<-c("Var1",
                                               "Var2",
                                               "RandomVar1",
                                               "fit_resp",
                                               "upr_resp",
                                               "lwr_resp",
                                               "se_resp")

        }


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

    NewData<-data.frame(Var1=Input_df$Var1,
                        Var2=Input_df$Var2) %>%
      dplyr::distinct()

        Metrics<-unique(as.character(Input_df$Metric))

    if(length(Metrics)>1) {for (j in seq_along(Metrics)){

      print(paste0("Assessing Metric: ", Metrics[[j]]))

      tmp_df<-Input_df %>%
        dplyr::filter(Metric==Metrics[[j]]& !Response%in%NA & !Var1%in%NA & !Var2%in%NA & !RandomVar1%in%NA)



      if(as.character(unique(tmp_df$Family))%in%c("poisson")){
        Mod1_tmp<-glm(Response~Var1*Var2,data=tmp_df,family="poisson")
        Mod2_tmp<-glm(Response~Var1+Var2,data=tmp_df,family="poisson")
        Mod3_tmp<-glm(Response~Var2,data=tmp_df,family="poisson")
        Mod4_tmp<-glm(Response~Var1,data=tmp_df,family="poisson")
        Mod5_tmp<-glm(Response~1,data=tmp_df,family="poisson")
      }

      if(as.character(unique(tmp_df$Family))=="beta"){
        Mod1_tmp<-mgcv::gam(Response~Var1*Var2,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod2_tmp<-mgcv::gam(Response~Var1+Var2,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod3_tmp<-mgcv::gam(Response~Var2,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod4_tmp<-mgcv::gam(Response~Var1,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod5_tmp<-mgcv::gam(Response~1,data=tmp_df, family=mgcv::betar(link="logit"))
      }

      if(as.character(unique(tmp_df$Family))=="gamma"){
        Mod1_tmp<-glm(Response~Var1*Var2,data=tmp_df, family=Gamma())
        Mod2_tmp<-glm(Response~Var1+Var2,data=tmp_df, family=Gamma())
        Mod3_tmp<-glm(Response~Var2,data=tmp_df, family=Gamma())
        Mod4_tmp<-glm(Response~Var1,data=tmp_df, family=Gamma())
        Mod5_tmp<-glm(Response~1,data=tmp_df, family=Gamma())
      }

      if(as.character(unique(tmp_df$Family))=="gaussian"){
        Mod1_tmp<-lm(Response~Var1*Var2,data=tmp_df)
        Mod2_tmp<-lm(Response~Var1+Var2,data=tmp_df)
        Mod3_tmp<-lm(Response~Var2,data=tmp_df)
        Mod4_tmp<-lm(Response~Var1,data=tmp_df)
        Mod5_tmp<-lm(Response~1,data=tmp_df)
      }

      AIC_Values<-MuMIn::AICc(Mod1_tmp,
                              Mod2_tmp,
                              Mod3_tmp,
                              Mod4_tmp,
                              Mod5_tmp) %>%
        dplyr::mutate(deltaAIC=AICc-min(AICc,na.rm=TRUE),
                      GoodBad=dplyr::case_when(deltaAIC<=2~"Good",
                                               deltaAIC>2~"Bad"))

      Good_dfs<-AIC_Values %>%
        dplyr::filter(GoodBad=="Good")

      Good_dfs_min<-min(Good_dfs$df)

      BestAICc<-AIC_Values %>%
        dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])



      if(!as.character(unique(tmp_df$Family))%in%c("beta","gaussian")){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=list(Mod1_tmp$formula,
                                         Mod2_tmp$formula,
                                         Mod3_tmp$formula,
                                         Mod4_tmp$formula,
                                         Mod5_tmp$formula),
                               Link=list(family(Mod1_tmp)$linkinv,
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
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
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

        names(BestOutput$Prediction[[1]])<-c("Var1","Var2",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")
      }

      if(as.character(unique(tmp_df$Family))=="beta"){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=list(Mod1_tmp$formula,
                                         Mod2_tmp$formula,
                                         Mod3_tmp$formula,
                                         Mod4_tmp$formula,
                                         Mod5_tmp$formula),
                               Link=list(family(Mod1_tmp)$linkinv,
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
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
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

        names(BestOutput$Prediction[[1]])<-c("Var1","Var2",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")

      }

      if(as.character(unique(tmp_df$Family))=="gaussian"){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=list(paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod1_tmp$call$formula))),
                               Link=list("identity",
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
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response,na.rm=TRUE)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)),
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
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

        names(BestOutput$Prediction[[1]])<-c("Var1","Var2",
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

      print(paste0("Assessing Metric: ", Metrics[[1]]))

      tmp_df<-Input_df %>%
        dplyr::filter(!Response%in%NA &
                        !Var1%in%NA &
                        !Var2%in%NA &
                        !RandomVar1%in%NA)

      if(as.character(unique(tmp_df$Family))=="poisson"){
        Mod1_tmp<-glm(Response~Var1*Var2,data=tmp_df,family="poisson")
        Mod2_tmp<-glm(Response~Var1+Var2,data=tmp_df,family="poisson")
        Mod3_tmp<-glm(Response~Var2,data=tmp_df,family="poisson")
        Mod4_tmp<-glm(Response~Var1,data=tmp_df,family="poisson")
        Mod5_tmp<-glm(Response~1,data=tmp_df,family="poisson")
      }
      if(as.character(unique(tmp_df$Family))=="beta"){
        Mod1_tmp<-mgcv::gam(Response~Var1*Var2,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod2_tmp<-mgcv::gam(Response~Var1+Var2,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod3_tmp<-mgcv::gam(Response~Var2,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod4_tmp<-mgcv::gam(Response~Var1,data=tmp_df, family=mgcv::betar(link="logit"))
        Mod5_tmp<-mgcv::gam(Response~1,data=tmp_df, family=mgcv::betar(link="logit"))
      }
      if(as.character(unique(tmp_df$Family))=="gamma"){
        Mod1_tmp<-glm(Response~Var1*Var2,data=tmp_df, family=Gamma())
        Mod2_tmp<-glm(Response~Var1+Var2,data=tmp_df, family=Gamma())
        Mod3_tmp<-glm(Response~Var2,data=tmp_df, family=Gamma())
        Mod4_tmp<-glm(Response~Var1,data=tmp_df, family=Gamma())
        Mod5_tmp<-glm(Response~1,data=tmp_df, family=Gamma())
      }

      if(as.character(unique(tmp_df$Family))=="gaussian"){
        Mod1_tmp<-lm(Response~Var1*Var2,data=tmp_df)
        Mod2_tmp<-lm(Response~Var1+Var2,data=tmp_df)
        Mod3_tmp<-lm(Response~Var2,data=tmp_df)
        Mod4_tmp<-lm(Response~Var1,data=tmp_df)
        Mod5_tmp<-lm(Response~1,data=tmp_df)
      }

      AIC_Values<-MuMIn::AICc(Mod1_tmp,
                              Mod2_tmp,
                              Mod3_tmp,
                              Mod4_tmp,
                              Mod5_tmp) %>%
        dplyr::mutate(deltaAIC=AICc-min(AICc,na.rm=TRUE),
                      GoodBad=dplyr::case_when(deltaAIC<=2~"Good",
                                               deltaAIC>2~"Bad"))

      Good_dfs<-AIC_Values %>%
        dplyr::filter(GoodBad=="Good")

      Good_dfs_min<-min(Good_dfs$df)

      BestAICc<-AIC_Values %>%
        dplyr::filter(GoodBad=="Good" & df==Good_dfs_min[[1]])



      if(!as.character(unique(tmp_df$Family))%in%c("beta","gaussian")){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=list(Mod1_tmp$formula,
                                         Mod2_tmp$formula,
                                         Mod3_tmp$formula,
                                         Mod4_tmp$formula,
                                         Mod5_tmp$formula),
                               Link=list(family(Mod1_tmp)$linkinv,
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
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
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

        names(BestOutput$Prediction[[1]])<-c("Var1","Var2",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")
        }

      if(as.character(unique(tmp_df$Family))=="beta"){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=list(Mod1_tmp$formula,
                                         Mod2_tmp$formula,
                                         Mod3_tmp$formula,
                                         Mod4_tmp$formula,
                                         Mod5_tmp$formula),
                               Link=list(family(Mod1_tmp)$linkinv,
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
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
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

        names(BestOutput$Prediction[[1]])<-c("Var1","Var2",
                                             "fit_link","se_link",
                                             "fit_resp","se_resp",
                                             "upr_resp","lwr_resp")

        }

      if(as.character(unique(tmp_df$Family))=="gaussian"){
        Output<-tibble::tibble(AIC=MuMIn::AICc(Mod1_tmp,
                                               Mod2_tmp,
                                               Mod3_tmp,
                                               Mod4_tmp,
                                               Mod5_tmp ),
                               Formula=list(paste0(deparse(Mod1_tmp$call$formula)),
                                         paste0(deparse(Mod2_tmp$call$formula)),
                                         paste0(deparse(Mod3_tmp$call$formula)),
                                         paste0(deparse(Mod4_tmp$call$formula)),
                                         paste0(deparse(Mod5_tmp$call$formula))),
                               Link=list("identity",
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
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response,na.rm=TRUE)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)),
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
                                          dplyr::summarise(Mean=mean(Response,na.rm=TRUE),
                                                           Se=se(Response),
                                                           CI=(sd(Response)*1.96)/2) %>%
                                          dplyr::mutate(Se=if_else(Se%in%NA,0,Se),
                                                        CI=if_else(CI%in%NA,0,CI)) ,
                                        tmp_df %>%
                                          dplyr::group_by(Var1,Var2) %>%
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

        names(BestOutput$Prediction[[1]])<-c("Var1","Var2",
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

