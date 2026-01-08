source("00_settings.R")

data <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")

        #### 1. RANDOM EFFECTS MODELS ####

#List to store all models
random_models <- list()


random_models[["model_1"]] <-  lmer(grades_std ~ cog_g*kjoenn_g+
                                           noncog_g*kjoenn_g+
                                           
                                           cog_parental_g*kjoenn_g+
                                           cog_parental_g*noncog_g*kjoenn_g+
                                           noncog_parental_g*noncog_g*kjoenn_g+
                                           noncog_parental_g*cog_g*kjoenn_g+
                                           
                                           
                                           (1 | lopenr_mor)  + (1 | lnr_org),
                                         data = data,
                                         REML = F,
                                         control = lmerControl(optimizer = "bobyqa"))


#gender
random_models[["model_2"]] <- lmer(grades_std ~ cog_g*kjoenn_g+
                                          noncog_g*kjoenn_g+
                                          
                                          cog_parental_g*cog_g*kjoenn_g+
                                          cog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*cog_g*kjoenn_g+
                                          
                                          (1 | lopenr_mor)  + (1+kjoenn_g | lnr_org),
                                        data = data,
                                        REML = F,
                                        control = lmerControl(optimizer = "bobyqa"))


#gender+noncog
random_models[["model_3"]] <- lmer(grades_std ~ cog_g*kjoenn_g+
                                          noncog_g*kjoenn_g+
                                          
                                          cog_parental_g*cog_g*kjoenn_g+
                                          cog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*cog_g*kjoenn_g+
                                          
                                          (1 | lopenr_mor)  + (1+kjoenn_g+noncog_g | lnr_org),
                                        data = data,
                                        REML = F,
                                        control = lmerControl(optimizer = "bobyqa"))

#gender+noncog+cog
random_models[["model_4"]] <- lmer(grades_std ~ cog_g*kjoenn_g+
                                          noncog_g*kjoenn_g+
                                          
                                          cog_parental_g*cog_g*kjoenn_g+
                                          cog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*cog_g*kjoenn_g+
                                          
                                          
                                          (1 | lopenr_mor)  + (1+kjoenn_g+noncog_g+cog_g | lnr_org),
                                        data = data,
                                        REML = F,
                                        control = lmerControl(optimizer = "bobyqa"))



#gender*noncog
random_models[["model_5"]] <- lmer(grades_std ~ cog_g*kjoenn_g+
                                          noncog_g*kjoenn_g+
                                          
                                          cog_parental_g*cog_g*kjoenn_g+
                                          cog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*cog_g*kjoenn_g+
                                          
                                          (1 | lopenr_mor)  + (1+kjoenn_g*noncog_g | lnr_org),
                                        data = data,
                                        REML = F,
                                        control = lmerControl(optimizer = "bobyqa"))

#gender*cog
random_models[["model_6"]] <- lmer(grades_std ~ cog_g*kjoenn_g+
                                     noncog_g*kjoenn_g+
                                     
                                     cog_parental_g*cog_g*kjoenn_g+
                                     cog_parental_g*noncog_g*kjoenn_g+
                                     noncog_parental_g*noncog_g*kjoenn_g+
                                     noncog_parental_g*cog_g*kjoenn_g+
                                     
                                     (1 | lopenr_mor)  + (1+kjoenn_g*cog_g | lnr_org),
                                   data = data,
                                   REML = F,
                                   control = lmerControl(optimizer = "bobyqa"))

#gender*noncog+cog
random_models[["model_7"]] <- lmer(grades_std ~ cog_g*kjoenn_g+
                                          noncog_g*kjoenn_g+
                                          
                                          cog_parental_g*cog_g*kjoenn_g+
                                          cog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*cog_g*kjoenn_g+
                                          
                                          (1 | lopenr_mor)  + (1+kjoenn_g*noncog_g+cog_g | lnr_org),
                                        data = data,
                                        REML = F,
                                        control = lmerControl(optimizer = "bobyqa"))

#gender*cog+noncog
random_models[["model_8"]] <- lmer(grades_std ~ cog_g*kjoenn_g+
                                          noncog_g*kjoenn_g+
                                          
                                          cog_parental_g*cog_g*kjoenn_g+
                                          cog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*cog_g*kjoenn_g+
                                          
                                          (1 | lopenr_mor)  + (1+kjoenn_g*cog_g+noncog_g | lnr_org),
                                        data = data,
                                        REML = F,
                                        control = lmerControl(optimizer = "nloptwrap"))

#gender*cog+noncog
random_models[["model_9"]] <- lmer(grades_std ~ cog_g*kjoenn_g+
                                          noncog_g*kjoenn_g+
                                          
                                          cog_parental_g*cog_g*kjoenn_g+
                                          cog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*noncog_g*kjoenn_g+
                                          noncog_parental_g*cog_g*kjoenn_g+
                                          
                                          (1 | lopenr_mor)  + (1+kjoenn_g*noncog_g+kjoenn_g*cog_g| lnr_org),
                                        data = data,
                                        REML = F,
                                        control = lmerControl(optimizer = "nloptwrap"))


#save(random_models, file = "N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/random_models.Rdata")

