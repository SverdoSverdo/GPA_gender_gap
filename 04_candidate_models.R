        #### 0. PREPARING DATA ####

source("00_settings.R")        
    
data <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")


#Creating dataset that contains school_year combos with at least two classrooms
data_two_class <- data[data$one_class == 1,]

#To standardize effect sizes, this function takes the school-environment as input, removes rows with NA, and scales GPA,the PGIs and school env.

process_column <- function(dataset, column_name) {
  # remove rows that are NA in the specified column
  data_clean <- dataset[!is.na(dataset[[column_name]]), ]
  
  # scale the specified columns, GPA, noncog_g, and cog_g
  data_clean[[column_name]] <- scale(data_clean[[column_name]])[, 1]
  data_clean[["noncog_g"]] <- scale(data_clean[["noncog_g"]])[, 1]
  data_clean[["cog_g"]] <- scale(data_clean[["cog_g"]])[, 1]
  data_clean[["grades_std"]] <- scale(data_clean[["grades_std"]])[, 1]
  
  # create new dataframe with name data_xxx where xxx is the column name
  new_df_name <- paste0("data_", column_name)
  assign(new_df_name, data_clean, envir = .GlobalEnv)
  
  # write to CSV file
 write.csv(data_clean, file = file.path("temp.data", paste0(new_df_name, ".csv")), row.names = FALSE)
  
  return(new_df_name)
}


        #### 1. CANDIDATE MODELS ####

# creating a list where the models are stored
candidate_models <- list()

process_column(data, "school_income_m")

candidate_models[["school_income"]] <-  lmer(grades_std ~ noncog_g*kjoenn_g*school_income_m+
                                                       cog_g*kjoenn_g*school_income_m+
                                                       
                                                       cog_parental_g*kjoenn_g*school_income_m+
                                                       noncog_parental_g*kjoenn_g*school_income_m+
                                                       
                                                       cog_parental_g*cog_g*kjoenn_g+
                                                       cog_parental_g*noncog_g*kjoenn_g+
                                                       noncog_parental_g*cog_g*kjoenn_g+
                                                       noncog_parental_g*noncog_g*kjoenn_g+
                                                       
                                                       (1 | lopenr_mor)  + (1 | lnr_org),
                                                     data = data_school_income_m,
                                                     REML = T,
                                                     control = lmerControl(optimizer = "bobyqa"))


process_column(data_two_class, "classroom_gender_gy")

candidate_models[["class_gender"]] <-  lmer(grades_std ~ noncog_g*kjoenn_cg*classroom_gender_gy+
                                                   cog_g*kjoenn_cg*classroom_gender_gy+
                                                   
                                                    cog_parental_g*kjoenn_cg*classroom_gender_gy+
                                                    noncog_parental_g*kjoenn_cg*classroom_gender_gy+
                                                     
                                                   cog_parental_g*cog_g*kjoenn_cg+
                                                   cog_parental_g*noncog_g*kjoenn_cg+
                                                   noncog_parental_g*cog_g*kjoenn_cg+
                                                   noncog_parental_g*noncog_g*kjoenn_cg+
                                                   
                                                   (1 | lopenr_mor)  + (1 | school_year/class),
                                                 data = data_classroom_gender_gy,
                                                 REML = T,
                                                 control = lmerControl(optimizer = "bobyqa"))

process_column(data_two_class, "class_grades_gy")

candidate_models[["class_grades"]] <-  lmer(grades_std ~ noncog_g*kjoenn_cg*class_grades_gy+
                                                      cog_g*kjoenn_cg*class_grades_gy+
                                                      
                                                      cog_parental_g*kjoenn_cg*class_grades_gy+
                                                      noncog_parental_g*kjoenn_cg*class_grades_gy+
                                                      
                                                      cog_parental_g*cog_g*kjoenn_cg+
                                                      cog_parental_g*noncog_g*kjoenn_cg+
                                                      noncog_parental_g*cog_g*kjoenn_cg+
                                                      noncog_parental_g*noncog_g*kjoenn_cg+
                                                      
                                                      (1 | lopenr_mor)  + (1 | school_year/class),
                                                    data = data_class_grades_gy,
                                                    REML = T,
                                                    control = lmerControl(optimizer = "bobyqa"))

process_column(data, "school_teacher_edu_m")

candidate_models[["teacher_edu"]] <-  lmer(grades_std ~ noncog_g*kjoenn_gy*school_teacher_edu_m+
                                                     cog_g*kjoenn_gy*school_teacher_edu_m+
                                                     
                                                     cog_parental_g*kjoenn_gy*school_teacher_edu_m+
                                                     noncog_parental_g*kjoenn_gy*school_teacher_edu_m+
                                                     
                                                     cog_parental_g*cog_g*kjoenn_gy+
                                                     cog_parental_g*noncog_g*kjoenn_gy+
                                                     noncog_parental_g*cog_g*kjoenn_gy+
                                                     noncog_parental_g*noncog_g*kjoenn_gy+
                                                     
                                                     school_middle*noncog_g*kjoenn_gy+
                                                     school_middle*cog_g*kjoenn_gy+
                                                     
                                                     (1 | lopenr_mor)  + (1 | school_year),
                                                   data = data_school_teacher_edu_m,
                                                   REML = T,
                                                   control = lmerControl(optimizer = "bobyqa"))

process_column(data, "school_teacher_gender_m")

candidate_models[["teacher_gender"]] <-  lmer(grades_std ~ noncog_g*kjoenn_gy*school_teacher_gender_m+
                                                     cog_g*kjoenn_gy*school_teacher_gender_m+
                                                       
                                                       cog_parental_g*kjoenn_gy*school_teacher_gender_m+
                                                       noncog_parental_g*kjoenn_gy*school_teacher_gender_m+
                                                     
                                                       cog_parental_g*cog_g*kjoenn_gy+
                                                       cog_parental_g*noncog_g*kjoenn_gy+
                                                       noncog_parental_g*cog_g*kjoenn_gy+
                                                       noncog_parental_g*noncog_g*kjoenn_gy+
                                                     
                                                       school_middle*noncog_g*kjoenn_gy+
                                                       school_middle*cog_g*kjoenn_gy+
                                                     
                                                     
                                                     (1 | lopenr_mor)  + (1 | school_year),
                                                   data = data_school_teacher_gender_m,
                                                   REML = T,
                                                   control = lmerControl(optimizer = "bobyqa"))

process_column(data, "school_turnover_m")

candidate_models[["turnover"]] <-  lmer(grades_std ~ noncog_g*kjoenn_g*school_turnover_m+
                                                     cog_g*kjoenn_g*school_turnover_m+
                                                     
                                                     cog_parental_g*kjoenn_g*school_turnover_m+
                                                     noncog_parental_g*kjoenn_g*school_turnover_m+
                                                  
                                                      cog_parental_g*cog_g*kjoenn_g+
                                                      cog_parental_g*noncog_g*kjoenn_g+
                                                      noncog_parental_g*cog_g*kjoenn_g+
                                                      noncog_parental_g*noncog_g*kjoenn_g+
                                                         
                                                      school_middle*noncog_g*kjoenn_g+
                                                      school_middle*cog_g*kjoenn_g+
                                                     
                                                     (1 | lopenr_mor)  + (1 | lnr_org),
                                                   data = data_school_turnover_m,
                                                   REML = T,
                                                   control = lmerControl(optimizer = "bobyqa"))

process_column(data, "school_student_gender_m")

candidate_models[["student_gender"]] <-  lmer(grades_std ~ noncog_g*kjoenn_gy*school_student_gender_m+
                                                     cog_g*kjoenn_gy*school_student_gender_m+
                                                     
                                                     cog_parental_g*kjoenn_gy*school_student_gender_m+
                                                     noncog_parental_g*kjoenn_gy*school_student_gender_m+
                                                     
                                                     cog_parental_g*cog_g*kjoenn_gy+
                                                     cog_parental_g*noncog_g*kjoenn_gy+
                                                     noncog_parental_g*cog_g*kjoenn_gy+
                                                     noncog_parental_g*noncog_g*kjoenn_gy+
                             
                                                     
                                                     (1 | lopenr_mor)  + (1 | school_year),
                                                   data = data_school_student_gender_m,
                                                   REML = T,
                                                   control = lmerControl(optimizer = "bobyqa"))

process_column(data, "pos_climate_m")

candidate_models[["pos_climate"]] <- lmer(grades_std ~ noncog_g*kjoenn_gy*pos_climate_m+
                                                   cog_g*kjoenn_gy*pos_climate_m+
                                            
                                            cog_parental_g*kjoenn_gy*pos_climate_m+
                                            noncog_parental_g*kjoenn_gy*pos_climate_m+
                                                   
                                                   cog_parental_g*cog_g*kjoenn_gy+
                                                   cog_parental_g*noncog_g*kjoenn_gy+
                                                   noncog_parental_g*cog_g*kjoenn_gy+
                                                   noncog_parental_g*noncog_g*kjoenn_gy+
                                                   
                                                   
                                                   (1 | lopenr_mor)  + (1 | school_year),
                                                 data = data_pos_climate_m,
                                                 REML = T,
                                                 control = lmerControl(optimizer = "bobyqa"))

process_column(data, "ext_behavior_m")

candidate_models[["ext_behavior"]] <- lmer(grades_std ~ noncog_g*kjoenn_gy*ext_behavior_m+
                                                   cog_g*kjoenn_gy*ext_behavior_m+
                                             
                                             cog_parental_g*kjoenn_gy*ext_behavior_m+
                                             noncog_parental_g*kjoenn_gy*ext_behavior_m+
                                                   
                                                   cog_parental_g*cog_g*kjoenn_gy+
                                                   cog_parental_g*noncog_g*kjoenn_gy+
                                                   noncog_parental_g*cog_g*kjoenn_gy+
                                                   noncog_parental_g*noncog_g*kjoenn_gy+

                                                   (1 | lopenr_mor)  + (1 | school_year),
                                                 data = data_ext_behavior_m,
                                                 REML = T,
                                                 control = lmerControl(optimizer = "bobyqa"))


#save(candidate_models, file = "N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/candidate_models.Rdata")



        #### 2. CANDIDATE MODELS WITH SQUARED TERMS ####

# reading all datafiles
file_paths <- list.files(path = "temp.data/", 
                         pattern = "(_m|_gy)\\.csv$", 
                         full.names = TRUE)
# read and name them
file_names <- tools::file_path_sans_ext(basename(file_paths))

for(i in seq_along(file_paths)) {
  assign(file_names[i], 
         fread(file_paths[i]), 
         envir = .GlobalEnv)
}

# creating list to store the results
candidate_models_sq <- list()

candidate_models_sq[["school_income"]] <-  lmer(grades_std ~ noncog_g*kjoenn_g*school_income_m+
                                                  cog_g*kjoenn_g*school_income_m+
                                                  
                                                  cog_parental_g*kjoenn_g*school_income_m+
                                                  noncog_parental_g*kjoenn_g*school_income_m+
                                                  
                                                  noncog_g*kjoenn_g*I(school_income_m^2)+
                                                  cog_g*kjoenn_g*I(school_income_m^2)+
                                                  noncog_parental_g*kjoenn_g*I(school_income_m^2)+
                                                  cog_parental_g*kjoenn_g*I(school_income_m^2)+
                                                  
                                                  cog_parental_g*cog_g*kjoenn_g+
                                                  cog_parental_g*noncog_g*kjoenn_g+
                                                  noncog_parental_g*cog_g*kjoenn_g+
                                                  noncog_parental_g*noncog_g*kjoenn_g+
                                                  
                                                  (1 | lopenr_mor)  + (1 | lnr_org),
                                                data = data_school_income_m,
                                                REML = T,
                                                control = lmerControl(optimizer = "bobyqa"))


candidate_models_sq[["class_gender"]] <-  lmer(grades_std ~ noncog_g*kjoenn_cg*classroom_gender_gy+
                                                 cog_g*kjoenn_cg*classroom_gender_gy+
                                                 
                                                 cog_parental_g*kjoenn_cg*classroom_gender_gy+
                                                 noncog_parental_g*kjoenn_cg*classroom_gender_gy+
                                                 
                                                 noncog_g*kjoenn_cg*I(classroom_gender_gy^2)+
                                                 cog_g*kjoenn_cg*I(classroom_gender_gy^2)+
                                                 noncog_parental_g*kjoenn_cg*I(classroom_gender_gy^2)+
                                                 cog_parental_g*kjoenn_cg*I(classroom_gender_gy^2)+
                                                 
                                                 cog_parental_g*cog_g*kjoenn_cg+
                                                 cog_parental_g*noncog_g*kjoenn_cg+
                                                 noncog_parental_g*cog_g*kjoenn_cg+
                                                 noncog_parental_g*noncog_g*kjoenn_cg+
                                                 
                                                 (1 | lopenr_mor)  + (1 | school_year/class),
                                               data = data_classroom_gender_gy,
                                               REML = T,
                                               control = lmerControl(optimizer = "bobyqa"))


candidate_models_sq[["class_grades"]] <-  lmer(grades_std ~ noncog_g*kjoenn_cg*class_grades_gy+
                                                 cog_g*kjoenn_cg*class_grades_gy+
                                                 
                                                 cog_parental_g*kjoenn_cg*class_grades_gy+
                                                 noncog_parental_g*kjoenn_cg*class_grades_gy+
                                                 
                                                 noncog_g*kjoenn_cg*I(class_grades_gy^2)+
                                                 cog_g*kjoenn_cg*I(class_grades_gy^2)+
                                                 noncog_parental_g*kjoenn_cg*I(class_grades_gy^2)+
                                                 cog_parental_g*kjoenn_cg*I(class_grades_gy^2)+
                                                 
                                                 cog_parental_g*cog_g*kjoenn_cg+
                                                 cog_parental_g*noncog_g*kjoenn_cg+
                                                 noncog_parental_g*cog_g*kjoenn_cg+
                                                 noncog_parental_g*noncog_g*kjoenn_cg+
                                                 
                                                 (1 | lopenr_mor)  + (1 | school_year/class),
                                               data = data_class_grades_gy,
                                               REML = T,
                                               control = lmerControl(optimizer = "bobyqa"))


candidate_models_sq[["teacher_edu"]] <-  lmer(grades_std ~ noncog_g*kjoenn_gy*school_teacher_edu_m+
                                                cog_g*kjoenn_gy*school_teacher_edu_m+
                                                
                                                cog_parental_g*kjoenn_gy*school_teacher_edu_m+
                                                noncog_parental_g*kjoenn_gy*school_teacher_edu_m+
                                                
                                                noncog_g*kjoenn_gy*I(school_teacher_edu_m^2)+
                                                cog_g*kjoenn_gy*I(school_teacher_edu_m^2)+
                                                noncog_parental_g*kjoenn_gy*I(school_teacher_edu_m^2)+
                                                cog_parental_g*kjoenn_gy*I(school_teacher_edu_m^2)+
                                                
                                                cog_parental_g*cog_g*kjoenn_gy+
                                                cog_parental_g*noncog_g*kjoenn_gy+
                                                noncog_parental_g*cog_g*kjoenn_gy+
                                                noncog_parental_g*noncog_g*kjoenn_gy+
                                                
                                                school_middle*noncog_g*kjoenn_gy+
                                                school_middle*cog_g*kjoenn_gy+
                                                
                                                
                                                (1 | lopenr_mor)  + (1 | school_year),
                                              data = data_school_teacher_edu_m,
                                              REML = T,
                                              control = lmerControl(optimizer = "bobyqa"))


candidate_models_sq[["teacher_gender"]] <-  lmer(grades_std ~ noncog_g*kjoenn_gy*school_teacher_gender_m+
                                                   cog_g*kjoenn_gy*school_teacher_gender_m+
                                                   
                                                   noncog_g*kjoenn_gy*I(school_teacher_gender_m^2)+
                                                   cog_g*kjoenn_gy*I(school_teacher_gender_m^2)+
                                                   noncog_parental_g*kjoenn_gy*I(school_teacher_gender_m^2)+
                                                   cog_parental_g*kjoenn_gy*I(school_teacher_gender_m^2)+
                                                   
                                                   noncog_g*kjoenn_gy*I(school_teacher_gender_m^2)+
                                                   cog_g*kjoenn_gy*I(school_teacher_gender_m^2)+
                                                   noncog_parental_g*kjoenn_gy*I(school_teacher_gender_m^2)+
                                                   cog_parental_g*kjoenn_gy*I(school_teacher_gender_m^2)+
                                                   
                                                   cog_parental_g*cog_g*kjoenn_gy+
                                                   cog_parental_g*noncog_g*kjoenn_gy+
                                                   noncog_parental_g*cog_g*kjoenn_gy+
                                                   noncog_parental_g*noncog_g*kjoenn_gy+
                                                   
                                                   school_middle*noncog_g*kjoenn_gy+
                                                   school_middle*cog_g*kjoenn_gy+
                                                   
                                                   
                                                   (1 | lopenr_mor)  + (1 | school_year),
                                                 data = data_school_teacher_gender_m,
                                                 REML = T,
                                                 control = lmerControl(optimizer = "bobyqa"))


candidate_models_sq[["turnover"]] <-  lmer(grades_std ~ noncog_g*kjoenn_g*school_turnover_m+
                                             cog_g*kjoenn_g*school_turnover_m+
                                             
                                             cog_parental_g*kjoenn_g*school_turnover_m+
                                             noncog_parental_g*kjoenn_g*school_turnover_m+
                                             
                                             noncog_g*kjoenn_g*I(school_turnover_m^2)+
                                             cog_g*kjoenn_g*I(school_turnover_m^2)+
                                             noncog_parental_g*kjoenn_g*I(school_turnover_m^2)+
                                             cog_parental_g*kjoenn_g*I(school_turnover_m^2)+
                                             
                                             cog_parental_g*cog_g*kjoenn_g+
                                             cog_parental_g*noncog_g*kjoenn_g+
                                             noncog_parental_g*cog_g*kjoenn_g+
                                             noncog_parental_g*noncog_g*kjoenn_g+
                                             
                                             school_middle*noncog_g*kjoenn_g+
                                             school_middle*cog_g*kjoenn_g+
                                             
                                             (1 | lopenr_mor)  + (1 | lnr_org),
                                           data = data_school_turnover_m,
                                           REML = T,
                                           control = lmerControl(optimizer = "bobyqa"))


candidate_models_sq[["student_gender"]] <-  lmer(grades_std ~ noncog_g*kjoenn_gy*school_student_gender_m+
                                                   cog_g*kjoenn_gy*school_student_gender_m+
                                                   
                                                   noncog_g*kjoenn_gy*I(school_student_gender_m^2)+
                                                   cog_g*kjoenn_gy*I(school_student_gender_m^2)+
                                                   noncog_parental_g*kjoenn_gy*I(school_student_gender_m^2)+
                                                   cog_parental_g*kjoenn_gy*I(school_student_gender_m^2)+
                                                   
                                                   cog_parental_g*cog_g*kjoenn_gy+
                                                   cog_parental_g*noncog_g*kjoenn_gy+
                                                   noncog_parental_g*cog_g*kjoenn_gy+
                                                   noncog_parental_g*noncog_g*kjoenn_gy+
                                                   
                                                   school_middle*noncog_g*kjoenn_gy+
                                                   school_middle*cog_g*kjoenn_gy+
                                                   
                                                   (1 | lopenr_mor)  + (1 | school_year),
                                                 data = data_school_student_gender_m,
                                                 REML = T,
                                                 control = lmerControl(optimizer = "bobyqa"))


candidate_models_sq[["pos_climate"]] <- lmer(grades_std ~ noncog_g*kjoenn_gy*pos_climate_m+
                                               cog_g*kjoenn_gy*pos_climate_m+
                                               
                                               noncog_g*kjoenn_gy*I(pos_climate_m^2)+
                                               cog_g*kjoenn_gy*I(pos_climate_m^2)+
                                               noncog_parental_g*kjoenn_gy*I(pos_climate_m^2)+
                                               cog_parental_g*kjoenn_gy*I(pos_climate_m^2)+
                                               
                                               cog_parental_g*cog_g*kjoenn_gy+
                                               cog_parental_g*noncog_g*kjoenn_gy+
                                               noncog_parental_g*cog_g*kjoenn_gy+
                                               noncog_parental_g*noncog_g*kjoenn_gy+
                                               
                                               
                                               (1 | lopenr_mor)  + (1 | school_year),
                                             data = data_pos_climate_m,
                                             REML = T,
                                             control = lmerControl(optimizer = "bobyqa"))


candidate_models_sq[["ext_behavior"]] <- lmer(grades_std ~ noncog_g*kjoenn_gy*ext_behavior_m+
                                                cog_g*kjoenn_gy*ext_behavior_m+
                                                
                                                noncog_g*kjoenn_gy*I(ext_behavior_m^2)+
                                                cog_g*kjoenn_gy*I(ext_behavior_m^2)+
                                                noncog_parental_g*kjoenn_gy*I(ext_behavior_m^2)+
                                                cog_parental_g*kjoenn_gy*I(ext_behavior_m^2)+
                                                
                                                cog_parental_g*cog_g*kjoenn_gy+
                                                cog_parental_g*noncog_g*kjoenn_gy+
                                                noncog_parental_g*cog_g*kjoenn_gy+
                                                noncog_parental_g*noncog_g*kjoenn_gy+
                                                
                                                (1 | lopenr_mor)  + (1 | school_year),
                                              data = data_ext_behavior_m,
                                              REML = T,
                                              control = lmerControl(optimizer = "bobyqa"))

#save(candidate_models_sq, file = "N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/candidate_models_sq.Rdata")
