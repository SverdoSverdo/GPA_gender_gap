        #### 0. LOADING DATA AND PACKAGES ####
        
source("00_settings.R")

data <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")

        
 #### 1. TEACHER EA AND GENDER ####

data_teacher_edu <- read.csv("temp.data/data_school_teacher_edu_m.csv")
data_teacher_gender <- read.csv("temp.data/data_school_teacher_gender_m.csv")

data_teacher <- data_teacher_gender %>%
  filter(w19_0634_lnr %in% data_teacher_edu$w19_0634_lnr)

teacher_gender_edu <- lmer(grades_std ~ noncog_g*kjoenn_gy*school_teacher_edu_m+
                                        cog_g*kjoenn_gy*school_teacher_edu_m+
                                        
                                        cog_parental_g*kjoenn_gy*school_teacher_edu_m+
                                        noncog_parental_g*kjoenn_gy*school_teacher_edu_m+
                                        
                                        cog_parental_g*cog_g*kjoenn_gy+
                                        cog_parental_g*noncog_g*kjoenn_gy+
                                        noncog_parental_g*cog_g*kjoenn_gy+
                                        noncog_parental_g*noncog_g*kjoenn_gy+
                                        
                                        noncog_g*kjoenn_gy*school_teacher_gender_m+
                                        cog_g*kjoenn_gy*school_teacher_gender_m+
                                        
                                        cog_parental_g*kjoenn_gy*school_teacher_gender_m+
                                        noncog_parental_g*kjoenn_gy*school_teacher_gender_m+
                                        
                                        cog_parental_g*cog_g*kjoenn_gy+
                                        cog_parental_g*noncog_g*kjoenn_gy+
                                        noncog_parental_g*cog_g*kjoenn_gy+
                                        noncog_parental_g*noncog_g*kjoenn_gy+
                                        
                                        school_middle*noncog_g*kjoenn_gy+
                                        school_middle*cog_g*kjoenn_gy+
                                        
                           (1 | lopenr_mor) + (1 | school_year),
                           data = data_teacher,
                           REML = T,
                           control = lmerControl(optimizer = "bobyqa"))

##### 1.1 retrieving estimates #####

fixed_effects_teacher <- summary(teacher_gender_edu)$coefficients

# specify the coefficients of interest
terms <- c("school_teacher_gender_m", "kjoenn_gy:school_teacher_gender_m","school_teacher_edu_m","kjoenn_gy:school_teacher_edu_m")

# extract the coefficients and variance-covariance matrix
coef_model <- fixef(teacher_gender_edu)[terms, "Estimate"]
vcov_model <- vcov(teacher_gender_edu)

# teacher gender and edu  effects for boys
teacher_edu_boys <- coef_model["school_teacher_edu_m"] + coef_model["kjoenn_gy:school_teacher_edu_m"]
teacher_gender_boys <- coef_model["school_teacher_gender_m"] + coef_model["kjoenn_gy:school_teacher_gender_m"]

# teacher gender and edu  effects for girls
teacher_edu_girls <- coef_model["school_teacher_edu_m"]
teacher_gender_girls <- coef_model["school_teacher_gender_m"] + coef_model["kjoenn_gy:school_teacher_gender_m"]

# variance/covariance terms
var_teacher_edu <- vcov_model["school_teacher_edu_m", "school_teacher_edu_m"]
var_teacher_edu_gender <- vcov_model["kjoenn_gy:school_teacher_edu_m", "kjoenn_gy:school_teacher_edu_m"]
cov_teacher_edu_gender <- vcov_model["school_teacher_edu_m", "kjoenn_gy:school_teacher_edu_m"]

# variance/covariance terms
var_teacher_gender <- vcov_model["school_teacher_gender_m", "school_teacher_gender_m"]
var_teacher_gender_gender <- vcov_model["kjoenn_gy:school_teacher_gender_m", "kjoenn_gy:school_teacher_gender_m"]
cov_teacher_gender_gender <- vcov_model["school_teacher_gender_m", "kjoenn_gy:school_teacher_gender_m"]

# standard errors
se_teacher_edu_girls <- sqrt(var_teacher_edu+var_teacher_edu_gender+ 2* (-1)* cov_teacher_edu_gender)
se_teacher_gender_girls <- sqrt(var_teacher_gender+var_teacher_gender_gender+ 2* (-1)* cov_teacher_gender_gender)

se_teacher_edu_boys <- sqrt(var_teacher_edu+var_teacher_edu_gender+ 2* (1)* cov_teacher_edu_gender)
se_teacher_gender_boys <- sqrt(var_teacher_gender+var_teacher_gender_gender+ 2* (1)* cov_teacher_gender_gender)

# calculate 95% confidence intervals
ci_teacher_gender_boys <- c(teacher_gender_boys - 1.96*se_teacher_edu_boys,
                            teacher_gender_boys + 1.96*se_teacher_edu_boys)
ci_teacher_gender_girls <- c(teacher_gender_girls - 1.96*se_teacher_gender_girls,
                             teacher_gender_girls + 1.96*se_teacher_gender_girls)
ci_teacher_edu_boys <- c(teacher_edu_boys - 1.96*se_teacher_edu_boys,
                         teacher_edu_boys + 1.96*se_teacher_edu_boys)
ci_teacher_edu_girls <- c(teacher_edu_girls - 1.96*se_teacher_edu_girls,
                          teacher_edu_girls + 1.96*se_teacher_edu_girls)

# putting it all into one DF
teacher_gender_edu_df <- data.frame(
  term = c("Proportion of female teachers (boys)", "Proportion of female teachers (girls)", "Teacher educational attainment (boys)", "Teacher educational attainment (girls)"),
  estimate = c(teacher_gender_boys,teacher_gender_girls,teacher_edu_boys,teacher_edu_girls),
  ci.lower = c(ci_teacher_gender_boys[1],ci_teacher_gender_girls[1], ci_teacher_edu_boys[1], ci_teacher_edu_girls[1]),
  ci.upper = c(ci_teacher_gender_boys[2],ci_teacher_gender_girls[2], ci_teacher_edu_boys[2], ci_teacher_edu_girls[2])
)

# extracting CIs for non-gendered terms
teacher_gender_PGI <- tidy(teacher_gender_edu)
teacher_gender_PGI <- select(teacher_gender_PGI, c(term,estimate,std.error))
teacher_gender_PGI <- teacher_gender_PGI[teacher_gender_PGI$term %in% c("noncog_g:school_teacher_edu_m","school_teacher_edu_m:cog_g",
                                                                        "noncog_g:school_teacher_gender_m","cog_g:school_teacher_gender_m"),]

teacher_gender_PGI$ci.upper <- teacher_gender_PGI$estimate + teacher_gender_PGI$std.error*1.96
teacher_gender_PGI$ci.lower <- teacher_gender_PGI$estimate - teacher_gender_PGI$std.error*1.96
teacher_gender_PGI$std.error <- NULL

# combining the two DFs
teacher_gender_edu_df <- rbind(teacher_gender_edu_df,teacher_gender_PGI)
teacher_gender_edu_df[2:ncol(teacher_gender_edu_df)] <- round(teacher_gender_edu_df[2:ncol(teacher_gender_edu_df)],3)

teacher_gender_edu_df[2:ncol(teacher_gender_edu_df)] <- round(teacher_gender_edu_df[2:ncol(teacher_gender_edu_df)],3)

write.table(teacher_gender_edu_df, row.names = F, quote = F, sep = "\t")


        #### 2 TEACHER TURNOVER POP CONTROL ####

# loading DF that contains all individuals in Norway and the municipality they lived in each year. limiting to the study period, 2018-2024
kommune <-  fread("N:/durable/data/registers/SSB/01_data/data_v6.0/POPULATION_RESIDENCY/csv/POPULATION_BOSTEDSKOMMUNE.csv",
                  select = c("w19_0634_lnr","bostedskommune_01_01_2018",
                                      "bostedskommune_01_01_2019","bostedskommune_01_01_2020","bostedskommune_01_01_2021",
                                      "bostedskommune_01_01_2022","bostedskommune_01_01_2023","bostedskommune_01_01_2024"), data.table = F)

# long format
kommune_long <- kommune %>%
  pivot_longer(
    cols = starts_with("bostedskommune_01_01_"),
    names_to = "year",
    names_prefix = "bostedskommune_01_01_",
    values_to = "municipality_code"
  )

# removing NA municipality codes
kommune_long <- kommune_long[!is.na(kommune_long$municipality_code),]

# population per municipality per year
kommune_pop <- kommune_long %>%
  group_by(year, municipality_code) %>%
  summarize(num_persons = n(), .groups = "drop")

kommune_pop$year <- as.integer(kommune_pop$year)

# run through datasets for all the years and connect school IDs to municipality codes

# specify the folder path
folder_path <- "N:/durable/data/registers/SSB/01_data/data_v6.0/EDUCATION_SCHOOLLEVEL/csv"

# list relevant files with full path
file_names <- list.files(path = folder_path, pattern = "EDUCATION_GSI_\\d{4}\\.csv", full.names = TRUE)
file_names <- file_names[26:32] # only need years 2018 to 2024

# loops through all datasets
school_data_long <- lapply(file_names, function(file) {
  # read the CSV file
  data <- fread(file, data.table = F)
  # extract the year from the file name
  year <- as.numeric(str_extract(file, "\\d{4}"))
  # select the relevant columns, convert kommnr to character, and add a year column
  data_selected <- data %>%
    select(lopenr_orgnr, kommnr) %>%
    mutate(
      kommnr = as.character(kommnr),  # Convert kommnr to character
      year = year
    )
  
  return(data_selected)
}) %>% bind_rows()

# removing NA school IDs
school_data_long <- school_data_long[!school_data_long$lopenr_orgnr == "",]

# merging back with municipality population data
school_kommune_pop <- merge(school_data_long, kommune_pop, by.x = c("year","kommnr"),
                            by.y = c("year","municipality_code"))

# reading in data for teacher turnover
turnover_data <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/data_school_turnover_m.csv")

# merging
turnover_data <- merge(turnover_data, school_kommune_pop, by.x = c("lnr_org","year"), by.y = c("lopenr_orgnr","year"))

turnover_data$num_persons <- scale(turnover_data$num_persons)

# estimating model
turnover_with_pop <-  lmer(grades_std ~ noncog_g*kjoenn_g*school_turnover_m+
                               cog_g*kjoenn_g*school_turnover_m+
                               
                               cog_parental_g*kjoenn_g*school_turnover_m+
                               noncog_parental_g*kjoenn_g*school_turnover_m+
                               
                               cog_parental_g*cog_g*kjoenn_g+
                               cog_parental_g*noncog_g*kjoenn_g+
                               noncog_parental_g*cog_g*kjoenn_g+
                               noncog_parental_g*noncog_g*kjoenn_g+
                               
                               school_middle*noncog_g*kjoenn_g+
                               school_middle*cog_g*kjoenn_g+
                               num_persons+
                               
                               (1 | lopenr_mor)  + (1 | lnr_org),
                             data = turnover_data,
                             REML = T,
                             control = lmerControl(optimizer = "bobyqa"))

summary(turnover_with_pop)

          ##### 2.1 retreiving estimates #####

turnover_fixed_effects <- summary(turnover_with_pop)$coefficients

# terms we need gender-specific values for
terms <- c("school_turnover_m", "kjoenn_g:school_turnover_m")

# extract the coefficients and variance-covariance matrix
coef_model <- turnover_fixed_effects[terms, "Estimate"]
vcov_model <- vcov(turnover_with_pop)


# gendered effects of teacher turnover
turnover_boys <- coef_model["school_turnover_m"] + coef_model["kjoenn_g:school_turnover_m"] * 1
turnover_girls <- coef_model["school_turnover_m"] - coef_model["kjoenn_g:school_turnover_m"] * 1

# variance/covariance terms
turnover_var <- vcov_model["school_turnover_m", "school_turnover_m"]
turnover_gender_var <- vcov_model["kjoenn_g:school_turnover_m", "kjoenn_g:school_turnover_m"]
turnover_gender_cov <- vcov_model["school_turnover_m", "kjoenn_g:school_turnover_m"]

# SE for turnvoer boys
se_turnover_boys <- sqrt((turnover_var +turnover_gender_var) + (1)*turnover_gender_cov*2 )

# SE for turnover girls
se_turnover_girls <- sqrt((turnover_var +turnover_gender_var) + (-1)*turnover_gender_cov*2 )

# 95% ciS
ci_turnover_boys <- c(turnover_boys - 1.96*se_turnover_boys, 
                      turnover_boys + 1.96*se_turnover_boys)

ci_turnover_girls <- c(turnover_girls - 1.96*se_turnover_girls, 
                       turnover_girls + 1.96*se_turnover_girls)

#merging it all into one df
turnover_gender <- data.frame(
  term = c("Teacher turnover (boys)", "Teacher turnover (girls)"),
estimate = c(turnover_boys,turnover_girls),
ci.lower = c(ci_turnover_boys[1],ci_turnover_girls[1]),
ci.upper = c(ci_turnover_boys[2],ci_turnover_girls[2]))

# extracting CIs for non-gendered terms
turnover_PGI <- tidy(turnover_with_pop)
turnover_PGI <- select(turnover_PGI, c(term,estimate,std.error))
turnover_PGI <- turnover_PGI[turnover_PGI$term %in% c("noncog_g:school_turnover_m","school_turnover_m:cog_g","num_persons"),]
turnover_PGI$ci.upper <- turnover_PGI$estimate + turnover_PGI$std.error*1.96
turnover_PGI$ci.lower <- turnover_PGI$estimate - turnover_PGI$std.error*1.96
turnover_PGI$std.error <- NULL

# combining the two DFs
turnover_df <- rbind(turnover_gender,turnover_PGI)
turnover_df[2:ncol(turnover_df)] <- round(turnover_df[2:ncol(turnover_df)],3)
  
write.table(turnover_df, row.names = F, quote = F, sep = "\t")


        #### 3. CLASSROOM GENDER & GRADES ####

data_class_grades <- read.csv("temp.data/data_class_grades_gy.csv")
data_class_gender <- read.csv("temp.data/data_classroom_gender_gy.csv")

# both models contain the exact same classrooms, doesn't matter which one we use
length(unique(data_class_gender$class))
length(unique(data_class_grades$class))
length(intersect(data_class_gender$class, data_class_grades$class))


class_grades_gender <-  lmer(grades_std ~ noncog_g*kjoenn_cg*class_grades_gy+
                                         cog_g*kjoenn_cg*class_grades_gy+
                                         
                                         cog_parental_g*kjoenn_cg*class_grades_gy+
                                         noncog_parental_g*kjoenn_cg*class_grades_gy+
                                         
                                         cog_parental_g*cog_g*kjoenn_cg+
                                         cog_parental_g*noncog_g*kjoenn_cg+
                                         noncog_parental_g*cog_g*kjoenn_cg+
                                         noncog_parental_g*noncog_g*kjoenn_cg+
                                         
                                         noncog_g*kjoenn_cg*classroom_gender_gy+
                                         cog_g*kjoenn_cg*classroom_gender_gy+
                                         
                                         cog_parental_g*kjoenn_cg*classroom_gender_gy+
                                         noncog_parental_g*kjoenn_cg*classroom_gender_gy+
                                         
                                         cog_parental_g*cog_g*kjoenn_cg+
                                         cog_parental_g*noncog_g*kjoenn_cg+
                                         noncog_parental_g*cog_g*kjoenn_cg+
                                         noncog_parental_g*noncog_g*kjoenn_cg+
                                         
                                         (1 | lopenr_mor)  + (1 | school_year/class),
                                       data = data_class_gender,
                                       REML = T,
                                       control = lmerControl(optimizer = "bobyqa"))


          ##### 3.1 retrieving estimates #####

fixed_effects_class <- summary(class_grades_gender)$coefficients

# specify the coefficients of interest
terms <- c("class_grades_gy", "kjoenn_cg:class_grades_gy","classroom_gender_gy","kjoenn_cg:classroom_gender_gy")

# extract the coefficients and variance-covariance matrix
coef_model <- fixed_effects_class[terms, "Estimate"]
vcov_model <- vcov(class_grades_gender)

# class grades and gender effects for boys
class_grades_boys <- coef_model["class_grades_gy"] + coef_model["kjoenn_cg:class_grades_gy"] 
class_gender_boys <- coef_model["classroom_gender_gy"] + coef_model["kjoenn_cg:classroom_gender_gy"] 

# class grades and gender effects for boys
class_grades_girls <- coef_model["class_grades_gy"] - coef_model["kjoenn_cg:class_grades_gy"] 
class_gender_girls <- coef_model["classroom_gender_gy"] - coef_model["kjoenn_cg:classroom_gender_gy"]


# variance/covariance terms
var_class_grades <- vcov_model["class_grades_gy", "class_grades_gy"]
var_class_grades_gender <- vcov_model["kjoenn_cg:class_grades_gy", "kjoenn_cg:class_grades_gy"]
cov_class_grades_gender <- vcov_model["class_grades_gy", "kjoenn_cg:class_grades_gy"]

# variance/covariance terms
var_class_gender <- vcov_model["classroom_gender_gy", "classroom_gender_gy"]
var_class_gender_gender <- vcov_model["kjoenn_cg:classroom_gender_gy", "kjoenn_cg:classroom_gender_gy"]
cov_class_gender_gender <- vcov_model["classroom_gender_gy", "kjoenn_cg:classroom_gender_gy"]

# standard errors
se_class_grades_girls <- sqrt(var_class_grades+var_class_grades_gender+ 2* (-1)* cov_class_grades_gender)
se_class_gender_girls <- sqrt(var_class_gender+var_class_gender_gender+ 2* (-1)* cov_class_gender_gender)

se_class_grades_boys <- sqrt(var_class_grades+var_class_grades_gender+ 2* (1)* cov_class_grades_gender)
se_class_gender_boys <- sqrt(var_class_gender+var_class_gender_gender+ 2* (1)* cov_class_gender_gender)


# calculate 95% confidence intervals
ci_class_gender_boys <- c(class_gender_boys - 1.96*se_class_gender_boys, 
                          class_gender_boys + 1.96*se_class_gender_boys)
ci_class_gender_girls <- c(class_gender_girls - 1.96*se_class_gender_girls, 
                          class_gender_girls + 1.96*se_class_gender_girls)
ci_class_grades_boys <- c(class_grades_boys - 1.96*se_class_grades_boys,
                         class_grades_boys + 1.96*se_class_grades_boys)
ci_class_grades_girls <- c(class_grades_girls - 1.96*se_class_grades_girls,
                         class_grades_girls + 1.96*se_class_grades_girls)

# putting it all into one DF
class_gender_df <- data.frame(
                                term = c("Classroom-level Proportion of girls (boys)", "Classroom-level Proportion of girls (girls)", "Classroom-level GPA (boys)", "Classroom-level GPA (girls)"),
                                estimate = c(class_gender_boys,class_gender_girls,class_grades_boys,class_grades_girls),
                                ci.lower = c(ci_class_gender_boys[1],ci_class_gender_girls[1], ci_class_grades_boys[1], ci_class_grades_girls[1]),
                                ci.upper = c(ci_class_gender_boys[2],ci_class_gender_girls[2], ci_class_grades_boys[2], ci_class_grades_girls[2])
)


# extracting CIs for non-gendered terms
classroom_PGI <- tidy(class_grades_gender)
classroom_PGI <- select(classroom_PGI, c(term,estimate,std.error))
classroom_PGI <- classroom_PGI[classroom_PGI$term %in% c("noncog_g:class_grades_gy","class_grades_gy:cog_g",
                                                       "noncog_g:classroom_gender_gy","cog_g:classroom_gender_gy"),]
classroom_PGI$ci.upper <- classroom_PGI$estimate + classroom_PGI$std.error*1.96
classroom_PGI$ci.lower <- classroom_PGI$estimate - classroom_PGI$std.error*1.96
classroom_PGI$std.error <- NULL

# combining the two DFs
classroom_df <- rbind(class_gender_df,classroom_PGI)
classroom_df[2:ncol(classroom_df)] <- round(classroom_df[2:ncol(classroom_df)],3)


classroom_df[2:ncol(classroom_df)] <- round(classroom_df[2:ncol(classroom_df)],3)

write.table(classroom_df, row.names = F, quote = F, sep = "\t")


