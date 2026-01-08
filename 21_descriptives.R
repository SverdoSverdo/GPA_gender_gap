
        #### 0. LOADING DATA AND PACKAGES ####
source("00_settings.R")
        
data <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")

# indexing school-level variables
school_environments <- subset(data, select = c(school_teacher_gender,school_turnover,
                                               school_teacher_edu,classroom_gender_gy,class_grades_gy,
                                               school_income,school_student_gender, ext_behavior,pos_climate,
                                               cog,noncog))

        #### 1. SCHOOL ENVIRONMENT DESCRIPTIVES ####

# function that gets correlations and p-values for cog and noncog with school variables
get_cor_and_p <- function(data, vars, outcomes) {
  results <- data.frame()
  
  for(var in vars) {
    for(outcome in outcomes) {
      test <- cor.test(data[[var]], data[[outcome]], use = "pairwise.complete.obs")
      results <- rbind(results, data.frame(
        variable = var,
        outcome = outcome,
        est = test$estimate,
        p = test$p.value
      ))
    }
  }
  return(results)
}

# get school variable names (excluding cog and noncog)
school_vars <- names(school_environments)[!names(school_environments) %in% c("cog", "noncog")]

# get correlations and p-values
cor_results <- get_cor_and_p(school_environments, school_vars, c("cog", "noncog"))

# reshaping
descriptives <- cor_results %>%
  pivot_wider(names_from = outcome, 
              values_from = c(est, p),
              names_sep = "_")


# mean, range and median students per school
data %>%
  group_by(lnr_org) %>%
  summarise(n_students = n()) %>%
  summarise(
    mean_students = mean(n_students),
    min_students = min(n_students,na.rm = T),
    max_students = max(n_students,na.rm = T),
    median_students = median(n_students)
  )

# mean, range and median students per class
data_two_class <- data[data$one_class == 1,]
data_two_class <- data_two_class[!is.na(data_two_class$class),]

data_two_class %>%
  group_by(class) %>%
  summarise(n_students = n()) %>%
  summarise(
    mean_students = mean(n_students),
    min_students = min(n_students, na.rm = T),
    max_students = max(n_students, na.rm = T),
    median_students = median(n_students)
  )

# functions that calculate means and variances for each school-level variable
means <- sapply(school_environments, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA)
sd <- sapply(school_environments, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA)

# create dataframe with means and variances
mean_var <- data.frame(
  variable = names(school_environments),
  mean = means,
  sd = sd
)

# removing Cog and NonCog rows
mean_var <- mean_var %>%
  filter(!(variable %in% c("cog", "noncog")))

# merging wtih descriptives
descriptives <- merge(mean_var, descriptives)


          #####  1.1 sample size per model #####

load("N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/candidate_models.Rdata")

# initiating DF
N_per_model <- data.frame(variable = rep(NA, 9),
                          N = rep(NA, 9),
                          N_schools = rep(NA, 9))

# indexing the school-level variables for the subsequent loop
env_variables <- c("school_income_m", "classroom_gender_gy", "class_grades_gy",
                   "school_teacher_edu_m", "school_teacher_gender_m", "school_turnover_m",
                   "school_student_gender_m", "pos_climate_m", "ext_behavior_m")

# function that runs through each model and gets N students and N schools per model
for(i in 1:length(candidate_models)) {
  N_per_model$variable[i] <- names(candidate_models[i])
  N_per_model$N[i] <- nobs(candidate_models[[i]])
  
  variable <- env_variables[i]
  data_m <- data[!is.na(data[[variable]]), ]
  N_per_model$N_schools[i] <- length(unique(data_m$lnr_org))
}

# changing names of in order to merge with descriptives
descriptives$variable <- c("class_grades","class_gender","ext_behavior",
                          "pos_climate","school_income","student_gender",
                          "teacher_edu", "teacher_gender","turnover")

descriptives <- merge(descriptives, N_per_model, by = "variable")

# rounding
descriptives[2:5] <- round(descriptives[2:5],2)
descriptives[6:7] <- round(descriptives[6:7],3)

# N classrooms in the models
lme4::ngrps(candidate_models$class_grades)
lme4::ngrps(candidate_models$class_gender)

write.table(descriptives, row.names = F, quote = F, sep = "\t")


          ##### 1.2 random effects model #####

load("N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/random_models.Rdata")

# N schools and students in the random effects model
random_model <- random_models$model_5
nobs(random_model)
lme4::ngrps(random_model)


        #### 2. INDIVIDUAL-LEVEL DESCRIPTIVES ####

# GPA descriptives
mean(data$grades)
sd(data$grades)

# boys
mean(data$grades[data$kjoenn == 1])
sd(data$grades[data$kjoenn == 1])

# girls
mean(data$grades[data$kjoenn == -1])
sd(data$grades[data$kjoenn == -1])

# how many siblings in our data
length(unique(data$lopenr_mor))
sum(duplicated(data$lopenr_mor))
  

        #### 3. CANDIDATE MODELS COR MATRIX ####

# school-level variables
candidate_env_cor <- data[,names(data) %in% school_vars]

# in order to have nice plotting names :)
plot_names <- data.frame(var_name = c(school_vars),
                         plot_name =  c("Proportion of female teachers",
                                        "Teacher turnover",
                                        "Teacher educational attainment",
                                        "Classroom-level proportion of girls",
                                        "Classroom-level GPA",
                                        "School-level SES",
                                        "School-level proportion of girls",
                                        "Externalizing behavior",
                                        "Positive school climate"))

# changing names
new_names <- setNames(plot_names$plot_name, plot_names$var_name)
candidate_env_cor <- candidate_env_cor %>%
  rename_with(~ new_names[.], all_of(plot_names$var_name))

# correlation
candidate_env_cor <- cor(candidate_env_cor, use = "pairwise")

# changing column names to numbers
colnames(candidate_env_cor) <- 1:ncol(candidate_env_cor)


tiff("plots/candidate_env_cor.tiff", 
     width = 90, 
     height = 90,  
     units = "mm", 
     res = 600,
     compression = "lzw")

corrplot(candidate_env_cor,
         method = "color",
         addCoef.col = "black",
         tl.pos = "lt",
         tl.srt = 0,
         tl.col = "black",
         number.cex = 0.4,
         tl.offset = .52,
         cl.pos = "n",
         tl.cex = .45,      
         family = "serif") 

dev.off()



