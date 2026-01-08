source("00_settings.R")

        #### 1. MERGING INDIVIDUAL- AND SCHOOL-LEVEL DATASETS ####

data <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/individ_data_cleaned.csv", data.table = F)
  names(data)[3] <- "year"

# pure middle school
pure_mid <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/91_pure_middle_schools.csv", data.table = F)
  data <- merge(data, pure_mid, by = c("lnr_org","year"), all.x = T)

# teacher gender
teacher_gender <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/92_teacher_gender.csv", data.table = F)
  data <- merge(data, teacher_gender, by = c("lnr_org","year"), all.x = T)

# teacher turnover
teacher_turnover <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/93_teacher_turnover.csv", data.table = F)
  data <- merge(data, teacher_turnover, by = "lnr_org", all.x = T)

# teacher educational attainment
teacher_edu <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/94_teacher_edu.csv", data.table = F)
  data <- merge(data, teacher_edu, by = c("lnr_org","year"), all.x = T)

# classroom gender and grades
classrooms <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/95_classrooms_stats.csv", data.table = F)
  data <- merge(data, classrooms, by ="w19_0634_lnr", all.x = T)

# school SES
school_ses <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/96_school_SES.csv", data.table = F)
  data <- merge(data, school_ses, by = "lnr_org", all.x = T)

# school-level student gender
school_gender <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/97_school_gender.csv", data.table = F)
  data <- merge(data, school_gender, by = c("lnr_org","year","kjoenn"), all.x = T)

#ext. behavior and positive school climate
u_data1 <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/98_ungdata.csv", data.table = F)

# joining factor scores to school/years for a given year a student was in school. 
# iteratively join to connect students with the survey that was administered closest to their graduation date.

# creating 3DFs with three different years to connect to.
u_data2 <- u_data1
u_data3 <- u_data1

# u_data1 contains the current year
names(u_data1)[2:4] <- paste0(names(u_data1)[2:4],"_1")

# u_data2 contains year that is 1 year prior
  u_data2$year <- u_data2$year - 1
  names(u_data2)[2:4] <- paste0(names(u_data2)[2:4],"_2")

# u_data3 contains year that is 2 year prior
  u_data3$year <- u_data3$year - 2
  names(u_data3)[2:4] <- paste0(names(u_data3)[2:4],"_3")

data <- merge(data, u_data1, by.x = c("lnr_org","year"), by.y = c("lnr_org","year_1"), all.x = T)
data <- merge(data, u_data2, by.x = c("lnr_org","year"), by.y = c("lnr_org","year_2"), all.x = T)
data <- merge(data, u_data3, by.x = c("lnr_org","year"), by.y = c("lnr_org","year_3"), all.x = T)

data$pos_climate <- NA
data$ext_behavior <- NA

# first connect the student's school to the factor scores for their last year
# if their school did not participate, merge with the year they were in 9th grade
# if the school did not participate that year , merge with the year they were in 8th grade
data <- data %>%
  rowwise() %>%
  mutate(
    pos_climate = coalesce(pos_climate_1, pos_climate_2, pos_climate_3),
    ext_behavior = coalesce(ext_behavior_1, ext_behavior_2, ext_behavior_3)
  ) %>%
  ungroup()

# removing the temporary columns
data <- subset(data, select = -c(pos_climate_1, pos_climate_2, pos_climate_3,
                                 ext_behavior_1, ext_behavior_2, ext_behavior_3))

          #### 2. SCALING/MEAN-CENTERING VARIABLES ####

# function that group-mean centers. If year = T, group-mean center on school/year simultaneously.
group_mean_center <- function(column_name, year = FALSE) {
  if (year) {
    data[[paste0(column_name, "_gy")]] <- with(data, {
      ave(data[[column_name]], school_year, FUN = function(x) {
        group_mean <- mean(x, na.rm = TRUE) 
        ifelse(is.na(x), NA, x - group_mean)
      })
    })
  } else {
    data[[paste0(column_name, "_g")]] <- with(data, {
      ave(data[[column_name]], lnr_org, FUN = function(x) {
        group_mean <- mean(x, na.rm = TRUE) 
        ifelse(is.na(x), NA, x - group_mean)
      })
    })
  }
  return(data)
}


# function that grand-mean centers. If year = T, we grand-mean center on year
grand_mean_center <- function(column_name, year = FALSE) {
  if (year) {
    data[[paste0(column_name, "_m")]] <- with(data, {
      ave(data[[column_name]], year, FUN = function(x) {
        grand_mean <- mean(x, na.rm = TRUE) 
        ifelse(is.na(x), NA, x - grand_mean)
      })
    })
  } else {
    grand_mean <- mean(data[[column_name]], na.rm = TRUE)
    new_column_name <- paste0(column_name, "_m")
    data[[new_column_name]] <- ifelse(
      is.na(data[[column_name]]), 
      NA, 
      data[[column_name]] - grand_mean
    )
  }
  return(data)
}

# effect coding gender before group-mean centering for easier interpretation
data$kjoenn[data$kjoenn == 0] <- -1

# GPA for additional analyses
data <- group_mean_center("grades_std", year = F)
data <- group_mean_center("grades_std", year = T)

# need both group-mean centering by school and school/year for gender
data <- group_mean_center("kjoenn", year = F)
data <- group_mean_center("kjoenn", year = T)

# group-mean center within school/year for classroom effects
data <- group_mean_center("classroom_gender", year = T)
data <- group_mean_center("class_grades", year = T)

# grand-mean centering on year/not year depending on the operationalization of the variable
data <- grand_mean_center("school_teacher_gender", year = T) 
data <- grand_mean_center("school_turnover", year = F) 
data <- grand_mean_center("school_teacher_edu", year = T) 
data <- grand_mean_center("school_income", year = T) 
data <- grand_mean_center("school_student_gender", year = T) 
data <- grand_mean_center("pos_climate", year = T) 
data <- grand_mean_center("ext_behavior", year = T) 

# group-mean center gender on classrooms for the classroom models
data <- data %>%
  group_by(class) %>%
  mutate(kjoenn_cg = kjoenn - mean(kjoenn, na.rm = TRUE)) %>%
  ungroup()

          ##### 2.1 Scaling based on full population #####

full_data <- data[!is.na(data$grades_std),]
full_data <- full_data[!full_data$lnr_org == "",]
full_data <- full_data[full_data$year >= 2018,]

full_data$school_turnover_m <- scale(full_data$school_turnover_m)
full_data$school_teacher_gender_m <- scale(full_data$school_teacher_gender_m)
full_data$school_teacher_edu_m <- scale(full_data$school_teacher_edu_m)
full_data$school_income_m <- scale(full_data$school_income_m)
full_data$school_student_gender_m <- scale(full_data$school_student_gender_m)

full_data$classroom_gender_gy <- scale(full_data$classroom_gender_gy)
full_data$class_grades_gy <- scale(full_data$class_grades_gy)


# column denoting whether there are more than one classroom per school/year
full_data <- full_data %>%
  group_by(school_year) %>%
  mutate(one_class = ifelse(n_distinct(class) > 1, 1, 0)) %>%
  ungroup()


        #### 3. CREATING FINAL DF ####

data <- full_data[!is.na(full_data$noncog_parental),]

# centering and scaling individual-level variables, both on year and on year/school 
data <- group_mean_center("noncog", year = T)
data <- group_mean_center("noncog", year = F)
  data$noncog_g <- scale(data$noncog_g)
  data$noncog_gy <- scale(data$noncog_gy)

data <- group_mean_center("cog", year = T)
data <- group_mean_center("cog", year = F)
  data$cog_g <- scale(data$cog_g)
  data$cog_gy <- scale(data$cog_gy)

data <- group_mean_center("noncog_parental", year = T)
data <- group_mean_center("noncog_parental", year = F)
  data$noncog_parental_g <- scale(data$noncog_parental_g)
  data$noncog_parental_gy <- scale(data$noncog_parental_gy)

data <- group_mean_center("cog_parental", year = T)
data <- group_mean_center("cog_parental", year = F)
  data$cog_parental_g <- scale(data$cog_parental_g)
  data$cog_parental_gy <- scale(data$cog_parental_gy)
  
data$grades_std_g <- scale(data$grades_std_g)
data$grades_std_gy <- scale(data$grades_std_gy)

# non-mean centered individual variables
data$grades_std <- scale(data$grades_std)
  data$cog <- scale(data$cog)
  data$noncog <- scale(data$noncog)
  data$cog_parental <- scale(data$cog_parental)
  data$noncog_parental <- scale(data$noncog_parental)

# school-level variables
data$school_turnover_m <- scale(data$school_turnover_m)
data$school_teacher_gender_m <- scale(data$school_teacher_gender_m)
data$school_teacher_edu_m <- scale(data$school_teacher_edu_m)
data$school_income_m <- scale(data$school_income_m)
data$school_student_gender_m <- scale(data$school_student_gender_m)
data$classroom_gender_gy <- scale(data$classroom_gender_gy)
data$class_grades_gy <- scale(data$class_grades_gy)
data$pos_climate_m <- scale(data$pos_climate_m)
data$ext_behavior_m <- scale(data$ext_behavior_m)

# effect coding school_middle so that we can have an intercept of 0
data$school_middle[data$school_middle == 0] <- -1

# despite having maternal genotype data, there are five missing lopenr_mor values. creating random numbers for these
data <- data %>%
  mutate(
    # for rows with missing lopenr_mor, create a grouping based on noncog_parental, indicating that they share the same mother if someone has the same
    temp_group = ifelse(is.na(lopenr_mor), 
                        paste0("missing_", as.character(noncog_parental)), 
                        NA),
    # assign unique IDs to each unique temp_group
    lopenr_mor = ifelse(is.na(lopenr_mor),
                        as.character(as.integer(factor(temp_group, levels = unique(temp_group)))),
                        lopenr_mor)) %>%
  select(-temp_group)


#write.csv(data, "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv", row.names = F)