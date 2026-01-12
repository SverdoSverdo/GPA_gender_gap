source("00_settings.R")

        #### 0. PREPARING DATA ####

data <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/individ_data_cleaned.csv", data.table = F)

# these are the years we are interested in
years <- c(2018:2024)

# these are the cohorts we are interested in
data <- data[data$avgdato %in% years,]

# school IDs that are part of our sample
schools <- unique(data$lnr_org)
# removing NA schools
  schools <- schools[!schools == ""]
  
# load in retrospectively to count the number of datapoints used per model
final_sample <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv",data.table = F)
final_schools <- unique(final_sample$lnr_org)

  
        #### 1 SCHOOL TYPE ####    
  
# this section identifies whether a school is a combined elementary/lower secondary school
  
# setting up loop to read through all GSI datasets
directory <- "N:/durable/data/registers/SSB/01_data/data_v6.0/EDUCATION_SCHOOLLEVEL/csv"
file_pattern <- "EDUCATION_GSI_%d.csv"

# create a list of all directory/dataset combinations
dataset_list <- list()

# add datasets from the original loop 
for (year in 2018:2024) {
  file_path <- sprintf(file_pattern, year)
  full_path <- file.path(directory, file_path)
  dataset_list[[as.character(year)]] <- full_path
}

# initialize an empty data frame to store results 
all_years_GSI <- data.frame()

## the loop runs through the school-level, GSI datasets for each year and obtains the number of 1-7 and 8-10 grade students per school
for (dataset_name in names(dataset_list)) {
  year <- as.numeric(dataset_name)
  full_path <- dataset_list[[dataset_name]]
  
  # read the data
  GSI <- fread(full_path, data.table = F)
  
  # filter for  schools
  GSI <- GSI[GSI$lopenr_orgnr %in% schools, ]
  GSI <- GSI[c("lopenr_orgnr", "d31", "d47")] #d31 = number of 1-7th grade students, d47 = number of 8-10th grade students
  
  # calculate the ratios
  GSI$total <- GSI$d31 + GSI$d47
  GSI$middle <- GSI$d47 / GSI$total
  
  # if a school has elementary school students, they receive a 0
  GSI$middle[GSI$middle != 1] <- 0
  
  # add a column for the year
  GSI$year <- year
  
  # rename columns
  GSI <- GSI[c("lopenr_orgnr", "middle", "year")]
  names(GSI) <- c("lnr_org", "school_middle", "year")
  
  # append the data for this year to the overall data frame
  all_years_GSI <- rbind(all_years_GSI, GSI)
}


# if schools have missing values for some years we use the year(s) prior to this to estabish if its a combined school or not
all_years_complete <- all_years_GSI %>%
  complete(lnr_org, year = years, fill = list(school_middle = NA))

# fill missing school_middle values using past values first, and then future values if there are still some NAs
all_years_complete <- all_years_complete %>%
  arrange(lnr_org, year) %>%
  group_by(lnr_org) %>%
  fill(school_middle, .direction = "down") %>%  # fill from past years
  fill(school_middle, .direction = "up") # fill from future year


#write.csv(all_years_complete, "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/91_pure_middle_schools.csv", row.names = F)


        #### 2. PROPORTION OF FEMALE TEACHERS ####

# initialize an empty list to store data frames
list_of_dfs <- list()

# single loop through the school-level, GSI datasets for each year
# teachers are linked to school IDs through either lopenr_orgnr or lopenr_orgnrbed.
for (dataset_name in names(dataset_list)) {
  year <- as.numeric(dataset_name)
  full_path <- dataset_list[[dataset_name]]
  
  # read the dataset
  df <- fread(full_path, data.table = F)
  
  # filter for lower secondary schools
  df <- df %>% filter(lopenr_orgnr %in% schools)
  
  # calculate ratio for lopenr_orgnr
  df_orgnr <- df %>%
    mutate(
      school_teacher_gender = d256 / (d255+d256),  # d256 = female teachers, d255 = male teachers
      year = year,
      lnr_org = lopenr_orgnr
    ) %>%
    select(lnr_org, school_teacher_gender, year)
  
  # filter for lower secondary schools (lopenr_orgnrbed)
  df_orgnrbed <- df %>% 
    filter(lopenr_orgnrbed %in% schools) %>%
    mutate(
      school_teacher_gender = d256 / (d255+d256),
      year = year,
      lnr_org = lopenr_orgnrbed
    ) %>%
    select(lnr_org, school_teacher_gender, year)
  
  # combine both versions for this year
  list_of_dfs[[dataset_name]] <- bind_rows(df_orgnr, df_orgnrbed)
}

# combine all years
teacher_gender <- bind_rows(list_of_dfs, .id = "year") %>%
  distinct(year, lnr_org, .keep_all = TRUE)


#write.csv(teacher_gender, "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/92_teacher_gender.csv", row.names = F)

        #### 3. TEACHER TURNOVER ####

# codes for primary school teachers, obtained from SSB
teachers <- c(85.201,85.202,85.203)

# initialize an empty list to store data frames
filtered_data_list <- list()


## the loop runs through all the datasets to connect teacher IDs to school IDs.
for (year in 2018:2024) {
  # construct the file path
  file_path <- paste0("N:/durable/data/registers/SSB/01_data/data_v6.0/EMPLOYMENT/csv/EMPLOYMENT_REGSYS_", year, ".csv")
  
  # read the dataset
  data <- fread(file_path, data.table = F)
  
  # filter the data to limit to main position, relevant schools, and teachers, then group by teacher ID
  filtered_data <- data %>%
    filter(
      (w19_0634_lopenr_virksomhet %in% schools | w19_0634_lopenr_foretak %in% schools) &  # foretak and virksomhet both refer to employer organization
        virk_nace1_sn07 %in% teachers &
        arb_hovedarbeid == 1
    ) %>%
    group_by(w19_0634_lnr) %>%
    slice_max(arb_stillingspst, n = 1, with_ties = FALSE) %>% # get the primary employer of the teacher
    ungroup() %>%
    mutate(
      # if virksomhet is NA, use foretak
      school_id = coalesce(w19_0634_lopenr_virksomhet, w19_0634_lopenr_foretak)
    ) %>%
    select(w19_0634_lnr, school_id) 
  
  # rename column for the current year
  colnames(filtered_data)[2] <- paste0("school_id_", year)
  
  # append the filtered data to the list
  filtered_data_list[[as.character(year)]] <- filtered_data
}

# combine the filtered data into a single dataframe
combined_data <- Reduce(function(x, y) full_join(x, y, by = "w19_0634_lnr"), filtered_data_list)

# this dataset contains teacher IDs in the first column and the rest of the cells are filled with their school IDs for each year
names(combined_data) <- c("w19_0634_lnr", "year_2018","year_2019","year_2020",
                          "year_2021","year_2022","year_2023","year_2024")

# reshaping combined_data into long format
teacher_df <- combined_data %>%
  pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "lnr_org")

# changing year values to numeric
teacher_df$year <- gsub("year_", "", teacher_df$year)
  teacher_df$year <- as.numeric(teacher_df$year)

# identifying teachers who left their job at the end of each year
teacher_df <- teacher_df %>%
  arrange(w19_0634_lnr, year) %>%
  group_by(w19_0634_lnr) %>%
  mutate(
    next_org = lead(lnr_org),  # get next year's school ID
    left_job = ifelse(
      !is.na(lnr_org) & (is.na(next_org) | lnr_org != next_org),  # left if current year has org AND next year is NA or different school ID
      1,
      0
    )
  ) %>%
  ungroup() %>%
  select(-next_org)

# summarise the number of teachers who left per school per year
teachers_leaving <- teacher_df %>%
  group_by(lnr_org, year) %>%
  summarise(teachers_left = sum(left_job), .groups = 'drop')

# create a complete set of combinations for school ID and year
years <- 2018:2024
  turnover_schools <- unique(teacher_df$lnr_org[!is.na(teacher_df$lnr_org)])
  complete_combinations <- expand.grid(lnr_org = turnover_schools, year = years)

# merges the teachers_leaving dataframe with the complete set to include 0 values where no teachers left
teachers_leaving <- complete_combinations %>%
  left_join(teachers_leaving, by = c("lnr_org", "year")) %>%
  replace_na(list(teachers_left = 0))

# count the number of teachers working in each school per year
teachers_working <- teacher_df %>%
  group_by(lnr_org, year) %>%
  summarise(teachers_working = n(), .groups = 'drop')

# calculate yearly turnover rates
turnover_rates <- teachers_working %>%
  left_join(teachers_leaving, by = c("lnr_org", "year")) %>%
  mutate(turnover_rate = teachers_left / teachers_working)

# sort and order the data
turnover_rates <- turnover_rates %>%
  arrange(lnr_org, year)

# there are year/schools with 100% turnover. This means that the school closed or changed organization ID.
# create the `present_next_year` column to see if in a given year no one worked there, changing these to NA
turnover_rates <- turnover_rates %>%
  group_by(lnr_org) %>%
  mutate(
    next_teachers_working = lead(teachers_working),
    drop_rate = (teachers_working - next_teachers_working) / teachers_working,
    present_next_year = ifelse(year == 2024 | is.na(next_teachers_working) | drop_rate >= 1, FALSE, TRUE)
  ) %>%
  ungroup() %>%
  select(-next_teachers_working, -drop_rate)  # Remove the temporary columns

# Remove rows where present_next_year is FALSE and rows for 2024
turnover_rates <- turnover_rates %>%
  filter(present_next_year == TRUE & year != 2024)

# N data points
turnover_IDs <- teacher_df %>%
  semi_join(turnover_rates, by = c("lnr_org", "year"))
    turnover_IDs <- semi_join(turnover_IDs, final_sample, by = c("lnr_org", "year"))
    length(unique(turnover_IDs$w19_0634_lnr))

# Calculate the mean turnover rate for each school
mean_turnover_rates <- turnover_rates %>%
  group_by(lnr_org) %>%
  summarise(school_turnover = mean(turnover_rate, na.rm = TRUE), .groups = 'drop')

#write.csv(mean_turnover_rates, file = "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/93_teacher_turnover.csv", row.names = F)


        #### 4. TEACHER EDUCATIONAL ATTAINMENT ####

# initialize an empty list to store the data
all_teacher_school_links <- list()

# loop through years 2018 to 2024 to link teachers with school IDs
for (year in 2018:2024) {
  
  # read the dataset
  file_path <- paste0("N:/durable/data/registers/SSB/01_data/data_v6.0/EMPLOYMENT/csv/EMPLOYMENT_REGSYS_", year, ".csv")
    data <- fread(file_path, data.table = F)
  
  # filter for virksomhet (school ID)
  teacher_school_virk <- data %>%
    filter(
      w19_0634_lopenr_virksomhet %in% schools & # filter by teachers and schools
        virk_nace1_sn07 %in% teachers 
    ) %>%
    mutate(
      year = year,
      lnr_org = w19_0634_lopenr_virksomhet
    ) %>%
    select(w19_0634_lnr, lnr_org, year)
  
  # filter for foretak (also a school ID indicator), excluding those already in virksomhet
  teacher_school_foretak <- data %>%
    filter(
      w19_0634_lopenr_foretak %in% schools & # filter by teachers and schools
        !(w19_0634_lopenr_foretak %in% teacher_school_virk$lnr_org) &
        virk_nace1_sn07 %in% teachers
    ) %>%
    mutate(
      year = year,
      lnr_org = w19_0634_lopenr_foretak
    ) %>%
    select(w19_0634_lnr, lnr_org, year)
  
  # combine both for this year
  all_teacher_school_links[[as.character(year)]] <- bind_rows(
    teacher_school_virk,
    teacher_school_foretak
  )
}

# combine all years
teacher_edu <- bind_rows(all_teacher_school_links)

          ##### 4.1 linking with educational data #####

# IDs of teachers
teachers_in_data <- unique(teacher_edu$w19_0634_lnr)

# loading in dataset containing highest educational attainment
edu <- fread("N:/durable/data/registers/SSB/01_data/data_v6.0/CORE/csv/EDUCATION_BU_UTD_IGANG_reduced.csv", data.table = F)
  edu <- edu[,c(1,41:47)] # limit to the years 2018 - 2024

# limiting DF to teachers
edu <- edu[edu$w19_0634_lnr %in% teachers_in_data,]

names(edu) <- c("w19_0634_lnr","year_2018","year_2019","year_2020","year_2021","year_2022","year_2023","year_2024")

# teacher-year-education in long format
edu_long <- edu %>%
  pivot_longer(-w19_0634_lnr, names_to = "year", values_to = "education_level") %>%
  mutate(year = as.numeric(gsub("year_", "", year)))

# merge teacher-school-year data with education data
merged_data <- teacher_edu %>%
  left_join(edu_long, by = c("w19_0634_lnr", "year"))

# Convert education_level to numeric
merged_data$education_level <- as.numeric(merged_data$education_level)

# calculate average teacher education per school per year
school_teacher_edu <- merged_data %>%
  group_by(lnr_org, year) %>%
  summarize(
    school_teacher_edu = mean(education_level, na.rm = TRUE),
    .groups = 'drop'
  )

# Check how many unique teachers we have in schools from final_sample
unique_teachers <- merged_data %>%
  semi_join(final_sample, by = c("lnr_org", "year")) %>%
  pull(w19_0634_lnr) %>%
  unique()
    length(unique_teachers)

#write.csv(school_teacher_edu, "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/94_teacher_edu.csv", row.names = F)


          #### 5. CLASSROOM ID ####

# reading in dataset containing classroom IDs
NT <- fread("N:/durable/data/registers/SSB/01_data/data_v6.0/EDUCATION_VGS_GRS/csv/EDUCATION_NASJONALE_PROVER.csv", data.table = F)

# they take national tests 1.5 years before they graduate, so include students 2 year prior to our sample
NT <- NT[NT$aargang > 201410,]

# doesnt matter which subject ID we use. Using reading scores from ninth grade
NT <- NT[NT$provekode == "NPLES09",]
NT <- NT[!is.na(NT$w19_0634_lnr),]

#167 duplicated IDs
sum(duplicated(NT$w19_0634_lnr))

# if there are duplicated IDs, keep the IDs with non-NA values for the test
duplicated_ids <- NT %>%
  group_by(w19_0634_lnr) %>%
  filter(n() > 1) %>%  
  ungroup()

cleaned_duplicates <- duplicated_ids %>%
  group_by(w19_0634_lnr) %>%
  filter(!is.na(poeng)) %>%
  dplyr::slice(1) %>%  
  ungroup()

non_duplicated <- NT %>%
  filter(!w19_0634_lnr %in% duplicated_ids$w19_0634_lnr)

NT <- bind_rows(cleaned_duplicates, non_duplicated)

# zero duplicated IDs now
sum(duplicated(NT$w19_0634_lnr))

#r emoving unnecessary columns and changing col. names
NT <- NT[c("w19_0634_lnr","w19_0634_lopenr_orgnr", "aargang","lopenr_gruppenr")]
names(NT)[2:4] <- c("lnr_org","year","class")

# checking that they all take the test in the same month
table(NT$year) #yes, they all take it in October

# removing test month and adding 2 since they take the tests 1.5 years before they graduate.
# each year now matches the year they graduated
NT$year <- NT$year %/% 100
 NT$year <- NT$year + 2 
 
# making a variable that combines year and school ID
NT$school_year <- paste0(NT$lnr_org,"_",NT$year)


          ##### 5.1 Finding classrooms #####
classrooms <- NT

# removing rows with NA school IDs
classrooms <- classrooms[!classrooms$lnr_org == "",]

# getting the student count per classroom
classrooms <- classrooms %>% 
  group_by(school_year, class) %>%
  summarise(
    student_count = n(),
    school_year = first(school_year), 
    .groups = "drop"
  )

# changing to wide format in order to get number of students in each classroom for each year
classrooms_wide <- classrooms %>%
  group_by(school_year) %>%
  summarise(class_counts = list(student_count)) %>%
  ungroup() %>%
  mutate(class_counts = map(class_counts, ~ {
    set_names(.x, paste0("class_", seq_along(.x)))
  })) %>%
  unnest_wider(class_counts, names_repair = "minimal")

# set all values that are not 10 or higher to NA. These are unreasonable small classes
classrooms_wide <- classrooms_wide %>%
  mutate(across(starts_with("class_"), ~ ifelse(.x < 10, NA, .x)))

# calculating the median number of students in each classroom per school/year combo
classrooms_wide <- classrooms_wide %>%
  rowwise() %>%
  mutate(
    median_students = median(c_across(starts_with("class_")), na.rm = TRUE),
    n_classes = sum(!is.na(c_across(starts_with("class_"))))
  ) %>%
  ungroup()


          ##### 5.2 merging and removing classrooms not included in the analysis #####

median_class <- classrooms_wide[c("school_year","median_students","n_classes")] 

classrooms <- merge(classrooms, median_class, by = "school_year")

# removing school/year combos with only one classroom
classrooms <- classrooms[classrooms$n_classes >1,]

# making cutoff columns. 20% below the median classroom size
classrooms$cutoff <- classrooms$median_students - (classrooms$median_students*0.2)

#674 classrooms with a student mass under the cutoff value 
fake_classrooms <- classrooms %>%
  filter(cutoff > student_count)

#Removing these classrooms. 
classrooms <- classrooms[!classrooms$class %in% fake_classrooms$class,]


# retrieving the IDs from the original dataset to merge classrooms and with ID
NT_IDs <- subset(NT, select = c(w19_0634_lnr,class))

classrooms <- merge(classrooms, NT_IDs, by = "class")


# loading in 10th grade dataset. Removing students who switched schools between 9th and 10th grade
individ_data <- fread("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/individ_data_cleaned.csv", data.table = F)
  grad_date <- subset(individ_data, select = c(w19_0634_lnr,lnr_org,avgdato))
  grad_date <- grad_date[grad_date$avgdato >2016,] #only those graduating from 2016 on wards
  names(grad_date)[2] <- "grad_lnr_org" #school ID from the school they graduated from
  
NT_school <- subset(NT, select = c(w19_0634_lnr,lnr_org,year) )

NT_grad_diff <- merge(grad_date,NT_school, by = "w19_0634_lnr")

NT_grad_diff <- NT_grad_diff %>%
  mutate(same_school = grad_lnr_org == lnr_org)

table(NT_grad_diff$same_school) # 4% switched schools

# removing those who switched schools
school_switchers <- NT_grad_diff$w19_0634_lnr[NT_grad_diff$same_school == F]
  classrooms <- classrooms[!classrooms$w19_0634_lnr %in% school_switchers,]
  
# adjusting the total class room sizes based on the fact that some switched schools, for calculating the classroom-level variables correctly.
classrooms <- classrooms %>%
  group_by(class, school_year) %>%  
  mutate(student_count = n()) %>%
  ungroup()

          ##### 5.3 classroom proportion of girls #####

# loading in gender dataset
gender <- fread("N:/durable/data/registers/SSB/01_data/data_v6.0/CORE/csv/POPULATION_FASTE_OPPLYSNINGER_reduced.csv", data.table = F)
  gender <- gender[c("w19_0634_lnr","kjoenn")]
  
classrooms <- merge(classrooms, gender, by ="w19_0634_lnr", all.x = T)

# recoding girls to 0
classrooms$kjoenn[classrooms$kjoenn == 2] <- 0

# calculating the counts of boys and girls for each class and each year
gender_counts <- classrooms %>%
  group_by(class, school_year) %>% 
  summarise(class_count_boys = sum(kjoenn), .groups = 'drop')

# calculating the total counts for each group and each year
total_counts <- classrooms %>%
  group_by(class, school_year) %>%
  summarise(class_count_total = n(), .groups = 'drop')

# merging total counts with gender counts
gender_and_total_counts <- gender_counts %>%
  left_join(total_counts, by = c("class", "school_year"))

# creating separate rows for boys and girls
gender_and_total_counts <- gender_and_total_counts %>%
  expand_grid(kjoenn = c(0, 1)) %>%
  arrange(class, school_year, kjoenn)

# number of students in the class, subtracting the focal person
gender_and_total_counts$class_count_total_adj <- gender_and_total_counts$class_count_total -1

# number of boys in the classroom, subtracting the focal person
gender_and_total_counts <- gender_and_total_counts %>%
  mutate(class_count_boys_adj = ifelse(kjoenn == 1, class_count_boys - 1, class_count_boys))

# calculating proportion of boys, excluding the focal person
gender_and_total_counts <- gender_and_total_counts %>%
  mutate(classroom_gender = class_count_boys_adj / class_count_total_adj)

# merge to get IDs back
gender_and_total_counts <- merge(gender_and_total_counts, classrooms, by = c("school_year","class","kjoenn"))

# df containing columns needed to link back and proportion of boys, when dubtracting the focal person
gender_proportion <- gender_and_total_counts[c("w19_0634_lnr","school_year","class","classroom_gender")]

# reverse-coding so the variable represents proportion of girls, not boys
gender_proportion$classroom_gender <- 1-gender_proportion$classroom_gender


          ##### 5.4 classroom grades #####

# extracting the standardized grades
grades <- individ_data[c("w19_0634_lnr","grades_std")]

# merging grades with classroom data
classrooms <- merge(classrooms, grades, by = "w19_0634_lnr", all.x = T )

# N datapoints for both classroom variables
unique_classrooms <- classrooms[classrooms$school_year %in% final_sample$school_year,]
unique_classrooms <- unique_classrooms %>%
  mutate(
    year_number = str_extract(school_year, "(?<=_)\\d+") %>% as.numeric()
  )

#f or classroom-level gender
unique_classroom_gender <- unique_classrooms[!is.na(unique_classrooms$kjoenn),]
  nrow(unique_classrooms)

# for classroom-level GPA
unique_classroom_grades <- unique_classrooms[!is.na(unique_classrooms$grades_std),]
  nrow(unique_classroom_grades)

# calculating the average grade in a classroom, deducting the grades of the focal person
classrooms <- classrooms %>%
  group_by(class) %>%
  mutate(class_grades = ifelse(!is.na(grades_std),
                                      (sum(grades_std, na.rm = TRUE) - grades_std) / (sum(!is.na(grades_std)) - 1),
                                      NA)) %>%
  ungroup()


classroom_grades <- subset(classrooms, select = c(w19_0634_lnr,class_grades))

# merging classroom gender and GPA datasets
classroom_stats <- merge(gender_proportion,classroom_grades, by = "w19_0634_lnr") 
  classroom_stats <- subset(classroom_stats, select = c(w19_0634_lnr,class,classroom_gender,class_grades))


#write.csv(classroom_stats, "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/95_classrooms_stats.csv", row.names = F)

        #### 6. SCHOOL SES ####

# student data needed to create school SES variable
SES_data <- individ_data[c("w19_0634_lnr","avgdato","lnr_org","lopenr_mor","lopenr_far")]
  SES_data <- SES_data[!SES_data$avgdato < 2017,] # limiting to 2018 and beyond

# loading in income data
income <- fread("N:/durable/data/registers/SSB/01_data/data_v6.0/INCOME/csv/INCOME_INNTEKT_2011_2023.csv",
                select = c("w19_0634_lnr","aargang","ies"), data.table = F)
  names(income)[2] <- "year"

# making a list of parental IDs of children in schools
parental_IDs <- c(unique(SES_data$lopenr_mor), unique(SES_data$lopenr_far))

income <- income[income$w19_0634_lnr %in% parental_IDs,]


# want to extract mother & father income three years prior to AVGDATO. including these years
years <- c(2015:2023)
income <- income[income$year %in% years,]

# 0 NA values
sum(is.na(income$ies))
# 11099 zero values
sum(income$ies == 0)

# bottom 2% of incomes is set to NA (ref SSB)
cutoff <- quantile(income$ies, 0.02, na.rm = TRUE)
income <- income[!(income$ies <= cutoff), ]

          ##### 6.1 mom's income #####

# kid and mom IDs
kid_mor_id <- SES_data[c("w19_0634_lnr","lopenr_mor")]
  kid_mor_id <- kid_mor_id[!duplicated(kid_mor_id),]

inc_mor <- income[income$w19_0634_lnr %in% SES_data$lopenr_mor,]
  names(inc_mor)[1] <- "lopenr_mor"

# changing to wide format
inc_mor <- inc_mor %>%
  pivot_wider(
    names_from = year,
    values_from = ies
  )

# changing NA introduced by reshaping to 0
inc_mor[is.na(inc_mor)] <- 0

inc_mor.temp <- SES_data[c("w19_0634_lnr","lopenr_mor","avgdato")]

# merging this to get childs graduation date, mother ID and mother income each year in the same DF
inc_mor <- merge(inc_mor.temp, inc_mor, by ="lopenr_mor", all.x = T)
  inc_mor <- inc_mor[!inc_mor$lopenr_mor == "",] 

# removing those students whose graduation date we dont know
inc_mor <- inc_mor[!is.na(inc_mor$avgdato),]

# changing order of columns
year_cols <- as.character(2015:2023) # Creates a vector with years as character strings
  other_cols <- setdiff(colnames(inc_mor), c("lopenr_mor", "w19_0634_lnr", "avgdato", year_cols))
  ordered_cols <- c("lopenr_mor", "w19_0634_lnr", "avgdato", year_cols, other_cols)
  inc_mor <- inc_mor[, ordered_cols]


# preparing parallell processing

no_cores <- detectCores() - 3
  cl <- makeCluster(no_cores)
  clusterExport(cl, varlist = c("inc_mor", "year_cols"))

# define the function to calculate the 3-year average salary for a row
calc_3yr_avg <- function(i) {
  grad_year <- as.integer(inc_mor$avgdato[i])
  
  # identify the columns for the 3 years before graduation
  years_to_avg <- as.character((grad_year-3):(grad_year-1))
  
  # ensure the years are in the dataframe
  years_to_avg <- years_to_avg[years_to_avg %in% year_cols]
  
  # calculate the mean for the available years
  mean(as.numeric(inc_mor[i, years_to_avg]), na.rm = TRUE)
}

# apply the function in parallel to each row index of the dataframe
results <- parLapply(cl, seq_len(nrow(inc_mor)), calc_3yr_avg)

stopCluster(cl)

# combine the results back into the original dataframe
inc_mor$mor_3_yr_avg <- unlist(results)
  inc_mor <- inc_mor[c("w19_0634_lnr","mor_3_yr_avg")]

rm(results)

          ##### 6.2 dad's income #####

# kid and dad ID
kid_far_id <- SES_data[c("w19_0634_lnr","lopenr_far")]
  kid_far_id <- kid_far_id[!duplicated(kid_far_id),]

inc_far <- income[income$w19_0634_lnr %in% SES_data$lopenr_far,]
  names(inc_far)[1] <- "lopenr_far"

# changing to wide format
inc_far <- inc_far %>%
  pivot_wider(
    names_from = year,
    values_from = ies
  )

# changing NA introduced by reshaping to 0
inc_far[is.na(inc_far)] <- 0
  inc_far.temp <- SES_data[c("w19_0634_lnr","lopenr_far","avgdato")]

# merging this to get childs graduation date, father ID and father income each year in the same DF
inc_far <- merge(inc_far.temp, inc_far, by ="lopenr_far", all.x = T)
inc_far <- inc_far[!inc_far$lopenr_far == "",] # removing NA father IDs

# removing those students whose graduation date we dont know
inc_far <- inc_far[!is.na(inc_far$avgdato),]

# changing order of columns
year_cols <- as.character(2015:2023) # Creates a vector with years as character strings
  other_cols <- setdiff(colnames(inc_far), c("lopenr_far", "w19_0634_lnr", "avgdato", year_cols))
  ordered_cols <- c("lopenr_far", "w19_0634_lnr", "avgdato", year_cols, other_cols)
  inc_far <- inc_far[, ordered_cols]

# preparing parallell processing
no_cores <- detectCores() - 3
  cl <- makeCluster(no_cores)
  clusterExport(cl, varlist = c("inc_far", "year_cols"))

# define the function to calculate the 3-year average salary for a row
calc_3yr_avg <- function(i) {
  grad_year <- as.integer(inc_far$avgdato[i])
  
  # identify the columns for the 3 years before graduation
  years_to_avg <- as.character((grad_year-3):(grad_year-1))
  
  # ensure the years are in the dataframe
  years_to_avg <- years_to_avg[years_to_avg %in% year_cols]
  
  # calculate the mean for the available years
  mean(as.numeric(inc_far[i, years_to_avg]), na.rm = TRUE)
}

# apply the function in parallel to each row index of the dataframe
results <- parLapply(cl, seq_len(nrow(inc_far)), calc_3yr_avg)

# stop the cluster
stopCluster(cl)

# combine the results back into the original dataframe
inc_far$far_3_yr_avg <- unlist(results)
  inc_far <- inc_far[c("w19_0634_lnr","far_3_yr_avg")]

rm(results)

          ##### 6.3 parental and school-level income #####

parental_income <- merge(inc_far, inc_mor, by = "w19_0634_lnr", all = T)

# NA income = 0 income (ref SSB)
parental_income$far_3_yr_avg[is.nan(parental_income$far_3_yr_avg)] <- 0
  parental_income$mor_3_yr_avg[is.nan(parental_income$mor_3_yr_avg)] <- 0

# merging mother and father income
parental_income$combined_income <- rowSums(parental_income[c(2:3)], na.rm = T)
  parental_income <- parental_income[c("w19_0634_lnr","combined_income")]
  parental_income<- parental_income[!is.na(parental_income$combined_income),]

# retrieving graduation year again
avgdato <- individ_data[c("w19_0634_lnr","avgdato")]

parental_income <- merge(parental_income, avgdato, by = "w19_0634_lnr")

# transform parental income into percentage ranks within each year
parental_income <- parental_income %>% 
  group_by(avgdato) %>% 
  mutate(quantiles = ntile(combined_income, 100)) %>%
  ungroup()

# merging back to student-level dataframe
SES_data <- merge(SES_data, parental_income, by = c("w19_0634_lnr","avgdato"), all.x = T)


#N data points
unique_SES <- SES_data[SES_data$avgdato %in% c(2018,2019,2020,2021,2022,2023,2024),]
  unique_moms <- unique_SES$lopenr_mor[unique_SES$lnr_org %in% final_sample$lnr_org]
  unique_dads <- unique_SES$lopenr_far[unique_SES$lnr_org %in% final_sample$lnr_org]
  length(c(unique_moms,unique_dads))

#write.csv(SES_data, "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/10_parental_income.csv", row.names = F)

# creating school-level income
school_ses <- SES_data %>%
  group_by(lnr_org) %>%
  summarize(school_income = mean(quantiles, na.rm = TRUE))

#write.csv(school_ses, "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/96_school_SES.csv", row.names = F)

        #### 7. SCHOOL-LEVEL PROPORTION OF GIRLS ####

# limiting to our sample years
school_gender <- individ_data[individ_data$avgdato %in% years,]

# Calculate total students and girls per school/year
summary_data <- school_gender %>%
  group_by(school_year) %>%
  summarise(
    total_students = n(),
    total_girls = sum(kjoenn == 0),
    .groups = 'drop'
  )

# Merge the summary data back to the original data and calculate proportion of girls excluding the focal individual
school_gender <- school_gender %>%
  left_join(summary_data, by = c("school_year")) %>%
  mutate(
    school_student_gender = if_else(
      total_students > 1, 
      (total_girls - (kjoenn == 0)) / (total_students - 1), 
      NA_real_
    )
  ) %>%
  select(lnr_org, avgdato, kjoenn, school_student_gender)

# N data points
unique_student_gender <- school_gender[school_gender$avgdato %in% c(2018,2019,2020,2021,2022,2023,2024),]
  unique_student_gender$school_year <- paste0(unique_student_gender$lnr_org,"_",unique_student_gender$avgdato)
  unique_student_gender <- school_gender[unique_student_gender$school_year %in% final_sample$school_year,]
  nrow(unique_student_gender)

# only need two values per school: one for boys and one for girls
school_gender <- distinct(school_gender)

names(school_gender)[2] <- "year"

#write.csv(school_gender, "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/97_school_gender.csv", row.names = F)




        #### 8. EXTERNALIZING BEHAVIOR & POSITIVE SCHOOL CLIMATE ####

# responses to ext. behavior and positive school climate are already aggregated to grade-levels within schools
u_data <- fread("N:/durable/data/questionnaires/Ungdata_NOVA/01_data/Ungdata_NOVA_2015-2024.csv")

# list of items and their description
variabel_list <- read_excel("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/Variabeloversikt_Ungdata_temp.xlsx")

# variables on ext. behavior and positive school climate
my_variables <- c("atfpro1", "atfpro12", "mobb1", "skolprob4", "mobb2", "atfpro15","atfpro18","atfpro16","atfpro25",
                       "atfpro30","atfpro31", "skole1","skole6","skole4","skole2","skole3", "atfpro11","atfpro7","atfpro5","skolprob1",
                       "skolprob5","mobb3","skolprob2","skulkgr2","skole8","skolprob3","trygg3","vold2","vold3","vold4")

# limiting the variable list to the variables in my_variables
variabel_list <- variabel_list[variabel_list$Variabelnavn %in% my_variables,]


          ##### 8.1 Making school-level means #####

# to make school-level averages, we need to compute the grand mean across all grades in that school.
# we therefore need to extract N and mean score for each grade within any given school/year combo and create weighted school-level means

# columns end in nu (number of respondents) or _mean (school-level mean for the item). Adding these to the variable list:
variables_n <- paste0(my_variables, "_nu")
variables_mu <- paste0(my_variables, "_mean")

# merging the two lists
variables_to_keep <- c(variables_n, variables_mu)

# we also need to extract school ID, year, and grade
columns_to_keep <- c("lnr_org", "aar","grade",variables_to_keep)

# extract these columns
u_data <- u_data %>% select(all_of(columns_to_keep))

# removing earlier years, and only including relevant classes
u_data <- u_data[u_data$aar > 2017,]
u_data <- u_data[u_data$grade <= 9,] # 9 and below are lower secondary school grades


# DF with school/year combos.
school_means <- u_data %>% 
  select(lnr_org, aar) %>% 
  distinct()

# the code loops through each variable, extracting variable means, then calculates a weighted mean based on the n per grade to get school-means
for (item in my_variables) {
  item_mean_col <- paste0(item, "_mean")
  item_nu_col <- paste0(item, "_nu")
  
    school_means <- u_data %>%
      group_by(lnr_org, aar) %>%
      summarise(!!paste0(item, "_overall_mean") := weighted.mean(.data[[item_mean_col]], .data[[item_nu_col]], na.rm = TRUE), .groups = 'drop') %>%
      right_join(school_means, by = c("lnr_org", "aar"))
  }


# renaming columns
school_means <- school_means %>%
  rename_with(
    ~ gsub("_overall_mean$", "", .),
    ends_with("_overall_mean")
  )

# changing nan values, introduced by NAs back to NA
school_means <- school_means %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

#reverse-coding items that are in "opposite" direction
school_means$mobb1 <- 7 - school_means$mobb1
school_means$mobb2 <- 7 - school_means$mobb2
school_means$mobb3 <- 7 - school_means$mobb3

# positive school climate variable names
pos_climate_item_names <- c("skole1","skole2","skole3","skole4","skole6")

# externalizing behavior variables only
ext_behavior_items <- school_means %>%
  select(-all_of(pos_climate_item_names))

# removing rows if they only have NAs in the items
ext_behavior_items <- ext_behavior_items %>%
  filter(rowSums(!is.na(select(., 3:ncol(.)))) > 0)

# create a dataframe with the column names and number of missing values
missing_counts <- sapply(ext_behavior_items, function(x) sum(is.na(x)))
missing_df <- data.frame(
  column_name = names(missing_counts),
  missing_values = missing_counts
)

# removing items with less than 50% response rates
cut_off <- nrow(ext_behavior_items)/2

cols_to_keep <- missing_df$column_name[missing_df$missing_values < cut_off]
  ext_behavior_items <- ext_behavior_items[, cols_to_keep]

# removing school-level details
ext_behavior_items <- ext_behavior_items[3:ncol(ext_behavior_items)]


          ##### 8.2 externalizing behavior #####

# plotting screeplot
tiff("plots/ext.behavior_scree.tiff", 
     width = 90, 
     height = 90,  # adjust as needed
     units = "mm", 
     res = 600,
     compression = "lzw")

par(cex = 0.8, mar = c(5, 4, 1, 2) + 0.1)

psych::fa.parallel(ext_behavior_items, fa="fa", main = "") 

dev.off()

# reset plotting parameters to default
par(cex = 1, mar = c(5, 4, 4, 2) + 0.1)

# EFA
efa_result <- psych::fa(ext_behavior_items, nfactors = 4, fm = "ml", rotate = "promax", missing = TRUE)

loadings_df <- as.data.frame(unclass(efa_result$loadings))
  loadings_df <- cbind(item = rownames(loadings_df), loadings_df)

#save(loadings_df, file = "tables/ext.behavior_EFA.RData")

# these items had support on the first factor
ext_final_items <- c("atfpro25","atfpro18","atfpro12","atfpro15","atfpro30","atfpro31")
  ext_final_item_desc <- variabel_list[variabel_list$Variabelnavn %in% ext_final_items,]

# final externalizing behavior df
ext_df <- school_means[c("lnr_org","aar",ext_final_items)]

# preparing data without school ID and year
ext_df_analysis <- subset(ext_df, select =
                                     -c(lnr_org,aar))

# model specification
ext_model <- 'ext_behavior =~ atfpro25+atfpro18+atfpro12+atfpro15+atfpro30+atfpro31'

# fit the model using FIML
ext_fit <- cfa(ext_model, data = ext_df_analysis, missing = "fiml", std.lv = T)
  fitMeasures(ext_fit)[c("rmsea","tli","srmr")]
  modificationIndices(ext_fit, sort = TRUE)
  parameterestimates(ext_fit)

# creating factor scores
ext_factor_scores <- lavPredict(ext_fit)

# indexing school/year
ext_item_schools <- subset(ext_df, select = c(lnr_org,aar))
  ext_item_schools<- cbind(ext_item_schools, ext_factor_scores)

          ##### 8.3 positive school climate #####

pos_df <- school_means[c("lnr_org","aar",pos_climate_item_names)]

# reverse coding item 4 and 6
pos_df$skole4 <- 5 - pos_df$skole4
pos_df$skole6 <- 5 - pos_df$skole6

# removing rows if they only have NA
pos_df <- pos_df %>%
  filter(!if_all(3:ncol(pos_df), is.na))

# analysis DF
pos_df_analysis <- pos_df[3:ncol(pos_df)]

# model specification
pos_model <- 'pos_climate =~ skole1+skole2+skole3+skole4+skole6'

pos_fit <- cfa(pos_model, data = pos_df_analysis, missing = "fiml")
  fitmeasures(pos_fit)[c("rmsea","tli","srmr")]
  modificationIndices(pos_fit, sort = TRUE)

# specifying residual correlation structure
pos_model_resid <- 'pos_climate =~ skole1+skole2+skole3+skole4+skole6

                    skole1 ~ skole3
                    skole4 ~ skole6
'

pos_fit_final <- cfa(pos_model_resid, data = pos_df_analysis, missing = "fiml", std.lv = T)
  fitMeasures(pos_fit_final)[c("rmsea","tli","srmr")]
  parameterestimates(pos_fit_final, standardized = T)

# creating factor scores
pos_factor_scores <- lavPredict(pos_fit_final)

# indexing school/year
pos_item_schools <- subset(pos_df, select = c(lnr_org,aar))
pos_item_schools<- cbind(pos_item_schools, pos_factor_scores)  
  
# combining factor score DFs for positive school climate and ext. behavior
factor_scores <- merge(pos_item_schools,ext_item_schools, by = c("lnr_org","aar"))
  names(factor_scores)[1:2] <- c("lnr_org","year")

# reverse coding positive climate
factor_scores$pos_climate <- factor_scores$pos_climate*(-1)

#write.csv(factor_scores, "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/98_ungdata.csv", row.names = F)


# counting N datapoints used in the analysis. Loading in the df again
count_data <- fread("N:/durable/data/questionnaires/Ungdata_NOVA/01_data/Ungdata_NOVA_2015-2024.csv", data.table = F)

# removing earlier years, and only including relevant classes
count_data <- count_data[count_data$aar > 2017,]
count_data <- count_data[count_data$grade <= 9,] # 9 and below are lower secondary school grades

# number of students answering each pos climate item
pos_climate_nu <- paste0(pos_climate_item_names, "_nu")

# making a DF with a school/year ID for the year a survey was carried out plus two years back
pos_climate_count <- count_data[c("lnr_org","aar",pos_climate_nu)]
  pos_climate_count$aar2 <- pos_climate_count$aar -1
  pos_climate_count$aar3 <- pos_climate_count$aar -2

pos_climate_count$school_year1 <- paste0(pos_climate_count$lnr_org,"_",pos_climate_count$aar)
  pos_climate_count$school_year2 <- paste0(pos_climate_count$lnr_org,"_",pos_climate_count$aar2)
  pos_climate_count$school_year3 <- paste0(pos_climate_count$lnr_org,"_",pos_climate_count$aar3)

# linking students in the analysis to these school/year combos
pos_climate_count <- pos_climate_count[pos_climate_count$school_year1 %in% final_sample$school_year
                                       | pos_climate_count$school_year2 %in% final_sample$school_year
                                       | pos_climate_count$school_year3 %in% final_sample$school_year,]

# out of the five items, which one had the highest response rate?
pos_climate_count$max_response <- apply(pos_climate_count[, 3:7], 1, max)

# N data points
sum(pos_climate_count$max_response)


# number of students answering each externalizing behavior item
ext_nu <- paste0(ext_final_items, "_nu")

# making a DF with a school/year ID for the year a survey was carried out plus two years back
ext_count <- count_data[c("lnr_org","aar",ext_nu)]
  ext_count$aar2 <- ext_count$aar -1
  ext_count$aar3 <- ext_count$aar -2

ext_count$school_year1 <- paste0(ext_count$lnr_org,"_",ext_count$aar)
  ext_count$school_year2 <- paste0(ext_count$lnr_org,"_",ext_count$aar2)
  ext_count$school_year3 <- paste0(ext_count$lnr_org,"_",ext_count$aar3)

  # linking students in the analysis to these school/year combos
ext_count <- ext_count[ext_count$school_year1 %in% final_sample$school_year
                                       | ext_count$school_year2 %in% final_sample$school_year
                                       | ext_count$school_year3 %in% final_sample$school_year,]

# out of the six items, which one had the highest response rate?
ext_count$max_response <- apply(ext_count[, 3:8], 1, max)
sum(ext_count$max_response)

