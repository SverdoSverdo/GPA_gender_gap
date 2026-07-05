setwd("N:/durable/projects/37323479_Sverre_GPA_gender_gap")

source("00_settings.R")

data <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")

        #### 1. SUBJECT GRADES ####

grade_data <- fread("N:/durable/data/registers/SSB/01_data/data_v6.0/EDUCATION_VGS_GRS/csv/EDUCATION_TAB_KAR_GRS.csv", data.table = F)
grade_data$avgdato <- grade_data$avgdato %/% 100

grade_data <- grade_data[grade_data$w19_0634_lnr %in% data$w19_0634_lnr,]

grade_data %>% count(fagkode, sort = TRUE)

fagkode_recode <- c(
  "ENG0012" = "ENG0030",
  "NOR0214" = "NOR0218",
  "NOR0215" = "NOR0219",
  "NOR0216" = "NOR0220",
  "MAT0010" = "MAT0015",
  "NAT0010" = "NAT0021"
)

grade_data <- grade_data %>%
  mutate(fagkode = dplyr::recode(fagkode, !!!fagkode_recode))

fagkode_labels <- c(
  "SAF0010" = "Social studies",
  "RLE0030" = "Religion & Ethics",
  "ENG0030" = "English",
  "NOR0218" = "Norwegian, written",
  "NOR0219" = "Norwegian, secondary",
  "NOR0220" = "Norwegian, oral",
  "MAT0015" = "Mathematics",
  "NAT0021" = "Science"
)

grade_data <- grade_data |>
  mutate(subject = dplyr::recode(fagkode, !!!fagkode_labels))

grade_data <- grade_data[grade_data$fagkode %in% names(fagkode_labels),]

#No basis for assessment is treated as a final grade, so chaning these to 1
grade_data$stp[grade_data$stp == "IV"] <- 1

grade_data <- grade_data %>%
  mutate(across(c(stp, termin1, termin2), 
                ~ ifelse(.x %in% c("1", "2", "3", "4", "5", "6"), .x, NA)))

#If someone has NA values in all grade columns, we remove the row
grade_data <- grade_data %>%
  filter(!(is.na(termin1) & is.na(termin2) & is.na(stp)))

#if someone has an NA value in stp, we choose the other semester grades instead.
grade_data <- grade_data %>%
  mutate(stp = coalesce(stp, termin2, termin1))


#4 people have duplicated course enrollments, we take their highest grade
grade_data <- grade_data %>%
  group_by(w19_0634_lnr, fagkode) %>%
  slice_max(as.numeric(stp), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(c(w19_0634_lnr,fagkode,stp,avgdato))

# pivoting to wide format
grade_data <- grade_data %>%
       pivot_wider(
             id_cols = c(w19_0634_lnr, avgdato),
             names_from = fagkode,
             values_from = stp
         )

subject_cols <- setdiff(names(grade_data), c("w19_0634_lnr", "avgdato"))

grade_data <- grade_data %>%
  mutate(across(all_of(subject_cols), as.numeric)) %>%
  group_by(avgdato) %>%
  mutate(across(all_of(subject_cols), ~ scale(.x)[, 1])) %>%
  ungroup() %>%
  select(-avgdato)

data <- merge(data, grade_data, by = "w19_0634_lnr", all.x = T)



        #### 2. STANDARDIZED TEST SCORES ####

NT <- fread("N:/durable/data/registers/SSB/01_data/data_v6.0/EDUCATION_VGS_GRS/csv/EDUCATION_NASJONALE_PROVER.csv", data.table = F)

NT <- NT %>%
  # limit to reading and maths tests
  filter(provekode %in% c("NPLES09", "NPREG09")) %>%
  
  # de-duplicate: prefer non-NA poeng, otherwise pick first
  arrange(w19_0634_lnr, provekode, is.na(poeng)) %>%
  distinct(w19_0634_lnr, provekode, .keep_all = TRUE) %>%
  
  # standardize poeng within aargang x provekode
  group_by(aargang, provekode) %>%
  mutate(poeng_z = scale(poeng)[, 1]) %>%
  ungroup() %>%
  
  # pivot to wide
  select(w19_0634_lnr, aargang, provekode, poeng_z) %>%
  pivot_wider(names_from = provekode, values_from = poeng_z) %>%
  rename(NT_reading = NPLES09, NT_maths = NPREG09) %>%
  
  # keep latest aargang per person (now safe, one row per person x aargang)
  group_by(w19_0634_lnr) %>%
  slice_max(aargang, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  
  # drop those missing both tests
  filter(!(is.na(NT_reading) & is.na(NT_maths))) %>%
  
  # limit to IDs in data, drop aargang
  semi_join(data, by = "w19_0634_lnr") %>%
  select(-aargang) %>%
  
  # re-standardize all three scores in the final sample
  mutate(across(c(NT_reading, NT_maths), ~ scale(.x)[, 1]))

data <- data %>%
  left_join(NT, by = "w19_0634_lnr")

#write.csv(data, file = "temp.data/data_test_grades.csv", row.names = F)
data <- read.csv("temp.data/data_test_grades.csv")

        #### 3. RANDOM EFFECTS MODELS ####


model_rhs <- list(
  
  model_1 = ~ cog_g*kjoenn_g +
    noncog_g*kjoenn_g +
    cog_parental_g*kjoenn_g +
    cog_parental_g*noncog_g*kjoenn_g +
    noncog_parental_g*noncog_g*kjoenn_g +
    noncog_parental_g*cog_g*kjoenn_g +
    (1 | lopenr_mor) + (1 | lnr_org),
  
  model_2 = ~ cog_g*kjoenn_g+
    noncog_g*kjoenn_g+
    
    cog_parental_g*cog_g*kjoenn_g+
    cog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*cog_g*kjoenn_g+
    
    (1 | lopenr_mor)  + (1+kjoenn_g | lnr_org),
  
  model_3 = ~ cog_g*kjoenn_g+
    noncog_g*kjoenn_g+
    
    cog_parental_g*cog_g*kjoenn_g+
    cog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*cog_g*kjoenn_g+
    
    (1 | lopenr_mor)  + (1+kjoenn_g+noncog_g | lnr_org),
  
  model_4 = ~ cog_g*kjoenn_g+
    noncog_g*kjoenn_g+
    
    cog_parental_g*cog_g*kjoenn_g+
    cog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*cog_g*kjoenn_g+
    
    (1 | lopenr_mor)  + (1+kjoenn_g+noncog_g+cog_g | lnr_org),
  
  model_5 = ~ cog_g*kjoenn_g+
    noncog_g*kjoenn_g+
    
    cog_parental_g*cog_g*kjoenn_g+
    cog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*cog_g*kjoenn_g+
    
    (1 | lopenr_mor)  + (1+kjoenn_g*noncog_g | lnr_org),
  
  model_6 = ~ cog_g*kjoenn_g+
    noncog_g*kjoenn_g+
    
    cog_parental_g*cog_g*kjoenn_g+
    cog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*cog_g*kjoenn_g+
    
    (1 | lopenr_mor)  + (1+kjoenn_g*cog_g | lnr_org),
  
  model_7 = ~ cog_g*kjoenn_g+
    noncog_g*kjoenn_g+
    
    cog_parental_g*cog_g*kjoenn_g+
    cog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*cog_g*kjoenn_g+
    
    (1 | lopenr_mor)  + (1+kjoenn_g*noncog_g+cog_g | lnr_org),
  
  model_8 =  ~ cog_g*kjoenn_g+
    noncog_g*kjoenn_g+
    
    cog_parental_g*cog_g*kjoenn_g+
    cog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*cog_g*kjoenn_g+
    
    (1 | lopenr_mor)  + (1+kjoenn_g*cog_g+noncog_g | lnr_org),
  
  model_9 = ~ cog_g*kjoenn_g+
    noncog_g*kjoenn_g+
    
    cog_parental_g*cog_g*kjoenn_g+
    cog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*noncog_g*kjoenn_g+
    noncog_parental_g*cog_g*kjoenn_g+
    
    (1 | lopenr_mor)  + (1+kjoenn_g*noncog_g+kjoenn_g*cog_g| lnr_org)
  
)

#terms that are included as random slopes
model_names <-c("null",
                "sex",
                "sex, noncog",
                "sex, noncog, cog",
                "sex,noncog, sex*noncog",
                "sex,cog, sex*cog",
                "sex,noncog,sex*noncog,cog",
                "sex,cog,sex*cog,noncog",
                "sex,cog,noncog,sex*cog,sex*noncog")  


outcomes <- c("NOR0218", "NOR0220", "RLE0030", "SAF0010", "ENG0030", "NAT0021", "MAT0015", "NOR0219", "NT_reading", "NT_maths")


results <- lapply(outcomes, function(outcome) {
  
  # subset to non-missing on this outcome
  data_out <- data[!is.na(data[[outcome]]), ]
  
  # standardize outcome within this sample
  data_out[[paste0(outcome, "_std")]] <- scale(data_out[[outcome]])[, 1]
  outcome_std <- paste0(outcome, "_std")
  
  # fit all models
  fits <- lapply(model_rhs, function(rhs) {
    formula <- as.formula(paste(outcome_std, paste(deparse(rhs), collapse = "")))
    lmer(formula, data = data_out, REML = FALSE,
         control = lmerControl(optimizer = "bobyqa"))
  })
  names(fits) <- model_names
  
  # fit table
  table <- data.frame(
    LogLik = round(sapply(fits, logLik), 2),
    AIC    = round(sapply(fits, AIC),    2),
    row.names = model_names
  )
  
  list(models = fits, table = table)
})
names(results) <- outcomes
fit_table <- do.call(cbind, lapply(results, function(x) x$table))


#save(results, file = "model_output/random_models_test_grades.Rdata")

        #### 3. CANDIDATE ENVIRONMENT  MODELS ####


# path to project folder
data_path <- "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data"

# find all csv files starting with "data_"
csv_files <- list.files(data_path, pattern = "^data_.*\\.csv$", full.names = TRUE)
csv_files <- csv_files[-10]

# read them all into a named list, stripping the .csv extension for the name
data_list <- lapply(csv_files, read.csv)
names(data_list) <- tools::file_path_sans_ext(basename(csv_files))

# selecting grade and test outcomes
outcomes_df <- data[, c("w19_0634_lnr", unlist(outcomes))]

# after loading the csvs, join outcomes onto each dataset
data_list <- lapply(data_list, function(df) {
  left_join(df, outcomes_df, by = "w19_0634_lnr")
})

# assign each to its own object in the global environment
list2env(data_list, envir = .GlobalEnv)


model_specs <- list(
  
  school_income = list(
    data = data_school_income_m,
    rhs  = ~ noncog_g*kjoenn_g*school_income_m +
      cog_g*kjoenn_g*school_income_m +
      cog_parental_g*kjoenn_g*school_income_m +
      noncog_parental_g*kjoenn_g*school_income_m +
      cog_parental_g*cog_g*kjoenn_g +
      cog_parental_g*noncog_g*kjoenn_g +
      noncog_parental_g*cog_g*kjoenn_g +
      noncog_parental_g*noncog_g*kjoenn_g +
      (1 | lopenr_mor) + (1 | lnr_org)
  ),
  
  class_gender = list(
    data = data_classroom_gender_gy,
    rhs  = ~ noncog_g*kjoenn_cg*classroom_gender_gy +
      cog_g*kjoenn_cg*classroom_gender_gy +
      cog_parental_g*kjoenn_cg*classroom_gender_gy +
      noncog_parental_g*kjoenn_cg*classroom_gender_gy +
      cog_parental_g*cog_g*kjoenn_cg +
      cog_parental_g*noncog_g*kjoenn_cg +
      noncog_parental_g*cog_g*kjoenn_cg +
      noncog_parental_g*noncog_g*kjoenn_cg +
      (1 | lopenr_mor) + (1 | school_year/class)
  ),
  
  class_grades = list(
    data = data_class_grades_gy,
    rhs  = ~ noncog_g*kjoenn_cg*class_grades_gy +
      cog_g*kjoenn_cg*class_grades_gy +
      cog_parental_g*kjoenn_cg*class_grades_gy +
      noncog_parental_g*kjoenn_cg*class_grades_gy +
      cog_parental_g*cog_g*kjoenn_cg +
      cog_parental_g*noncog_g*kjoenn_cg +
      noncog_parental_g*cog_g*kjoenn_cg +
      noncog_parental_g*noncog_g*kjoenn_cg +
      (1 | lopenr_mor) + (1 | school_year/class)
  ),
  
  teacher_edu = list(
    data = data_school_teacher_edu_m,
    rhs  = ~ noncog_g*kjoenn_gy*school_teacher_edu_m +
      cog_g*kjoenn_gy*school_teacher_edu_m +
      cog_parental_g*kjoenn_gy*school_teacher_edu_m +
      noncog_parental_g*kjoenn_gy*school_teacher_edu_m +
      cog_parental_g*cog_g*kjoenn_gy +
      cog_parental_g*noncog_g*kjoenn_gy +
      noncog_parental_g*cog_g*kjoenn_gy +
      noncog_parental_g*noncog_g*kjoenn_gy +
      school_middle*noncog_g*kjoenn_gy +
      school_middle*cog_g*kjoenn_gy +
      (1 | lopenr_mor) + (1 | school_year)
  ),
  
  teacher_gender = list(
    data = data_school_teacher_gender_m,
    rhs  = ~ noncog_g*kjoenn_gy*school_teacher_gender_m +
      cog_g*kjoenn_gy*school_teacher_gender_m +
      cog_parental_g*kjoenn_gy*school_teacher_gender_m +
      noncog_parental_g*kjoenn_gy*school_teacher_gender_m +
      cog_parental_g*cog_g*kjoenn_gy +
      cog_parental_g*noncog_g*kjoenn_gy +
      noncog_parental_g*cog_g*kjoenn_gy +
      noncog_parental_g*noncog_g*kjoenn_gy +
      school_middle*noncog_g*kjoenn_gy +
      school_middle*cog_g*kjoenn_gy +
      (1 | lopenr_mor) + (1 | school_year)
  ),
  
  turnover = list(
    data = data_school_turnover_m,
    rhs  = ~ noncog_g*kjoenn_g*school_turnover_m +
      cog_g*kjoenn_g*school_turnover_m +
      cog_parental_g*kjoenn_g*school_turnover_m +
      noncog_parental_g*kjoenn_g*school_turnover_m +
      cog_parental_g*cog_g*kjoenn_g +
      cog_parental_g*noncog_g*kjoenn_g +
      noncog_parental_g*cog_g*kjoenn_g +
      noncog_parental_g*noncog_g*kjoenn_g +
      school_middle*noncog_g*kjoenn_g +
      school_middle*cog_g*kjoenn_g +
      (1 | lopenr_mor) + (1 | lnr_org)
  ),
  
  student_gender = list(
    data = data_school_student_gender_m,
    rhs  = ~ noncog_g*kjoenn_gy*school_student_gender_m +
      cog_g*kjoenn_gy*school_student_gender_m +
      cog_parental_g*kjoenn_gy*school_student_gender_m +
      noncog_parental_g*kjoenn_gy*school_student_gender_m +
      cog_parental_g*cog_g*kjoenn_gy +
      cog_parental_g*noncog_g*kjoenn_gy +
      noncog_parental_g*cog_g*kjoenn_gy +
      noncog_parental_g*noncog_g*kjoenn_gy +
      (1 | lopenr_mor) + (1 | school_year)
  ),
  
  pos_climate = list(
    data = data_pos_climate_m,
    rhs  = ~ noncog_g*kjoenn_gy*pos_climate_m +
      cog_g*kjoenn_gy*pos_climate_m +
      cog_parental_g*kjoenn_gy*pos_climate_m +
      noncog_parental_g*kjoenn_gy*pos_climate_m +
      cog_parental_g*cog_g*kjoenn_gy +
      cog_parental_g*noncog_g*kjoenn_gy +
      noncog_parental_g*cog_g*kjoenn_gy +
      noncog_parental_g*noncog_g*kjoenn_gy +
      (1 | lopenr_mor) + (1 | school_year)
  ),
  
  ext_behavior = list(
    data = data_ext_behavior_m,
    rhs  = ~ noncog_g*kjoenn_gy*ext_behavior_m +
      cog_g*kjoenn_gy*ext_behavior_m +
      cog_parental_g*kjoenn_gy*ext_behavior_m +
      noncog_parental_g*kjoenn_gy*ext_behavior_m +
      cog_parental_g*cog_g*kjoenn_gy +
      cog_parental_g*noncog_g*kjoenn_gy +
      noncog_parental_g*cog_g*kjoenn_gy +
      noncog_parental_g*noncog_g*kjoenn_gy +
      (1 | lopenr_mor) + (1 | school_year)
  )
)

candidate_models <- lapply(outcomes, function(outcome) {
  lapply(model_specs, function(spec) {
    
    data_out <- spec$data[!is.na(spec$data[[outcome]]), ]
    data_out[[paste0(outcome, "_std")]] <- scale(data_out[[outcome]])[, 1]
    outcome_std <- paste0(outcome, "_std")
    
    formula <- as.formula(paste(outcome_std, paste(deparse(spec$rhs), collapse = "")))
    
    lmer(formula, data = data_out, REML = TRUE,
         control = lmerControl(optimizer = "bobyqa"))
  })
})
names(candidate_models) <- outcomes

save(candidate_models, file = "model_output/candidate_models_test_grades.Rdata")

load("model_output/candidate_models_test_grades.Rdata")


        #### 4. CANDIDATE MODEL TABLES ####

#### 4. CANDIDATE MODEL TABLES ####

#z-multipliers: 95% CI is the default, 97.5% CI is used for any term involving
#cog_g or noncog_g (bonferroni-style correction for testing both genetic scores)
z_95  <- qnorm(0.975)   #~1.96
z_975 <- qnorm(0.9875)  #~2.24

all_tables <- lapply(outcomes, function(outcome) {
  
  #extract fixed effects
  full_model_info <- data.frame()
  
  for (i in names(candidate_models[[outcome]])) {
    tidy_output <- broom.mixed::tidy(candidate_models[[outcome]][[i]], effects = "fixed")
    new_data <- tidy_output[, c("term", "estimate", "std.error", "p.value")]
    new_data$model <- i
    full_model_info <- rbind(full_model_info, new_data)
  }
  
  #manual CIs: 97.5% for terms involving cog_g/noncog_g, 95% otherwise
  #("noncog_g" contains "cog_g" as a substring, so one check covers both)
  full_model_info$z_mult    <- ifelse(str_detect(full_model_info$term, "cog_g"), z_975, z_95)
  full_model_info$conf.low  <- full_model_info$estimate - full_model_info$z_mult * full_model_info$std.error
  full_model_info$conf.high <- full_model_info$estimate + full_model_info$z_mult * full_model_info$std.error
  
  #filtering and variance
  model_info <- full_model_info[!grepl("parental|school_middle|(Intercept)", full_model_info$term, ignore.case = TRUE), ]
  model_info <- select(model_info, -c(conf.low, conf.high))
  model_info$var <- model_info$std.error^2
  
  #standardize kjoenn variants
  model_info$term <- gsub("kjoenn_g|kjoenn_gy|kjoenn_gc|kjoenn_cg", "kjoenn", model_info$term)
  
  #interaction column
  model_info <- model_info %>%
    mutate(
      interaction = map_chr(seq_along(term), function(i) {
        x <- term[i]
        
        # three-way interactions with kjoenn and cog/noncog
        if (str_count(x, ":") == 2 & str_detect(x, "kjoenn") & str_detect(x, "(cog_g|noncog_g)")) {
          genetic_var <- str_extract(x, "(cog_g|noncog_g)")
          parts <- str_split(x, ":")[[1]]
          env_var <- parts[!parts %in% c("kjoenn", genetic_var)]
          if (length(env_var) > 0) {
            option1 <- paste0(env_var, ":", genetic_var)
            option2 <- paste0(genetic_var, ":", env_var)
            if (option1 %in% model_info$term) return(option1)
            else if (option2 %in% model_info$term) return(option2)
            else return(NA_character_)
          }
        }
        
        # two-way interactions with kjoenn
        else if (str_count(x, ":") == 1 & str_detect(x, "kjoenn")) {
          parts <- str_split(x, ":")[[1]]
          env_var <- parts[parts != "kjoenn"]
          if (length(env_var) > 0) return(env_var)
        }
        
        return(NA_character_)
      })
    )
  
  # vcov matrices — store in a local list
  vcov_list <- lapply(unique(model_info$model), function(model_name) {
    mat <- vcov(candidate_models[[outcome]][[model_name]])
    rownames(mat) <- str_replace_all(rownames(mat), "kjoenn_(gy|g|cg|gc)", "kjoenn")
    colnames(mat) <- str_replace_all(colnames(mat), "kjoenn_(gy|g|cg|gc)", "kjoenn")
    mat
  })
  names(vcov_list) <- unique(model_info$model)
  
  model_info <- model_info %>%
    mutate(
      cov = pmap_dbl(list(term, interaction, model), function(t, i, m) {
        if (is.na(i)) return(NA_real_)
        vcov_matrix <- vcov_list[[m]]
        if (is.null(vcov_matrix)) return(NA_real_)
        if (t %in% rownames(vcov_matrix) && i %in% rownames(vcov_matrix)) {
          return(vcov_matrix[t, i])
        }
        return(NA_real_)
      }),
      interaction_var = pmap_dbl(list(interaction, model), function(x, m) {
        if (!is.na(x)) {
          match_idx <- which(model_info$term == x & model_info$model == m)
          if (length(match_idx) > 0) return(model_info$var[match_idx[1]])
        }
        return(NA_real_)
      }),
      interaction_estimate = pmap_dbl(list(interaction, model), function(x, m) {
        if (!is.na(x)) {
          match_idx <- which(model_info$term == x & model_info$model == m)
          if (length(match_idx) > 0) return(model_info$estimate[match_idx[1]])
        }
        return(NA_real_)
      })
    )
  
  # gender-specific CIs — z_mult is per-row (carried over from full_model_info),
  # so rows whose term involves cog_g/noncog_g automatically get the 97.5% multiplier
  model_info$estimate_boys  <- model_info$interaction_estimate + model_info$estimate
  model_info$estimate_girls <- model_info$interaction_estimate - model_info$estimate
  model_info$se_boys  <- sqrt(model_info$var + model_info$interaction_var + 2*model_info$cov)
  model_info$se_girls <- sqrt(model_info$var + model_info$interaction_var - 2*model_info$cov)
  model_info$ci.upper_boys  <- model_info$estimate_boys  + model_info$se_boys  * model_info$z_mult
  model_info$ci.lower_boys  <- model_info$estimate_boys  - model_info$se_boys  * model_info$z_mult
  model_info$ci.upper_girls <- model_info$estimate_girls + model_info$se_girls * model_info$z_mult
  model_info$ci.lower_girls <- model_info$estimate_girls - model_info$se_girls * model_info$z_mult
  
  # final table
  table_linear <- select(model_info, c(model, term, interaction, estimate,
                                       estimate_boys, estimate_girls,
                                       ci.upper_boys, ci.lower_boys,
                                       ci.upper_girls, ci.lower_girls, p.value))
  
  non_gender_CIs <- select(full_model_info, c(model, term, estimate, conf.low, conf.high))
  names(non_gender_CIs)[3:5] <- c("estimate_all", "ci.lower", "ci.upper")
  
  table_linear <- left_join(table_linear, non_gender_CIs, by = c("model" = "model", "interaction" = "term"))
  
  # renaming interactions
  converting_names <- data.frame(
    plot_name     = c("Classroom gender", "Classroom GPA", "Ext.behavior",
                      "Positive school climate", "School SES", "School gender",
                      "Teacher EA", "teacher gender", "Teacher turnover"),
    variable_name = c("classroom_gender_gy", "class_grades_gy", "ext_behavior_m",
                      "pos_climate_m", "school_income_m", "school_student_gender_m",
                      "school_teacher_edu_m", "school_teacher_gender_m", "school_turnover_m")
  )
  name_lookup <- setNames(converting_names$plot_name, converting_names$variable_name)
  
  table_linear <- table_linear %>%
    mutate(interaction = str_replace_all(interaction, name_lookup)) %>%
    mutate(interaction = str_replace(interaction, "I\\((.+?)\\^2\\)", "\\1^2"))
  
  table_linear <- table_linear %>%
    mutate(interaction = str_replace_all(interaction, c(
      "noncog_g" = "NonCog",
      "kjoenn"   = "Gender",
      "cog_g"    = "Cog",
      ":"        = " x "
    )))
  
  # rearranging so environment comes first
  table_linear <- table_linear %>%
    mutate(interaction = case_when(
      str_count(interaction, " x ") == 2 ~ {
        parts <- str_split(interaction, " x ")
        sapply(parts, function(p) {
          gender_idx       <- which(p == "Gender")
          plot_name_idx    <- which(p %in% c(converting_names$plot_name, paste0(converting_names$plot_name, "^2")))
          if (length(plot_name_idx) > 0) {
            other_idx <- setdiff(1:3, c(gender_idx, plot_name_idx))
            c(p[plot_name_idx[1]], p[other_idx], p[gender_idx]) %>% paste(collapse = " x ")
          } else {
            c(p[-gender_idx], p[gender_idx]) %>% paste(collapse = " x ")
          }
        })
      },
      str_count(interaction, " x ") == 1 ~ {
        parts <- str_split(interaction, " x ")
        sapply(parts, function(p) {
          plot_name_interactions <- c(converting_names$plot_name, paste0(converting_names$plot_name, "^2"))
          if (p[2] %in% plot_name_interactions) paste(p[2], p[1], sep = " x ")
          else if (p[1] %in% plot_name_interactions) paste(p, collapse = " x ")
          else paste(p, collapse = " x ")
        })
      },
      TRUE ~ interaction
    ))
  
  # removing unwanted rows
  table_linear <- table_linear %>%
    filter(!interaction %in% c("NonCog", "Gender", "Cog", "NonCog x Gender", "Gender x Cog"))
  table_linear <- table_linear[!is.na(table_linear$interaction), ]
  
  # final formatting
  table_linear_final <- table_linear %>%
    mutate(across(4:ncol(.), ~ round(., 3))) %>%
    mutate(p.value = ifelse(p.value == 0, "<.001", as.character(p.value))) %>%
    mutate(across(c(4:ncol(.))[!names(.)[4:ncol(.)] %in% "p.value"],
                  ~ gsub("^0\\.", ".", as.character(.)))) %>%
    mutate(
      estimate_all_ci   = paste0(estimate_all,   " (", ci.lower,       ", ", ci.upper,       ")"),
      estimate_boys_ci  = paste0(estimate_boys,  " (", ci.lower_boys,  ", ", ci.upper_boys,  ")"),
      estimate_girls_ci = paste0(estimate_girls, " (", ci.lower_girls, ", ", ci.upper_girls, ")")
    ) %>%
    mutate(model = ifelse(duplicated(model), "", model)) %>%
    select(model, interaction, estimate_all_ci, estimate_boys_ci, estimate_girls_ci, p.gender_diff = p.value)
  
  table_linear_final
})

names(all_tables) <- outcomes


save(all_tables, file = "model_output/all.tables.RData")



        #### 5 RANDOM EFFECTS PLOTS: GRADES ####

load("model_output/random_models_test_grades.Rdata")
outcomes <- c("NOR0218", "NOR0220", "RLE0030", "SAF0010", "ENG0030", "NAT0021", "MAT0015", "NOR0219", "NT_reading", "NT_maths")



#retrieving the model summary for the best-fitting models per outcome
results_best <- lapply(outcomes, function(outcome) {
  best_model_name <- rownames(results[[outcome]]$table)[which.min(results[[outcome]]$table$AIC)]
  results[[outcome]]$models[[best_model_name]]
})
names(results_best) <- outcomes

          ##### 5.1 extract variance components #####
variance_components <- lapply(outcomes, function(outcome) {
  
  tables <- results[[outcome]]$table
  best_model_name <- rownames(tables)[which.min(tables$AIC)]
  best_model <- results[[outcome]]$models[[best_model_name]]
  
  fe <- fixef(best_model)
  beta_noncog       <- ifelse("noncog_g" %in% names(fe), fe["noncog_g"], 0)
  beta_cog         <- ifelse("cog_g" %in% names(fe), fe["cog_g"], 0)
  beta_noncoggender <- ifelse("kjoenn_g:noncog_g" %in% names(fe), fe["kjoenn_g:noncog_g"],
                              ifelse("noncog_g:kjoenn_g" %in% names(fe), fe["noncog_g:kjoenn_g"], 0))
  
  beta_coggender <- ifelse("kjoenn_g:cog_g" %in% names(fe), fe["kjoenn_g:cog_g"],
                              ifelse("cog_g:kjoenn_g" %in% names(fe), fe["cog_g:kjoenn_g"], 0))
  
  beta_gender       <- ifelse("kjoenn_g" %in% names(fe), fe["kjoenn_g"], 0)
  
  beta_noncog_boys  <- beta_noncog + beta_noncoggender
  beta_noncog_girls <- beta_noncog - beta_noncoggender
  
  beta_cog_boys  <- beta_cog + beta_coggender
  beta_cog_girls <- beta_cog - beta_coggender
  
  random_var <- as.matrix(VarCorr(best_model)$lnr_org)
  rn <- rownames(random_var)
  
  var_noncog <- ifelse("noncog_g" %in% rn, random_var["noncog_g", "noncog_g"], NA)
  var_gender <- ifelse("kjoenn_g" %in% rn, random_var["kjoenn_g", "kjoenn_g"], NA)
  
  has_interaction  <- "kjoenn_g:noncog_g" %in% rn | "noncog_g:kjoenn_g" %in% rn
  interaction_term <- ifelse("kjoenn_g:noncog_g" %in% rn, "kjoenn_g:noncog_g", "noncog_g:kjoenn_g")
  
  var_noncoggender        <- ifelse(has_interaction, random_var[interaction_term, interaction_term], NA)
  cov_noncoggender_noncog <- ifelse(has_interaction & "noncog_g" %in% rn,
                                    random_var[interaction_term, "noncog_g"], NA)
  
  var_boys  <- ifelse(has_interaction, var_noncog + var_noncoggender + 2*cov_noncoggender_noncog, var_noncog)
  var_girls <- ifelse(has_interaction, var_noncog + var_noncoggender - 2*cov_noncoggender_noncog, var_noncog)
  sd_boys   <- sqrt(var_boys)
  sd_girls  <- sqrt(var_girls)
  
  mean_observed_gap <- beta_gender * 2
  sd_gender <- ifelse(!is.na(var_gender), sqrt(var_gender) * 2, NA) #multiply by 2 to account for effects coding
  
  # intercept covariances for R2 plot
  var_intercept <- ifelse("(Intercept)" %in% rn, random_var["(Intercept)", "(Intercept)"], NA)
  sd_intercept  <- sqrt(var_intercept)
  
  # noncog-intercept covariance (gender-specific)
  cov_noncog_int <- ifelse("noncog_g" %in% rn & "(Intercept)" %in% rn,
                           random_var["noncog_g", "(Intercept)"], NA)
  cov_noncoggender_int <- ifelse(has_interaction & "(Intercept)" %in% rn,
                                 random_var[interaction_term, "(Intercept)"], NA)
  
  # gender-specific multipliers: how noncog slope changes per unit of intercept
  mult_noncog_boys  <- ifelse(has_interaction,
                       (cov_noncog_int + cov_noncoggender_int) / sd_intercept,
                       cov_noncog_int / sd_intercept)
  mult_noncog_girls <- ifelse(has_interaction,
                       (cov_noncog_int - cov_noncoggender_int) / sd_intercept,
                       cov_noncog_int / sd_intercept)
  
  # cog
  has_cog_random   <- "cog_g" %in% rn
  var_cog          <- ifelse(has_cog_random, random_var["cog_g", "cog_g"], NA)
  sd_cog           <- ifelse(has_cog_random, sqrt(var_cog), NA)
  cov_cog_int      <- ifelse(has_cog_random & "(Intercept)" %in% rn,
                             random_var["cog_g", "(Intercept)"], NA)
  mult_cog         <- ifelse(has_cog_random, cov_cog_int / sd_intercept, NA)
  
  # check cog significance
  model_tidy   <- tidy(best_model, effects = "fixed")
  cog_row      <- model_tidy[model_tidy$term == "cog_g", ]
  has_cog_sig  <- nrow(cog_row) > 0 && cog_row$p.value < 0.05
  
  list(
    outcome              = outcome,
    beta_noncog_boys     = beta_noncog_boys,
    beta_noncog_girls    = beta_noncog_girls,
    sd_boys              = sd_boys,
    sd_girls             = sd_girls,
    mean_observed_gap    = mean_observed_gap,
    sd_gender            = sd_gender,
    best_model_name      = best_model_name,
    has_interaction      = has_interaction,
    beta_cog             = beta_cog,
    beta_cog_boys        = beta_cog_boys,
    beta_cog_girls       = beta_cog_girls,
    sd_cog               = sd_cog,
    has_cog_random       = has_cog_random,
    has_cog_sig          = has_cog_sig,
    var_intercept        = var_intercept,
    sd_intercept         = sd_intercept,
    mult_noncog_boys     = mult_noncog_boys,
    mult_noncog_girls           = mult_noncog_girls,
    mult_cog             = mult_cog,
    cov_cog_int          = cov_cog_int
  )
})

names(variance_components) <- outcomes


grade_outcomes <- outcomes[!outcomes == "NT_reading" & !outcomes == "NT_maths"]
outcome_labels <- fagkode_labels
test_outcomes <- outcomes[!outcomes %in% grade_outcomes]


        ##### 5.2 extracting BLUPs #####


# pre-compute blup data for all outcomes
blup_data <- lapply(seq_along(grade_outcomes), function(idx) {
  outcome <- grade_outcomes[idx]
  vc      <- variance_components[[outcome]]
  label   <- outcome_labels[outcome]
  
  best_model <- results[[outcome]]$models[[vc$best_model_name]]
  
  # extract random effects from lnr_org
  re_schools         <- as.data.frame(ranef(best_model)$lnr_org)
  re_schools$lnr_org <- rownames(re_schools)
  
  # standardize column names — find intercept and noncog columns
  rn             <- names(re_schools)
  intercept_col  <- "(Intercept)"
  noncog_col     <- "noncog_g"
  gender_noncog_col <- intersect(c("kjoenn_g:noncog_g", "noncog_g:kjoenn_g"), rn)
  
  has_noncog        <- noncog_col %in% rn
  has_gender_noncog <- length(gender_noncog_col) > 0
  
  if (!has_noncog) return(NULL)  # skip outcomes without noncog
  
  # compute gender-specific noncog blups
  re_schools <- re_schools %>%
    mutate(
      noncog_re        = .data[[noncog_col]],
      gender_noncog_re = if (has_gender_noncog) .data[[gender_noncog_col[1]]] else 0,
      nonCog_boys      = vc$beta_noncog_boys  + noncog_re + gender_noncog_re,
      nonCog_girls     = vc$beta_noncog_girls + noncog_re - gender_noncog_re
    )
  
  # pivot to long
  school_blups_long <- re_schools %>%
    pivot_longer(
      cols      = c(nonCog_boys, nonCog_girls),
      names_to  = "Gender",
      values_to = "nonCog_blup"
    ) %>%
    mutate(Gender = recode(Gender,
                           "nonCog_boys"  = "Boys",
                           "nonCog_girls" = "Girls"))
  
  list(
    long  = school_blups_long,
    wide  = re_schools,         
    intercept = re_schools[["(Intercept)"]],  
    label = label
  )
})
names(blup_data) <- grade_outcomes



          ##### 5.3 gender gaps ######

# compute mean gap per outcome for ordering
mean_gaps <- sapply(grade_outcomes, function(outcome) {
  d  <- blup_data[[outcome]]
  vc <- variance_components[[outcome]]
  if (is.null(d)) return(NA)
  mean(vc$mean_observed_gap + d$wide$kjoenn_g * 2, na.rm = TRUE)
})
grade_outcomes_ordered <- grade_outcomes[order(mean_gaps)]

# combine all gap data into one dataframe
gap_df <- map_dfr(grade_outcomes_ordered, function(outcome) {
  d  <- blup_data[[outcome]]
  vc <- variance_components[[outcome]]
  if (is.null(d)) return(NULL)
  
  d$wide %>%
    mutate(
      school_gap = vc$mean_observed_gap + kjoenn_g * 2,
      subject    = factor(outcome_labels[[outcome]],
                          levels = outcome_labels[grade_outcomes_ordered])
    )
})


#range of slopes
y_lim_gap <- range(gap_df$school_gap, na.rm = TRUE)


# plot
plot_bottom <- ggplot(gap_df, aes(x = 1, y = school_gap)) +
  geom_hline(
    yintercept = pretty(y_lim_gap, n = 5),
    color = "grey92", linewidth = 0.3
 ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  geom_violin(fill = "#7B2D8B", alpha = 1,linewidth = 0.3) +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  facet_wrap(~ subject, nrow = 1) +
  labs(
    y = "Gender gap in grade",
    x = NULL
  ) +
  theme_sverdo() +
  theme(
    panel.spacing     = unit(0, "lines"),
    panel.grid        = element_blank(),
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.ticks.y      = element_blank(),
    axis.text.y       = element_text()
  )


          ##### 5.4 NonCog-slopes #####

# combine all blup data into one dataframe
blup_df <- map_dfr(grade_outcomes_ordered, function(outcome) {
  d <- blup_data[[outcome]]
  if (is.null(d)) return(NULL)
  
  d$long %>%
    mutate(subject = factor(outcome_labels[[outcome]],
                            levels = outcome_labels[grade_outcomes_ordered]))
})


# create a named vector mapping outcome to label with asterisk where significant
sig_outcomes <- c("NOR0218", "NOR0220", "NOR0219", "ENG0030","NAT0021") 

levels_ordered <- outcome_labels[grade_outcomes_ordered]
levels_with_stars <- ifelse(grade_outcomes_ordered %in% sig_outcomes,
                            paste0(levels_ordered, "*"),
                            levels_ordered)

y_range <- range(blup_df$nonCog_blup, na.rm = TRUE)

# apply to blup_df
blup_df <- blup_df %>%
  mutate(subject = factor(subject,
                          levels = levels_ordered,
                          labels = levels_with_stars))

#plot
plot_top <- ggplot(blup_df, aes(x = Gender, y = nonCog_blup, fill = Gender)) +
  geom_hline(
    yintercept = pretty(y_range, n = 5),
    color = "grey92", linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  geom_violin(alpha = 1, linewidth = 0.3,
              position = position_dodge(width = 0)) +   
  scale_fill_manual(values = c("Boys" = color_boys, "Girls" = color_girls)) +
  coord_cartesian(ylim = y_range) +
  facet_wrap(~ subject, nrow = 1) +
  scale_y_continuous(breaks = seq(0, 0.25, by = 0.05)) +
  scale_x_discrete(expand = expansion(add = 0.8)) +
  labs(
    y    = "NonCog-grade slope",
    x    = NULL,
    fill = NULL
  ) +
  guides(fill = guide_legend(override.aes = list(shape = NA, color = NA, linetype = 0),
                             keywidth  = unit(0.35, "cm"),
                             keyheight = unit(0.35, "cm"))) +
  theme_sverdo() +
  theme(
    panel.spacing      = unit(0, "lines"),
    strip.text         = element_text(hjust = 0.5),
    panel.grid         = element_blank(),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.text.y        = element_text(),
    legend.key         = element_rect(color = NA, fill = NA),
    legend.background  = element_rect(color = NA, fill = NA),
    legend.position    = c(1, 0.05),
    legend.justification = c("right", "bottom"),
    legend.key.size    = unit(0.1, "cm")
  )



          ##### 5.5 figure 3 #####

figure3 <- plot_grid(
  plot_top, plot_bottom,
  ncol = 1,
  rel_heights = c(5, 4),
  labels = c("a", "b"),
  label_fontfamily = "serif",
  label_fontface = "bold",
  label_size = 10
)

tiff("plots/figure3.tiff",
     width = 179,
     height = 100,
     units = "mm",
     res = 600,
     compression = "lzw")

figure3

dev.off()


#sd in gender gaps
sd_gender <- sapply(variance_components, function(vc) vc$sd_gender)

cat("smallest sd_gender:", names(which.min(sd_gender)), "-", round(min(sd_gender), 4), "\n")
cat("largest sd_gender:",  names(which.max(sd_gender)), "-", round(max(sd_gender), 4), "\n")



          #### 5 RANDOM EFFECTS PLOTS: GRADES ####

outcome_test_labels <- c("NT_reading" = "Reading",
                         "NT_maths" = "Mathematics")

            ##### 6.1 extracting BLUPs #####

# pre-compute blup data for all outcomes
blup_data <- lapply(seq_along(test_outcomes), function(idx) {
  outcome <- test_outcomes[idx]
  vc      <- variance_components[[outcome]]
  label   <- outcome_test_labels[outcome]
  
  best_model <- results[[outcome]]$models[[vc$best_model_name]]
  
  # extract random effects from lnr_org
  re_schools          <- as.data.frame(ranef(best_model)$lnr_org)
  re_schools$lnr_org  <- rownames(re_schools)
  
  # standardize column names - find intercept, noncog, and cog columns
  rn                <- names(re_schools)
  intercept_col     <- "(Intercept)"
  noncog_col        <- "noncog_g"
  cog_col           <- "cog_g"
  gender_noncog_col <- intersect(c("kjoenn_g:noncog_g", "noncog_g:kjoenn_g"), rn)
  gender_cog_col    <- intersect(c("kjoenn_g:cog_g",    "cog_g:kjoenn_g"),    rn)
  
  has_noncog        <- noncog_col %in% rn
  has_cog           <- cog_col %in% rn
  has_gender_noncog <- length(gender_noncog_col) > 0
  has_gender_cog    <- length(gender_cog_col)    > 0
  
  if (!has_noncog) return(NULL)  # skip outcomes without noncog
  
  # compute gender-specific noncog and cog blups
  re_schools <- re_schools %>%
    mutate(
      noncog_re        = .data[[noncog_col]],
      gender_noncog_re = if (has_gender_noncog) .data[[gender_noncog_col[1]]] else 0,
      nonCog_boys      = vc$beta_noncog_boys  + noncog_re + gender_noncog_re,
      nonCog_girls     = vc$beta_noncog_girls + noncog_re - gender_noncog_re,
      
      cog_re           = if (has_cog)           .data[[cog_col]]              else 0,
      gender_cog_re    = if (has_gender_cog)    .data[[gender_cog_col[1]]]    else 0,
      Cog_boys         = vc$beta_cog_boys  + cog_re + gender_cog_re,
      Cog_girls        = vc$beta_cog_girls + cog_re - gender_cog_re
    )
  
  # pivot noncog to long
  noncog_long <- re_schools %>%
    pivot_longer(
      cols      = c(nonCog_boys, nonCog_girls),
      names_to  = "Gender",
      values_to = "nonCog_blup"
    ) %>%
    mutate(Gender = recode(Gender,
                           "nonCog_boys"  = "Boys",
                           "nonCog_girls" = "Girls"))
  
  # pivot cog to long
  cog_long <- re_schools %>%
    pivot_longer(
      cols      = c(Cog_boys, Cog_girls),
      names_to  = "Gender",
      values_to = "Cog_blup"
    ) %>%
    mutate(Gender = recode(Gender,
                           "Cog_boys"  = "Boys",
                           "Cog_girls" = "Girls"))
  
  # join noncog and cog long frames
  school_blups_long <- noncog_long %>%
    left_join(
      cog_long %>% select(lnr_org, Gender, Cog_blup),
      by = c("lnr_org", "Gender")
    )
  
  list(
    long      = school_blups_long,
    wide      = re_schools,
    intercept = re_schools[["(Intercept)"]],
    label     = label
  )
})
names(blup_data) <- test_outcomes



        ##### 6.2 Gender gaps ######



# combine all gap data into one dataframe
gap_df <- map_dfr(test_outcomes, function(outcome) {
  d  <- blup_data[[outcome]]
  vc <- variance_components[[outcome]]
  if (is.null(d)) return(NULL)
  
  d$wide %>%
    mutate(
      school_gap = vc$mean_observed_gap + kjoenn_g * 2,
      subject    = factor(outcome_test_labels[[outcome]],
                          levels = outcome_test_labels[test_outcomes])
    )
})

y_lim_gap <- range(gap_df$school_gap, na.rm = TRUE)


#plotting gender gap across schools in test scores
test_scores_gender_plot <- ggplot(gap_df, aes(x = 1, y = school_gap)) +
  geom_hline(
    yintercept = pretty(y_lim_gap, n = 5),
    color = "grey92", linewidth = 0.3
  ) +
  geom_violin(fill = "#7B2D8B", alpha = 1,linewidth = 0.3) +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  facet_wrap(~ subject, nrow = 1) +
  labs(
    y = "Gender gap in test score",
    x = NULL
  ) +
  theme_sverdo() +
  theme(
    panel.spacing     = unit(0, "lines"),
    panel.grid        = element_blank(),
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.ticks.y      = element_blank(),
    axis.text.y       = element_text(),
    strip.text = element_text(size = 8,hjust = 0.5)
  )


          ##### 6.3 NonCog-slopes #####

# combine all blup data into one dataframe
blup_df <- map_dfr(test_outcomes, function(outcome) {
  d <- blup_data[[outcome]]
  if (is.null(d)) return(NULL)
  
  d$long %>%
    mutate(subject = factor(outcome_test_labels[[outcome]],
                            levels = outcome_test_labels[test_outcomes]))
})


#change to factor for plotting
blup_df <- blup_df %>%
  mutate(subject = factor(subject))

#change to long format
blup_df_long <- blup_df %>%
  pivot_longer(
    cols      = c(nonCog_blup, Cog_blup),
    names_to  = "PGI",
    values_to = "blup"
  ) %>%
  mutate(
    PGI   = recode(PGI, "nonCog_blup" = "NonCog", "Cog_blup" = "Cog"),   # clean labels
    group = interaction(PGI, Gender, sep = " "),
    group = factor(group, levels = c("NonCog Boys", "NonCog Girls", "Cog Boys", "Cog Girls"))
  )


#plotting variance of PGI-test score associations across schools
test_scores_PGI_variance <- ggplot(blup_df_long, aes(x = group, y = blup, fill = group, pattern = PGI)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  geom_violin_pattern(
    pattern_color   = "white",
    pattern_fill    = "white",
    pattern_density = 0.05,
    pattern_spacing = 0.02,
    pattern_angle   = 45,
    linewidth       = 0.3,
    color           = NA
  ) +
  scale_pattern_manual(
    values = c("NonCog" = "none", "Cog" = "stripe"),
    guide  = "none"
  ) +
  scale_fill_manual(
    values = c(
      "NonCog Boys"  = color_boys,
      "NonCog Girls" = color_girls,
      "Cog Boys"     = color_boys,
      "Cog Girls"    = color_girls
    ),
    guide = "none"
  ) +
  # dummy points for 4-item legend
  geom_point(aes(shape = group), alpha = 0, size = 0) +
  scale_shape_manual(
    values = c(
      "NonCog Boys"  = 22,
      "NonCog Girls" = 22,
      "Cog Boys"     = 22,
      "Cog Girls"    = 22
    ),
    name   = NULL,
    labels = c(
      "NonCog Boys"  = "NonCog Boys",
      "NonCog Girls" = "NonCog Girls",
      "Cog Boys"     = "Cog Boys",
      "Cog Girls"    = "Cog Girls"
    ),
    guide  = guide_legend(
      nrow = 1,
      override.aes = list(
        fill     = c(color_boys, color_girls, color_boys,  color_girls),
        color    = c(color_boys, color_girls, color_boys,  color_girls),
        size     = 3,
        alpha    = 1,
        linetype = c(0, 0, 1, 1)
      ),
      keywidth  = unit(0.5, "cm"),
      keyheight = unit(0.5, "cm")
    )
  ) +
  scale_x_discrete(labels = c(
    "NonCog Boys"  = "Boys",
    "NonCog Girls" = "Girls",
    "Cog Boys"     = "Boys",
    "Cog Girls"    = "Girls"
  )) +
  scale_y_continuous(
    breaks = seq(0, 0.35, by = 0.05),
    limits = c(NA, NA)
  ) +
  facet_wrap2(~ subject, nrow = 1, scales = "fixed", axes = "all") +
  labs(y = "PGI-test score slope", x = NULL) +
  theme_sverdo() +
  theme(
    panel.spacing        = unit(0.5, "lines"),
    panel.grid.major.y   = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y   = element_blank(),
    panel.grid.major.x   = element_blank(),
    strip.text = element_text(size = 8,hjust = 0.5),
    axis.text.x          = element_blank(),
    axis.ticks.x         = element_blank(),
    axis.ticks.y         = element_line(linewidth = 0.3),
    axis.text.y          = element_text(),
    legend.key           = element_rect(color = NA, fill = NA),
    legend.background    = element_rect(color = NA, fill = NA),
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.box           = "horizontal",
    legend.key.size      = unit(0.4, "cm"),
    legend.spacing.x     = unit(0.2, "cm"),
    legend.margin = margin(t = -13, r = 0, b = 0, l = 0),
    plot.margin   = margin(t = 5, r = 5, b = 0, l = 5)
  )





          ##### 6.4 intercept-variance explained #####

r2_df <- map(test_outcomes, function(outcome) {
  d  <- blup_data[[outcome]]
  vc <- variance_components[[outcome]]
  if (is.null(d)) return(NULL)
  
  intercept_vals <- seq(-3, 3, by = 0.01) * vc$sd_intercept
  
  intercept_sd_seq <- seq(-3, 3, by = 0.01)  # in SD units
  
  # noncog prediction lines
  pred_noncog_boys  <- (vc$beta_noncog_boys  + vc$mult_noncog_boys  * intercept_sd_seq)^2
  pred_noncog_girls <- (vc$beta_noncog_girls + vc$mult_noncog_girls * intercept_sd_seq)^2
  
  df_noncog <- data.frame(
    intercept   = rep(intercept_vals, 2),
    expected_r2 = c(pred_noncog_boys, pred_noncog_girls),
    gender      = rep(c("Boys", "Girls"), each = length(intercept_sd_seq)),
    subject     = outcome_test_labels[[outcome]]
  )
  
  # empirical density from intercept blups
  x_density <- seq(-3 * vc$sd_intercept, 3 * vc$sd_intercept, length.out = 200)
  dens      <- density(d$intercept)
  y_dens    <- approx(dens$x, dens$y, xout = x_density)$y
  y_dens[is.na(y_dens)] <- 0
  
  desired_bottom <- 0
  desired_height <- diff(range(c(pred_noncog_boys, pred_noncog_girls), na.rm = TRUE)) * 0.4
  y_dens_scaled  <- desired_bottom + (y_dens / max(y_dens, na.rm = TRUE)) * desired_height
  
  df_density <- data.frame(
    x       = c(x_density, rev(x_density)),
    y       = c(y_dens_scaled, rep(desired_bottom, length(y_dens_scaled))),
    subject = outcome_test_labels[[outcome]]
  )
  
  # cog prediction lines if significant
  df_cog <- NULL
  if (!is.na(vc$has_cog_sig) && vc$has_cog_sig) {
    pred_cog_boys  <- (vc$beta_cog_boys  + vc$mult_cog * intercept_sd_seq)^2
    pred_cog_girls <- (vc$beta_cog_girls + vc$mult_cog * intercept_sd_seq)^2
    
    df_cog <- data.frame(
      intercept   = rep(intercept_vals, 2),
      expected_r2 = c(pred_cog_boys, pred_cog_girls),
      gender      = rep(c("Boys", "Girls"), each = length(intercept_sd_seq)),
      subject     = outcome_test_labels[[outcome]]
    )
  }
  
  list(noncog = df_noncog, density = df_density, cog = df_cog)
})

#creating the DFs
r2_noncog_df  <- map_dfr(r2_df, "noncog")
r2_density_df <- map_dfr(r2_df, "density")
r2_cog_df <- map_dfr(Filter(Negate(is.null), lapply(r2_df, `[[`, "cog")), identity)

# reorder so reading comes first (left)
r2_noncog_df <- r2_noncog_df %>% mutate(subject = factor(subject, levels = c("Reading", "Mathematics")))
r2_cog_df    <- r2_cog_df    %>% mutate(subject = factor(subject, levels = c("Reading", "Mathematics")))
r2_density_df <- r2_density_df %>% mutate(subject = factor(subject, levels = c("Reading", "Mathematics")))

#plotting r squared of PGIs as a function of school-level average test scores
test_scores_int_PGI_plot <- ggplot() +
  geom_polygon(data = r2_density_df,
               aes(x = x, y = y),
               fill = "grey", alpha = 0.7) +
  geom_line(data = r2_noncog_df,
            aes(x = intercept, y = expected_r2,
                color    = paste("NonCog", gender),
                linetype = paste("NonCog", gender)),
            linewidth = 0.5) +
  geom_line(data = r2_cog_df,
            aes(x = intercept, y = expected_r2,
                color    = paste("Cog", gender),
                linetype = paste("Cog", gender)),
            linewidth = 0.5) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "NonCog Boys"  = color_boys,
      "NonCog Girls" = color_girls,
      "Cog Boys"     = color_boys,
      "Cog Girls"    = color_girls
    )
  ) +
  scale_linetype_manual(
    name   = NULL,
    values = c(
      "NonCog Boys"  = "solid",
      "NonCog Girls" = "solid",
      "Cog Boys"     = "dashed",
      "Cog Girls"    = "dashed"
    )
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 0.8,
        linetype  = c("11", "11", "solid", "solid")
      ),
      keywidth  = unit(0.5, "cm"),
      keyheight = unit(0.3, "cm")
    ),
    linetype = "none"
  ) +
  facet_wrap(~ subject, nrow = 1, scales = "fixed") +
  labs(
    x = "Average school test score",
    y = "Variance explained in test score by PGI"
  ) +
  theme_sverdo() +
  theme(
    axis.text.x          = element_text(),
    axis.ticks.x         = element_line(),
    panel.grid.major     = element_line(color = "gray95"),
    panel.grid.minor     = element_blank(),
    legend.position      = c(0.75, 1),
    legend.justification = c("left", "top"),
    strip.text = element_text(size = 8,hjust = 0.5)
  )


          ##### 6.5 combining plots ######

top    <- ggdraw(test_scores_PGI_variance)
bottom <- plot_grid(test_scores_gender_plot, test_scores_int_PGI_plot, nrow = 1,
                    labels = c("b", "c"),
                    label_fontfamily = "serif",
                    label_fontface = "bold",
                    label_size = 12)

tiff("plots/supp_figure2.tiff",
     width = 179,
     height = 160,
     units = "mm",
     res = 600,
     compression = "lzw")

plot_grid(top, NULL, bottom, nrow = 3, rel_heights = c(5, 0.2, 4),
          labels = c("a", "", ""),          # "a" on top, skip spacer and bottom
          label_fontfamily = "serif",
          label_fontface = "bold",
          label_size = 12)

dev.off()



        ##### 6.6 model output table #####

calc_gender_effects <- function(main_term, model) {
  
  coefs    <- fixef(model)
  vcov_mat <- vcov(model)
  df_res   <- df.residual(model)
  t_crit   <- qt(0.975, df_res)
  
  main_pos  <- which(names(coefs) == main_term)
  main_coef <- coefs[main_pos]
  var_main  <- vcov_mat[main_pos, main_pos]
  se_main   <- sqrt(var_main)
  
  # handle gender term separately (no interaction)
  if (main_term == "kjoenn_g") {
    boys_effect  <-  main_coef   # +1 coding
    girls_effect <- -main_coef   # -1 coding
    se_boys      <- se_main
    se_girls     <- se_main
    
    return(data.frame(
      Variable       = main_term,
      Beta_boys      = boys_effect,
      CI_lower_boys  = boys_effect  - t_crit * se_boys,
      CI_upper_boys  = boys_effect  + t_crit * se_boys,
      Beta_girls     = girls_effect,
      CI_lower_girls = girls_effect - t_crit * se_girls,
      CI_upper_girls = girls_effect + t_crit * se_girls,
      interaction_p  = NA
    ))
  }
  
  # all other terms: find interaction and compute gender-specific effects
  interaction_term <- find_interaction(main_term, model)
  int_pos          <- which(names(coefs) == interaction_term)
  int_coef         <- coefs[int_pos]
  
  var_int      <- vcov_mat[int_pos, int_pos]
  cov_main_int <- vcov_mat[main_pos, int_pos]
  
  boys_effect  <- main_coef + int_coef
  girls_effect <- main_coef - int_coef
  se_boys      <- sqrt(var_main + var_int + 2 * cov_main_int)
  se_girls     <- sqrt(var_main + var_int - 2 * cov_main_int)
  
  se_interaction <- sqrt(vcov_mat[int_pos, int_pos])
  t_interaction  <- int_coef / se_interaction
  p_interaction  <- 2 * pt(abs(t_interaction), df_res, lower.tail = FALSE)
  
  data.frame(
    Variable       = main_term,
    Beta_boys      = boys_effect,
    CI_lower_boys  = boys_effect  - t_crit * se_boys,
    CI_upper_boys  = boys_effect  + t_crit * se_boys,
    Beta_girls     = girls_effect,
    CI_lower_girls = girls_effect - t_crit * se_girls,
    CI_upper_girls = girls_effect + t_crit * se_girls,
    interaction_p  = p_interaction
  )
}

# update find_interaction to also take a model argument
find_interaction <- function(main_term, model) {
  coefs     <- fixef(model)
  int_term1 <- paste0(main_term, ":kjoenn_g")
  int_term2 <- paste0("kjoenn_g:", main_term)
  
  if (int_term1 %in% names(coefs)) {
    return(int_term1)
  } else if (int_term2 %in% names(coefs)) {
    return(int_term2)
  } else {
    stop(paste("could not find interaction term for", main_term))
  }
}

predictors    <- c("cog_g", "noncog_g", "cog_parental_g", "noncog_parental_g","kjoenn_g")
fixed_effects <- data.frame()

for (pred in predictors) {
  results       <- calc_gender_effects(pred, model = results_best$NT_reading)
  fixed_effects <- rbind(fixed_effects, results)
}

fixed_effects_NT_reading <- fixed_effects |>
  mutate(
    across(c(Beta_boys, CI_lower_boys, CI_upper_boys,
             Beta_girls, CI_lower_girls, CI_upper_girls), \(x) round(x, 2)),
    interaction_p = round(interaction_p, 3),
    boys_ci  = paste0(Beta_boys,  " (", CI_lower_boys,  ", ", CI_upper_boys,  ")"),
    girls_ci = paste0(Beta_girls, " (", CI_lower_girls, ", ", CI_upper_girls, ")"),
    interaction_p = ifelse(is.na(interaction_p), "-", as.character(interaction_p))
  ) |>
  select(Variable, boys_ci, girls_ci, interaction_p)

VarCorr(results_best$NT_reading)


fixed_effects <- data.frame()

for (pred in predictors) {
  results       <- calc_gender_effects(pred, model = results_best$NT_maths)
  fixed_effects <- rbind(fixed_effects, results)
}

fixed_effects_NT_maths <- fixed_effects |>
  mutate(
    across(c(Beta_boys, CI_lower_boys, CI_upper_boys,
             Beta_girls, CI_lower_girls, CI_upper_girls), \(x) round(x, 2)),
    interaction_p = round(interaction_p, 3),
    boys_ci  = paste0(Beta_boys,  " (", CI_lower_boys,  ", ", CI_upper_boys,  ")"),
    girls_ci = paste0(Beta_girls, " (", CI_lower_girls, ", ", CI_upper_girls, ")"),
    interaction_p = ifelse(is.na(interaction_p), "-", as.character(interaction_p))
  ) |>
  select(Variable, boys_ci, girls_ci, interaction_p)
