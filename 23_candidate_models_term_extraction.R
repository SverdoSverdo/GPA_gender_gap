#### 1. QUADRATIC TERMS EXTRACTION ####

          ##### 1.1 obtaining model output #####

load("N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/candidate_models_sq.Rdata")

# function to extract output for each model
extract_lmer_info <- function(model) {
  
  # get the fixed effects for each model and extracting parameters
  tidy_output <- tidy(model, effects = "fixed", conf.int = TRUE)
  new_data <- tidy_output[, c("term", "estimate","std.error","conf.low","conf.high","p.value")]
  
  new_data$model <- i
  
  # access and update the model info
  full_model_info <<- rbind(full_model_info, new_data)
}

# initializing df
full_model_info <- data.frame()

# running function
for(i in names(candidate_models_sq)) {extract_lmer_info(candidate_models_sq[[i]])}

# removing estimates that are not of interest
model_info <- full_model_info[!grepl("parental|school_middle|(Intercept)", full_model_info$term, ignore.case = TRUE), ]
model_info <- select(model_info, -c(conf.low,conf.high))


          ##### 1.2 getting variance and covariances for interaction with gender #####

# variance of terms
model_info$var <- model_info$std.error^2

# changing all gender variables to the same string
model_info$term <- gsub("kjoenn_g|kjoenn_gy|kjoenn_gc|kjoenn_cg", "kjoenn", model_info$term)

# these are the terms we want to calculate gender-specific CIs for
linear_terms <-c(
  "classroom_gender_gy",
  "class_grades_gy",
  "ext_behavior_m",
  "pos_climate_m",
  "school_income_m",
  "school_student_gender_m",
  "school_teacher_edu_m",
  "school_teacher_gender_m",
  "school_turnover_m"
)

#function that aligns terms with their higher order interaction
model_info <- model_info %>%
  mutate(
    interaction = map_chr(seq_along(term), function(i) {
      x <- term[i]
      # check for three-way interactions
      if(str_count(x, ":") == 2 & str_detect(x, "kjoenn") & str_detect(x, "(cog_g|noncog_g)")) {
        # extract the PGI variables
        genetic_var <- str_extract(x, "(cog_g|noncog_g)")
        # split the term by colons
        parts <- str_split(x, ":")[[1]]
        # find the environmental variable
        env_var <- parts[!parts %in% c("kjoenn", genetic_var)]
        
        if(length(env_var) > 0) {
          #interaction terms can be written in both orders: GxE and ExG
          option1 <- paste0(env_var, ":", genetic_var)
          option2 <- paste0(genetic_var, ":", env_var)
          # check which one exists in the term column
          if(option1 %in% model_info$term) {
            return(option1)
          } else if(option2 %in% model_info$term) {
            return(option2)
          } else {
            return(NA_character_)
          }
        }
      }
      # check for two-way interactions with gender
      else if(str_count(x, ":") == 1 & str_detect(x, "kjoenn")) {
        # split by colon and extract the part that's not gender
        parts <- str_split(x, ":")[[1]]
        env_var <- parts[parts != "kjoenn"]
        
        if(length(env_var) > 0) {
          return(env_var)
        }
      }
      
      return(NA_character_)
    })
  )

# function that aligns the variances between the interactions
model_info <- model_info %>%
  mutate(
    interaction_var = map_dbl(interaction, function(x) {
      if(!is.na(x)) {
        # find the matching term in the original data
        match_idx <- which(model_info$term == x)
        if(length(match_idx) > 0) {
          return(model_info$var[match_idx[1]])
        }
      }
      return(NA_real_)
    }),
    interaction_estimate = map_dbl(interaction, function(x) {
      if(!is.na(x)) {
        # find the matching term in the original data
        match_idx <- which(model_info$term == x)
        if(length(match_idx) > 0) {
          return(model_info$estimate[match_idx[1]])
        }
      }
      return(NA_real_)
    })
  )


          ##### 1.3 adding covariance between terms #####

# create vcov matrices for each model
for(model_name in unique(model_info$model)) {
  name_vcov <- paste0("vcov_", model_name)
  assign(name_vcov, vcov(candidate_models_sq[[model_name]]))
}

# get all vcov matrices in environment
vcov_names <- ls(pattern = "^vcov_")

# loop through each vcov matrix and standardize the naming
for(vcov_name in vcov_names) {
  # get the vcov matrix
  vcov_matrix <- get(vcov_name)
  # replace all gender variants in row and column names
  rownames(vcov_matrix) <- str_replace_all(rownames(vcov_matrix), "kjoenn_(gy|g|cg)", "kjoenn")
  colnames(vcov_matrix) <- str_replace_all(colnames(vcov_matrix), "kjoenn_(gy|g|cg)", "kjoenn")
  # assign the modified matrix back
  assign(vcov_name, vcov_matrix, envir = .GlobalEnv)
}


# adding the covariances to the DF
model_info <- model_info %>%
  mutate(
    cov = pmap_dbl(list(term, interaction, model), function(t, i, m) {
      # skip if interaction is NA
      if(is.na(i)) {
        return(NA_real_)
      }
      # construct the vcov matrix name
      vcov_name <- paste0("vcov_", m)
      # check if this vcov exists
      if(!exists(vcov_name)) {
        return(NA_real_)
      }
      # get the vcov matrix
      vcov_matrix <- get(vcov_name)
      # get the covariance
      if(t %in% rownames(vcov_matrix) && i %in% rownames(vcov_matrix)) {
        return(vcov_matrix[t, i])
      } else {
        return(NA_real_)
      }
    })
  )


          ##### 2.4  calculating gender-specific CIs #####

# adding gender-interactions to get gender-specific effects
model_info$estimate_boys <- model_info$interaction_estimate + model_info$estimate
model_info$estimate_girls <- model_info$interaction_estimate - model_info$estimate

# gender-specific SEs
model_info$se_boys <- sqrt(model_info$var + model_info$interaction_var + 2*model_info$cov)
model_info$se_girls <- sqrt(model_info$var + model_info$interaction_var - 2*model_info$cov)

# 95% CIs
model_info$ci.upper_boys <- model_info$estimate_boys + model_info$se_boys*1.96
model_info$ci.lower_boys <- model_info$estimate_boys - model_info$se_boys*1.96

model_info$ci.upper_girls <- model_info$estimate_girls + model_info$se_girls*1.96
model_info$ci.lower_girls <- model_info$estimate_girls - model_info$se_girls*1.96


          ##### 2.5 making final table #####

table_squared <- select(model_info, c(model,term, interaction, estimate,estimate_boys,estimate_girls,
                                      ci.upper_boys, ci.lower_boys, ci.upper_girls, ci.lower_girls, p.value))


# merging back with the DF with all estimates, to retrieve CIs for non-gender estimates
non_gender_CIs <- select(full_model_info, c(model,term,estimate,conf.low,conf.high))
names(non_gender_CIs)[3:5] <- c("estimate_all","ci.lower","ci.upper")

# table with average effects and gender-specific effects of all estimates
table_squared <- left_join(table_squared, non_gender_CIs, by = c("model" = "model", "interaction" = "term"))

# short-hand names for the candidate environment
converting_names <- data.frame(plot_name = c(
  "Classroom gender",
  "Classroom GPA",
  "Ext.behavior",
  "Positive school climate",
  "School SES",
  "School gender",
  "Teacher EA",
  "teacher gender",
  "Teacher turnover"),
  
  variable_name = c(
    "classroom_gender_gy",
    "class_grades_gy",
    "ext_behavior_m",
    "pos_climate_m",
    "school_income_m",
    "school_student_gender_m",
    "school_teacher_edu_m",
    "school_teacher_gender_m",
    "school_turnover_m"
  ))

name_lookup <- setNames(converting_names$plot_name, converting_names$variable_name)

# cleaning up
table_squared <- table_squared %>%
  mutate(interaction = str_replace_all(interaction, name_lookup))

table_squared <- table_squared %>%
  mutate(interaction = str_replace(interaction, "I\\((.+?)\\^2\\)", "\\1^2"))

table_squared <- table_squared %>%
  mutate(interaction = str_replace_all(interaction, c(
    "noncog_g" = "NonCog",
    "kjoenn" = "Gender",
    "cog_g" = "Cog",
    ":" = " x "
  )))


# rearranging the interactions so that the candidate environment comes first
table_squared <- table_squared %>%
  mutate(interaction = case_when(
    
    # triple interactions
    str_count(interaction, " x ") == 2 ~ {
      parts <- str_split(interaction, " x ")
      
      sapply(parts, function(p) {
        # find indices
        gender_idx <- which(p == "Gender")
        # check for both regular and squared versions
        plot_name_idx <- which(p %in% converting_names$plot_name | 
                                 p %in% paste0(converting_names$plot_name, "^2"))
        
        if (length(plot_name_idx) > 0) {
          # plot_name interaction first, other interaction second, gender last
          other_idx <- setdiff(1:3, c(gender_idx, plot_name_idx))
          c(p[plot_name_idx[1]], p[other_idx], p[gender_idx]) %>%
            paste(collapse = " x ")
        } else {
          # just put Gender last if no plot_name match
          c(p[-gender_idx], p[gender_idx]) %>%
            paste(collapse = " x ")
        }
      })
    },
    # double interactions
    str_count(interaction, " x ") == 1 ~ {
      parts <- str_split(interaction, " x ")
      
      sapply(parts, function(p) {
        # check for both regular and squared versions
        plot_name_interactions <- c(converting_names$plot_name, paste0(converting_names$plot_name, "^2"))
        
        if (p[2] %in% plot_name_interactions) {
          # swap to put plot_name interaction first
          paste(p[2], p[1], sep = " x ")
        } else if (p[1] %in% plot_name_interactions) {
          # already in correct order
          paste(p, collapse = " x ")
        } else {
          # keep as is
          paste(p, collapse = " x ")
        }
      })
    },
    
    TRUE ~ interaction  # keep all other interactions unchanged
  ))


# removing these terms
table_squared <- table_squared %>%
  filter(!interaction %in% c("NonCog", "Gender", "Cog","NonCog x Gender", "Gender x Cog"))

# removing last unnecessary terms
table_squared <- table_squared[!is.na(table_squared$interaction),]

table_squared_final <- table_squared %>%
  # rounding to 3 decimal places
  mutate(across(4:ncol(.), ~ round(., 3))) %>%
  # change p-values of 0 to "<.001"
  mutate(p.value = ifelse(p.value == 0, "<.001", as.character(p.value))) %>%
  
  # remove leading zeros execpt for p.values
  mutate(across(c(4:ncol(.))[!names(.)[4:ncol(.)] %in% "p.value"], 
                ~ gsub("^0\\.", ".", as.character(.)))) %>%
  
  # create combined columns for average, boys, and girls estimates
  mutate(
    estimate_all_ci = paste0(estimate_all, " (", ci.lower, ", ", ci.upper, ")"),
    estimate_boys_ci = paste0(estimate_boys, " (", ci.lower_boys, ", ", ci.upper_boys, ")"),
    estimate_girls_ci = paste0(estimate_girls, " (", ci.lower_girls, ", ", ci.upper_girls, ")")
  ) %>%
  # keep only first instance of repeating values in model column
  mutate(model = ifelse(duplicated(model), "", model)) %>%
  # select columns in desired order
  select(model, interaction, estimate_all_ci, estimate_boys_ci, estimate_girls_ci, p.value)


save(table_squared_final, file = "tables/table_squared_final.RData")

          ##### 2.6 preparing plots  #####

#4 squared terms show signifiance below the traditional <.05 level
(significant_sqr_terms <- model_info %>%
   filter(str_detect(term, "I\\(") & p.value < 0.05) %>%
   pull(c(term)))


# extracting fixed effect of gender. basically the same in all models: from -.281 to -2.9
beta_gender <- model_info[model_info$term == "kjoenn",]
beta_gender <- mean(beta_gender$estimate)


          ##### 2.7 plotting school income ####

# create a sequence of school income values across the range
school_income_range <- seq(-3, 3, by = 0.1)

# create predictions for boys
pred_boys <- data.frame(
  school_income = school_income_range,
  predicted_gpa = 
    table_squared$estimate_boys[table_squared$interaction == "School SES"] * school_income_range +
    table_squared$estimate_boys[table_squared$interaction == "School SES^2"] * school_income_range^2,
  linear_pred = table_squared$estimate_boys[table_squared$interaction == "School SES"] * school_income_range,
  gender = "Boys"
)

# create predictions for girls
pred_girls <- data.frame(
  school_income = school_income_range,
  predicted_gpa = 
    table_squared$estimate_girls[table_squared$interaction == "School SES"] * school_income_range +
    table_squared$estimate_girls[table_squared$interaction == "School SES^2"] * school_income_range^2,
  linear_pred = table_squared$estimate_girls[table_squared$interaction == "School SES"] * school_income_range,
  gender = "Girls"
)

# combine both datasets
pred_data_school_income <- rbind(pred_boys, pred_girls)

# create the plot
school_income_plot <- ggplot(pred_data_school_income, aes(x = school_income, y = predicted_gpa, color = gender)) +
  geom_line(size = .5) +
  geom_line(aes(y = linear_pred, linetype = "Linear associations"), size = .5, alpha = 0.6) +
  scale_color_manual(values = c("Boys" = color_boys, "Girls" = color_girls), name = "") +
  scale_linetype_manual(values = c("Linear associations" = "dotted"), name = "")+
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = "solid"),keywidth = unit(.9, "cm")),
    linetype = guide_legend(order = 2, 
                            override.aes = list(color = "black"),
                            keywidth = unit(1, "cm"), 
                            label.theme = element_text(margin = margin(l = -2.5), 
                                                       size = 6.2, 
                                                       family = "serif"))
  ) +                    
  labs(
    x = "School-level SES",
    y = "Expected GPA",
    title = ""
  ) +
  theme_sverdo() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.20, 0.85),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major = element_line(color = "grey95", size = 0.3),
    legend.spacing.y = unit(-0.29, "cm"),
  ) +
  scale_x_continuous(breaks = seq(-3, 3, by = 1)) +
  scale_y_continuous(limits = c(-0.4, 0.8), breaks = seq(-0.4, 0.8, by = 0.2))


          ##### 2.8 plotting teacher educational attainment #####


# create a sequence of school income values across the range
teacher_edu_range <- seq(-3, 3, by = 0.1)

# create predictions for boys
pred_boys <- data.frame(
  teacher_edu = teacher_edu_range,
  predicted_gpa = 
    table_squared$estimate_boys[table_squared$interaction == "Teacher EA"] * teacher_edu_range +
    table_squared$estimate_boys[table_squared$interaction == "Teacher EA^2"] * teacher_edu_range^2,
  linear_pred = table_squared$estimate_boys[table_squared$interaction == "Teacher EA"] * teacher_edu_range,
  gender = "Boys"
)

# create predictions for girls
pred_girls <- data.frame(
  teacher_edu = teacher_edu_range,
  predicted_gpa = 
    table_squared$estimate_girls[table_squared$interaction == "Teacher EA"] * teacher_edu_range +
    table_squared$estimate_girls[table_squared$interaction == "Teacher EA^2"] * teacher_edu_range^2,
  linear_pred = table_squared$estimate_girls[table_squared$interaction == "Teacher EA"] * teacher_edu_range,
  gender = "Girls"
)

# combine both datasets
pred_data_teacher_edu <- rbind(pred_boys, pred_girls)

# create the plot
teacher_edu_plot <- ggplot(pred_data_teacher_edu, aes(x = teacher_edu, y = predicted_gpa, color = gender)) +
  geom_line(size = .5) +
  geom_line(aes(y = linear_pred, linetype = "Linear associations"), size = .5, alpha = 0.6) +
  scale_color_manual(values = c("Boys" = color_boys, "Girls" = color_girls), name = "") +
  scale_linetype_manual(values = c("Linear associations" = "dotted"), name = "")+
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = "solid"),keywidth = unit(.9, "cm")),
    linetype = guide_legend(order = 2, 
                            override.aes = list(color = "black"),
                            keywidth = unit(1, "cm"), 
                            label.theme = element_text(margin = margin(l = -2.5), 
                                                       size = 6.2, 
                                                       family = "serif"))
  ) +                    
  labs(
    x = "Teacher educational attainment",
    y = "Expected GPA",
    title = ""
  ) +
  theme_sverdo() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.20, 0.85),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major = element_line(color = "grey95", size = 0.3),
    legend.spacing.y = unit(-0.29, "cm"),
  ) +
  scale_x_continuous(breaks = seq(-3, 3, by = 1)) +
  scale_y_continuous(limits = c(-0.4, 0.8), breaks = seq(-0.4, 0.8, by = 0.2))


tiff("plots/income_teacher_edu_sq.tiff", 
     width = 180, 
     height = 90,  
     units = "mm", 
     res = 600,
     compression = "lzw")


grid.arrange(
  school_income_plot, teacher_edu_plot,
  ncol = 2
)

dev.off()


        #### 2. LINEAR TERMS EXTRACTION ####


          ##### 2.1 obtaining model output #####

load("N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/candidate_models.Rdata")


# function to extract output for each model
extract_lmer_info <- function(model) {
  
  # fixed effects for each model and extracting parameters
  tidy_output <- tidy(model, effects = "fixed", conf.int = TRUE)
  new_data <- tidy_output[, c("term", "estimate","std.error","conf.low","conf.high","p.value")]
  
  new_data$model <- i
  
  # access and update the model info
  full_model_info <<- rbind(full_model_info, new_data)
}

# initializing df
full_model_info <- data.frame()

# running function
for(i in names(candidate_models)) {extract_lmer_info(candidate_models[[i]])}

# removing estimates that are not of interest
model_info <- full_model_info[!grepl("parental|school_middle|(Intercept)", full_model_info$term, ignore.case = TRUE), ]
model_info <- select(model_info, -c(conf.low,conf.high))


          ##### 2.2 getting variance and covariances for interaction with gender #####


# variance of terms
model_info$var <- model_info$std.error^2

# changing all gender variables to the same string
model_info$term <- gsub("kjoenn_g|kjoenn_gy|kjoenn_gc|kjoenn_cg", "kjoenn", model_info$term)

# these are the terms we want to calculate gender-specific CIs for
linear_terms <-c(
  "classroom_gender_gy",
  "class_grades_gy",
  "ext_behavior_m",
  "pos_climate_m",
  "school_income_m",
  "school_student_gender_m",
  "school_teacher_edu_m",
  "school_teacher_gender_m",
  "school_turnover_m"
)

# function that aligns terms with their higher order interaction
model_info <- model_info %>%
  mutate(
    interaction = map_chr(seq_along(term), function(i) {
      x <- term[i]
      
      # Check for three-way interactions
      if(str_count(x, ":") == 2 & str_detect(x, "kjoenn") & str_detect(x, "(cog_g|noncog_g)")) {
        # extract the PGI variables
        genetic_var <- str_extract(x, "(cog_g|noncog_g)")
        
        # Split the term by colons
        parts <- str_split(x, ":")[[1]]
        
        # Find the environmental variable
        env_var <- parts[!parts %in% c("kjoenn", genetic_var)]
        
        if(length(env_var) > 0) {
          #interaction terms can be written in both orders: GxE and ExG
          option1 <- paste0(env_var, ":", genetic_var)
          option2 <- paste0(genetic_var, ":", env_var)
          # check which one exists in the term column
          if(option1 %in% model_info$term) {
            return(option1)
          } else if(option2 %in% model_info$term) {
            return(option2)
          } else {
            return(NA_character_)
          }
        }
      }
      # check for two-way interactions with gender
      else if(str_count(x, ":") == 1 & str_detect(x, "kjoenn")) {
        # split by colon and extract the part that's not gender
        parts <- str_split(x, ":")[[1]]
        env_var <- parts[parts != "kjoenn"]
        
        if(length(env_var) > 0) {
          return(env_var)
        }
      }
      
      return(NA_character_)
    })
  )

# function that aligns the variances between the interactions
model_info <- model_info %>%
  mutate(
    interaction_var = map_dbl(interaction, function(x) {
      if(!is.na(x)) {
        # find the matching term in the original data
        match_idx <- which(model_info$term == x)
        if(length(match_idx) > 0) {
          return(model_info$var[match_idx[1]])
        }
      }
      return(NA_real_)
    }),
    interaction_estimate = map_dbl(interaction, function(x) {
      if(!is.na(x)) {
        # find the matching term in the original data
        match_idx <- which(model_info$term == x)
        if(length(match_idx) > 0) {
          return(model_info$estimate[match_idx[1]])
        }
      }
      return(NA_real_)
    })
  )


          ##### 2.3 adding covariance between terms #####

# vcov matrices for each model
for(model_name in unique(model_info$model)) {
  name_vcov <- paste0("vcov_", model_name)
  assign(name_vcov, vcov(candidate_models[[model_name]]))
}

# get all vcov matrices in environment
vcov_names <- ls(pattern = "^vcov_")

# loop through each vcov matrix and standardize the naming
for(vcov_name in vcov_names) {
  # get the vcov matrix
  vcov_matrix <- get(vcov_name)
  # replace all gender variants in row and column names
  rownames(vcov_matrix) <- str_replace_all(rownames(vcov_matrix), "kjoenn_(gy|g|cg)", "kjoenn")
  colnames(vcov_matrix) <- str_replace_all(colnames(vcov_matrix), "kjoenn_(gy|g|cg)", "kjoenn")
  # assign the modified matrix back
  assign(vcov_name, vcov_matrix, envir = .GlobalEnv)
}


# adding the covariances to the DF
model_info <- model_info %>%
  mutate(
    cov = pmap_dbl(list(term, interaction, model), function(t, i, m) {
      # skip if interaction is NA
      if(is.na(i)) {
        return(NA_real_)
      }
      # construct the vcov matrix name
      vcov_name <- paste0("vcov_", m)
      # Check if this vcov exists
      if(!exists(vcov_name)) {
        return(NA_real_)
      }
      # get the vcov matrix
      vcov_matrix <- get(vcov_name)
      # get the covariance
      if(t %in% rownames(vcov_matrix) && i %in% rownames(vcov_matrix)) {
        return(vcov_matrix[t, i])
      } else {
        return(NA_real_)
      }
    })
  )


          ##### 2.4  calculating gender-specific CIs #####

# adding gender-interactions to get gender-specific effects
model_info$estimate_boys <- model_info$interaction_estimate + model_info$estimate
model_info$estimate_girls <- model_info$interaction_estimate - model_info$estimate

# gender-specific SEs
model_info$se_boys <- sqrt(model_info$var + model_info$interaction_var + 2*model_info$cov)
model_info$se_girls <- sqrt(model_info$var + model_info$interaction_var - 2*model_info$cov)

# 95% CIs
model_info$ci.upper_boys <- model_info$estimate_boys + model_info$se_boys*1.96
model_info$ci.lower_boys <- model_info$estimate_boys - model_info$se_boys*1.96

model_info$ci.upper_girls <- model_info$estimate_girls + model_info$se_girls*1.96
model_info$ci.lower_girls <- model_info$estimate_girls - model_info$se_girls*1.96




          ##### 2.5 making final table #####

table_linear <- select(model_info, c(model,term, interaction, estimate,estimate_boys,estimate_girls,
                                     ci.upper_boys, ci.lower_boys, ci.upper_girls, ci.lower_girls, p.value))


# merging back with the DF with all estimates, to retrieve CIs for non-gender estimates
non_gender_CIs <- select(full_model_info, c(model,term,estimate,conf.low,conf.high))
names(non_gender_CIs)[3:5] <- c("estimate_all","ci.lower","ci.upper")

# table with average effects and gender-specific effects of all estimates
table_linear <- left_join(table_linear, non_gender_CIs, by = c("model" = "model", "interaction" = "term"))

# short-hand names for the candidate environment
converting_names <- data.frame(plot_name = c(
  "Classroom gender",
  "Classroom GPA",
  "Ext.behavior",
  "Positive school climate",
  "School SES",
  "School gender",
  "Teacher EA",
  "teacher gender",
  "Teacher turnover"),
  
  variable_name = c(
    "classroom_gender_gy",
    "class_grades_gy",
    "ext_behavior_m",
    "pos_climate_m",
    "school_income_m",
    "school_student_gender_m",
    "school_teacher_edu_m",
    "school_teacher_gender_m",
    "school_turnover_m"
  ))


name_lookup <- setNames(converting_names$plot_name, converting_names$variable_name)

# cleaning up
table_linear <- table_linear %>%
  mutate(interaction = str_replace_all(interaction, name_lookup))

table_linear <- table_linear %>%
  mutate(interaction = str_replace(interaction, "I\\((.+?)\\^2\\)", "\\1^2"))

table_linear <- table_linear %>%
  mutate(interaction = str_replace_all(interaction, c(
    "noncog_g" = "NonCog",
    "kjoenn" = "Gender",
    "cog_g" = "Cog",
    ":" = " x "
  )))


# rearranging the interactions so that the candidate environment comes first
table_linear <- table_linear %>%
  mutate(interaction = case_when(
    # triple interactions
    str_count(interaction, " x ") == 2 ~ {
      parts <- str_split(interaction, " x ")
      
      sapply(parts, function(p) {
        # find indices
        gender_idx <- which(p == "Gender")
        # check for both regular and squared versions
        plot_name_idx <- which(p %in% converting_names$plot_name | 
                                 p %in% paste0(converting_names$plot_name, "^2"))
        
        if (length(plot_name_idx) > 0) {
          # plot_name interaction first, other interaction second, gender last
          other_idx <- setdiff(1:3, c(gender_idx, plot_name_idx))
          c(p[plot_name_idx[1]], p[other_idx], p[gender_idx]) %>%
            paste(collapse = " x ")
        } else {
          # put gender last if no plot_name match
          c(p[-gender_idx], p[gender_idx]) %>%
            paste(collapse = " x ")
        }
      })
    },
    
    # double interactions
    str_count(interaction, " x ") == 1 ~ {
      parts <- str_split(interaction, " x ")
      
      sapply(parts, function(p) {
        # check for both regular and squared versions
        plot_name_interactions <- c(converting_names$plot_name, paste0(converting_names$plot_name, "^2"))
        
        if (p[2] %in% plot_name_interactions) {
          # swap to put plot_name interaction first
          paste(p[2], p[1], sep = " x ")
        } else if (p[1] %in% plot_name_interactions) {
          # already in correct order
          paste(p, collapse = " x ")
        } else {
          # keep as is
          paste(p, collapse = " x ")
        }
      })
    },
    
    TRUE ~ interaction  # Keep all other interactions unchanged
  ))


# removing these
table_linear <- table_linear %>%
  filter(!interaction %in% c("NonCog", "Gender", "Cog","NonCog x Gender", "Gender x Cog"))

# removing last unnecessary terms
table_linear <- table_linear[!is.na(table_linear$interaction),]

table_linear_final <- table_linear %>%
  # rounding to 3 decimal places
  mutate(across(4:ncol(.), ~ round(., 3))) %>%
  # change p-values of 0 to "<.001"
  mutate(p.value = ifelse(p.value == 0, "<.001", as.character(p.value))) %>%
  # remove leading zeros execpt for p.values
  mutate(across(c(4:ncol(.))[!names(.)[4:ncol(.)] %in% "p.value"], 
                ~ gsub("^0\\.", ".", as.character(.)))) %>%
  
  # create combined columns for average, boys, and girls estimates
  mutate(
    estimate_all_ci = paste0(estimate_all, " (", ci.lower, ", ", ci.upper, ")"),
    estimate_boys_ci = paste0(estimate_boys, " (", ci.lower_boys, ", ", ci.upper_boys, ")"),
    estimate_girls_ci = paste0(estimate_girls, " (", ci.lower_girls, ", ", ci.upper_girls, ")")
  ) %>%
  
  # keep only first instance of repeating values in model column
  mutate(model = ifelse(duplicated(model), "", model)) %>%
  # select columns in desired order
  select(model, interaction, estimate_all_ci, estimate_boys_ci, estimate_girls_ci, p.value)


save(table_linear_final, file = "tables/table_linear_final.RData")
