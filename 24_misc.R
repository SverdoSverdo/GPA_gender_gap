        #### 0. LOADING DATA AND PACKAGES ####

source("00_settings.R")

data <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")


        #### 1. GENDER-SPECIFIC ICCs FROM NULL MODELS ####

# gender-specific DFs
data_boys <- data[data$kjoenn == 1,]
data_girls <- data[data$kjoenn == -1,]

# gender-specific null models
null_boys <- lmer(grades_std ~ 1 +  (1|lnr_org), data=data_boys)
null_girls <- lmer(grades_std ~ 1 +   (1|lnr_org), data=data_girls)

# define ICC calculation function
calc.icc <- function(model) {
  var_components <- VarCorr(model)
  sigma_between <- as.numeric(var_components[[1]][1])  # Between-group variance
  sigma_within <- attr(var_components, "sc")^2         # Within-group variance
  
  total_variance <- sigma_between + sigma_within
  icc <- sigma_between / total_variance
  
  return(icc)
}

# estimated gender-specific ICCs
icc_boys <- calc.icc(null_boys)
icc_girls <- calc.icc(null_girls)


# boostrapping CIs
set.seed(1337)

boot.icc_boys <- bootMer(null_boys, calc.icc, nsim = 20000)
boot.icc_girls <- bootMer(null_girls, calc.icc, nsim = 20000)

# extract 95% CIs
boys_ci <- c(quantile(boot.icc_boys$t, 0.025),
             icc_boys,
             quantile(boot.icc_boys$t, 0.975))

girls_ci <- c(quantile(boot.icc_girls$t, 0.025),
              icc_girls,
              quantile(boot.icc_girls$t, 0.975))


        #### 2. rGE AFTER PARENTAL CONTROLS ####

res_data <- data

# create row ID for safe merges
res_data$row_number <- seq_len(nrow(res_data))

# residualize noncog on parental
res_data$noncog_res <- resid(lm(noncog ~ noncog_parental, data = res_data))

# residualize cog on parental
res_data$cog_res <- resid(lm(cog ~ cog_parental, data = res_data))


          ##### 2.1 random effect models #####

icc_noncog <- lmer(noncog ~ 1 +  (1| lnr_org), data = res_data)
icc(icc_noncog)

icc_noncog_res <- lmer(noncog_res ~ 1 + (1| lnr_org), data = res_data)

icc_cog <- lmer(cog ~ 1 +  (1| lnr_org), data = res_data)
icc(icc_cog)


          ##### 2.2 candidate environments #####

# indexing the school-level variables for the subsequent loop
env_variables <- c("school_income_m", "classroom_gender_gy","class_grades_gy",
                   "school_teacher_edu_m","school_teacher_gender_m","school_turnover_m",
                   "school_student_gender_m","pos_climate_m","ext_behavior_m")

# initiating DF
rGE <- data.frame(variable = env_variables)

for(i in 1:9) {
  # extracting the environmental model
  variable <- env_variables[i]
  
  #NonCog-school cor with parental controls
  cor_test <- cor.test(res_data$noncog_res, res_data[[variable]])
  rGE$noncog_p[i] <- cor_test$estimate
  rGE$noncog_p_ci_lower[i] <- cor_test$conf.int[1]
  rGE$noncog_p_ci_upper[i] <- cor_test$conf.int[2]
   
   #Cog-school cor with parental controls
   cor_test <- cor.test(res_data$cog_res, res_data[[variable]])
   rGE$cog_p[i] <- cor_test$estimate
   rGE$cog_p_ci_lower[i] <- cor_test$conf.int[1]
   rGE$cog_p_ci_upper[i] <- cor_test$conf.int[2]
   
   #NonCog-school cor without parental controls
   cor_test <- cor.test(res_data$noncog, res_data[[variable]])
   rGE$noncog[i] <- cor_test$estimate
   rGE$noncog_ci_lower[i] <- cor_test$conf.int[1]
   rGE$noncog_ci_upper[i] <- cor_test$conf.int[2]

     #NonCog-school cor without parental controls
   cor_test <- cor.test(res_data$cog, res_data[[variable]])
   rGE$cog[i] <- cor_test$estimate
   rGE$cog_ci_lower[i] <- cor_test$conf.int[1]
   rGE$cog_ci_upper[i] <- cor_test$conf.int[2]

}

          ##### 2.3 plot #####

# for nice plotting names:)
rGE$variable <- c("School-level SES","Classroom-level proportion of Girls","Classroom-level GPA","Teacher educational attainment",   
                     "Teacher female proportion", "Teacher turnover", "School-level proportion of girls", "Positive school climate",          
                     "Externalizing behavior" )

# add a tiny bit to teacher turnover (basically zero) so that it shows up on the plot
rGE_plot <- rGE
rGE_plot$cog_p[rGE$variable == "Teacher Turnover"] <-rGE_plot$cog_p[rGE_plot$variable == "Teacher Turnover"] +.001

# reshape data to long format with estimates and CIs
rGE_long <- rGE_plot %>%
  pivot_longer(cols = c(noncog, noncog_p, cog, cog_p),
               names_to = "estimate_type",
               values_to = "estimate") %>%
  mutate(
    ci_lower = case_when(
      estimate_type == "noncog" ~ noncog_ci_lower,
      estimate_type == "noncog_p" ~ noncog_p_ci_lower,
      estimate_type == "cog" ~ cog_ci_lower,
      estimate_type == "cog_p" ~ cog_p_ci_lower
    ),
    ci_upper = case_when(
      estimate_type == "noncog" ~ noncog_ci_upper,
      estimate_type == "noncog_p" ~ noncog_p_ci_upper,
      estimate_type == "cog" ~ cog_ci_upper,
      estimate_type == "cog_p" ~ cog_p_ci_upper
    ),
    # create position variable for manual spacing
    position = case_when(
      estimate_type == "noncog" ~ 1,
      estimate_type == "noncog_p" ~ 2,
      estimate_type == "cog" ~ 3.5,
      estimate_type == "cog_p" ~ 4.5
    ),
    # create better labels
    estimate_type = factor(estimate_type, 
                           levels = c("noncog", "noncog_p", "cog", "cog_p"),
                           labels = c("NonCog-PGI", 
                                      "NonCog-PGI with parental PGIs",
                                      "Cog-PGI",
                                      "Cog-PGI with parental PGIs"))
  )


rGE_plot <- ggplot(rGE_long, aes(x = position, y = estimate, fill = estimate_type)) +
  geom_bar(stat = "identity", position = "identity", width = 1) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.18) +
  facet_wrap(~ variable) +
  scale_y_continuous(limits = c(-0.03, 0.10),
                     breaks = seq(-0.02, 0.08, by = 0.02)) +
  scale_fill_manual(values = c("NonCog-PGI" = "#2E7D32",
                               "NonCog-PGI with parental PGIs" = "#81C784",
                               "Cog-PGI" = "#6A1B9A",
                               "Cog-PGI with parental PGIs" = "#BA68C8")) +
  labs(x = "", y = "Estimate", fill = "") +
  theme_sverdo() +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    strip.text = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 7),
    legend.position = "bottom")

tiff("plots/rGE.tiff", 
     width = 140, 
     height = 140,  
     units = "mm", 
     res = 600,
     compression = "lzw")

rGE_plot

dev.off()

          ##### 2.4  rGE table #####

table_rGE <- rGE %>%
  # round numeric columns to 3 decimal places
  mutate(across(2:ncol(.), ~ round(., 3))) %>%
  
  # remove leading zeros
  mutate(across(c(noncog_p, noncog_p_ci_lower, noncog_p_ci_upper, 
                  cog_p, cog_p_ci_lower, cog_p_ci_upper,
                  noncog, noncog_ci_lower, noncog_ci_upper,
                  cog, cog_ci_lower, cog_ci_upper), 
                ~ gsub("^0\\.", ".", gsub("^-0\\.", "-.", as.character(.))))) %>%
  
  # create combined columns for each estimate type
  mutate(
    noncog_p_ci = paste0(noncog_p, " (", noncog_p_ci_lower, ", ", noncog_p_ci_upper, ")"),
    cog_p_ci = paste0(cog_p, " (", cog_p_ci_lower, ", ", cog_p_ci_upper, ")"),
    noncog_ci = paste0(noncog, " (", noncog_ci_lower, ", ", noncog_ci_upper, ")"),
    cog_ci = paste0(cog, " (", cog_ci_lower, ", ", cog_ci_upper, ")")
  ) %>%
  
  select(variable, noncog_ci, noncog_p_ci, cog_ci, cog_p_ci)


save(table_rGE, file = "tables/rGE_table.Rdata")


        #### 3. MODEL COMPARISON ####

load("N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/random_models.Rdata")

model_fit <- as.data.frame(anova(random_models[[1]],
                                 random_models[[2]],
                                 random_models[[3]],
                                 random_models[[4]],
                                 random_models[[5]],
                                 random_models[[6]],
                                 random_models[[7]],
                                 random_models[[8]],
                                 random_models[[9]]))

#terms that are included as random slopes
row.names(model_fit) <- c("null",
                          "sex",
                          "sex, noncog",
                          "sex, noncog, cog",
                          "sex,noncog, sex*noncog",
                          "sex,noncog,sex*noncog,cog",
                          "sex,cog,sex*cog,noncog",
                          "sex,cog,noncog,sex*cog,sex*noncog")

model_fit[,1:8] <- round(model_fit[,1:8],2)


        #### 4. ARTIFICIAL CENSORING SENSITIVITY ####

censor <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/cluster_analyses/artifical_censoring.csv")

# loading in the CIs for each censor threshold

# get all CSV files matching the pattern
folder_dir <- "N:/durable/projects/37323479_Sverre_GPA_gender_gap/cluster_analyses/"

csv_files <- list.files(
  path = folder_dir,
  pattern = "artificial_censoring_.*\\.csv$",
  full.names = TRUE
)

# read and combine all CSV files
results_combined <- do.call(rbind, lapply(csv_files, read.csv))

# sort by censoring level 
results_combined <- results_combined[order(results_combined$censoring_level), ]

# reshape to get the data in the right format
plot_data <- results_combined %>%
  # Convert censoring_levels to character
  mutate(
    censoring_level = as.character(censoring_level)
  ) %>%
  pivot_longer(
    cols = -censoring_level,
    names_to = "variable",
    values_to = "value"
  ) %>%
  # Remove NA values
  filter(!is.na(value)) %>%
  # Convert value to numeric
  mutate(value = as.numeric(value)) %>%
  # Parse the variable names to extract components
  mutate(
    gender = ifelse(grepl("boys", variable), "Boys", "Girls"),
    stat_type = case_when(
      grepl("_lower$", variable) ~ "lower",
      grepl("_upper$", variable) ~ "upper",
      TRUE ~ "estimate"
    )
  ) %>%
  # Now pivot wider to get estimate, lower, upper as separate columns
  pivot_wider(
    id_cols = c(censoring_level, gender),
    names_from = stat_type,
    values_from = value
  ) %>%
  # Create a numeric position for ordering - handle non-censored separately
  mutate(
    x_position = if_else(
      censoring_level == "non-censored", 
      2.05, 
      suppressWarnings(as.numeric(censoring_level))
    )
  )

# Create the plot - faceted by gender, one on top of the other
articifial_censor_plot <- ggplot(plot_data, aes(x = x_position, y = estimate, color = gender)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    linewidth = 0.5,
    fatten = 2
  ) +
  facet_wrap(
    ~ gender,
    ncol = 1,
    scales = "fixed",
    strip.position = "left"
  ) +
  scale_x_reverse(
    breaks = c(2.05, seq(2, 1.5, by = -0.05)),
    labels = c("non-censored", seq(2, 1.5, by = -0.05))
  ) +
  scale_y_continuous(
    trans = "reverse",
    limits = c(0.0, -.06)
  ) +
  scale_color_manual(
    values = c("Boys" = color_boys, "Girls" = color_girls)
  ) +
  labs(
    x = "GPA Censoring Level",
    y = "slope-intercept correlation",
  ) +
  theme_sverdo() +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 7, face = "bold"),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(color = "grey95", size = 0.3),
    axis.title.y = element_text(angle = 90),
    axis.title.x = element_text(size = 6),
    legend.position = "none"
  )

tiff("plots/artificial_censoring.tiff", 
     width = 120, 
     height = 90,  
     units = "mm", 
     res = 600,
     compression = "lzw")

articifial_censor_plot

dev.off()


        #### 6. MISSINGNESS PATTERNS ####

school_variables <- c("school_income_m", "school_teacher_edu_m","school_student_gender_m","school_turnover_m",
                      "school_teacher_gender_m","classroom_gender_gy","class_grades_gy","ext_behavior_m","pos_climate_m")


# create a DF where a cell gets a 1 if it is NA, and 0 otherwise
missing_data <- data

missing_data[school_variables] <- lapply(missing_data[school_variables], function(column) {
  ifelse(is.na(column), 1, 0)
})

missing_desc <- data.frame(variable = school_variables,
                           missing_percent = rep(NA, 9),
                           beta = rep(NA, 9),
                           p = rep(NA,9))

# loops through each school environment and calculates NA percentage and correlation with GPA
for(i in 1:nrow(missing_desc)) {
  
  variable <- missing_desc$variable[i]
  
  #percentage missing data
  missing_desc$missing_percent[i] <- sum(missing_data[,variable]) / nrow(missing_data)
  
  #correlation between being missing and GPA
  correlation <- cor.test(missing_data[,variable], missing_data$grades_std)
  missing_desc$beta[i] = correlation$estimate
  missing_desc$p[i] = correlation$p.value
}

missing_desc[2:ncol(missing_desc)] <- round(missing_desc[2:ncol(missing_desc)],3)

write.table(missing_desc, row.names = F, quote = F, sep = "\t")
