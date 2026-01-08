source("00_settings.R")

        #### 0. LOADING DATA AND PACKAGES ####

data <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")
load("N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/candidate_models.Rdata")

        #### 1 OBTAINING MODEL OUTPUT ####

# function to extract output for each model
extract_lmer_info <- function(model) {
  
  # get the fixed effects for each model and extracting parameters
  tidy_output <- tidy(model, effects = "fixed", conf.int = TRUE)
   new_data <- tidy_output[, c("term", "estimate", "p.value","conf.low","conf.high")]
  
  new_data$model <- i
  
  # access and update the model info
  model_info <<- rbind(model_info, new_data)
}

# initializing df
model_info <- data.frame()

# running function
for(i in names(candidate_models)) {extract_lmer_info(candidate_models[[i]])}

# changing name of all gender-terms for consistency
model_info$term <- gsub("kjoenn_g|kjoenn_gy|kjoenn_gc|kjoenn_cg", "kjoenn", model_info$term)

# removing estimates that are not of interest
model_info <- model_info[!grepl("parental|school_middle", model_info$term, ignore.case = TRUE), ]

# removing terms that contain these exact strings: 
exact_terms_to_remove <- c("cog_g", "noncog_g", "kjoenn","kjoenn:cog_g",
                           "kjoenn:noncog_g","(Intercept)")
model_info <- model_info[!model_info$term %in% exact_terms_to_remove, ]


        #### 2. GENDER X ENVIRONMENT EFFECTS ####

# removing genetic terms
non_gen_effects <- model_info %>%
  filter(!grepl("cog_g|noncog_g", term))

# removing gendered effects
main_effects <- non_gen_effects %>%
  filter(!grepl("kjoenn", term))

# this DF contains parameters related to the interaction with gender
gender_effects <- non_gen_effects %>%
  filter(grepl("kjoenn", term)) %>%
  dplyr::select(-c(term))
    names(gender_effects)[1:4] <- paste0(names(gender_effects)[1:4],"_gender")

# merging main and gendered effects
main_effects <- merge(main_effects, gender_effects, by = c("model"))

          ##### 2.1 gender-specific estimates #####

input_df <- main_effects %>%
  dplyr::select(c(model,term))

# function loops through all the models and computes gender-specific effects and uncertainty parameters based on the main effects + main effects*gender
calculate_gender_specific_stats <- function(models_list, input_df) {
  # initialize a new data frame for results
  results <- input_df
  # prepare new columns for CI, estimates, and p-values
  results$conf.high_girls <- NA
  results$conf.low_girls <- NA
  results$conf.high_boys <- NA
  results$conf.low_boys <- NA
  results$estimate_girls <- NA
  results$estimate_boys <- NA
  results$p.value_girls <- NA
  results$p.value_boys <- NA
  
  # loop through each row of the input data frame
  for (i in seq_len(nrow(input_df))) {
    # extract model name and term for this iteration
    model_name <- input_df$model[i]
    term <- input_df$term[i]
    
    # get the model from the models list
    model <- models_list[[model_name]]
    
    # extract the necessary coefficients
    coefs <- summary(model)$coefficients
    
    # for these models want year-specific effects. Needed to extract the correct gender terms
    year_list <- c("student_gender", "teacher_edu", "teacher_gender", "ext_behavior", "pos_climate")
    
    # new list for class-level year effects. Needed to extract the correct gender terms
    year_class_list <- c("class_grades", "class_gender")
    
    if (model_name %in% year_list) {
      interaction_term <- paste0("kjoenn_gy:", term)
    } else if (model_name %in% year_class_list) {
      interaction_term <- paste0("kjoenn_cg:", term)
    } else {
      interaction_term <- paste0("kjoenn_g:", term)
    }
    
    # retrieving model parameters
    beta_term <- coefs[term, "Estimate"]
    beta_interact <- coefs[interaction_term, "Estimate"]
    
    # compute slopes (gender is effect coded)
    slope_girls <- beta_term - (beta_interact)
    slope_boys <- beta_term + (beta_interact)
    
    # bet variance-covariance matrix
    vcov_matrix <- as.matrix(vcov(model))
    
    # calculate gender-specific standard errors based on var/cov rules
    se_girls <- sqrt(vcov_matrix[term, term] +
                       vcov_matrix[interaction_term, interaction_term] -
                       2 * vcov_matrix[term, interaction_term])
    
    se_boys <- sqrt(vcov_matrix[term, term] +
                      vcov_matrix[interaction_term, interaction_term] +
                      2 * vcov_matrix[term, interaction_term])
    
    # calculate 95% confidence intervals
    critical_value <- qnorm(0.975)  # 95% CI
    
    ci_girls <- c(slope_girls - critical_value * se_girls, slope_girls + critical_value * se_girls)
    ci_boys <- c(slope_boys - critical_value * se_boys, slope_boys + critical_value * se_boys)
    
    # calculate p-values for slopes
    t_girls <- slope_girls / se_girls
    t_boys <- slope_boys / se_boys
    
    p_value_girls <- 2 * (1 - pnorm(abs(t_girls)))
    p_value_boys <- 2 * (1 - pnorm(abs(t_boys)))
    
    # store the results in the results data frame
    results$conf.low_girls[i] <- ci_girls[1]
    results$conf.high_girls[i] <- ci_girls[2]
    results$conf.low_boys[i] <- ci_boys[1]
    results$conf.high_boys[i] <- ci_boys[2]
    results$estimate_girls[i] <- slope_girls
    results$estimate_boys[i] <- slope_boys
    results$p.value_girls[i] <- p_value_girls
    results$p.value_boys[i] <- p_value_boys
    
  }
  
  # feturn the data frame with added CI, estimate, and p-value columns
  return(results)
}


# run the  function
results_with_cis <- calculate_gender_specific_stats(candidate_models, input_df)

p.value_gender <- main_effects[c("model","term","p.value_gender")]

# merge  with original DF
main_effects <- merge(results_with_cis,p.value_gender, by = c("model","term"))

        #### 3. GENDER * ENVIRONMENT ####

main_effects$term <- c(
  "Classroom-level proportion of girls",
  "Classroom-level GPA",
  "Externalizing behavior",
  "Positive school climate",
  "School-level SES",
  "School-level proportion of girls",
  "Teacher educational attainment",
  "Proportion of female teachers",
  "Teacher turnover")

# arrange the dataframe by descending order for girls
main_effects <- main_effects %>%
  arrange(estimate_girls)

# custom_order based on the sorted term
custom_order <- main_effects$term

# reorder the factor levels of the term column
main_effects$term <- factor(main_effects$term, levels = custom_order)


# long format df with estimates
main_effects_long <- main_effects %>%
  pivot_longer(
    cols = starts_with("estimate") | starts_with("conf"),
    names_to = c(".value", "gender"),
    names_pattern = "(.*)_(girls|boys)"
  )

#rounding p.values for plotting
main_effects_long$p.value_girls <- round(main_effects_long$p.value_girls,3)
main_effects_long$p.value_boys <- round(main_effects_long$p.value_boys,3)
#capitalize first letters in boys and girls
main_effects_long$gender <- toTitleCase(main_effects_long$gender)

# Convert term to a numeric
main_effects_long$term_numeric <- as.numeric(factor(main_effects_long$term, levels = unique(main_effects_long$term)))

# creating brackets for estimates with significant gender-differences
brackets <- main_effects_long %>%
  filter(gender %in% c("Boys", "Girls")) %>%
  select(term, term_numeric, gender, estimate, p.value_gender) %>%
  pivot_wider(names_from = gender, values_from = estimate) %>%
  filter(!is.na(Boys) & !is.na(Girls) & !is.na(p.value_gender)) %>%
  mutate(
    xstart = pmin(Boys, Girls),
    xend = pmax(Boys, Girls),
    y.position = term_numeric + 0.35,  # b,o8 Increased offset
    label = case_when(
      p.value_gender < 0.001 ~ "***",
      p.value_gender < 0.01 ~ "**",
      p.value_gender < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  filter(label != "ns")

#final df for plotting
main_effects_long <- main_effects_long %>%
  mutate(
    #making it so that a p-value <.001 is shown as such
    pval_label = case_when(
      gender == "Girls" & p.value_girls < 0.001 ~ "<.001",
      gender == "Girls" ~ sub("^0\\.", ".", as.character(signif(p.value_girls, 3))),
      gender == "Boys" & p.value_boys < 0.001 ~ "<.001",
      gender == "Boys" ~ sub("^0\\.", ".", as.character(signif(p.value_boys, 3)))
    )
  ) %>%
  group_by(term) %>%
  mutate(
    anchor_side = ifelse(estimate[gender == "Boys"] >= 0, "right", "left")
  ) %>%
  ungroup() %>%
  mutate(
    label_x = ifelse(anchor_side == "right", conf.high + 0.005, conf.low - 0.005),
    label_hjust = ifelse(anchor_side == "right", 0, 1)
  )


          ##### 3.1 plot #####

gender_plot <- ggplot(main_effects_long, aes(x = estimate, y = term_numeric, color = gender)) +
  # Error bars
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, 
        y = term_numeric + ifelse(gender == "Boys", 0.12, -0.12)),
    height = 0, linewidth = 0.5
  ) +
  # Points
  geom_point(aes(y = term_numeric + ifelse(gender == "Boys", 0.12, -0.12)), size = 1.5) +
  # p-value labels placed consistently relative to CI
  geom_text(
    aes(
      x = label_x, 
      label = pval_label,
      y = term_numeric + ifelse(gender == "Boys", 0.12, -0.12),
      hjust = label_hjust,
      family = "serif",
    ),
    size = 2, show.legend = FALSE
  ) +

  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Estimate", y = NULL, title = "") +
  scale_y_continuous(breaks = main_effects_long$term_numeric, 
                     labels = main_effects_long$term) +
  scale_color_manual(values = c("Boys" = color_boys, "Girls" = color_girls),
                     name = "Gender") +
  scale_x_continuous(
    limits = c(-0.1, 0.17),
    breaks = seq(-0.1, 0.16, by = 0.05)
  )+
  coord_cartesian(ylim = c(0.5, max(main_effects_long$term_numeric) + 0.5), clip = "off")+
  theme_sverdo() +
  guides(color = guide_legend(title.position = "left"))+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95", linewidth = 0.3),
    legend.position = "bottom",
    legend.title = element_text(size = 7, face = "bold"),  # Specify which element
    axis.text.y = element_text(family = "serif"),
    axis.title.x = element_text(hjust = .37),
    legend.margin = margin(t = -5) 
  )+
  #gender-p.value brackets
  geom_bracket(
    data = brackets,
    aes(xmin = xstart, xmax = xend, y.position = y.position, label = label),
    tip.length = 0.01,
    inherit.aes = FALSE,
    label.size = 2.5
  )
  


        #### 4. PGI X ENVIRONMENTS ####

# terms with PGI interactions
gen_terms <- model_info %>%
  filter(grepl("cog_g|noncog_g", term)) %>%
  filter(!term %in% c("noncog_g:kjoenn", "noncog_g:kjoenn", "cog_g:kjoenn", "kjoenn:cog_g"))


# retrieving 97.5% CIs for plotting the PGI effects
extract_lmer_gen <- function(model) {
  
  # get the fixed effects for each model and extracting parameters
  tidy_output <- tidy(model, effects = "fixed", conf.int = FALSE)
  new_data <- tidy_output[, c("term", "estimate", "p.value","std.error")]
  
  new_data$model <- i
  
  # manually calculate 97.5% CIs
  se <- tidy_output$std.error
  new_data$conf.low <- tidy_output$estimate - qnorm(0.9875) * se
  new_data$conf.high <- tidy_output$estimate + qnorm(0.9875) * se
  
  # access and update the model info
  gen_effects <<- rbind(gen_effects, new_data)
}

# initializing df
gen_effects <- data.frame()

# running function
for(i in names(candidate_models)) {extract_lmer_gen(candidate_models[[i]])}

# limiting DF to the terms that includes interactions with PGIs
gen_effects <- gen_effects[gen_effects$term %in% gen_terms$term,]

# df with NonCog effects
noncog_effects <- gen_effects %>%
  filter(grepl("noncog_g", term))

# extract main effects
noncog_main_effects <- noncog_effects %>%
  filter(!grepl("kjoenn", term))

# extract gender-interaction effects and their p-values
noncog_gender <- noncog_effects %>%
  filter(grepl("kjoenn", term)) %>%
  select(model, gender_pvalue = p.value)

# merge main effects with gender interaction p-values
noncog_final <- noncog_main_effects %>%
  left_join(noncog_gender, by = "model")

# changing names
noncog_final$term <- c("School-level SES",
                    "Classroom-level proportion of girls",
                    "Classroom-level GPA",
                    "Teacher educational attainment",
                    "Proportion of female teachers",
                    "Teacher turnover",
                    "School-level proportion of girls",
                    "Positive school climate",
                    "Externalizing behavior")


# making DF with Cog effects
cog_effects <- gen_effects %>%
  filter(!grepl("noncog_g", term)) %>%
  filter(grepl("cog_g", term))

# extract main effects
cog_main_effects <- cog_effects %>%
  filter(!grepl("kjoenn", term))

# extract gender interaction effects and their p-values
cog_gender <- cog_effects %>%
  filter(grepl("kjoenn", term)) %>%
  select(model, gender_pvalue = p.value)

# merge main effects with gender interaction p-values
cog_final <- cog_main_effects %>%
  left_join(cog_gender, by = "model")

# changing names
cog_final$term <- noncog_final$term


# merging NonCog and Cog DFs
cognoncog <- rbind(
  noncog_final %>% mutate(group = "NonCog"),
  cog_final %>% mutate(group = "Cog")
)

# convert term to a factor with the specified order
term_order <- unique(main_effects$term)
  cognoncog$term <- factor(cognoncog$term, levels = term_order)

          ##### 4.1 plot #####

PGI_plot <- ggplot(cognoncog, aes(x = estimate, y = term, color = group)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0),
                 position = position_dodge(width = 0.4),
                 linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.4), size = 1.5) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Estimate", y = NULL, title = "") +
  scale_y_discrete(position = "right", expand = expansion(add = c(0.46, 0.46)))+
  scale_x_continuous(limits = c(-0.06, 0.04), breaks = seq(-0.04, 0.04, by = 0.02)) +
  scale_color_manual(values = c("NonCog" = "#518D3F", "Cog" = "#7b3f8d"),
                     name = "Polygenic index",
                     labels = c("NonCog" = "Non-cognitive skills", "Cog" = "Cognitive skills")) +
  theme_sverdo()+
  theme(
    axis.text.y = element_blank(),
    legend.title.align = 0.5,
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95", linewidth = 0.3),
    legend.position = "bottom",
    legend.title = element_text(size = 7, face = "bold"),
    legend.margin = margin(t = -5) ,
    axis.title.x = element_text(hjust = 0.6)
  ) +
  guides(color = guide_legend(title.position = "left"))+
  coord_cartesian(ylim = c(0.5, 9.5), clip = "off")+
  # P-values for NonCog 
  geom_text(data = data.frame(
    term = "Teacher educational attainment",
    estimate = -0.0276,  
    label = "p <.001"  
  ),
  aes(label = label, x = estimate, y = term),
  color = "#518D3F",  
  position = position_nudge(x = -0.027, y = 0.13),  
  size = 2,
  family = "serif",
  inherit.aes = FALSE) +
  geom_text(data = data.frame(
    term = "School-level SES",
    estimate = -0.0189,   
    label = "p =.010"  
  ),
  aes(label = label, x = estimate, y = term),
  color = "#518D3F", 
  position = position_nudge(x = -0.022, y = 0.13),  
  size = 2,
  family = "serif",
  inherit.aes = FALSE)


          ##### 4.2 combinging plots #####

tiff("plots/candidate_environments.tiff", 
     width = 180, 
     height = 100,  # adjust as needed
     units = "mm", 
     res = 600,
     compression = "lzw")

grid.arrange(
  gender_plot, PGI_plot,
  ncol = 2,
  widths = c(1.6, 1))
  
dev.off()  


         #### 5. TEACHER EDU * NONCOG ####

# loading in the dataset
data_school_teacher_edu_m <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/data_school_teacher_edu_m.csv")

# re-estimating the model to create CIs
fit <- lmer(grades_std ~ noncog_g*kjoenn_gy*school_teacher_edu_m+
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


          ##### 5.1 confidence intervals #####

# function to extract CIs
GPA_pred <- function(fit) {
  # get fixed effects
  fixed_ef <- fixef(fit)
  
  noncog <- fixed_ef[["noncog_g"]]
  gender <- fixed_ef[["kjoenn_gy"]]
  teacher_edu <- fixed_ef[["school_teacher_edu_m"]]
  teacher_edu_noncog <- fixed_ef[["noncog_g:school_teacher_edu_m"]]
  gender_teacher_edu <- fixed_ef[["kjoenn_gy:school_teacher_edu_m"]]
  
  # create sequence of values
  noncog_vals <- seq(-3, 3, by = 0.1)
  teacher_edu_vals <- c(-2,0,2)
  
  # DF wih results
  result_df <- expand.grid(
    noncog_vals = noncog_vals,
    teacher_edu_vals = teacher_edu_vals,
    Gender = c("Girls", "Boys")
  ) %>%
    mutate(
      Gender_effect = ifelse(Gender == "Girls", -1 * gender, gender),
      Gender_teacher_edu_effect = ifelse(Gender == "Girls", 
                                         -1 * gender_teacher_edu * teacher_edu_vals, 
                                         gender_teacher_edu * teacher_edu_vals),
      
      # predicted GPA based on NonCog, teacher_edu, gender, gender*teacher_edu
      Expected_GPA = noncog * noncog_vals +                                    
        teacher_edu_noncog * noncog_vals * teacher_edu_vals +    
        teacher_edu * teacher_edu_vals +                         
        Gender_effect +                                          
        Gender_teacher_edu_effect                                
    )
  
  # contains predicted values for each combination
  return(result_df$Expected_GPA)
}

# setup parallel
n_cores <- detectCores()-6
cl <- makeCluster(n_cores)

clusterEvalQ(cl, {
  library(lme4)
  library(boot)
  library(dplyr)
})

clusterExport(cl, varlist = c("GPA_pred","data"), envir = environment())

# Run bootstrap
n_iterations <- 20000

boot_results <- bootMer(fit, 
                        FUN = GPA_pred,
                        nsim = n_iterations,
                        type = "parametric",
                        parallel = "snow",
                        ncpus = n_cores,
                        cl = cl,
                        .progress = "txt")

result_df <- as.data.frame(boot_results$t)

stopCluster(cl)

# Calculate CIs for each prediction point (each column)
ci_lower <- apply(boot_results$t, 2, quantile, probs = 0.025)
ci_upper <- apply(boot_results$t, 2, quantile, probs = 0.975)
mean_pred <- GPA_pred(fit) # predicted GPA based on the main model

# restructuring the CIs for plotting
noncog_vals <- seq(-3, 3, by = 0.1)
teacher_edu_vals <- c(-2, 0, 2)

df_structure <- expand.grid(
  noncog_vals = noncog_vals,
  teacher_edu_vals = teacher_edu_vals,
  Gender = c("Girls", "Boys")
)

# combining everything
final_df <- df_structure %>%
  mutate(
    Expected_GPA = mean_pred,
    CI_lower = ci_lower,
    CI_upper = ci_upper
  )

# colors for different groups
final_df <- final_df %>%
  mutate(color_gender = case_when(
    Gender == "Girls" & teacher_edu_vals == -2 ~ "Girls.Low (-2 SD)",
    Gender == "Girls" & teacher_edu_vals == 0 ~ "Girls.Average",
    Gender == "Girls" & teacher_edu_vals == 2 ~ "Girls.High (+2 SD)",
    Gender == "Boys" & teacher_edu_vals == -2 ~ "Boys.Low (-2 SD)",
    Gender == "Boys" & teacher_edu_vals == 0 ~ "Boys.Average",
    Gender == "Boys" & teacher_edu_vals == 2 ~ "Boys.High (+2 SD)"
  ))

          ##### 5.2 plot #####

teacher_edu_plot <- ggplot(final_df, aes(x = noncog_vals, y = Expected_GPA,
                                         color = color_gender)) +
  geom_line(size = .7) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper,
                  fill = color_gender),
              alpha = 0.3, linetype = 0) +
  facet_wrap(~ Gender, strip.position = "bottom") +
  scale_color_manual(values = plot_colors) +
  scale_fill_manual(values = plot_colors) + 
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1)) +
  scale_y_continuous(limits = c(-1.2, 1), breaks = seq(-1, 1, by = .25))+
  theme_sverdo()+
  coord_cartesian(clip = "off") +
  theme(
        panel.grid.major = element_line(color = "grey95", linewidth = 0.3),
        strip.text = element_blank(),
    legend.position = "none",
    strip.placement = "none",
    strip.background = element_blank()
  ) +
  # legend
  geom_text(data = data.frame(
    x = -1.3,
    y = 0.95,
    label = "Teacher educational\nattainment",
    Gender = c("Girls", "Boys")
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  hjust = 0.5,
  size = 2,  
  family = "serif",
  fontface = "bold",
  color = "black") +
  geom_rect(data = data.frame(
    xmin = rep(-2.18, 6),
    xmax = rep(-1.88, 6),  
    ymin = c(0.78, 0.70, 0.62, 0.78, 0.70, 0.62),
    ymax = c(0.84, 0.76, 0.68, 0.84, 0.76, 0.68),  
    Gender = c(rep("Girls", 3), rep("Boys", 3))
  ),
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  fill = c(plot_colors["Girls.High (+2 SD)"], plot_colors["Girls.Average"], plot_colors["Girls.Low (-2 SD)"],
           plot_colors["Boys.Low (-2 SD)"], plot_colors["Boys.Average"], plot_colors["Boys.High (+2 SD)"]),
  color = NA,
  inherit.aes = FALSE) +
  geom_text(data = data.frame(
    x = rep(-1.80, 6),
    y = c(0.81, 0.73, 0.65, 0.81, 0.73, 0.65),
    label = rep(c("+2 SD", "Average", "-2 SD"), 2),
    Gender = c(rep("Girls", 3), rep("Boys", 3))
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  family = "serif",
  hjust = 0,
  size = 2) +  
  labs(x = "",
       y = "Expected GPA")

# adding text to plot
x_label_text <- "Polygenic index for non-cognitive skills"

grob_per_facet <- gTree(children = gList(
  # facet labels 
  textGrob("Boys",
           x = unit(.34, "npc"), y = unit(0.74, "npc"),
           gp = gpar(fontfamily = "serif", fontface = "bold", fontsize = 8),
           just = "center"),
  textGrob("Girls",
           x = unit(0.77, "npc"), y = unit(0.75, "npc"),
           gp = gpar(fontfamily = "serif", fontface = "bold", fontsize = 8),
           just = "center"),
  # x-axis titles 
  textGrob(x_label_text,
           x = unit(0.34, "npc"), y = unit(2, "npc"),
           gp = gpar(fontfamily = "serif",  fontsize = 6.2),
           just = "center"),
  textGrob(x_label_text,
           x = unit(0.77, "npc"), y = unit(2, "npc"),
           gp = gpar(fontfamily = "serif",  fontsize = 6.2),
           just = "center")
))


tiff("plots/teacher_edu_noncog.tiff", 
     width = 90, 
     height = 90,
     units = "mm", 
     res = 600,
     compression = "lzw")

grid.arrange(
  teacher_edu_plot, grob_per_facet,
  ncol = 1,
  heights = c(10, .5)   
)

dev.off()


          ##### 5.3 differences in expected GPA #####

# for boys with low noncog (-2 SD)
boys_low_noncog <- final_df %>%
  filter(Gender == "Boys", noncog_vals == -2)

boys_diff <- boys_low_noncog$Expected_GPA[boys_low_noncog$teacher_edu_vals == 2] - 
  boys_low_noncog$Expected_GPA[boys_low_noncog$teacher_edu_vals == -2]

# for girls with low noncog (-2 SD)  
girls_low_noncog <- final_df %>%
  filter(Gender == "Girls", noncog_vals == -2)

girls_diff <- girls_low_noncog$Expected_GPA[girls_low_noncog$teacher_edu_vals == 2] - 
  girls_low_noncog$Expected_GPA[girls_low_noncog$teacher_edu_vals == -2]

cat("Boys (low noncog): Difference between high vs low teacher edu =", round(boys_diff, 3), "\n")
cat("Girls (low noncog): Difference between high vs low teacher edu =", round(girls_diff, 3), "\n")

