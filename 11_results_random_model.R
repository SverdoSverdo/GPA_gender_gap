#### 0. LOADING DATA AND PACKAGES ####
source("00_settings.R")

data <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")
load("N:/durable/projects/37323479_Sverre_GPA_gender_gap/model_output/random_models.Rdata")


#Best-fitting model used for the plots
best_model <- random_models$model_5

        #### 1. MODEL ESTIMATES ####

# random effects correlations
VarCorr(best_model)

# extract coefficients and vcov matrix
coefs <- fixef(best_model)
vcov_mat <- vcov(best_model)

# function to find the interaction term regardless of order
find_interaction <- function(main_term) {
  # both possible orderings
  int_term1 <- paste0(main_term, ":kjoenn_g")
  int_term2 <- paste0("kjoenn_g:", main_term)
  
  if(int_term1 %in% names(coefs)) {
    return(int_term1)
  } else if(int_term2 %in% names(coefs)) {
    return(int_term2)
  } else {
    stop(paste("Could not find interaction term for", main_term))
  }
}

# updated function
calc_gender_effects <- function(main_term) {
  
  # gind the interaction term
  interaction_term <- find_interaction(main_term)
  
  # het positions in coefficient vector
  main_pos <- which(names(coefs) == main_term)
  int_pos <- which(names(coefs) == interaction_term)
  
  # point estimates
  main_coef <- coefs[main_pos]
  int_coef <- coefs[int_pos]
  
  boys_effect <- main_coef + int_coef    # +1 coding
  girls_effect <- main_coef - int_coef   # -1 coding
  
  # standard errors using variance-covariance matrix
  var_main <- vcov_mat[main_pos, main_pos]
  var_int <- vcov_mat[int_pos, int_pos]
  cov_main_int <- vcov_mat[main_pos, int_pos]
  
  se_boys <- sqrt(var_main + var_int + 2 * cov_main_int)
  se_girls <- sqrt(var_main + var_int - 2 * cov_main_int)
  
  # calculate interaction p-value
  se_interaction <- sqrt(vcov_mat[int_pos, int_pos])
  t_interaction <- int_coef / se_interaction
  df_res <- df.residual(best_model)
  p_interaction <- 2 * pt(abs(t_interaction), df_res, lower.tail = FALSE)
  
  # calculate CIs and p-values for gender-specific effects
  t_crit <- qt(0.975, df_res)
  
  results <- data.frame(
    Variable = rep(main_term, 2),
    Gender = c("Boys", "Girls"),
    Effect = c(boys_effect, girls_effect),
    SE = c(se_boys, se_girls),
    CI_lower = c(boys_effect - t_crit * se_boys, 
                 girls_effect - t_crit * se_girls),
    CI_upper = c(boys_effect + t_crit * se_boys, 
                 girls_effect + t_crit * se_girls),
    t_value = c(boys_effect/se_boys, girls_effect/se_girls),
    p_value = c(2 * pt(abs(boys_effect/se_boys), df_res, lower.tail = FALSE),
                2 * pt(abs(girls_effect/se_girls), df_res, lower.tail = FALSE)),
    interaction_p = rep(p_interaction, 2)  # Same p-value for both rows
  )
  
  return(results)
}

# apply to all variables
predictors <- c("cog_g", "noncog_g", "cog_parental_g", "noncog_parental_g")
fixed_effects <- data.frame()

for(pred in predictors) {
  results <- calc_gender_effects(pred)
  fixed_effects <- rbind(fixed_effects, results)
}


# removing t.value col and rounding
fixed_effects$t_value <- NULL
  fixed_effects[3:ncol(fixed_effects)] <- round(fixed_effects[3:ncol(fixed_effects)],3)


#write.table(fixed_effects, sep = ";", quote = F, row.names = T)

        #### .2 RANDOM EFFECT PLOTS ####
  
          ##### 2.1 defining terms for plotting #####
  
# extract fixed effects
beta_noncog <- fixef(best_model)["noncog_g"]
beta_noncoggender <- fixef(best_model)["kjoenn_g:noncog_g"]
beta_gender <- fixef(best_model)["kjoenn_g"]

beta_noncog_boys <-beta_noncog + beta_noncoggender
beta_noncog_girls <-beta_noncog - beta_noncoggender

#extract variance/covariance terms random effects terms
random_var <- as.matrix(VarCorr(best_model)$lnr_org)

sd_gender <- sqrt(random_var["kjoenn_g", "kjoenn_g"]) #SD of gender slope
sd_intercept <- sqrt(random_var["(Intercept)", "(Intercept)"]) #SD of intercept

var_gender <- random_var["kjoenn_g", "kjoenn_g"] #variance of gender slope
var_noncog <- random_var["noncog_g", "noncog_g"] #variance of NonCog
var_noncoggender <- random_var["kjoenn_g:noncog_g", "kjoenn_g:noncog_g"] #variance of NonCog*gender
var_intercept <- random_var["(Intercept)","(Intercept)"] #variance of intercept 

cov_gender_int <- random_var["kjoenn_g", "(Intercept)"] #gender-intercept covariance
cov_noncog_int <- random_var["noncog_g", "(Intercept)"] #NonCog-intercept covariance
cov_noncog_gender <- random_var["noncog_g", "kjoenn_g"] #NonCog-gender covariance

cov_noncoggender_int <- random_var["kjoenn_g:noncog_g", "(Intercept)"] #gender*NonCog-intercept covariance
cov_noncoggender_gender <- random_var["kjoenn_g:noncog_g", "kjoenn_g"] #gender*NonCog-gender covariance
cov_noncoggender_noncog <- random_var["kjoenn_g:noncog_g","noncog_g"] #gender*NonCog-noncog covariance


#the mean gender gap
mean_observed_gap <- beta_gender * 2  


          ##### 2.1 variance in gender effects #####

# values for density of random slope for gender
gender_slope_vals_fine <- seq(-4, 4, by = 0.01)
  gender_gap_actual_fine <- gender_slope_vals_fine * sd_gender + mean_observed_gap
  density_vals <- dnorm(gender_slope_vals_fine, mean = 0, sd = 1)  

# create density dataframe
df_density <- data.frame(
  gender_gap = gender_gap_actual_fine,
  density = density_vals
)


# plotting 
gender_SD <- ggplot(df_density, aes(x = gender_gap, y = density, fill = "Gender Gap")) +
  geom_area(alpha = 0.8, position = "identity") +
  geom_vline(xintercept = mean_observed_gap, linetype = "dashed", color = "gray30", linewidth = .5) +
  labs(
    x = "School-specific average GPA gender gap",
    y = ""
  ) +
 scale_x_continuous(
 breaks = seq(-0.7, -0.4, by = 0.1)
 ) +
  scale_fill_manual(values = c("Gender Gap" = "purple"), name = NULL) +
  annotate("text", x = -.65, y = max(df_density$density) * 0.54,
           label = sprintf("β = %.2f (SD = %.2f)", mean_observed_gap, sd_gender*2),
           hjust = 1, size = figure_annotation_size, color = "black", family = "serif") +
  theme_sverdo() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )


          ##### 2.2 variance in PGI effects #####

# gender-specific variance of NonCog
var_boys <- var_noncog + var_noncoggender + 2*cov_noncoggender_noncog
var_girls <- var_noncog + var_noncoggender - 2*cov_noncoggender_noncog

# standard deviations
sd_boys <- sqrt(var_boys)
sd_girls <- sqrt(var_girls)

# gender-specific theoretical distributions of NonCog
x_range <- seq(beta_noncog_boys - 5*sd_boys, beta_noncog_boys + 5*sd_boys, length.out = 1000)

# df containing data to be plotted
plot_data <- data.frame(
  slope = c(x_range, x_range),
  density = c(dnorm(x_range, mean = beta_noncog_boys, sd = sd_boys),
              dnorm(x_range, mean = beta_noncog_girls, sd = sd_girls)),
  gender = rep(c("Boys", "Girls"), each = length(x_range))
)

plot_data$gender <- factor(plot_data$gender, levels = c("Boys", "Girls"))


# plot
PGI_SD <- ggplot(plot_data, aes(x = slope, y = density, fill = gender)) +
  geom_area(alpha = 0.8, position = "identity") +
  geom_vline(xintercept = beta_noncog_boys, linetype = "dashed", color = color_boys, linewidth = .5) +
  geom_vline(xintercept = beta_noncog_girls, linetype = "dashed", color = color_girls, linewidth = .5) +
  labs(
    x = "School-specific NonCog-GPA association",
    y = ""
  ) +
scale_x_continuous(
  limits = c(-0.1, 0.4),
  breaks = seq(-0.1, 0.4, by = 0.1)
)+
  scale_fill_manual(values = c("Boys" = color_boys, "Girls" = color_girls),name = NULL) +
  annotate("text", x = 0.075, y = max(plot_data$density) * 0.50,
           label = paste0("Girls: β = ", round(beta_noncog_girls, 2), " (SD = ", round(sd_girls, 2),")"),
           hjust = 1, size = figure_annotation_size, color = color_girls, family = "serif") +
  annotate("text", x = 0.075, y = max(plot_data$density) * 0.58,
           label = paste0("Boys: β = ", round(beta_noncog_boys, 2), " (SD = ", round(sd_boys, 2),")"),
           hjust = 1, size = figure_annotation_size, color = color_boys, family = "serif") +
  theme_sverdo()+
  theme(
    legend.position = c(0.85, 0.55),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()) 


tiff("plots/PGI_gender_SD.tiff", 
     width = 180, 
     height = 90, 
     units = "mm", 
     res = 600,
     compression = "lzw")

grid.arrange(
  gender_SD, PGI_SD,
  ncol = 2,
  widths = c(1, 1))
 
dev.off()


            ##### 2.3 gender-intercept slopes #####

# sequence of school average GPAs for plotting lines
school_int_raw_seq <- seq(-3 * sd_intercept, 3 * sd_intercept, length.out = 100)
  school_int_sd_seq <- seq(-3, 3, length.out = 100)
  
# expected gains for boys and girls per 1 SD increase in intercept
gender_slope <- cov_gender_int / (sd_intercept)
  mult_int_girls <- -gender_slope
  mult_int_boys <- gender_slope

# combining all estimates
boys_gpa <- beta_gender + (school_int_sd_seq * mult_int_boys) + school_int_raw_seq
girls_gpa <- -beta_gender + (school_int_sd_seq * mult_int_girls) + school_int_raw_seq


# data frame for regression lines
line_data <- data.frame(
  school_avg = rep(school_int_raw_seq, 2),
  student_gpa = c(girls_gpa, boys_gpa),
  gender = factor(rep(c("Girls", "Boys"), each = length(school_int_raw_seq)),
                  levels = c("Girls", "Boys"))
)

# x values for the density curve OVER THE FULL RANGE
x_density <- seq(-3 * sd_intercept, 3 * sd_intercept, length.out = 200)

# calculate density values
y_density <- dnorm(x_density, mean = 0, sd = sd_intercept)

# adjusting y-axis for plotting
desired_bottom <- -1.2
desired_height <- 1.0

# scale the density
y_density_scaled <- desired_bottom + (y_density / max(y_density)) * desired_height

# create density data frame
density_data <- data.frame(
  x = c(x_density, rev(x_density)),
  y = c(y_density_scaled, rep(desired_bottom, length(y_density_scaled)))
)

# plot
gender_int_plot <- ggplot() +
  geom_polygon(data = density_data,
               aes(x = x, y = y),
               fill = "grey",
               alpha = 0.7) +
  geom_line(data = line_data,
            aes(x = school_avg, y = student_gpa, color = gender),
            linewidth = .5) +
  scale_color_manual(values = c("Girls" = color_girls, "Boys" = color_boys),
                     name = "") +
  scale_x_continuous(
    limits = c(-3 * sd_intercept, 3 * sd_intercept),
    breaks = seq(-0.75, 0.75, by = 0.25),
    labels = seq(-0.75, 0.75, by = 0.25)
  ) +
  scale_y_continuous(limits = c(-1.2, 1),
                     breaks = seq(-1, 1, by = 0.5)) +
  guides(color = guide_legend(override.aes = list(linewidth = .8)))+
  labs(x = "Average school GPA",
       y = "Expected GPA",
       title = "",
       subtitle = "") +
  theme_sverdo() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(t = -5, b = -18),
    plot.margin = margin(t = -8, r = 10, b = 20, l = 10))

tiff("plots/gender_intercept_cor.tiff", 
     width = 90, 
     height = 90,  
     units = "mm", 
     res = 600,
     compression = "lzw")

gender_int_plot

dev.off()


          ##### 2.4 Noncog effect over gender gap #####

# correlations between slope for NonCog and the gender-slope
mult_gender_noncog_girls <- (cov_noncog_gender - (cov_noncoggender_gender)) / sd_gender
mult_gender_noncog_boys <- (cov_noncog_gender + (cov_noncoggender_gender)) / sd_gender

# create sequence of gender slope values in SD units
gender_slope_sd_vals <- seq(-3, 3, by = 0.1)

# convert SD units to actual gender gap values for x-axis
gender_gap_actual <- gender_slope_sd_vals * sd_gender + mean_observed_gap

# calculate predicted Y for each gender slope value
pred_girls <- beta_noncog_girls + mult_gender_noncog_girls * gender_slope_sd_vals
pred_boys <- beta_noncog_boys + mult_gender_noncog_boys * gender_slope_sd_vals

# density plot for theoretical distribution of school-specific gender gap
gender_slope_vals_fine <- seq(-4, 4, by = 0.01)
  gender_gap_actual_fine <- gender_slope_vals_fine * sd_gender + mean_observed_gap
  density_vals <- dnorm(gender_slope_vals_fine, mean = 0, sd = 1)  

# scale density to be visible on the plot
max_y_value <- 0.02  
density_scaled <- density_vals * max_y_value / max(density_vals)

# create density dataframe
df_density <- data.frame(
  gender_gap = gender_gap_actual_fine,
  density = density_scaled
)

# create lines dataframe with actual gender gap values
df_lines <- data.frame(
  gender_gap = rep(gender_gap_actual, 2), 
  expected_slope = c(pred_boys, pred_girls),
  gender = rep(c("Boys", "Girls"), each = length(gender_gap_actual))
)

#squaring to get variance explained
df_lines$expected_slope <- df_lines$expected_slope^2
 Plot
gender_noncog_plot <- ggplot(df_lines, aes(x = gender_gap, y = expected_slope, color = gender)) +
  geom_line(linewidth = .5) +
  geom_ribbon(data = df_density, 
              aes(x = gender_gap, ymin = 0, ymax = density), 
              fill = "purple", alpha = 0.3, inherit.aes = FALSE) +
  scale_color_manual(
    values = c("Girls" = color_boys, "Boys" = color_girls),
    labels = c("Girls" = "Girls", "Boys" = "Boys")
  ) +
  labs(
    title = "",
    x = "School-level GPA gender gap",
    y = "Variance explained in GPA by NonCog-PGI",
    color = NULL
  ) +
  theme_minimal() +
  scale_x_continuous(limits = range(df_lines$gender_gap),
                     breaks = seq(-.80, -.30, by = 0.1)) +
  geom_segment(aes(x = mean_observed_gap, xend = mean_observed_gap, y = 0, yend = 0.036), 
               linetype = "dashed", alpha = 1,color = "black",linewidth = .4)+
  annotate("text", x = mean_observed_gap, y = 0.037, label = "average school-level gender gap",
           size = figure_annotation_size-.5, hjust = .25, vjust = 0)+
  theme_sverdo()+
  theme(
    panel.grid.major = element_line(color = "gray95"),
    legend.position = "bottom",
    legend.margin = margin(t = -5, b = -18),
    plot.margin = margin(t = -8, r = 10, b = 20, l = 10))
  

tiff("plots/gender_noncog_cor.tiff", 
     width = 90, 
     height = 90,  
     units = "mm", 
     res = 600,
     compression = "lzw")

gender_noncog_plot

dev.off()
 
          ##### 2.5 figure 2a #####

# slope of NonCog over SD for boys and girls
mult_noncog_girls <- (cov_noncog_int - cov_noncoggender_int) / sd_intercept
mult_noncog_boys <- (cov_noncog_int + cov_noncoggender_int) / sd_intercept

# boys with -1 SD NonCog
int_gain_boys_low <- sd_intercept + 
  (mult_noncog_boys * -1)+
  (mult_int_boys)

# boys with +1 SD NonCog
int_gain_boys_high <- sd_intercept + 
  (mult_noncog_boys * 1)+
  (mult_int_boys)

# girls with -1 SD NonCog
int_gain_girls_low <- sd_intercept + 
  (mult_noncog_girls * -1)+
  (mult_int_girls)

# girls with +1 SD NonCog
int_gain_girls_high <- sd_intercept + 
  (mult_noncog_girls * 1)+
  (mult_int_girls)

# create plot data
plot_data <- data.frame(
  Gender = rep(c("Boys", "Girls"), each = 2),
  NonCog = rep(c("-1 SD", "+1 SD"), 2),
  Expected_Gain = c(int_gain_boys_low, int_gain_boys_high, 
                    int_gain_girls_low, int_gain_girls_high)
)

# make NonCog a factor with correct order
plot_data$NonCog <- factor(plot_data$NonCog, levels = c("-1 SD", "+1 SD"))

# adding CIS from bootstrapping from the cluster
int_gain_CIs <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/cluster_analyses/fig2a_CI.csv")

# adding parameters names back into the DF
names(int_gain_CIs) <- c("int_gain_boys_low", "int_gain_boys_high","int_gain_girls_low",
                  "int_gain_girls_high")

# calculating 95% CIs for each parameter
ci_data <- data.frame(
  NonCog = rep(c("-1 SD", "+1 SD"), 2),
  Gender = rep(c("Boys", "Girls"), each = 2),
  lower = c(
    quantile(int_gain_CIs$int_gain_boys_low, 0.025),
    quantile(int_gain_CIs$int_gain_boys_high, 0.025),
    quantile(int_gain_CIs$int_gain_girls_low, 0.025),
    quantile(int_gain_CIs$int_gain_girls_high, 0.025)
  ),
  upper = c(
    quantile(int_gain_CIs$int_gain_boys_low, 0.975),
    quantile(int_gain_CIs$int_gain_boys_high, 0.975),
    quantile(int_gain_CIs$int_gain_girls_low, 0.975),
    quantile(int_gain_CIs$int_gain_girls_high, 0.975)
  )
)

# merge CIs with plot data
plot_data <- merge(plot_data, ci_data, by = c("Gender", "NonCog"))

# plot
figure2a <- ggplot(plot_data, aes(x = NonCog, y = Expected_Gain, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.4) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.5), 
                width = 0.1) +
  scale_fill_manual(values = c("Boys" = color_boys, "Girls" = color_girls)) +
  labs(x = "Polygenic index for non-cognitive skills", 
       y = "Association between school-level GPA and individual GPA",
       fill = "Gender") +
  theme_sverdo()+
  scale_y_continuous(breaks = seq(0, 0.4, 0.05)) +
  theme(
    legend.position = c(0.85, 0.85),
    panel.grid.major = element_line(color = "grey95", size = 0.3),
    panel.grid.minor = element_blank(),
    legend.key.width = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 5, face = "bold"),  
    legend.text = element_text(size = 5)  
  )


tiff("plots/int_gains.tiff", 
     width = 90, 
     height = 90,  
     units = "mm", 
     res = 600,
     compression = "lzw")

figure2a

dev.off()





          ##### 2.6 df with predicted GPA #####

# sequence of values for NonCog, with low, average, and high performing schools
noncog_vals <- seq(-3, 3, by = 0.05) 
school_levels <- c(-2, 0, 2)

plot_data <- expand.grid(
  NonCog = noncog_vals,
  School_Performance = school_levels,
  Gender = c("Girls", "Boys")
) %>%
  mutate(
    # NonCog coefficient for each gender/school combination
    NonCog_coef = case_when(
      Gender == "Girls" ~ beta_noncog_girls + mult_noncog_girls * School_Performance,
      Gender == "Boys" ~ beta_noncog_boys + mult_noncog_boys * School_Performance
    ),
    
    # fixed gender effect
    Gender_effect = ifelse(Gender == "Girls", (-1)*beta_gender, beta_gender),
    
    # intercept-student GPA relationship for both genders
    School_gender_interaction = case_when(
      Gender == "Boys" ~ mult_int_boys * School_Performance,   # +0.066 at +2SD, -0.066 at -2SD
      Gender == "Girls" ~ mult_int_girls * School_Performance  # -0.066 at +2SD, +0.066 at -2SD
    ),
    
    # expected GPA includes: school intercept, fixed gender effect, intercept*gender, intercept*gender*NonCog
    Expected_GPA = School_Performance * sd_intercept +  
      Gender_effect +
      School_gender_interaction +                       
      NonCog * NonCog_coef,                 
    
    # create labels for plotting
    School_Label = factor(School_Performance, 
                          levels = c(-2, 0, 2),
                          labels = c("Low (-2 SD)", "Average", "High (+2 SD)")),
    
    # factor for coloring
    Color_Group = interaction(Gender, School_Label, sep = ".")
  )


          ##### 2.7 figure 2b #####

# version sent to the cluster for faster computation
all_preds <- read.csv("N:/durable/projects/37323479_Sverre_GPA_gender_gap/cluster_analyses/fig2b_CI.csv")

# merging with point esimates
point_estimates <- select(plot_data, c(NonCog,School_Performance,Gender,Expected_GPA))

ci_summary <- merge(all_preds, point_estimates, by = c("NonCog","School_Performance","Gender"))



# prepare data with eight different groups, 2SD deviations in NonCog and school-GPA for each gender
plot_data_2b <- ci_summary %>%
  mutate(
    school_int = factor(School_Performance,
                        levels = c(-2, 0, 2),
                        labels = c("Low (-2 SD)", "Average", "High (+2 SD)")),
    Gender = factor(Gender, levels = c("Boys", "Girls")),
    NonCog = as.numeric(as.character(NonCog))
  ) %>%
  mutate(
    x_pos = ifelse(NonCog == -2, -0.15, 0.15) # to adjust the x-axis on the plot
  )


# calculating the differences in GPA of different groups
pm <- function(noncog, school, gender) {
  plot_data_2b$Expected_GPA[plot_data_2c$NonCog == noncog & plot_data_2b$School_Performance == school & plot_data_2b$Gender == gender]
}

difference_df <- plot_data_2b %>%
  mutate(
    # difference-in-differences for low noncog students (low vs high school)
    low_noncog_low_high_school = (pm(-2, 2, "Girls") - pm(-2, -2, "Girls")) - 
      (pm(-2, 2, "Boys") - pm(-2, -2, "Boys")),
    
    # difference-in-differences for high noncog students (low vs high school)
    high_noncog_low_high_school = (pm(2, 2, "Girls") - pm(2, -2, "Girls")) - 
      (pm(2, 2, "Boys") - pm(2, -2, "Boys")),
    
    # difference between high and low noncog in high schools
    noncog_diff_high_school = (pm(2, 2, "Girls") - pm(-2, 2, "Girls")) - 
      (pm(2, 2, "Boys") - pm(-2, 2, "Boys")),
    
    # difference between high and low noncog in low schools
    noncog_diff_low_school = (pm(2, -2, "Girls") - pm(-2, -2, "Girls")) - 
      (pm(2, -2, "Boys") - pm(-2, -2, "Boys"))
  )


#plot
figure2b <- ggplot(plot_data_2b, aes(x = x_pos, y = Expected_GPA,
                                     fill = interaction(Gender, school_int),
                                     color = interaction(Gender, school_int),
                                     group = interaction(Gender, school_int))) +

  geom_line(size = 0.6, alpha = 0.4) +
  geom_point(aes(shape = as.factor(School_Performance)), size = 2.2) +
  scale_shape_manual(values = c("2" = 17, "-2" = 15), guide = "none") +
  scale_fill_manual(values = plot_colors, guide = "none") +
  scale_color_manual(values = plot_colors, guide = "none") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = interaction(Gender, school_int)),
                width = 0, size = 0.4, alpha = 0.7) +
  scale_x_continuous(breaks = c(-0.15, 0.15), labels = c("-2", "2"), 
                     limits = c(-0.2, 0.2)) +
  scale_y_continuous(limits = c(-1.7, 1.5), breaks = seq(-1.5, 1.5, by = 0.5)) +
  coord_cartesian(xlim = c(-0.25, 0.25), ylim = c(-1.7, 1.5)) +
  labs(title = "", x = "Polygenic index for non-cognitive skills", y = "Expected GPA") +
  theme_sverdo() +
  theme(
    axis.title.x = element_text(hjust = 0.5), 
    legend.position = "none",
    panel.grid.major = element_line(color = "grey95", size = 0.3),
    panel.grid.minor = element_blank(),
  ) +
  geom_text(data = data.frame(
    x = 0.175,
    y = -1.05,
    label = "Gender and school-level GPA"
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  hjust = 0.5,
  size = 2.2,
  fontface = "bold",
  family = "serif",
  color = "black") +
  #legend
  geom_point(data = data.frame(x = 0.12, y = -1.21),
             aes(x = x, y = y),
             shape = 17, size = 1.8,
             fill = plot_colors["Girls.High (+2 SD)"],
             color = plot_colors["Girls.High (+2 SD)"],
             inherit.aes = FALSE) +
  geom_point(data = data.frame(x = 0.12, y = -1.33),
             aes(x = x, y = y),
             shape = 17, size = 1.8,
             fill = plot_colors["Boys.High (+2 SD)"],
             color = plot_colors["Boys.High (+2 SD)"],
             inherit.aes = FALSE) +
  geom_point(data = data.frame(x = 0.12, y = -1.44),
             aes(x = x, y = y),
             shape = 15, size = 1.8,
             fill = plot_colors["Girls.Low (-2 SD)"],
             color = plot_colors["Girls.Low (-2 SD)"],
             inherit.aes = FALSE) +
  geom_point(data = data.frame(x = 0.12, y = -1.56),
             aes(x = x, y = y),
             shape = 15, size = 1.8,
             fill = plot_colors["Boys.Low (-2 SD)"],
             color = plot_colors["Boys.Low (-2 SD)"],
             inherit.aes = FALSE) +
  geom_text(data = data.frame(
    x = rep(0.132, 4),
    y = c(-1.20, -1.32, -1.44, -1.56),
    label = c("Girls +2 SD", "Boys +2 SD", "Girls -2 SD", "Boys -2 SD")
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  hjust = 0,
  size = 2,
  color = "black")


tiff("plots/figure2b.tiff", 
     width = 90, 
     height = 90, 
     units = "mm", 
     res = 600,
     compression = "lzw")

figure2b

dev.off()


          ##### 2.8 figure 2c_1 #####
figure2c_1 <- ggplot(plot_data %>% filter(Gender == "Girls"),
                   aes(x = NonCog, y = Expected_GPA,
                       color = Color_Group,
                       group = School_Label)) +
  geom_line(size = .9) +
  scale_color_manual(values = plot_colors_girls, guide = "none") +
  labs(
    x = "Polygenic index for non-cognitive skills",
    y = "Expected GPA",
    caption = "Girls"
  ) +
  theme_sverdo() +
  theme(
    plot.caption = element_text(hjust = 0.5, face = "bold", vjust = 1), 
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),  
  ) +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  scale_y_continuous(breaks = seq(-2, 2, .5), limits = c(-1.7, 1.5)) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-1.7, 1.5)) +

  #legend
  geom_text(data = data.frame(
    x = 1.75,
    y = -1.05,
    label = "School-level GPA"
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  hjust = 0.5,
  size = 2.2,  
  fontface = "bold",
  family = "serif",  
  color = "black") +

  geom_rect(data = data.frame(
    xmin = rep(1.34, 3),
    xmax = rep(1.50, 3),
    ymin = c(-1.59, -1.42, -1.25),  
    ymax = c(-1.49, -1.32, -1.15),  
    fill_color = plot_colors_girls[c("Girls.Low (-2 SD)", "Girls.Average", "Girls.High (+2 SD)")] 
  ),
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_color),
  color = NA,
  size = 0.3,
  inherit.aes = FALSE) +
  scale_fill_identity() +
  
  geom_text(data = data.frame(
    x = rep(1.55, 3),
    y = c(-1.20, -1.37, -1.54),  
    label = c("+2 SD", "Average", "-2 SD")
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  hjust = 0,
  family = "serif",
  size = 2,
  color = "black")


          ##### 2.9 figure2c_2 #####

figure2c_2 <- ggplot(plot_data %>% filter(Gender == "Boys"),
                   aes(x = NonCog, y = Expected_GPA,
                       color = Color_Group,
                       group = School_Label)) +
  geom_line(size = .9) +
  scale_color_manual(values = plot_colors_boys, guide = "none") +
  labs(
    x = "Polygenic index for non-cognitive skills",
    y = "Expected GPA",
    caption = "Boys"
  ) +
  theme_sverdo() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    plot.caption = element_text(hjust = 0.5, face = "bold", vjust = 1)
  ) +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  scale_y_continuous(breaks = seq(-2, 2, .5), limits = c(-1.7, 1.5)) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-1.7, 1.5)) +
  #legend
  geom_text(data = data.frame(
    x = -1.75,
    y = 1.40,
    label = "School-level GPA"
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  hjust = 0.5,
  size = 2.2,
  fontface = "bold",
  family = "serif",
  color = "black") +

  geom_rect(data = data.frame(
    xmin = rep(-1.99, 3),
    xmax = rep(-2.15, 3),
    ymin = c(1.20, 1.03, 0.86),
    ymax = c(1.30, 1.13, 0.96),
    fill_color = plot_colors_boys[c("Boys.High (+2 SD)", "Boys.Average", "Boys.Low (-2 SD)")]
  ),
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_color),
  color = NA,
  size = 0.3,
  inherit.aes = FALSE) +
  scale_fill_identity() +

  geom_text(data = data.frame(
    x = rep(-1.94, 3),
    y = c(1.25, 1.08, 0.91),
    label = c("+2 SD", "Average", "-2 SD")
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  hjust = 0,
  size = 2,
  family = "serif",
  color = "black")


          ##### 2.55. figure 2c ##### 


tiff("plots/figure2c.tiff", 
     width = 180, 
     height = 90,  # adjust as needed
     units = "mm", 
     res = 600,
     compression = "lzw")

grid.arrange(
  figure2c_1, figure2c_2,
  ncol = 2,
  widths = c(1, 1)
)

dev.off()




  
  