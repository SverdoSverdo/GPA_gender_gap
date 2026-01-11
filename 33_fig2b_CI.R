library(lme4)
library(boot)
library(dplyr)
library(parallel)
set.seed(1337)

# Load data and fit model
data <- read.csv("/ess/p805/data/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")

mod <- lmer(grades_std ~ cog_g*kjoenn_g + 
              noncog_g*kjoenn_g +
              cog_parental_g*cog_g*kjoenn_g +
              cog_parental_g*noncog_g*kjoenn_g +
              noncog_parental_g*noncog_g*kjoenn_g +
              noncog_parental_g*cog_g*kjoenn_g +
              (1 | lopenr_mor) + (1+kjoenn_g*noncog_g | lnr_org),
            data = data,
            REML = FALSE,
            control = lmerControl(optimizer = "bobyqa"))


# create prediction grid
noncog_vals <- c(-2, 2)
school_levels <- c(-2, 2)

result_df <- expand.grid(
  NonCog = noncog_vals,
  School_Performance = school_levels,
  Gender = c("Girls", "Boys")
)

# expected GPA function
predict_fun <- function(fit) {
  # recreate result_df inside the function
  noncog_vals <- c(-2, 2)
  school_levels <- c(-2, 2)
  
  result_df <- expand.grid(
    NonCog = noncog_vals,
    School_Performance = school_levels,
    Gender = c("Girls", "Boys")
  )
  
  # extract parameters
  beta_N <- fixef(fit)["noncog_g"]
  beta_NxG <- fixef(fit)["kjoenn_g:noncog_g"]
  beta_G <- fixef(fit)["kjoenn_g"]
  
  # parameters we need
  random_var <- as.matrix(VarCorr(fit)$lnr_org)
  
  sd_gender <- sqrt(random_var["kjoenn_g", "kjoenn_g"])
  sd_intercept <- sqrt(random_var["(Intercept)", "(Intercept)"])

  cov_gender_int <- random_var["kjoenn_g", "(Intercept)"] #gender-intercept covariance
  cov_noncog_int <- random_var["noncog_g", "(Intercept)"] #NonCog-intercept covariance
  cov_noncoggender_int <- random_var["kjoenn_g:noncog_g", "(Intercept)"] #gender*NonCog-intercept covariance
  
  mult_girls <- (cov_noncog_int - cov_noncoggender_int) / sd_intercept
  mult_boys <- (cov_noncog_int + cov_noncoggender_int) / sd_intercept
  
  school_slope_diff <- cov_gender_int / sd_intercept
  
  gender_effect_boys <- beta_G
  gender_effect_girls <- beta_G * (-1)
  
  # calculate expected GPA
  predictions <- numeric(nrow(result_df))
  
  for(i in 1:nrow(result_df)) {
    noncog <- result_df$NonCog[i]
    School_Performance <- result_df$School_Performance[i]
    gender <- result_df$Gender[i]
    
    NonCog_coef <- ifelse(gender == "Girls",
                          beta_N - beta_NxG + mult_girls * School_Performance,
                          beta_N + beta_NxG + mult_boys * School_Performance)
    
    Gender_effect <- ifelse(gender == "Girls", 
                            gender_effect_girls, 
                            gender_effect_boys)
    
    School_gender_interaction <- ifelse(gender == "Boys",
                                        school_slope_diff * School_Performance,
                                        -school_slope_diff * School_Performance)
    
    predictions[i] <- School_Performance * intercept_sd +
      Gender_effect +
      School_gender_interaction +
      noncog * NonCog_coef
  }
  
  return(predictions)
}


# setup parallel
n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
cl <- makeCluster(n_cores)

clusterEvalQ(cl, {
  library(lme4)
  library(dplyr)
  library(boot)
})

# Export only what's needed (data is already in mod object)
clusterExport(cl, varlist = c("predict_fun","data"), envir = environment())

# Run bootstrap
n_iterations <- 20000

boot_results <- bootMer(mod, 
                        FUN = predict_fun,
                        nsim = n_iterations,
                        type = "parametric",
                        parallel = "snow",
                        ncpus = n_cores,
                        cl = cl,
                        .progress = "txt")

stopCluster(cl)

# extract results
all_preds <- boot_results$t

# function that calculates CIs
ci_results <- t(apply(all_preds, 2, function(x) {
  quantile(x, probs = c(0.025, 0.975))
}))

result_df$ci_lower <- ci_results[, 1]
result_df$ci_upper <- ci_results[, 2]

write.csv(result_df, "fig2b_CI.csv", row.names = FALSE)
