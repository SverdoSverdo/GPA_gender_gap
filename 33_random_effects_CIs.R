library(lme4)
library(boot)
library(dplyr)
library(parallel)
set.seed(1337)

# load data
data <- read.csv("/ess/p805/data/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")

# fit model
fit <- lmer(grades_std ~ cog_g*kjoenn_g +
              noncog_g*kjoenn_g +
              cog_parental_g*cog_g*kjoenn_g +
              cog_parental_g*noncog_g*kjoenn_g +
              noncog_parental_g*noncog_g*kjoenn_g +
              noncog_parental_g*cog_g*kjoenn_g +
              (1 | lopenr_mor) + (1+kjoenn_g*noncog_g | lnr_org),
            data = data,
            REML = T,
            control = lmerControl(optimizer = "bobyqa"))

# function to extract the 4 values
random_CIs <- function(fit) {

  #extract parameters we need
  random_var <- as.matrix(VarCorr(fit)$lnr_org)
  
  sd_intercept <- sqrt(random_var["(Intercept)", "(Intercept)"]) #SD of intercept
  
  cov_gender_int <- random_var["kjoenn_g", "(Intercept)"] #gender-intercept covariance
  cov_noncog_int <- random_var["noncog_g", "(Intercept)"] #NonCog-intercept covariance

  cov_noncoggender_int <- random_var["kjoenn_g:noncog_g", "(Intercept)"] #gender*NonCog-intercept covariance

  # expected random slopes given the school's intercept
  cor_gender_int <- cov_gender_int / (sd_intercept)
  
  mult_int_girls <- -cor_gender_int
  mult_int_boys <- cor_gender_int
  
  # gender-specific slope of NonCog over school-level intercept
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
  

  return(c(int_gain_boys_low, int_gain_boys_high,int_gain_girls_low,
           int_gain_girls_high))
}


# setup parallel
n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

cl <- makeCluster(n_cores)

clusterEvalQ(cl, {
  library(lme4)
})

clusterExport(cl, varlist = c("random_CIs","data"), envir = environment())


# run bootstrap
n_iterations <- 20000


boot_results <- bootMer(fit, 
                        FUN = random_CIs,
                        nsim = n_iterations,
                        type = "parametric",
                        parallel = "snow",
                        ncpus = n_cores,
                        cl = cl,
                        .progress = "txt")

stopCluster(cl)

# convert to dataframe
result_df <- as.data.frame(boot_results$t)

# Save
write.csv(result_df, "random_effects_CIs.csv", row.names = FALSE)

