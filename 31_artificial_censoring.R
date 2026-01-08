library(dplyr)
library(parallel)
library(lme4)
library(boot)
set.seed(1337)

data <- read.csv("/ess/p805/data/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/final_data.csv")

censoring_level <- Sys.getenv("CENSOR_LEVEL")

# censoring at different censoring levels, we skip censoring for the first iteration
if (censoring_level == "uncensored") {
  data_censored <- data
} else {
  censoring_level <- as.numeric(censoring_level)  # convert here, inside else block
  data_censored <- data %>%
    mutate(GPA_censored = ifelse(grades_std > censoring_level, censoring_level, grades_std))
}

# fit model
fit <- lmer(GPA_censored ~ cog_g*kjoenn_g+
              noncog_g*kjoenn_g+
              cog_parental_g*cog_g*kjoenn_g+
              cog_parental_g*noncog_g*kjoenn_g+
              noncog_parental_g*cog_g*kjoenn_g+
              noncog_parental_g*noncog_g*kjoenn_g+
              (1 | lopenr_mor) + (1+kjoenn_g*noncog_g | lnr_org),
            data = data_censored,
            REML = T,
            control = lmerControl(optimizer = "bobyqa"))

  # define the function to extract parameters of interest
extract_params <- function(fit) {
  
# extract variance/covariance terms with intercept
random_var <- as.matrix(VarCorr(fit)$lnr_org)

sd_intercept <- sqrt(random_var["(Intercept)", "(Intercept)"]) #SD of intercept
cov_noncog_int <- random_var["noncog_g", "(Intercept)"] #Noncog-intercept covariance
cov_noncoggender_int <- random_var["kjoenn_g:noncog_g", "(Intercept)"] #gender*Noncog-intercept covariance

# calculate slopes
mult_girls <- ((cov_noncog_int) - (cov_noncoggender_int)) / sd_intercept
mult_boys <- ((cov_noncog_int) + (cov_noncoggender_int)) / sd_intercept

# return all parameters of interest
return(c(noncog_int_cor_boys = mult_boys,
         noncog_int_cor_girls = mult_girls))
  }


# set up parallel
n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

cl <- makeCluster(n_cores)

clusterEvalQ(cl, {
  library(lme4)
  library(dplyr)
  library(boot)
})

clusterExport(cl, varlist = c("extract_params","data_censored","data"), envir = environment())

# run bootstrap
n_iterations <- 20000
set.seed(1337)

boot_results <- bootMer(fit,
                        FUN = extract_params,
                        nsim = n_iterations,
                        type = "parametric",
                        parallel = "snow",
                        ncpus = n_cores,
                        cl = cl,
                        .progress = "txt")

stopCluster(cl)

ci_results <- apply(boot_results$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)

# get the estimated effect from the model
random_var <- as.matrix(VarCorr(fit)$lnr_org)

sd_intercept <- sqrt(random_var["(Intercept)", "(Intercept)"]) #SD of intercept
cov_noncog_int <- random_var["noncog_g", "(Intercept)"] #Noncog-intercept covariance
cov_noncoggender_int <- random_var["kjoenn_g:noncog_g", "(Intercept)"] #gender*Noncog-intercept covariance

# calculate slopes
girls <- ((cov_noncog_int) - (cov_noncoggender_int)) / sd_intercept
boys <- ((cov_noncog_int) + (cov_noncoggender_int)) / sd_intercept


# calculate confidence intervals
ci_results <- data.frame(
  censoring_level = censoring_level,
  boys = boys,
  boys_lower = ci_results[1,1],
  boys_upper = ci_results[2,1],
  girls = girls,
  girls_lower = ci_results[1,2],
  girls_upper = ci_results[2,2]  
)

output_file <- sprintf("artificial_censoring_%.2f.csv", censoring_level)
write.csv(ci_results, output_file, row.names = FALSE)