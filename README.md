# File information
- 01_cleaning.R: cleaning of individual-level data (GPA, PGIs, mother & father IDs)
- 02_school_variables.R: computation of the candidate environment models
- 03_last_cleaning.R: merging of individual- and school-level variables. Also group-mean centers and scales all variables.
- 04_candidate_models.R: model specification and estimation of all candidate environment models, as well as candidate environment models with squared terms.
- 05_random_effects_models.R: specification and estimation of all the random effects models that were compared.

- 11_results_random_models.R: creates plots and tables for the fixed and random effects from the random effects models.
- 12_results_candidate_models.R: calculates gender-specific estimates and CIs from all the candidate environment models. Also creates plots and tables.
- 21_descriptives.R: computes descriptive statistics
- 22_candidate_models_additional_analyses.R: includes additional analyses performed for some of the candidate models.
- 23_candidate_models_term_extraction.R: extraction of gender-specific estimates from all candidate environment models, calculates gender-specific CIs, and creates a table from the linear as well as non-linear candidate models.
- 24_misc.R: gender-specific intra-class correlations across schols, gene-environment correlation with and without parental PGIs. Random effects model comparison table, artificial censoring sensitivity analysis plot, missingness pattern table, confidence intervals for all random effect estimates from the best-fitting model.
- 25_tests_and_grades.R: Additional analyses that include random and fixed effects models for specific subject grades and test scores. Also creates plots and tables

- 31_artificial_censoring.R: creates bootstrapped CIs for the artificial censoring sensitivity analysis plot.
- 33_fig2b_CI.R: creates bootstrapped CIs for figure 2b.
- 34_random_effects_CI.R: creates bootstrapped CIs for all random effects from the best-fitting random effects model.
