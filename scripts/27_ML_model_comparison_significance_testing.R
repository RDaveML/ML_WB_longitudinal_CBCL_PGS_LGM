# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-08-21
#
# Script Name: 27_ML_model_comparison_significance_testing.R
#
# Script Description: This script compares the performances of all models 
# to find significant differences and see which model and which variable set
# perform best.
#
#
# Notes: Model performance results were calculated in the scripts
# 09 (model A), 13 (model B), 16 (model C), 20 (model D) and 23 (model E)
#
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "haven", "foreign", "here", "readr",
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "fastDummies", "RANN",
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest", "boot", "iml",
               "fastshap", "shapviz", "PMCMRplus", "tsutils", "lme4",
               "lmerTest", "emmeans", "clusrank", "flextable",
               "rempsyc", "kableExtra")


## loading in bootstrapped model performance measures: points estimates and CIs

## model A
boot_performance_A <- readRDS(here::here(
  "data", "intermediate", "bootstrap", "boot_results_performace_A.rds"))
## save the sub_elements error_df in new list
## and remove them from the sub_elements of the original lists
error_dfs_A <- lapply(boot_performance_A, function(x) x[[length(x)]])
boot_performance_A <- lapply(boot_performance_A, function(x) x[-length(x)])



## model B
boot_performance_B <- readRDS(here::here(
  "data", "intermediate", "bootstrap", "boot_results_performace_B.rds"))
error_dfs_B <- lapply(boot_performance_B, function(x) x[[length(x)]])
boot_performance_B <- lapply(boot_performance_B, function(x) x[-length(x)])

## model C
boot_performance_C <- readRDS(here::here(
  "data", "intermediate", "bootstrap", "boot_results_performace_C.rds"))
error_dfs_C <- lapply(boot_performance_C, function(x) x[[length(x)]])
boot_performance_C <- lapply(boot_performance_C, function(x) x[-length(x)])

## model D
boot_performance_D <- readRDS(here::here(
  "data", "intermediate", "bootstrap", "boot_results_performace_D.rds"))
error_dfs_D <- lapply(boot_performance_D, function(x) x[[length(x)]])
boot_performance_D <- lapply(boot_performance_D, function(x) x[-length(x)])


## model E
boot_performance_E <- readRDS(here::here(
   "data", "intermediate", "bootstrap", "boot_results_performace_E.rds"))
error_dfs_E <- lapply(boot_performance_E, function(x) x[[length(x)]])
boot_performance_E <- lapply(boot_performance_E, function(x) x[-length(x)])

list_performances <- list(A = boot_performance_A,
                          B = boot_performance_B,
                          C = boot_performance_C,
                          D = boot_performance_D, 
                          E = boot_performance_E
                          )

## list of error dfs
list_error_dfs <- list(A = error_dfs_A,
                       B = error_dfs_B,
                       C = error_dfs_C,
                       D = error_dfs_D,
                       E = error_dfs_E
                       )

## next step: Create dataframe 
## this df will be used for summary tables of the ML performances

list_model_metric_dfs <- lapply(1:length(list_performances), function(x){
  df_metrics <- data.frame()
  feature_set <- names(list_performances)[x]
  
  for(mod in 1:length(list_performances[[x]])){
    metrics <- c(feature_set = feature_set,
                 unlist(list_performances[[x]][[mod]]))
    df_metrics <- rbind(df_metrics, metrics)
  }
  
  names(df_metrics) <- c("feature_set", "algorithm",
                         "rmse_est", "r2_est", "mae_est",
                         "rmse_ci_lower", "rmse_ci_upper",
                         "r2_ci_lower", "r2_ci_upper",
                         "mae_ci_lower", "mae_ci_upper")
  return(df_metrics)
})

model_performance_df <- do.call(rbind, list_model_metric_dfs) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(contains("rmse") | 
                contains("r2") |
                contains("mae"),
         as.character)) %>%
  mutate(across(contains("rmse") |
                contains("r2") |
                contains("mae"),
         as.numeric)) %>%
  filter(algorithm != "xgb_stack") %>% ## was dropped from analyses
  droplevels() ## dropping unused levels

## saving model performance df
if(!dir.exists(here::here("data", "final"))){
  dir.create(here::here("data", "final"))
}
saveRDS(model_performance_df,
        file = here::here("data", "final", "model_performance_df_OG.rds"))

##############################################################################

## significance testing: using the error dfs

## create one df out of all error dfs. 
full_error_df <- do.call(rbind,
                         lapply(list_error_dfs, function(x) {
                           do.call(rbind, x)
                         })) %>%
  mutate(across(where(is.character), as.factor)) #%>%
  # filter(algorithm == "rf" | algorithm == "lm_stack")

rownames(full_error_df) <- NULL

saveRDS(full_error_df,
        file = here::here("data", "final", "full_error_df_OG.rds"))


## model performance: testing for significance
## with linear mixed effects model 
## pairwise comparisons, including clustering for families (correlated errors)
## and individuals (some individuals appear in multiple feature sets)

## model: regress error (per individual) on variable set and algorithm,
## their interaction and respect nested structure (families and individuals
## appear in multiple variable sets)
model_set <- lmerTest::lmer(
  sq_error ~ feature_set + 
             algorithm +
             algorithm * feature_set +
             (1 | FamilyNumber/FISNumber),
  data = filter(full_error_df, algorithm != "xgb_stack")) # was dropped

summary(model_set)

anova(model_set)

# pairwise comparisons with p-value correction (fdr corrected)
emm_options(pbkrtest.limit = 20000, lmerTest.limit = 20000)
emm_alg <- emmeans(model_set, ~ algorithm)
pairs(emm_alg, adjust = "fdr")

## This indicates that in general, the random forest performs best across 
## all feature sets (Interaction not respected though)

emm_set <- emmeans(model_set, ~ feature_set)
pairs(emm_set, adjust = "fdr")


## pairwise comparisons where both algorithm and variable set are included
emm_alg_set <- emmeans(model_set, ~ algorithm*feature_set)

pairs(emm_alg_set, adjust = "fdr") %>% 
  as.data.frame() %>%
  filter(p.value < 0.05)

nrow(pairs(emm_alg_set, adjust = "fdr") %>% 
       as.data.frame())


nrow(pairs(emm_alg_set, adjust = "fdr") %>% 
       as.data.frame() %>% 
       filter(p.value < 0.05))
## 50 / 105 are significant, full table in supplement of paper

saveRDS(emm_alg_set, 
        file = here::here("data", "final", "comparison_lmer.rds"))


## filtering only for pairs with same algorithm
comparisons <- as.data.frame(pairs(emm_alg_set, adjust = "fdr"))[, "contrast"]
comparisons <- comparisons[grepl("\\blm_stack\\b.*\\blm_stack\\b", comparisons)| 
                           grepl("\\brf\\b.*\\brf\\b", comparisons) | 
                           grepl("\\bxgb\\b.*\\bxgb\\b", comparisons)]

alg_constant <- 
as.data.frame(pairs(emm_alg_set, adjust = "fdr")) %>%
  filter(contrast %in% comparisons)

nrow(alg_constant)

alg_constant_sig <- alg_constant %>%
  filter(p.value < 0.05) %>%
  arrange(contrast)

alg_constant_sig

nrow(alg_constant_sig)

saveRDS(alg_constant_sig, 
        file = here::here("data", "final", "comparison_lmer_sig_alg.rds"))

## code for tables and figures in report based on the calculations 
## in this script is available from author upon request

# eoS


