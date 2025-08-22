# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-08-21
#
# Script Name: 31_ML_model_comparison_significance_testing.R
#
# Script Description: This script compares the performances of all models 
# to find significant differences and see which model and which feature set
# performs best.
#
#
# Notes: Model performance results were calculated in the scripts
# 11 (model A), 16 (model B), 20 (model C), 25 (model D) and 29 (model E)
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
               "lmerTest")


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
# boot_performance_E <- readRDS(here::here(
  # "data", "intermediate", "bootstrap", "boot_results_performace_E.rds"))
# error_dfs_E <- lapply(boot_performance_E, function(x) x[[length(x)]])
# boot_performance_E <- lapply(boot_performance_E, function(x) x[-length(x)])

list_performances <- list(A = boot_performance_A,
                          B = boot_performance_B,
                          C = boot_performance_C,
                          D = boot_performance_D #,
                          #E = boot_performance_E
                          )

## list of error dfs
list_error_dfs <- list(A = error_dfs_A,
                       B = error_dfs_B,
                       C = error_dfs_C,
                       D = error_dfs_D #,
                       #E = error_dfs_E
                       )

## next step: Create dataframe with the variables feature_set, model_name,
## rmse_est, r2_est, mae_est,
## rmse_ci_lower, rmse_ci_upper,
## r2_ci_lower, r2_ci_upper,
## mae_ci_lower, mae_ci_upper

## this df will be used for summary tables of the ML performances

list_model_metric_dfs <- lapply(1:length(list_performances), function(x){
  df_metrics <- data.frame()
  feature_set <- names(list_performances)[x]
  ## create a new list from the list currently being iterated where the 
  ## last list element is not included
  
  for(mod in 1:length(list_performances[[x]])){
    metrics <- c(feature_set = feature_set, unlist(list_performances[[x]][[mod]]))
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
  mutate(across(where(is.character), as.factor))

##----------------------------------------------------------------------------

## significance testing: using the error dfs

## create one df out of all error dfs. 
full_error_df <- do.call(rbind,
                         lapply(list_error_dfs, function(x) {
                           do.call(rbind, x)
                         })) %>%
  mutate(across(where(is.character), as.factor))

rownames(full_error_df) <- NULL


## Friedman test: Are there differences in the errors between the 
## algorithms?

## Note: Not possible to compare all sets against each other since A and D 
## have more observations than B, C and E

## For now: Calculate Friedman test for A vs. D and B vs. C vs. E separately

## A vs. D, testing squared error
df_A_wide_sq <- full_error_df %>%
  filter(feature_set == "A") %>%
  select(FISNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

friedman.test(as.matrix(df_A_wide_sq[ , -1]))
## no significant differences between the algorithms of feature set A

df_D_wide_sq <- full_error_df %>%
  filter(feature_set == "D") %>%
  select(FISNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

friedman.test(as.matrix(df_D_wide_sq[ , -1]))
## significant differences between the algorithms of feature set D

nemenyi(data = df_D_wide_sq[, -1], sort = TRUE, plottype = "mcb")
nemenyi(data = df_D_wide_sq[, -1], sort = TRUE, plottype = "vmcb")
nemenyi(data = df_D_wide_sq[, -1], sort = TRUE, plottype = "line")
nemenyi(data = df_D_wide_sq[, -1], sort = TRUE, plottype = "vline")



df_AD_wide_sq <- inner_join(df_A_wide_sq, df_D_wide_sq,
                            by = "FISNumber", suffix = c("_A", "_D"))

friedman.test(as.matrix(df_AD_wide_sq[ , -1]))
## significant, check follow up 
nemenyi(data = df_AD_wide_sq[, -1], sort = TRUE, plottype = "mcb")

## calculate pair-wise Nemenyi's test







##-----------------------------------------------------------------------------

## B vs. C (vs. E), testing squared error
df_B_wide_sq <- full_error_df %>%
  filter(feature_set == "B") %>%
  select(FISNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

friedman.test(as.matrix(df_B_wide_sq[ , -1]))
## no significant differences between the algorithms of feature set B

df_C_wide_sq <- full_error_df %>%
  filter(feature_set == "C") %>%
  select(FISNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

friedman.test(as.matrix(df_C_wide_sq[ , -1]))
## no significant differences between the algorithms of feature set C
nemenyi(data = df_C_wide_sq[, -1], sort = TRUE, plottype = "mcb")

df_BC_wide_sq <- inner_join(df_B_wide_sq, df_C_wide_sq,
                            by = "FISNumber", suffix = c("_B", "_C"))

friedman.test(as.matrix(df_BC_wide_sq[ , -1]))

## significant, check follow up
nemenyi(data = df_BC_wide_sq[, -1], sort = TRUE, plottype = "mcb")
## random forest and stacked lm of feature set C perform significantly better
## than all feature set B models

## Alternative: Use only errors of individuals who have genetic data 
## to also be able to directly compare A, B, C, D against each other

ids_BC <- unique(full_error_df %>%
  filter(feature_set == "B" | feature_set == "B") %>%
  select(FISNumber) %>%
  pull())

ids_AD <- unique(full_error_df %>%
  filter(feature_set == "A" | feature_set == "D") %>%
  select(FISNumber) %>%
  pull())

ids_ABCD <- intersect(ids_AD, ids_BC)


df_A_wide_sq_2 <- full_error_df %>%
  filter(feature_set == "A" & FISNumber %in% ids_ABCD) %>%
  select(FISNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

df_B_wide_sq_2 <- full_error_df %>%
  filter(feature_set == "B" & FISNumber %in% ids_ABCD) %>%
  select(FISNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

df_C_wide_sq_2 <- full_error_df %>%
  filter(feature_set == "C" & FISNumber %in% ids_ABCD) %>%
  select(FISNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

df_D_wide_sq_2 <- full_error_df %>%
  filter(feature_set == "D" & FISNumber %in% ids_ABCD) %>%
  select(FISNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

## join all 4 previous dfs together by FISNumber and give the columns 
## that would otherwise have the same names suffixes with
## suffix = c("_A", "_B", "_C", "_D")
df_ABCD_wide_sq <- df_A_wide_sq_2 %>%
  inner_join(df_B_wide_sq_2, by = "FISNumber", suffix = c("_A", "_B")) %>%
  inner_join(df_C_wide_sq_2, by = "FISNumber") %>%
  rename("lm_stack_C" = lm_stack, "rf_C" = rf, "xgb_C" = xgb) %>%
  inner_join(df_D_wide_sq_2, by = "FISNumber") %>%
  rename("xgb_stack_A" = xgb_stack,
         "lm_stack_D" = lm_stack, "rf_D" = rf, "xgb_D" = xgb)

nemenyi(data = df_ABCD_wide_sq[, -1], sort = TRUE, plottype = "mcb")

## here, no dominance of one method but sample size much smaller thus 
## more uncertainty






## alternative: linear mixed effects model with lme4::lmer
model_set <- lmerTest::lmer(
  sq_error ~ feature_set + algorithm + algorithm* feature_set + (1 | FISNumber), 
  data = full_error_df)

summary(model_set)

anova(model_set)

library(emmeans)

emm_options(pbkrtest.limit = 20000, lmerTest.limit = 20000)
emm_alg <- emmeans(model_set, ~ algorithm)
pairs(emm_alg, adjust = "holm")  # pairwise comparisons with p-value correction

emm_set <- emmeans(model_set, ~ feature_set)
pairs(emm_set, adjust = "holm")  # pairwise comparisons with p-value correction
## Does not get estimated, uncler why
## CONTINUE HERE!!!


