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
               "lmerTest", "emmeans", "clusrank")


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


## purely descriptively, we see that the point estimats consistently indicate
## higher performance of the random forest models over the 
## xgb models
## since they are the most parsimoneous models, they will be used for the 
## analysis of the comparisons of the feature set

##----------------------------------------------------------------------------

## significance testing: using the error dfs

## create one df out of all error dfs. 
full_error_df <- do.call(rbind,
                         lapply(list_error_dfs, function(x) {
                           do.call(rbind, x)
                         })) %>%
  mutate(across(where(is.character), as.factor)) #%>%
  # filter(algorithm == "rf" | algorithm == "lm_stack")

rownames(full_error_df) <- NULL


## Friedman test: Are there differences in the errors between the 
## algorithms?

## Note: Not possible to compare all sets against each other since A and D 
## have more observations than B, C and E

## For now: Calculate Friedman test for A vs. D and B vs. C vs. E separately

## A vs. D, testing squared error
df_A_wide_sq <- full_error_df %>%
  filter(feature_set == "A") %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

friedman.test(as.matrix(df_A_wide_sq[ , -c(1, 2)]))
## no significant differences between the algorithms of feature set A

df_D_wide_sq <- full_error_df %>%
  filter(feature_set == "D") %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

friedman.test(as.matrix(df_D_wide_sq[ , -c(1, 2)]))
## significant differences between the algorithms of feature set D

nemenyi(data = df_D_wide_sq[, -1], sort = TRUE, plottype = "mcb")
nemenyi(data = df_D_wide_sq[, -1], sort = TRUE, plottype = "vmcb")
nemenyi(data = df_D_wide_sq[, -1], sort = TRUE, plottype = "line")
nemenyi(data = df_D_wide_sq[, -1], sort = TRUE, plottype = "vline")



df_AD_wide_sq <- inner_join(df_A_wide_sq, df_D_wide_sq,
                            by = c("FISNumber", "FamilyNumber"),
                            suffix = c("_A", "_D"))

friedman.test(as.matrix(df_AD_wide_sq[ , -c(1, 2)]))
## significant, check follow up 
nemenyi(data = df_AD_wide_sq[, -c(1, 2)], sort = TRUE, plottype = "mcb")

## calculate pair-wise Nemenyi's test

## Wilcoxon signed-rank test: Also including familyNr because errors 
## within families are correlated
## note: this only works for algorithms that have the same participants

## example for comparing rf_A against rf_D
clusWilcox.test(x = df_AD_wide_sq$rf_A,
                y = df_AD_wide_sq$rf_D,
                cluster = df_AD_wide_sq$FamilyNumber,
                paired = TRUE, method = "rgl")


## comparing all algorithms pairwise (multiple testing not yet addressed!)
## A / D
test_counter <- 0
for(alg in 3:ncol(df_AD_wide_sq)){
  for(alg2 in 3:ncol(df_AD_wide_sq)){
    if(alg == alg2 | alg < alg2){
      next
    }
    cat("Comparison algorithm ", colnames(df_AD_wide_sq)[alg],
        " against algorithm ", colnames(df_AD_wide_sq)[alg2], "\n")
    
    print(
    clusWilcox.test(x = as.numeric(unlist(df_AD_wide_sq[, alg])),
                    y = as.numeric(unlist(df_AD_wide_sq[, alg2])),
                    cluster = df_AD_wide_sq$FamilyNumber,
                    paired = TRUE, method = "rgl")
    )
    
    cat("\n")
    test_counter <- test_counter + 1
    cat("test_counter: ", test_counter, "\n", "\n")
  }
}




##-----------------------------------------------------------------------------

## B vs. C (vs. E), testing squared error
df_B_wide_sq <- full_error_df %>%
  filter(feature_set == "B") %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

friedman.test(as.matrix(df_B_wide_sq[ , -c(1, 2)]))
## no significant differences between the algorithms of feature set B

df_C_wide_sq <- full_error_df %>%
  filter(feature_set == "C") %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

friedman.test(as.matrix(df_C_wide_sq[ , -c(1, 2)]))
## no significant differences between the algorithms of feature set C
nemenyi(data = df_C_wide_sq[, -c(1, 2)], sort = TRUE, plottype = "mcb")

df_E_wide_sq <- full_error_df %>%
  filter(feature_set == "E") %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

friedman.test(as.matrix(df_E_wide_sq[ , -c(1, 2)]))
## significant differences between the algorithms of feature set E
nemenyi(data = df_E_wide_sq[, -c(1, 2)], sort = TRUE, plottype = "mcb")

df_BCE_wide_sq <- inner_join(df_B_wide_sq, df_C_wide_sq,
                             by = c("FISNumber", "FamilyNumber"),
                             suffix = c("_B", "_C")) %>%
  inner_join(df_E_wide_sq, by = c("FISNumber", "FamilyNumber")) %>%
  rename("lm_stack_E" = lm_stack, "rf_E" = rf, "xgb_E" = xgb)

friedman.test(as.matrix(df_BCE_wide_sq[ , -c(1, 2)]))

## significant, check follow up
nemenyi(data = df_BCE_wide_sq[, -c(1, 2)], sort = TRUE, plottype = "mcb")
## random forest and stacked lm of feature set C perform significantly better
## than all feature set B models


## comparing all algorithms pairwise (multiple testing not yet addressed!)
## B / C / E
## with the Wilcoxon signed rank test
test_counter <- 0
for(alg in 3:ncol(df_BCE_wide_sq)){
  for(alg2 in 3:ncol(df_BCE_wide_sq)){
    if(alg == alg2 | alg < alg2){
      next
    }
    cat("Comparison algorithm ", colnames(df_BCE_wide_sq)[alg],
        " against algorithm ", colnames(df_BCE_wide_sq)[alg2], "\n")
    
    print(
      clusWilcox.test(x = as.numeric(unlist(df_BCE_wide_sq[, alg])),
                      y = as.numeric(unlist(df_BCE_wide_sq[, alg2])),
                      cluster = df_BCE_wide_sq$FamilyNumber,
                      paired = TRUE, method = "rgl")
    )
    
    cat("\n")
    test_counter <- test_counter + 1
    cat("test_counter: ", test_counter, "\n", "\n")
  }
}



## Alternative: Use only errors of individuals who have genetic data 
## to also be able to directly compare A, B, C, D against each other

ids_BCE <- unique(full_error_df %>%
  filter(feature_set == "B" | feature_set == "B") %>%
  select(FISNumber) %>%
  pull())

ids_AD <- unique(full_error_df %>%
  filter(feature_set == "A" | feature_set == "D") %>%
  select(FISNumber) %>%
  pull())

ids_ABCDE <- intersect(ids_AD, ids_BCE)


df_A_wide_sq_2 <- full_error_df %>%
  filter(feature_set == "A" & FISNumber %in% ids_ABCDE) %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

df_B_wide_sq_2 <- full_error_df %>%
  filter(feature_set == "B" & FISNumber %in% ids_ABCDE) %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

df_C_wide_sq_2 <- full_error_df %>%
  filter(feature_set == "C" & FISNumber %in% ids_ABCDE) %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

df_D_wide_sq_2 <- full_error_df %>%
  filter(feature_set == "D" & FISNumber %in% ids_ABCDE) %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )

df_E_wide_sq_2 <- full_error_df %>%
  filter(feature_set == "E" & FISNumber %in% ids_ABCDE) %>%
  select(FISNumber, FamilyNumber, algorithm, sq_error) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = sq_error
  )


## join all 4 previous dfs together by FISNumber and give the columns 
## that would otherwise have the same names suffixes with
## suffix = c("_A", "_B", "_C", "_D")
df_ABCDE_wide_sq <- df_A_wide_sq_2 %>%
  inner_join(df_B_wide_sq_2, by = c("FISNumber", "FamilyNumber"),
             suffix = c("_A", "_B")) %>%
  inner_join(df_C_wide_sq_2, by = c("FISNumber", "FamilyNumber")) %>%
  rename("lm_stack_C" = lm_stack, "rf_C" = rf, "xgb_C" = xgb) %>%
  inner_join(df_D_wide_sq_2, by = c("FISNumber", "FamilyNumber")) %>%
  rename("lm_stack_D" = lm_stack, "rf_D" = rf, "xgb_D" = xgb) %>%
  inner_join(df_E_wide_sq_2, by = c("FISNumber", "FamilyNumber")) %>%
  rename("xgb_stack_A" = xgb_stack,
         "lm_stack_E" = lm_stack, "rf_E" = rf, "xgb_E" = xgb)
  

nemenyi(data = df_ABCDE_wide_sq[, -c(1:2)], sort = TRUE, plottype = "mcb")

## here, no dominance of one method but sample size much smaller thus 
## more uncertainty






## alternative: linear mixed effects model with lme4::lmer
## taking out the xgb_stacked model of feature set A to enable pairwise
## comparisons, including clustering for families (correlated errors) and 
## individuals (some individuals appear in multiple feature sets)

model_set <- lmerTest::lmer(
  sq_error ~ feature_set + 
             algorithm +
             algorithm * feature_set +
             (1 | FamilyNumber/FISNumber),
  ## adding (1 | FISNumber) leads to failure of convergence
  data = filter(full_error_df, algorithm != "xgb_stack"))

summary(model_set)

anova(model_set)


emm_options(pbkrtest.limit = 20000, lmerTest.limit = 20000)
emm_alg <- emmeans(model_set, ~ algorithm)
pairs(emm_alg, adjust = "fdr")  # pairwise comparisons with p-value correction
## alternative: adjust = "bonferroni" or adjust = "hochberg"

## This indicates that in general, the random forest performs best across 
## all feature sets (Interaction not respected though)


emm_set <- emmeans(model_set, ~ feature_set)
pairs(emm_set, adjust = "fdr")  # pairwise comparisons with p-value correction
## This is likely the comparison I want to report, only still unclear whether 
## to also condition on algorithm or not



## Final linear model: Only comparing the random forest models
model_set_rf <- lmerTest::lmer(
  sq_error ~ feature_set + 
    (1 | FamilyNumber/FISNumber), 
  data = filter(full_error_df, algorithm == "rf"))

summary(model_set_rf)

anova(model_set_rf)

emm_rf_set <- emmeans(model_set_rf, ~ feature_set)
pairs(emm_rf_set, adjust = "fdr")  # pairwise comparisons with p-value correction
## This is likely the comparison I want to report, only still unclear whether 
## to also condition on algorithm or not
## alternative: adjust = "bonferroni" or adjust = "hochberg"

## Interpretation follows here once true results are in 
## CONTINUE HERE!!!



