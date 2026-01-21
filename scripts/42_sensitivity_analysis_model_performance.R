# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-10-13
#
# Script Name: 42_sensitivity_analysis_model_performance.R
#
# Script Description: Calculating for every measure of model performance if
# confidence intervals overlap between original analysis and sensitivity
# analysis (MCDc outliers removed before ML)
#
#
# Notes:
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
               "rempsyc", "kableExtra", "lmerTest")


## loading in performance df and error df of OG analysis
model_performance_df_OG <- readRDS(
  file = here::here("data", "final", "model_performance_df_OG.rds"))

error_df_OG <- readRDS(
  file = here::here("data", "final", "full_error_df_OG.rds"))

## loading in performance df of sensitivity analysis
model_performance_df_sen <- readRDS(
  file = here::here("data", "final", "model_performance_df_sensitivity.rds"))

error_df_sen <- readRDS(
  file = here::here("data", "final", "full_error_df_sensitivity.rds"))

table_sensitivity <- TRUE
if(table_sensitivity){
  
  ## Formatting to APH style table
  model_performance_APH_sen <- model_performance_df_sen
  
  model_descriptions <- c(
    "A (Only CBCL)",
    "B (Only PGS)",
    "C (CBCL + PGS)",
    "D (CBCL + Long)",
    "E (CBCL + Long + PGS)"
  )
  
  levels(model_performance_APH_sen$feature_set) <- model_descriptions
  
  algs <- c("Stacked LM", "Random Forest", "XGBoost")
  
  levels(model_performance_APH_sen$algorithm) <- algs
  
  model_performance_APH_sen <- model_performance_APH_sen %>%
    mutate(
      ## showing always 2 decimals
      RMSE = sprintf("%.2f", round(rmse_est, 2)),
      R2 = sprintf("%.2f", round(r2_est, 2)),
      MAE = sprintf("%.2f", round(mae_est, 2)), 
      RMSE_CI_lower = sprintf("%.2f", round(rmse_ci_lower, 2)),
      RMSE_CI_upper = sprintf("%.2f", round(rmse_ci_upper, 2)),
      R2_CI_lower = sprintf("%.2f", round(r2_ci_lower, 2)),
      R2_CI_upper = sprintf("%.2f", round(r2_ci_upper, 2)),
      MAE_CI_lower = sprintf("%.2f", round(mae_ci_lower, 2)),
      MAE_CI_upper = sprintf("%.2f", round(mae_ci_upper, 2))
    ) %>%
    mutate(RMSE = paste0(RMSE, " [",
                         RMSE_CI_lower, ", ",
                         RMSE_CI_upper, "]"),
           R2 = paste0(R2, " [",
                       R2_CI_lower, ", ",
                       R2_CI_upper, "]"),
           MAE = paste0(MAE, " [",
                        MAE_CI_lower, ", ",
                        MAE_CI_upper, "]")
    ) %>%
    select(feature_set, algorithm, RMSE, R2, MAE) %>%
    group_by(feature_set) %>%
    mutate(feature_set = ifelse(row_number() == 1,
                                as.character(feature_set), "")) %>%
    ungroup()
  
  ## APA-style flextable
  model_performance_flextable_sen <- 
    model_performance_APH_sen %>%
    flextable() %>%
    set_header_labels(
      feature_set = "Variable Set",
      algorithm = "Algorithm",
      RMSE = "RMSE",
      R2 = "R²",
      MAE = "MAE"
    ) %>%
    # theme_vanilla() %>%
    fontsize(size = 12, part = "all") %>%
    bold(part = "header") %>%
    autofit()
  
  colnames(model_performance_APH_sen) <- c(
    "Variable Set",
    "Algorithm",
    "RMSE",
    "R²",
    "MAE")
  
  # Export as LaTeX table
  caption_comparison <- 
    "Comparison of model performance across variable sets and algorithms"
  latex_model_performance_sen <- kable(model_performance_APH_sen,
                                   format = "latex",
                                   booktabs = TRUE, 
                                   caption = caption_comparison, 
                                   align = c("l", "l", "l", "l", "l")) %>%
    kable_styling(latex_options = c("hold_position"))
  
  latex_model_performance_sen

}

##############################################################################

## calculating deviation between measures

model_performance_df_OG <- model_performance_df_OG %>%
  mutate(rmse_ci_length = abs(rmse_ci_upper) - abs(rmse_ci_lower),
         r2_ci_length = abs(r2_ci_upper) - abs(r2_ci_lower),
         mae_ci_length = abs(mae_ci_upper) - abs(mae_ci_lower))

model_performance_df_sen <- model_performance_df_sen %>%
  mutate(rmse_ci_length = abs(rmse_ci_upper) - abs(rmse_ci_lower),
         r2_ci_length = abs(r2_ci_upper) - abs(r2_ci_lower),
         mae_ci_length = abs(mae_ci_upper) - abs(mae_ci_lower))

## creating dataframe with model, algorithm and whether CIs overlap
comp_df <- data_frame(feature_set = model_performance_df_OG$feature_set,
                      algorithm = model_performance_df_OG$algorithm)


comp_df$rmse_est_dif <- rep(NA, nrow(comp_df))
comp_df$r2_est_dif <- rep(NA, nrow(comp_df))
comp_df$mae_est_dif <- rep(NA, nrow(comp_df))
comp_df$rmse_ci_lower_dif <- rep(NA, nrow(comp_df))
comp_df$rmse_ci_upper_dif <- rep(NA, nrow(comp_df))
comp_df$r2_ci_lower_dif <- rep(NA, nrow(comp_df))
comp_df$r2_ci_upper_dif <- rep(NA, nrow(comp_df))
comp_df$mae_ci_lower_dif <- rep(NA, nrow(comp_df))
comp_df$mae_ci_upper_dif <- rep(NA, nrow(comp_df))
comp_df$r2_ci_length_dif <- rep(NA, nrow(comp_df))
comp_df$rmse_ci_length_dif <- rep(NA, nrow(comp_df))
comp_df$mae_ci_length_dif <- rep(NA, nrow(comp_df))

comp_df$rmse_ci_overlap <- rep(NA, nrow(comp_df))
comp_df$r2_ci_overlap <- rep(NA, nrow(comp_df))
comp_df$mae_ci_overlap <- rep(NA, nrow(comp_df))

## do confidence intervals overlap?
for(model in 1:nrow(comp_df)){
  comp_df[model, "rmse_ci_overlap"] <- 
    ifelse(model_performance_df_OG[model, "rmse_ci_lower"] < 
             model_performance_df_sen[model, "rmse_ci_upper"] & 
           model_performance_df_OG[model, "rmse_ci_upper"] > 
             model_performance_df_sen[model, "rmse_ci_lower"],
           "overlap", "no overlap")
  
  comp_df[model, "r2_ci_overlap"] <- 
    ifelse(model_performance_df_OG[model, "r2_ci_lower"] < 
             model_performance_df_sen[model, "r2_ci_upper"] & 
            model_performance_df_OG[model, "r2_ci_upper"] > 
             model_performance_df_sen[model, "r2_ci_lower"],
           "overlap", "no overlap")
  
  comp_df[model, "mae_ci_overlap"] <- 
    ifelse(model_performance_df_OG[model, "mae_ci_lower"] < 
             model_performance_df_sen[model, "mae_ci_upper"] & 
            model_performance_df_OG[model, "mae_ci_upper"] > 
             model_performance_df_sen[model, "mae_ci_lower"],
           "overlap", "no overlap")
  
  ## for every measure: how high is absolute difference in estimate
  for(col in c(3:14)){
    comp_df[model, col] <- model_performance_df_OG[model, col] - 
                              model_performance_df_sen[model, col]
  }
  
}


sum(comp_df$rmse_ci_overlap != "overlap")
sum(comp_df$r2_ci_overlap != "overlap")
sum(comp_df$mae_ci_overlap != "overlap")
## All CIs of performance measures overlap


## RMSE: negative value -> was higher / worse in sensitivity analysis
range(abs(comp_df$rmse_est_dif))
range(comp_df$rmse_est_dif)

## R2: negative value -> was higher / better in sensitivity analysis
range(abs(comp_df$r2_est_dif))
range(comp_df$r2_est_dif)

## R2: negative value -> was higher / worse in sensitivity analysis
range(comp_df$mae_est_dif)
range(abs(comp_df$mae_est_dif))

## differences in width of CI
range(abs(comp_df$rmse_ci_length_dif))
range(comp_df$rmse_ci_length_dif)
range(abs(comp_df$r2_ci_length_dif))
range(comp_df$r2_ci_length_dif)
range(abs(comp_df$mae_ci_length_dif))
range(comp_df$mae_ci_length_dif)

#############################################################################

## correlation of performance measures between original and
## sensitivity analysis

## rank correlation of point estimates
cor(model_performance_df_OG$rmse_est,
    model_performance_df_sen$rmse_est,
    method = "spearman")

cor(model_performance_df_OG$r2_est,
    model_performance_df_sen$r2_est,
    method = "spearman")

cor(model_performance_df_OG$mae_est,
    model_performance_df_sen$mae_est,
    method = "spearman")

## rank correlation of CI width
cor(model_performance_df_OG$rmse_ci_length,
    model_performance_df_sen$rmse_ci_length,
    method = "spearman")

cor(model_performance_df_OG$r2_ci_length,
    model_performance_df_sen$r2_ci_length,
    method = "spearman")

cor(model_performance_df_OG$mae_ci_length,
    model_performance_df_sen$mae_ci_length,
    method = "spearman")


## rounding values 
comp_df <- comp_df %>%
  mutate(across(where(is.numeric), round, 3))

## formatting results for report

## latex and flextable
model_descriptions <- c(
  "A (Only CBCL)",
  "B (Only PGS)",
  "C (CBCL + PGS)",
  "D (CBCL + Long)",
  "E (CBCL + Long + PGS)"
)

levels(comp_df$feature_set) <- model_descriptions

algs <- c("Stacked LM", "Random Forest", "XGBoost")

levels(comp_df$algorithm) <- algs


df_table <- comp_df[, 1:11]

## creating data frame with absolute difference between metrics in cell, 
## absolute difference of lower / upper end of CI in 
## square brackets
df_table <- df_table %>%
  mutate(rmse_summary = paste0(rmse_est_dif, " [", 
                               rmse_ci_lower_dif, ", ", 
                               rmse_ci_upper_dif, "]"),
         r2_summary = paste0(r2_est_dif, " [", 
                             r2_ci_lower_dif, ", ", 
                             r2_ci_upper_dif, "]"),
         mae_summary = paste0(mae_est_dif, " [", 
                              mae_ci_lower_dif, ", ", 
                              mae_ci_upper_dif, "]")) %>%
  select(feature_set, algorithm, 
         rmse_summary,
         r2_summary,
         mae_summary)

colnames(df_table) <- c("Variable Set",
                        "Algorithm",
                        "Difference RMSE",
                        "Difference R²",
                        "Difference MAE")

diff_performance_flextable <- 
  df_table %>%
  flextable() %>%
  # theme_vanilla() %>%
  fontsize(size = 12, part = "all") %>%
  bold(part = "header") %>%
  autofit()



# Export as LaTeX table
caption_absolute <-
  "Absolute differences in performance measures between original analysis and analysis with top 5% MCD outliers removed"
latex_model_performance_diff <- kable(df_table,
                                 format = "latex",
                                 booktabs = TRUE, 
                                 caption = caption_absolute,
                                 align = c("l", "l", "l", "l", "l")) %>%
  kable_styling(latex_options = c("hold_position"))

latex_model_performance


## eoS
