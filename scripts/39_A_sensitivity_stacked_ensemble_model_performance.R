# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-10-06
#
# Script Name: 39_A_sensitivity_stacked_ensemble_model_performance.R
#
# Script Description: Re-calculating stacked ensemble model 
# and model performance metrics for model A after removing 5% MCD outliers
# (sensitivity analysis)
#
#
# Notes: Only the random forest model and the xgb model were included because 
# the svr model did not produce stable results
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
               "fastshap", "shapviz")



## loading in ML custom ML + Hypertuning functions
source(here::here("scripts", "functions", "functions_ml.R"))

## Steps: 

##-----------------------------------------------------------------------------

## A) Inspect performances of original models (later: bootstrapped CIs
## for performance measures)

## loading in workspace from sensitivity analysis
filepath <- here::here(
  "data", "intermediate", "workspaces_sensitivity_analysis_MCD")

workspace <- readRDS(
  paste0(filepath, "/", "sensitivity_workspace_model_A_iteration_1.rds"))


## 1) Loading in original train test split
train_ids <- workspace$train_ids

test_ids <- workspace$test_ids

## 2) Preparing data

## loading in data with family numbers (Object is called df_FIS_fam)
# load(here::here("data", "intermediate", "FIS_fam_nr.RData"))
df_FIS_fam <- readRDS(here::here("data", "intermediate", "FIS_fam_nr.rds")) %>%
  filter(FISNumber %in% train_ids | FISNumber %in% test_ids)


## Loading in prediction data from random forest and xbg
pred_rf <- workspace$run_rf$preds_df_rf 

pred_xgb <- workspace$run_xgb$preds_df_xgb 

original_predictors <- workspace$predictors_level_1


## checking if all values of the column "original_prediction" in pred_rf and 
## pred_xgb are equal to 1, stopping script otherwise
if (!all(pred_rf$original_prediction == 1) |
    !all(pred_xgb$original_prediction == 1)) {
  stop("Error: Original prediction was not used in at least one of the models!
       Load in original prediction")
}

pred_rf <- pred_rf %>%
  select(-train, -original_prediction)

pred_xgb <- pred_xgb %>%
  select(-train, -original_prediction)

## true Y
data_outcome <- readRDS(here::here("data", "intermediate", "data_outcome.RDS")) %>%
  filter(FISNumber %in% train_ids | FISNumber %in% test_ids)

## creating full dataframe
data_full <- data_outcome %>% 
  full_join(pred_rf, by = "FISNumber") %>%
  full_join(pred_xgb, by = "FISNumber") %>%
  full_join(df_FIS_fam, by = "FISNumber") %>%
  mutate(QoL_simple =  as.numeric(QoL_simple)) ## recoding outcome to numeric


## Creating training and test set
data_train <- data_full %>% 
  filter(FISNumber %in% train_ids) 

data_test <- data_full %>% 
  filter(FISNumber %in% test_ids) 


## 3) Train meta-learner stacked ensemble model

## a) Linear regression model 

## creating folds so that families stay together during training
set.seed(7)
folds <- groupKFold(group = data_train$FamilyNumber, k = 10)

## coding a linear regression machine learning model with the train function
## from the caret package. QoL_simple is the outcome variable, the ID
## variable "FISNumber" should not be part of the features
## the tuning parameter "Intercept" should be contained in the model

model_lm_stack <- train(QoL_simple ~ . - FISNumber - FamilyNumber, 
                        data = data_train, 
                        method = "lm",
                        trControl = trainControl(method = "cv", 
                                                 index = folds,
                                                 savePredictions = TRUE,
                                                 allowParallel = TRUE))


model_rf <- workspace$run_rf$model_bayes_rf 

model_xgb <- workspace$run_xgb$model_bayes_xgb

list_models <- list("lm_stack" = model_lm_stack,
                    "rf" = model_rf,
                    "xgb" = model_xgb)

## comparison of model performance

## 1) Point estimates 

## postResample function calculates RMSE, R² and MAE at once
metrics_comp <- vector("list", length = 3)
metrics_df <- data.frame()

list_models <- list("lm_stack" = model_lm_stack,
                    "rf" = model_rf,
                    "xgb" = model_xgb)

for(mod in 1:length(list_models)) {
  if (mod == 1) {
    predictions <- predict(list_models[[mod]], data_test)
  } else if (mod == 2) {
    predictions <- workspace$run_rf$preds_df_rf %>%
      filter(FISNumber %in% test_ids) %>%
      select(predictions_rf) %>%
      pull()
  } else if (mod == 3) {
    predictions <- workspace$run_xgb$preds_df_xgb %>%
      filter(FISNumber %in% test_ids) %>%
      select(predictions_xgb) %>%
      pull()
  } else {
    stop("Error, model not listed")
  }
  
  metrics_comp[[mod]] <- postResample(predictions, data_test$QoL_simple)
  print(metrics_comp[[mod]])
  
  metrics_df <- rbind(metrics_df, metrics_comp[[mod]])
}
names(metrics_df) <- c("RMSE", "R²", "MAE")

metrics_df <- cbind(data.frame(model_name = names(list_models)), metrics_df)



## Bootstrapped CIs for all model performance measures 

metric_function <- function(data, indices) {
  d <- data[indices, ]
  metrics <- postResample(pred = d$pred, obs = d$obs)
  return(metrics)
}

# Set number of bootstrap samples
n_boot <- 1000

# Placeholder for results
boot_results <- list()

# Loop through each model
for (i in 1:length(list_models)) {
  # Prepare predictions and observed values
  if (i == 1) {
    preds <- predict(list_models[[i]], data_test)
  } else if (i == 2) {
    preds <- workspace$run_rf$preds_df_rf %>%
      filter(FISNumber %in% test_ids) %>%
      pull(predictions_rf)
  } else if (i == 3) {
    preds <- workspace$run_xgb$preds_df_xgb %>%
      filter(FISNumber %in% test_ids) %>%
      pull(predictions_xgb)
  } else {
    preds <- run_xgb_stack$pred_xgb_stack
  }
  
  df_model <- data.frame(obs = data_test$QoL_simple, pred = preds)
  
  # Run bootstrap
  set.seed(123)  # For reproducibility
  boot_out <- boot(data = df_model, statistic = metric_function, R = n_boot)
  
  ## saving the squared error for significance testing
  df_error <- df_model %>%
    mutate(feature_set = "A",
           algorithm = names(list_models)[i],
           error = (preds - obs),
           sq_error = (preds - obs)^2
    )
  
  ## re-appending FISNumber and FamilyNumber
  df_error$FISNumber <- data_test$FISNumber
  df_error$FamilyNumber <- data_test$FamilyNumber
  
  
  # Confidence intervals
  ci_rmse <- boot.ci(boot_out, type = "perc", index = 1)$percent[4:5]
  ci_r2   <- boot.ci(boot_out, type = "perc", index = 2)$percent[4:5]
  ci_mae  <- boot.ci(boot_out, type = "perc", index = 3)$percent[4:5]
  
  ## saving CIs and point estimates (from metrics df)
  boot_results[[i]] <- list(
    model_name = names(list_models)[i],
    rmse_est = metrics_df[i, "RMSE"],
    r2_est = metrics_df[i, "R²"],
    mae_est = metrics_df[i, "MAE"],
    rmse_ci = ci_rmse,
    r2_ci = ci_r2,
    mae_ci = ci_mae,
    error_df = df_error
  )
}

filename <- "boot_results_performace_A_sensitivity.rds"

saveRDS(boot_results, here::here("data",
                                 "intermediate",
                                 "bootstrap",
                                 filename))


## eoS
