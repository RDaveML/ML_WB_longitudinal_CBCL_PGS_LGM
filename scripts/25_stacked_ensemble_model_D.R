# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-07-15
#
# Script Name: 25_stacked_ensemble_model_D.R
#
# Script Description: This script codes the stacked ensemble model 
# for model D based on the predictions from the first level models 
# and evaluates the stacked ensemble model and the first level models in terms 
# of performance 
#
#
# Notes: Only the random forest model and the xgb model were included because 
# (svr was dropped after model A because it did not produce stable results)
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

## 1) Loading in original train test split
train_ids <- readRDS(
  here::here("data", "intermediate", "indices_train.rds"))

test_ids <- readRDS(here::here("data", "intermediate", "indices_test.rds"))

## 2) Preparing data

## loading in data with family numbers (Object is called df_FIS_fam)
## and filtering to have only participants with PGS data
df_FIS_fam <- readRDS(here::here("data", "intermediate", "FIS_fam_nr.rds")) %>%
  filter(FISNumber %in% c(train_ids, test_ids))


## Loading in prediction data from random forest and xbg
pred_rf <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_D_iteration_1.rds"))$run_rf$preds_df_rf 

pred_xgb <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_D_iteration_1.rds"))$run_xgb$preds_df_xgb 

## loading in full workspace original prediction
workspace_orig <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_D_iteration_1.rds"))

original_predictors <- workspace_orig$predictors_level_1


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
data_outcome <- readRDS(
  here::here("data", "intermediate", "data_outcome_D.RDS"))

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

## Bootstrap with the 100 iterations (iterate over all bootstrapped indices)

## comparison of model performance

## loading in model objects from the original predictions
## Loading in prediction data from random forest and xbg
model_rf <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_D_iteration_1.rds"))$run_rf$model_bayes_rf 

model_xgb <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_D_iteration_1.rds"))$run_xgb$model_bayes_xgb

list_models <- list("lm_stack" = model_lm_stack,
                    "rf" = model_rf,
                    "xgb" = model_xgb)

## RMSE stacked model
RMSE_comp <- vector("list", length = 3)

for(mod in 1:length(list_models)) {
  if (mod == 1) {
    predictions <- predict(list_models[[mod]], data_test)
  } else if (mod == 2) {
    predictions <- workspace_orig$run_rf$preds_df_rf %>%
      filter(FISNumber %in% test_ids) %>%
      select(predictions_rf) %>%
      pull()
  } else if (mod == 3) {
    predictions <- workspace_orig$run_xgb$preds_df_xgb %>%
      filter(FISNumber %in% test_ids) %>%
      select(predictions_xgb) %>%
      pull()
  } else {
    predictions <- NA
  }
  
  RMSE_comp[[mod]] <- RMSE(predictions, data_test$QoL_simple)
  print(RMSE_comp[[mod]])
}

RMSE_df <- data.frame(model_name = names(list_models),
                      RMSE = unlist(RMSE_comp))




### R² of all models
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
    predictions <- workspace_orig$run_rf$preds_df_rf %>%
      filter(FISNumber %in% test_ids) %>%
      select(predictions_rf) %>%
      pull()
  } else if (mod == 3) {
    predictions <- workspace_orig$run_xgb$preds_df_xgb %>%
      filter(FISNumber %in% test_ids) %>%
      select(predictions_xgb) %>%
      pull()
  }
  
  metrics_comp[[mod]] <- postResample(predictions, data_test$QoL_simple)
  print(metrics_comp[[mod]])
  
  metrics_df <- rbind(metrics_df, metrics_comp[[mod]])
}
names(metrics_df) <- c("RMSE", "R²", "MAE")

metrics_df <- cbind(data.frame(model_name = names(list_models)), metrics_df)


##-----------------------------------------------------------------------------

## Calculate Bootstrapped CIs of the metrics

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
    preds <- workspace_orig$run_rf$preds_df_rf %>%
      filter(FISNumber %in% test_ids) %>%
      pull(predictions_rf)
  } else if (i == 3) {
    preds <- workspace_orig$run_xgb$preds_df_xgb %>%
      filter(FISNumber %in% test_ids) %>%
      pull(predictions_xgb)
  }
  
  df_model <- data.frame(obs = data_test$QoL_simple, pred = preds)
  
  # Run bootstrap
  set.seed(123)  # For reproducibility
  boot_out <- boot(data = df_model, statistic = metric_function, R = n_boot)
  
  # Confidence intervals
  ci_rmse <- boot.ci(boot_out, type = "perc", index = 1)$percent[4:5]
  ci_r2   <- boot.ci(boot_out, type = "perc", index = 2)$percent[4:5]
  ci_mae  <- boot.ci(boot_out, type = "perc", index = 3)$percent[4:5]
  
  boot_results[[i]] <- list(
    model_name = names(list_models)[i],
    rmse_ci = ci_rmse,
    r2_ci = ci_r2,
    mae_ci = ci_mae
  )
}

saveRDS(boot_results, 
        here::here(
          "data", "intermediate", "bootstrap", "boot_results_performace_D.rds"))

##-----------------------------------------------------------------------------


## B) stability of model performance

## iterate over all bootstrapped workspace items (might need to switch order
## and place this part more at the beginning of the script)

## creating list of workspaces
bootstrap_stability <- TRUE
if(bootstrap_stability) {
  filepath <- here::here("data", "intermediate", "bootstrap")
  list_files <- grep("model_D", list.files(filepath), value = TRUE)
  
  
  ## rf model level 1
  #boot_metrics_rf <- vector("list", length = length(list_files))
  boot_metrics_rf <- data.frame()
  
  for (run in 1:length(list_files)) {
    
    ## Because workspaces are not numerically sorted, assigning run_id 
    ## anew
    run_id <- as.numeric(
      regmatches(list_files, gregexpr("[0-9]+", list_files)))[run]
    print(run_id)
    
    filename <- paste0(filepath, "/", list_files[run])
    print(filename)
    
    
    ## getting train and test IDs
    train_ids_run <- readRDS(filename)[["train_ids"]]
    
    test_ids_run <- readRDS(filename)[["test_ids"]]
    
    data_test_run <- data_full %>%
      filter(FISNumber %in% test_ids_run)
    
    rf_pred_df <- readRDS(filename)[["run_rf"]][["preds_df_rf"]]
    
    predictions <- rf_pred_df %>%
      filter(FISNumber %in% test_ids_run) %>%
      select(predictions_rf) %>%
      pull()
    
    ## calculating metrics for this specific instance
    metrics_run <- postResample(predictions, data_test_run$QoL_simple)
    
    metrics_line <- c(run_id, metrics_run)
    boot_metrics_rf <- rbind(boot_metrics_rf, metrics_line)
  }
  
  colnames(boot_metrics_rf) <- c("run", "RMSE", "R²", "MAE")
  ## potentially later to create entire dataframe with the metrics of all
  ## models:
  ## colnames(boot_metrics_rf) <- c("run", "RMSE_rf", "R²_rf", "MAE_rf")
  
  RMSE_rf_boot <- boot_metrics_rf %>%
    select(RMSE) %>%
    pull()
  
  R2_rf_boot <- boot_metrics_rf %>%
    select(`R²`) %>%
    pull()
  
  MAE_rf_boot <- boot_metrics_rf %>%
    select(MAE) %>%
    pull()
  
  ## Calculate Bootstrapped CIs of the metrics
  
  ## RMSE
  boot_obj_rmse_rf <- boot(
    data = RMSE_rf_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_rmse_rf, type = "perc")
  
  ## R²
  boot_obj_R2_rf <- boot(
    data = R2_rf_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_R2_rf, type = "perc")
  
  ## MAE
  boot_obj_MAE_rf <- boot(
    data = MAE_rf_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_MAE_rf, type = "perc")
  
  
  
  ## xgb model level 1
  #boot_metrics_xgb <- vector("list", length = length(list_files))
  boot_metrics_xgb <- data.frame()
  
  for (run in 1:length(list_files)) {
    
    ## Because workspaces are not numerically sorted, assigning run_id 
    ## anew
    run_id <- as.numeric(
      regmatches(list_files, gregexpr("[0-9]+", list_files)))[run]
    print(run_id)
    
    filename <- paste0(filepath, "/", list_files[run])
    print(filename)
    
    ## getting train and test IDs
    train_ids_run <- readRDS(filename)[["train_ids"]]
    
    test_ids_run <- readRDS(filename)[["test_ids"]]
    
    data_test_run <- data_full %>%
      filter(FISNumber %in% test_ids_run)
    
    xgb_pred_df <- readRDS(filename)[["run_xgb"]][["preds_df_xgb"]]
    
    predictions <- xgb_pred_df %>%
      filter(FISNumber %in% test_ids_run) %>%
      select(predictions_xgb) %>%
      pull()
    
    ## calculating metrics for this specific instance
    metrics_run <- postResample(predictions, data_test_run$QoL_simple)
    
    metrics_line <- c(run_id, metrics_run)
    boot_metrics_xgb <- rbind(boot_metrics_xgb, metrics_line)
  }
  
  colnames(boot_metrics_xgb) <- c("run", "RMSE", "R²", "MAE")
  ## potentially later to create entire dataframe with the metrics of all
  ## models:
  ## colnames(boot_metrics_rf) <- c("run", "RMSE_xgb", "R²_xgb", "MAE_xgb")
  
  RMSE_xgb_boot <- boot_metrics_xgb %>%
    select(RMSE) %>%
    pull()
  
  R2_xgb_boot <- boot_metrics_xgb %>%
    select(`R²`) %>%
    pull()
  
  MAE_xgb_boot <- boot_metrics_xgb %>%
    select(MAE) %>%
    pull()
  
  ## Calculate Bootstrapped CIs of the metrics
  
  ## RMSE
  boot_obj_rmse_xgb <- boot(
    data = RMSE_xgb_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_rmse_xgb, type = "perc")
  
  ## R²
  boot_obj_R2_xgb <- boot(
    data = R2_xgb_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_R2_xgb, type = "perc")
  
  ## MAE
  boot_obj_MAE_xgb <- boot(
    data = MAE_xgb_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_MAE_xgb, type = "perc")
  
  
  ## For bootstrapped lm model (move training part here!)
  
  boot_metrics_stacked_lm <- data.frame()
  
  for (run in 1:length(list_files)) {
    
    ## Because workspaces are not numerically sorted, assigning run_id 
    ## anew
    run_id <- as.numeric(
      regmatches(list_files, gregexpr("[0-9]+", list_files)))[run]
    print(run_id)
    
    filename <- paste0(filepath, "/", list_files[run])
    print(filename)
    
    ## getting train and test IDs
    train_ids_run <- readRDS(filename)[["train_ids"]]
    
    test_ids_run <- readRDS(filename)[["test_ids"]]
    
    pred_rf <- readRDS(filename)$run_rf$preds_df_rf
    
    pred_xgb <- readRDS(filename)$run_xgb$preds_df_xgb
    
    data_full <- data_outcome %>%
      full_join(pred_rf, by = "FISNumber") %>%
      full_join(pred_xgb, by = "FISNumber") %>%
      full_join(df_FIS_fam, by = "FISNumber") %>%
      mutate(QoL_simple =  as.numeric(QoL_simple))
    ## recoding outcome to numeric
    
    data_train_run <- data_full %>%
      filter(FISNumber %in% train_ids_run)
    
    data_test_run <- data_full %>%
      filter(FISNumber %in% test_ids_run)
    
    set.seed(7)
    folds <- groupKFold(group = data_train_run$FamilyNumber, k = 10)
    
    ## calculating stacked ensemble model (lm)
    model_lm_stack_run <- train(
      QoL_simple ~ . - FISNumber - FamilyNumber,
      data = data_train_run,
      method = "lm",
      trControl = trainControl(
        method = "cv",
        index = folds,
        savePredictions = TRUE,
        allowParallel = TRUE
      )
    )
    
    
    predictions <- predict(model_lm_stack_run, data_test_run)
    
    ## calculating metrics for this specific instance
    metrics_run <- postResample(predictions, data_test_run$QoL_simple)
    
    metrics_line <- c(run_id, metrics_run)
    boot_metrics_stacked_lm <- rbind(boot_metrics_stacked_lm, metrics_line)
  }
  
  colnames(boot_metrics_stacked_lm) <- c("run", "RMSE", "R²", "MAE")
  ## potentially later to create entire dataframe with the metrics of all
  ## models:
  ## colnames(boot_metrics_rf) <- c(
  ## "run", "RMSE_stack_lm", "R²_stack_lm", "MAE_stack_lm")
  
  
  RMSE_stacked_lm_boot <- boot_metrics_stacked_lm %>%
    select(RMSE) %>%
    pull()
  
  R2_stacked_lm_boot <- boot_metrics_stacked_lm %>%
    select(`R²`) %>%
    pull()
  
  MAE_stacked_lm_boot <- boot_metrics_stacked_lm %>%
    select(MAE) %>%
    pull()
  
  ## Calculate Bootstrapped CIs of the metrics
  
  ## RMSE
  boot_obj_rmse_stacked_lm <- boot(
    data = RMSE_stacked_lm_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_rmse_stacked_lm, type = "perc")
  
  ## R²
  boot_obj_R2_stacked_lm <- boot(
    data = R2_stacked_lm_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_R2_stacked_lm, type = "perc")
  
  ## MAE
  boot_obj_MAE_stacked_lm <- boot(
    data = MAE_stacked_lm_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_MAE_stacked_lm, type = "perc")
  
  
  
  
  
}

## saving and loading in workspace
## so that data do not have to be loaded in several times)

save.image(
  here::here("data", "intermediate", "workspace_stacking_D_server.RData"))


load(here::here("data", "intermediate", "workspace_stacking_D_server.RData"))



##-----------------------------------------------------------------------------



##-----------------------------------------------------------------------------


## Comparison of models: Friedman's Test









## Comparison of models: Nememyi's Test







## Comparison of models: Wilcoxon signed rank tests





