# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-05-01
#
# Script Name: 11_stacked_ensemble_model_A.R
#
# Script Description: This script codes the stacked ensemble model 
# for model A based on the predictions from the first level models 
# and evaluates the stacked ensemble model and the first level models in terms 
# of performance and feature importance
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

## 1) Loading in original train test split
train_ids <- readRDS(here::here("data", "intermediate", "indices_train.rds"))

test_ids <- readRDS(here::here("data", "intermediate", "indices_test.rds"))

## 2) Preparing data

## loading in data with family numbers (Object is called df_FIS_fam)
# load(here::here("data", "intermediate", "FIS_fam_nr.RData"))
df_FIS_fam <- readRDS(here::here("data", "intermediate", "FIS_fam_nr.rds"))


## Loading in prediction data from random forest and xbg
pred_rf <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_A_iteration_1.rds"))$run_rf$preds_df_rf 

pred_xgb <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_A_iteration_1.rds"))$run_xgb$preds_df_xgb 

## loading in full workspace original prediction
workspace_orig <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_A_iteration_1.rds"))

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
data_outcome <- readRDS(here::here("data", "intermediate", "data_outcome.RDS"))

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
             "workspace_model_A_iteration_1.rds"))$run_rf$model_bayes_rf 

model_xgb <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_A_iteration_1.rds"))$run_xgb$model_bayes_xgb

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

## lm did not do well, training Cross-validated bayesian hypertuning boosted 
## xgb as stacked ensemble model

## setting bounds for the hyperparameter space for xgboost
bounds_xgb <- list(
  num_parallel_tree = c(1L, 100L),
  max_depth = c(3L, 6L),
  min_child_weight = c(5L, 10L),
  subsample = c(0.1, 1),
  colsample_bytree = c(0.5, 1),
  eta = c(0.01, 0.3),
  gamma = c(0, 5),
  lambda = c(0, 10),
  alpha = c(0, 10),
  nrounds = c(50L, 500L)
)

bounds_xgb <- list(
  num_parallel_tree = c(1L, 100L),
  max_depth = c(1L, 3L),          # don't go too deep with only 3 features
  min_child_weight = c(5L, 10L),
  subsample = c(0.1, 1),
  colsample_bytree = c(0.5, 1),
  eta = c(0.01, 0.3),
  gamma = c(0, 5),
  lambda = c(0, 10),
  alpha = c(0, 10),
  nrounds = c(50L, 500L)
)

## running if not yet run on server
stacked_run <- FALSE

if(stacked_run) {
  run_locally <- TRUE
  ## locally with 4 cores for checking code
  if (run_locally) {
    run_xgb_stack <- bayes_hyper_xgb(
      df_train = data_train,
      df_test = data_test,
      folds = folds,
      bounds_xgb = bounds_xgb,
      ncores = 4,
      iters.n = 4,
      iters.k = 4
    )
  } else {
    ## full run, on ntr-compute1 server
    ncore_ntr <- 64
    run_xgb_stack <- bayes_hyper_xgb(
      df_train = data_train,
      df_test = data_test,
      folds = folds,
      bounds_xgb = bounds_xgb,
      ncores = ncore_ntr,
      iters.n = ncore_ntr,
      iters.k = ncore_ntr
    )
    ## Insert saving server run objects here!
    
    
  }
  
} else {
  ## loading in run of stacked xgb model from workspace
  ## (was run on ntr-compute1 server)
  run_xgb_stack <- readRDS(here::here("data", "intermediate",
                                      "run_stacked_models_A.rds"))
}


pred_xgb_stack <- run_xgb_stack$pred_xgb_stack

 RMSE_df[(nrow(RMSE_df) + 1), "model_name"] <- "xgb_stacked"
 RMSE_df[nrow(RMSE_df), "RMSE"] <- RMSE(pred_xgb_stack, data_test$QoL_simple)

RMSE_df

## Conclusion: RMSE lowest in rf model (not stacked)?


### R² of all models
## postResample function calculates RMSE, R² and MAE at once
metrics_comp <- vector("list", length = 4)
metrics_df <- data.frame()

list_models <- list("lm_stack" = model_lm_stack,
                    "rf" = model_rf,
                    "xgb" = model_xgb,
                    "xgb_stack" = run_xgb_stack$run_xgb_stack)

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
    predictions <- run_xgb_stack$pred_xgb_stack
  }
  
  metrics_comp[[mod]] <- postResample(predictions, data_test$QoL_simple)
  print(metrics_comp[[mod]])
  
  metrics_df <- rbind(metrics_df, metrics_comp[[mod]])
}
names(metrics_df) <- c("RMSE", "R²", "MAE")

metrics_df <- cbind(data.frame(model_name = names(list_models)), metrics_df)


##-----------------------------------------------------------------------------




## B) bootstrapped confidence intervals of model performance

## iterate over all bootstrapped workspace items (might need to switch order
## and place this part more at the beginning of the script)

## creating list of workspaces
bootstrap_metrics <- TRUE
if(bootstrap_metrics) {
  filepath <- here::here("data", "intermediate", "bootstrap")
  list_files <- grep(".rds", list.files(filepath), value = TRUE)
  
  
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
  ## potentially later to create entire dataframe with the metrics of all models:
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
  ## potentially later to create entire dataframe with the metrics of all models:
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
      mutate(QoL_simple =  as.numeric(QoL_simple)) ## recoding outcome to numeric
    
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
  ## potentially later to create entire dataframe with the metrics of all models:
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

  
  
  ## For stacked Xgboost model
  boot_metrics_xgb_stack <- data.frame()
  filepath_xgb_stack <- here::here("data", "intermediate", "stack_xgb_A")
  list_files_xgb_stack <- grep(
    ".rds", list.files(filepath_xgb_stack), value = TRUE
    )
  
  for (run in 1:length(list_files_xgb_stack)) {
    ## Because workspaces are not numerically sorted, assigning run_id
    ## anew
    run_id <- as.numeric(
      regmatches(list_files_xgb_stack, gregexpr("[0-9]+", list_files_xgb_stack))
      )[run]
    print(run_id)
    
    filename <- paste0(filepath_xgb_stack, "/", list_files_xgb_stack[run])
    print(filename)
    
    ## getting train and test IDs
    train_ids_run <- readRDS(filename)[["train_ids"]]
    
    test_ids_run <- readRDS(filename)[["test_ids"]]
    
    data_test_run <- data_full %>%
      filter(FISNumber %in% test_ids_run)
    
    xgb_stack_pred_df <- readRDS(filename)[["run_xgb_stack"]][["preds_df_xgb"]]
    
    predictions <- xgb_stack_pred_df %>%
      filter(FISNumber %in% test_ids_run) %>%
      select(predictions_xgb) %>%
      pull()
    
    ## calculating metrics for this specific instance
    metrics_run <- postResample(predictions, data_test_run$QoL_simple)
    
    metrics_line <- c(run_id, metrics_run)
    boot_metrics_xgb_stack <- rbind(boot_metrics_xgb_stack, metrics_line)
  }
  
  colnames(boot_metrics_xgb_stack) <- c("run", "RMSE", "R²", "MAE")
  ## potentially later to create entire dataframe with the metrics of all models:
  ## colnames(boot_metrics_rf) <- c("run", "RMSE_xgb", "R²_xgb", "MAE_xgb")
  
  RMSE_xgb_stack_boot <- boot_metrics_xgb_stack %>%
    select(RMSE) %>%
    pull()
  
  R2_xgb_stack_boot <- boot_metrics_xgb_stack %>%
    select(`R²`) %>%
    pull()
  
  MAE_xgb_stack_boot <- boot_metrics_xgb_stack %>%
    select(MAE) %>%
    pull()
  
  ## Calculate Bootstrapped CIs of the metrics
  
  ## RMSE
  boot_obj_rmse_xgb_stack <- boot(
    data = RMSE_xgb_stack_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_rmse_xgb_stack, type = "perc")
  
  ## R²
  boot_obj_R2_xgb_stack <- boot(
    data = R2_xgb_stack_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_R2_xgb_stack, type = "perc")
  
  ## MAE
  boot_obj_MAE_xgb_stack <- boot(
    data = MAE_xgb_stack_boot,
    statistic = function(d, i)
      mean(d[i]),
    R = 1000
  )
  boot.ci(boot_obj_MAE_xgb_stack, type = "perc")
  

}

## First train the models on server (alternative: run 2 test runs on SNELLIUS,
## check how much budget it consumed)


##-----------------------------------------------------------------------------


## saving and loading in workspace
## CONTINUE HERE (next: turn part above into function, also including xgboost model, 
## so that data do not have to be loaded in several times)
save.image(here::here("data", "intermediate", "workspace_stacking_A_server.RData"))
load(here::here("data", "intermediate", "workspace_stacking_A_server.RData"))


##-----------------------------------------------------------------------------


## Comparison of models: Friedman's Test









## Comparison of models: Nememyi's Test







## Comparison of models: Wilcoxon signed rank tests





