# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-06-25
#
# Script Name: 19_inspection_bootstrap_stability_model_C.R
#
# Script Description: 
# This script does the bootstraping stability check of ML predictions (Riley et.
# 2023). To assess if model predictions are stable and if feature space needs 
# to be shrunken down further
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
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest", "tidyverse",
               "rlang")

## Sourcing custom plot functions
source(here::here("scripts", "functions", "functions_plotting_ML.R"))
## ---------------------------------------------------------------------------

## Steps to be coded:

## Loading in the predictions from the B = 100 bootstrap procedures

## creating list of workspaces
#filepath <- "A:/ML_WB_longitudinal_CBCL_PGS_LGM/data/intermediate/bootstrap"
filepath <- here::here("data", "intermediate", "bootstrap")
list_files <- grep("model_C", list.files(filepath), value = TRUE)




workspaces_bootstrap <- vector("list", length = length(list_files))
## from all workspaces, only load in the objects "run_rf$preds_df_rf", 
## and "run_xgb$preds_df_xgb"
## it should result in a list of dataframes for each model

## (Note: svr model was dropped after model A)

for(workspace in 1:length(workspaces_bootstrap)){
  filename <- paste0(filepath, "/", list_files[workspace])
  workspaces_bootstrap[[workspace]][[1]] <- readRDS(filename)[["run_rf"]][["preds_df_rf"]]
  workspaces_bootstrap[[workspace]][[2]] <- readRDS(filename)[["run_xgb"]][["preds_df_xgb"]]
}

## re-write code lines 56 to 61 with vectorized loop instead of inefficient for loop 


names(workspaces_bootstrap) <- paste0("workspace_bootstrap_",
                                      1:length(list_files))

## all elements in workspaces_bootstrap have the following names:
## covariates_full, iter, ncore_cl, predictors_level_1, train_ids, test_ids,
## run_rf, run_svr, run_xgb

## run_ objects in the workspaces contain the results of the specific machine
## learning models. The objects in the run_ lists have the following names:
## best_params, time_hypertuning, early_stopping_triggered, niters, stopStatus,
## totalTime, a model object starting with "model_bayes" (for each model),
## and a dataframe with predictions, starting with "preds" (for each model)
## run_rf contains all objects from running a random forest model, 
## run_svr from running a support vector regression model, 
## run_xgb from running an xgboost model

## since the models were run, multiple times, the intention is to 
## check how stable the predictions are across the bootstrapped samples
## the functions to plot the stability plots were already read in from the 
## file functions_plotting_ML.R

## The goal now is to plot for each model separately (rf, svr, xgb) the 
## prediction instability plot, calibration instability plot and MAPE 
## instability plot

## Step 1 stability check: Prediction instability plot
## x-axis: originally predicted value (from non-bootstrap model)
## y-axis: scatter of b predicted value for each individual

## the function pred_plot_inst takes a dataframe as input with the columns
## true y, original_prediction, the identifying column "FISNumber" and the
## columns for bootstrapped predictions. 
## this dataframe should be created as follows: From the workspace objects, 
## the predictions of the first workspace should become the column 
## "original_prediction", the predictions of the other workspaces should 
## be named "bootstrapped_prediction_1", "bootstrapped_prediction_2" and so 
## forth

## the true y column should be named "true_y" and will be extracted from the 
## outcome dataset. 

## loading in data with true y scores
## outcome_saved <- TRUE
outcome_saved <- FALSE
if(outcome_saved == FALSE){
  
  train_ids <- readRDS(
    here::here("data", "intermediate", "indices_train_PGS.rds")) 
  test_ids <- readRDS(
    here::here("data", "intermediate", "indices_test_PGS.rds")) 
  ids_full <- c(train_ids, test_ids)
  
  ## reading in full data, selecting output column and filter for ids that 
  ## have PGS
  full_prepared_data_C <- readRDS(
    here::here(
      "data", "intermediate", "prep_data_C", "full_prepared_data_C_1.rds"))
  data_full_raw <- rbind(full_prepared_data_C$x_train, full_prepared_data_C$x_test)
  data_true_y <- data_full_raw %>%
    select(FISNumber, QoL_simple) %>%
    filter(FISNumber %in% ids_full)
  
  ## saving outcome data to load them in simpler later
  saveRDS(data_true_y, here::here("data", "intermediate", "data_outcome_C.RDS"))
  rm(data_full_raw)
  rm(full_prepared_data_C)
}

## loading in data with true y scores
data_true_y <- readRDS(here::here("data", "intermediate", "data_outcome_C.RDS")) %>%
  rename(true_y = QoL_simple)

## Make one dataframe for all random forest predictions from the workspaces
## this can be achieved by joining together all $run_rf$preds_df_rf objects
## from the workspace objects by their FISNumber

list_bootstrap_rf <- lapply(1:length(workspaces_bootstrap), function(i){
  ## first element in list is rf dataframe
  pred_df <- workspaces_bootstrap[[i]][[1]] %>%
    ## renaming the indicator column because plotting function
    ## takes the column name original_prediction for the predicted values
    rename(orig_indicator = original_prediction) %>%
    ## prediction indicator not needed here
    select(FISNumber, predictions_rf)
  
  if(i == 1){
    pred_df <- pred_df %>%
      rename(original_prediction = predictions_rf)
  } else {
    colname_boot <- paste0("bootstrapped_prediction_", i-1)
    pred_df <- pred_df %>%
      rename(!!colname_boot := predictions_rf)
  }
  return(pred_df)
})

## joining all dataframes in the list by FISNumber

data_bootstrap_rf <- Reduce(function(x, y) merge(x, y, by = "FISNumber"),
                            list_bootstrap_rf)

## joining with true_y data
data_bootstrap_rf <- data_bootstrap_rf %>%
  full_join(data_true_y, by = "FISNumber")


## plotting prediction instability plot for rf model
plot_pred_inst_rf <- plot_pred_inst(data_bootstrap_rf)

plot_pred_inst_rf

## xgb
list_bootstrap_xgb <- lapply(1:length(workspaces_bootstrap), function(i){
  ## first element in list is xgb dataframe
  pred_df <- workspaces_bootstrap[[i]][[2]] %>%
    ## renaming the indicator column because plotting function
    ## takes the column name original_prediction for the predicted values
    rename(orig_indicator = original_prediction) %>%
    ## prediction indicator not needed here
    select(FISNumber, predictions_xgb)
  
  if(i == 1){
    pred_df <- pred_df %>%
      rename(original_prediction = predictions_xgb)
  } else {
    colname_boot <- paste0("bootstrapped_prediction_", i-1)
    pred_df <- pred_df %>%
      rename(!!colname_boot := predictions_xgb)
  }
  return(pred_df)
})

## joining all dataframes in the list by FISNumber

data_bootstrap_xgb <- Reduce(function(x, y) merge(x, y, by = "FISNumber"),
                             list_bootstrap_xgb)

## joining with true_y data
data_bootstrap_xgb <- data_bootstrap_xgb %>%
  full_join(data_true_y, by = "FISNumber")


## plotting prediction instability plot for xgb model
plot_pred_inst_xgb <- plot_pred_inst(data_bootstrap_xgb)

plot_pred_inst_xgb

##-----------------------------------------------------------------------------

## Step 2 stability check: Calibration instability plot
## x-axis: estimated outcome of original and bootstrapped models
## y-axis: Observed outcome in original dataset

## Step 3 stability check: MAPE instability plot
## x-axis: estimated outcome from original prediction model
## y-axis: scatter of MAPE value for each individual


## getting all plots for all three models in one function

# Model metadata
model_info <- list(
  rf = list(index = 1, pred_col = "predictions_rf"),
  xgb = list(index = 2, pred_col = "predictions_xgb")
)

# Container to store outputs
all_plots_models <- list()

# Loop over each model
for (model_name in names(model_info)) {
  
  model_index <- model_info[[model_name]]$index
  prediction_column <- model_info[[model_name]]$pred_col
  
  list_bootstrap_df <- lapply(seq_along(workspaces_bootstrap), function(i) {
    
    pred_df <- workspaces_bootstrap[[i]][[model_index]] %>%
      rename(orig_indicator = original_prediction) %>%
      select(FISNumber, !!sym(prediction_column))
    
    if (i == 1) {
      # Name like: original_prediction_rf
      new_name <- paste0("original_prediction")
      pred_df <- pred_df %>%
        rename(!!new_name := !!sym(prediction_column))
    } else {
      # Name like: bootstrapped_prediction_1_rf
      colname_boot <- paste0("bootstrapped_prediction_", i - 1, "_")
      pred_df <- pred_df %>%
        rename(!!colname_boot := !!sym(prediction_column))
    }
    
    return(pred_df)
  })
  
  # Merge all bootstrap replicates for this model
  data_bootstrap <- Reduce(function(x, y) merge(x, y, by = "FISNumber"), list_bootstrap_df)
  
  # Join with the true values
  data_bootstrap <- data_bootstrap %>%
    full_join(data_true_y, by = "FISNumber")
  
  # Generate model-specific plots
  plot_pred_inst_model <- plot_pred_inst(data_bootstrap)
  plot_cal_inst_model <- plot_cal_inst(data_bootstrap,
                                       round = FALSE #TRUE
  )
  plot_mape_inst_model <- plot_mape_inst(data_bootstrap)
  
  # Store result
  all_plots_models[[model_name]] <- list(
    model_name = model_name,
    data_bootstrap = data_bootstrap,
    plot_pred_inst_model = plot_pred_inst_model,
    plot_cal_inst_model = plot_cal_inst_model,
    plot_mape_inst_model = plot_mape_inst_model
  )
}

## saving plots

## prediction instability
ggsave(filename = "C_pred_inst_rf.png",
       plot = all_plots_models$rf$plot_pred_inst_model,
       device = "png",
       path = here::here("data", "intermediate", "bootstrap", "plots"),
       create.dir = TRUE)

ggsave(filename = "C_pred_inst_xgb.png",
       plot = all_plots_models$xgb$plot_pred_inst_model,
       device = "png",
       path = here::here("data", "intermediate", "bootstrap", "plots"),
       create.dir = TRUE)

## calibration instability
ggsave(filename = "C_cal_inst_smooth_rf.png",
       plot = all_plots_models$rf$plot_cal_inst_model,
       device = "png",
       path = here::here("data", "intermediate", "bootstrap", "plots"),
       create.dir = TRUE)

ggsave(filename = "C_cal_inst_smooth_xgb.png",
       plot = all_plots_models$xgb$plot_cal_inst_model,
       device = "png",
       path = here::here("data", "intermediate", "bootstrap", "plots"),
       create.dir = TRUE)

## MAPE instability
# Define the path and filename
plot_path <- here::here("data", "intermediate", "bootstrap", "plots")

# Save the plot
png(filename = file.path(plot_path, "C_mape_inst_rf.png"), width = 800, height = 600)
replayPlot(all_plots_models$rf$plot_mape_inst_model)
dev.off()

png(filename = file.path(plot_path, "C_mape_inst_xgb.png"), width = 800, height = 600)
replayPlot(all_plots_models$xgb$plot_mape_inst_model)
dev.off()

## saving image to re-use
save.image(
  here::here("data", "intermediate", "workspace_stability_model_C.RData")
)
## eoS



