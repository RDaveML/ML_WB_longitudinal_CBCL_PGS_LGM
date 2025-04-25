# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-03-06
#
# Script Name: 10_bootstrap_stability_A.R
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
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest", "tidyverse")

## Sourcing custom plot functions
source(here::here("scripts", "functions", "functions_plotting_ML.R"))
## ---------------------------------------------------------------------------

## Steps to be coded:

## Loading in the predictions from the B = 100 bootstrap procedures

## creating list of workspaces
filepath <- "A:/ML_WB_longitudinal_CBCL_PGS_LGM/data/intermediate/bootstrap"
list_files <- grep("old", list.files(
  "A:/ML_WB_longitudinal_CBCL_PGS_LGM/data/intermediate/bootstrap"),
   value = TRUE, invert = TRUE)




workspaces_bootstrap <- vector("list", length = length(list_files))
## from all workspaces, only load in the objects "run_rf$preds_df_rf", 
## "run_svr$preds_df_svr" and "run_xgb$preds_df_xgb"
## it should result in a list of dataframes for each model



for(workspace in 1:length(workspaces_bootstrap)){
  filename <- paste0(filepath, "/", list_files[workspace])
  workspaces_bootstrap[[workspace]][[1]] <- readRDS(filename)[["run_rf"]][["preds_df_rf"]]
  workspaces_bootstrap[[workspace]][[2]] <- readRDS(filename)[["run_svr"]][["preds_df_svr"]]
  workspaces_bootstrap[[workspace]][[3]] <- readRDS(filename)[["run_xgb"]][["preds_df_xgb"]]
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
outcome_saved <- TRUE
if(outcome_saved == FALSE){
load(here::here("data", "intermediate", "data_model_0.Rdata"))
data_true_y <- data_full_raw %>%
  select(FISNumber, QoL_simple)

## saving outcome data to load them in simpler later
saveRDS(data_true_y, here::here("data", "intermediate", "data_outcome.RDS"))
}

## loading in data with true y scores
data_true_y <- readRDS(here::here("data", "intermediate", "data_outcome.RDS")) %>%
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
## CONTINUE HERE!!!

##-----------------------------------------------------------------------------

## Step 2 stability check: Calibration instability plot
## x-axis: estimated outcome of original and bootstrapped models
## y-axis: Observed outcome in original dataset



## Step 3 stability check: MAPE instability plot
## x-axis: estimated outcome from original prediction model
## y-axis: scatter of MAPE value for each individual




## getting all plots for all three models in one function

## CONTINUE HERE!! ADD MODEL NAME INTO PLOT FUNCTION
all_plots_models <- vector("list", length = 3)
all_plots_models <- lapply(c(1:length(all_plots_models)), function(x){
  ## continue here, something is still off, all the same is printed, so only 
  ## for one model, see why assigning x does not work
  x <<- x
  ## adjusting column names according to model
  model_name <- case_when(
    x == 1 ~ "rf",
    x == 2 ~ "svr",
    x == 3 ~ "xgb"
  )
  col_model_name <- paste0("predictions_", model_name)
  list_bootstrap_df <- lapply(1:length(workspaces_bootstrap), function(i){
    ## first element in list is rf dataframe
    
    ## CONTINUE HERE!! SOMETHING IS OFF, the data are always the same!
    pred_df <- workspaces_bootstrap[[i]][[x]] %>%
      ## renaming the indicator column because plotting function
      ## takes the column name original_prediction for the predicted values
      rename(orig_indicator = original_prediction) %>%
      ## prediction indicator not needed here
      select(-orig_indicator)

    
    if(i == 1){
      pred_df <- pred_df %>%
        rename(original_prediction = !!col_model_name)
    } else {
      colname_boot <- paste0("bootstrapped_prediction_", i-1)
      pred_df <- pred_df %>%
        rename(!!colname_boot := !!col_model_name)
    }
    return(pred_df)
  })
  
  ## joining all dataframes in the list by FISNumber
  
  data_bootstrap <- Reduce(function(x, y) merge(x, y, by = "FISNumber"),
                              list_bootstrap_rf)
  
  ## joining with true_y data
  data_bootstrap <- data_bootstrap %>%
    full_join(data_true_y, by = "FISNumber")
  
  
  ## plotting prediction instability plot for rf model
  plot_pred_inst_model <- plot_pred_inst(data_bootstrap)
  
  plot_cal_inst_model <- plot_cal_inst(data_bootstrap)
  
  plot_mape_inst_model <- plot_mape_inst(data_bootstrap)
  
  return(list(model_name = model_name,
              data_bootstrap = data_bootstrap,
              plot_pred_inst_model = plot_pred_inst_model,
              plot_cal_inst_model = plot_cal_inst_model,
              plot_mape_inst_model = plot_mape_inst_model))
  
})

names(all_plots_models) <- c("rf", "svr", "xgb")



