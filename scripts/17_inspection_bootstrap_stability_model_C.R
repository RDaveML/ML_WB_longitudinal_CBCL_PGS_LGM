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
# This script does the bootstrapping stability check of ML predictions 
## (Riley et. 2023) for the models estimated on variable set C
## (CBCL items + PGS + covariates). 
## Assessing if model predictions are stable and if feature
## space needs to be shrunken down further. Prediction-, calibration and MAPE
# instability plots are created and saved
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


## Loading in the predictions from the B = 100 bootstrap procedures

## creating list of workspaces
filepath <- here::here("data", "intermediate", "bootstrap")
list_files <- grep("model_C", list.files(filepath), value = TRUE)




workspaces_bootstrap <- vector("list", length = length(list_files))
## (Note: svr model was dropped after model A)

## loading in model objects
system.time({
  for(workspace in 1:length(workspaces_bootstrap)){
    filename <- paste0(filepath, "/", list_files[workspace])
    workspaces_bootstrap[[workspace]][[1]] <- 
      readRDS(filename)[["run_rf"]][["preds_df_rf"]]
    workspaces_bootstrap[[workspace]][[2]] <- 
      readRDS(filename)[["run_xgb"]][["preds_df_xgb"]]
  }
})


names(workspaces_bootstrap) <- paste0("workspace_bootstrap_",
                                      1:length(list_files))


## since the models were run, multiple times, the intention is to 
## check how stable the predictions are across the bootstrapped samples
## the functions to plot the stability plots were already read in from the 
## file functions_plotting_ML.R

## plot for each model separately (rf, xgb) the 
## prediction instability plot, calibration instability plot and MAPE 
## instability plot


## 
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
  data_full_raw <- rbind(full_prepared_data_C$x_train,
                         full_prepared_data_C$x_test)
  data_true_y <- data_full_raw %>%
    select(FISNumber, QoL_simple) %>%
    filter(FISNumber %in% ids_full)
  
  ## saving outcome data to load them in simpler later
  saveRDS(data_true_y, here::here("data", "intermediate", "data_outcome_C.RDS"))
  rm(data_full_raw)
  rm(full_prepared_data_C)
}

## loading in data with true y scores
data_true_y <- readRDS(
  here::here("data", "intermediate", "data_outcome_C.RDS")) %>%
  rename(true_y = QoL_simple)


##-----------------------------------------------------------------------------

## stability check: prediction instability plot
## x-axis: predicted outcome in bootstrapped models
## y-axis: predicted outcome in original dataset

## stability check: Calibration instability plot
## x-axis: estimated outcome of original and bootstrapped models
## y-axis: Observed outcome in original dataset

## stability check: MAPE instability plot
## x-axis: estimated outcome from original prediction model
## y-axis: scatter of MAPE value for each individual


## getting all plots for all models in one go

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
      new_name <- paste0("original_prediction")
      pred_df <- pred_df %>%
        rename(!!new_name := !!sym(prediction_column))
    } else {
      colname_boot <- paste0("bootstrapped_prediction_", i - 1, "_")
      pred_df <- pred_df %>%
        rename(!!colname_boot := !!sym(prediction_column))
    }
    
    return(pred_df)
  })
  
  # Merge all bootstrap replicates for this model
  data_bootstrap <- Reduce(function(x, y) merge(x, y, by = "FISNumber"),
                           list_bootstrap_df)
  
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

## storing all plots in object
saveRDS(all_plots_models,
        file = here::here(
          "data", "final", "stability", "stability_plots_model_C.rds"))

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
png(filename = file.path(plot_path, "C_mape_inst_rf.png"),
    width = 800, height = 600)
replayPlot(all_plots_models$rf$plot_mape_inst_model)
dev.off()

png(filename = file.path(plot_path, "C_mape_inst_xgb.png"),
    width = 800, height = 600)
replayPlot(all_plots_models$xgb$plot_mape_inst_model)
dev.off()

## saving image to re-use
save.image(
  here::here("data", "intermediate", "workspace_stability_model_C.RData")
)
## eoS


