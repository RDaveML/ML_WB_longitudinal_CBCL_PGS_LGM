# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-03-06
#
# Script Name: 10_bootstrap_stability_0.R
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
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "purrr")


## ---------------------------------------------------------------------------

## Steps to be coded:

## Loading in the predictions from the B = 100 bootstrap procedures

list.files(here::here("data", "intermediate", "bootstrap_stability"))

files_predictions <- list.files(here::here("data",
                                           "intermediate",
                                           "bootstrap_stability"))


## creating a list of datasets
list_predictions <- vector("list", length = length(files_predictions))

## loading in the datasets
for(file in length(files_predictions)){
  filename <- paste0(here::here("data", "intermediate"), "/", files_predictions[[file]])
  temp_env <- new.env()  # Create a temporary environment
  load(filename, envir = temp_env)  # Load data into the environment
  obj_name <- ls(temp_env)  # Get the object name
  list_predictions[[file]] <- temp_env[[obj_name]]  # Extract the object directly
  
  ## naming
  if(file == 1){
    names(list_predictions)[file] <- "original_prediction"
  } else {
    names(list_predictions)[file] <- paste0("prediction_b_", file)
  }
}

## combining the dfs by FISNumber
pred_df <- reduce(list_predictions, left_join, by = "FISNumber")

## vector with column names of the bootstrapped predictions
names_boot <- setdiff(grep("pred", colnames(pred_df), value = TRUE), "original_prediction")

## Re-calculating B (should be 100)
B <- length(names_boot)

## Step 1 stability check: Prediction instability plot
## x-axis: originally predicted value (from non-bootstrap model)
## y-axis: scatter of b predicted value for each individual




##-----------------------------------------------------------------------------

## Step 2 stability check: Calibration instability plot
## x-axis: estimated outcome of original and bootstrapped models
## y-axis: Observed outcome in original dataset
## Load in actual outcome
load(here::here("data", "intermediate", "workspace_sandbox_custom_functions.Rdata"))
outcome_df <- data_model_A %>%
  select(FISNumber, QoL_simple)
rm(data_model_A)



## Step 3 stability check: MAPE instability plot
## x-axis: estimated outcome from original prediction model
## y-axis: scatter of MAPE value for each individual
## Calculating individual MAPEs
pred_df <- pred_df %>%
  mutate(
    mape_ind = rowMeans(abs(across(all_of(names_boot)) - prediction_original))
    )

## Now, I basically have all values I need: Next: Check how to best plot
## with help of the Riley paper and if necessary CGPT

## CONTINUE HERE!!








