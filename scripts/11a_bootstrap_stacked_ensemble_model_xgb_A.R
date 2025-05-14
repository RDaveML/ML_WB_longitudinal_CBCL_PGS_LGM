# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-05-07
#
# Script Name: 11a_bootstrap_stacked_ensemble_model_xgb_A.R
#
# Script Description: This script codes the stacked ensemble model (xgb)
# for model A based on the predictions of the level 1 models (rf and xgb)
#
#
# Notes: Only the random forest model and the xgb model were included because 
# the svr model did not produce stable results
#
#

# --------------------------------------------------------------
# ---------- Get Iteration Number ------------------------------
# --------------------------------------------------------------



# !/usr/bin/env Rscript
iter <- commandArgs(trailingOnly=TRUE) ## use this as index for the datasets!
# iter <- 1
## this can be tested and returned on ntr1 server run exiting the script 
iter <- as.numeric(iter)

if(iter > 1){
  b_iter <- iter - 1
} else {
  b_iter <- 0
}
print(b_iter)
b_iter <- as.numeric(b_iter)
print(b_iter)
cat("Iteration / Index for Bootstrapped dataset: ", b_iter, "\n")

test <- FALSE
if(test){
  filename <- paste0("workspace_model_0_iteration_", iter, ".rds")
  saveRDS(iter, file = paste0(here::here("data", "intermediate", "bootstrap", filename)))
  stop("Iteration print works / does not work, testrun with only iteration saved successfully!")
}

# Set options
t00 <- Sys.time()

cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "haven", "foreign", "here", "readr",
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "fastDummies", "RANN",
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest", "boot")



## loading in ML custom ML + Hypertuning functions
source(here::here("scripts", "functions", "functions_ml.R"))

## creating list of workspaces
filepath <- here::here("data", "intermediate", "bootstrap")
list_files <- grep(".rds", list.files(filepath), value = TRUE)

run_id <- as.numeric(
  regmatches(list_files, gregexpr("[0-9]+", list_files)))[iter]
print(run_id)

filename <- paste0(filepath, "/", list_files[iter])
print(filename)


## train and test ids were saved in the output of the boostrapped run
train_ids <- readRDS(filename)[["train_ids"]]
test_ids <- readRDS(filename)[["test_ids"]]

## specifying number of cores to be used for parallelization
ncore_ntr <- 72
## Preparing data

## loading in data with family numbers (Object is called df_FIS_fam)
load(here::here("data", "intermediate", "FIS_fam_nr.RData"))


## Loading in prediction data from random forest and xbg
pred_rf <- readRDS(filename)$run_rf$preds_df_rf 

pred_xgb <- readRDS(filename)$run_xgb$preds_df_xgb 

## loading in full workspace original prediction
# workspace_orig <- readRDS(
#  here::here("data", "intermediate", "bootstrap",
#             "workspace_model_A_iteration_1.rds"))

# original_predictors <- workspace_orig$predictors_level_1


## checking if all values of the column "original_prediction" in pred_rf and 
## pred_xgb are equal to 1, stopping script otherwise
#if (!all(pred_rf$original_prediction == 1) |
#    !all(pred_xgb$original_prediction == 1)) {
#  stop("Error: Original prediction was not used in at least one of the models!
#       Load in original prediction")
#}

## de-selecting indicator columns
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

set.seed(7)
folds <- groupKFold(group = data_train$FamilyNumber, k = 10)

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

## run_locally <- TRUE
run_locally <- FALSE
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
  run_xgb_stack <- bayes_hyper_xgb(
    df_train = data_train,
    df_test = data_test,
    folds = folds,
    bounds_xgb = bounds_xgb,
    ncores = ncore_ntr,
    iters.n = ncore_ntr,
    iters.k = ncore_ntr
  )
  
  
}

cat("stacked xgb model calculated successfully!", "\n")

## saving workspace objects needed for analyzing bootstrapped output
workspace_objects <- mget(c("iter", "run_id", "train_ids", "test_ids",
                            "run_xgb_stack"))


# Save the list to an RDS file
## with run ID instead of iteration! 
filename_stack <- paste0("workspace_model_A_stack_xgb_run_", run_id, ".rds")
saveRDS(workspace_objects,
        file = paste0(
          here::here("data", "intermediate", "stack_xgb_A", filename_stack))
        )


## time tracking
t01 <- Sys.time()

cat("duration entire script (model A, stacked_xgb): ",
    difftime(t01, t00, unit = "mins"), " minutes")