# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-09-29
#
# Script Name: 35_B_run_sensitivity_analysis_ML.R
#
# Script Description: This script is supposed to re-run the ML training part for 
# variable set B after removing the top 5% multivariate outliers to 
# see if results are robust. No stability assessment will take place,
# thus, no bootstrapping, models are only trained once
#
#
# Notes: 35_B_run_sensitivity_analysis_ML.R re-runs the analysis
# for variable set B (PGS + covariates)
#
#



# !/usr/bin/env Rscript


# Set options
t00 <- Sys.time()

options(scipen = 999, expressions = 500000)
## note that 500000 is the absolute max allowed

iter <- 1
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


install_manually <- FALSE
if(install_manually == TRUE){
  pacman::p_load("fs", "usethis", "brio", "later", "jquerylib", "bslib", "mime",
                 "memoise", "shiny", "downlit", "rappdirs", "htmlwidgets",
                 "profvis", "httr2", "xml2", "roxygen2", "pkgdown", "ellipsis",
                 "pkgbuild", "remotes", "sessioninfo", "pak", "devtools", "bit",
                 "bit64", "vroom", "readr", "haven")
  pak::pak("tidyverse/readxl")
  pak::pak('topepo/caret/pkg/caret')
  devtools::install_github("cran/car")
}

pacman::p_load("dplyr", "haven", "foreign", "here", "readr",
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "fastDummies", "RANN",
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest")


## Sourcing custom functions
## (files need to be located in same directory, also on cluster)
source(here::here("scripts", "functions", "functions_ml.R"))


if (b_iter == 0) {
  train_ids <- readRDS(
    here::here("data", "intermediate", "indices_train_PGS.rds"))
} else {
  train_ids <- readRDS(
    here::here(
      "data", "intermediate", "indices_bootstrap_PGS.rds"))[[b_iter]][[1]]
}


if (b_iter == 0) {
  test_ids <- readRDS(
    here::here("data", "intermediate", "indices_test_PGS.rds"))
} else {
  test_ids <- readRDS(
    here::here(
      "data", "intermediate", "indices_bootstrap_PGS.rds"))[[b_iter]][[2]]
}


train_ids_remove <- readRDS(
  here::here("data", "intermediate", "outlierIDs_MCD_dataset_B.rds"))

## specifying number of cores to be used for parallelization
ncore_cl <- 48


## loading in covariate names
covariates_names <- readRDS(
  here::here("data", "intermediate", "names_covariates.rds"))
covariates_names

## loading in vector with names genetic covariates
gen_covariates <- readRDS(
  here::here("data", "intermediate", "names_genetic_covariates.rds"))

## one vector with all covariates names
covariates_names <- c(covariates_names, gen_covariates)


## removing flagged multivariate outliers, preprocessing data

## loading in dataset (unpreprocessed, variables already dummy coded)
## + filtering out the 5% multivariate MCD outliers
data_model_B_train <- readRDS(
  here::here("data", "intermediate", "data_model_B_train.rds")) %>%
  filter(!FISNumber %in% train_ids_remove)

## loading in full covariates names
covariates_full <- readRDS(
  here::here("data", "intermediate", "covariates_full_B.rds"))

## reading in (unpreprocessed) test set
data_model_B_test <- readRDS(
  here::here("data", "intermediate", "data_model_B_test.rds"))

data_model_B <- rbind(data_model_B_train,
                           data_model_B_test)

## sanity check: were covariates completely saved?
setdiff(covariates_names, covariates_full)
setdiff(covariates_full, covariates_names)

## preprocessing for machine learning
## (this is done in the function ml_preprocess)
## nzv removal, high cor removal, linear combination removal, imputation
preprocessed_B <- ml_preprocess(
  df = data_model_B,
  train_ids = setdiff(train_ids, train_ids_remove),
  test_ids = test_ids,
  covariates = covariates_full
)

## checking if family is still included in training variables
"FamilyNumber" %in% colnames(preprocessed_B$x_train_comb)

x_train <- preprocessed_B$x_train_comb

x_test <- preprocessed_B$x_test_comb

train_ids <- setdiff(train_ids, train_ids_remove)

cat("All covariates in training set: ",
    all(covariates_full %in% colnames(x_train)), "\n")

cat("All covariates in test set: ",
    all(covariates_full %in% colnames(x_test)), "\n")

## saving data
filename_dataset_processed <- paste0("prepared_data_B_MCD_", iter, ".rds")

saveRDS(
  list(
    x_train = x_train,
    x_test = x_test,
    train_ids = train_ids,
    test_ids = test_ids
  ),
  here::here(
    "data",
    "intermediate",
    "prep_data_MCD",
    filename_dataset_processed
  )
)
cat("data preparation finished for model B (MCD outliers removed)", "\n", "\n")


t0a <- Sys.time()
cat("duration data preparation: ",
    difftime(t0a, t00, unit = "mins"), " minutes")


##-----------------------------------------------------------------------------
##############################################################################

## Machine learning

## specifying number of cores to be used for parallelization
## depends on capacity
# ncore_cl <- 96
ncore_cl <- 48

set.seed(7)

## creating folds so that during training, families stay together
folds <- caret::groupKFold(group = x_train$FamilyNumber, k = 10)

# Define predictors by excluding ID and FamilyNumber and outcome
predictor_vars_enet <- setdiff(names(x_train), c("FISNumber", "FamilyNumber",
                                                 "QoL_simple"))

## Iterating (looping over conditions)
## number of cores that can be requested per node 
## on genoa needs to be divisible by 16

## defining bounds for hyperparameter tuning elastic net
bounds_enet <- list(alpha = c(0, 1), lambda = c(0.001, 1))

## running elastic net regression with bayesian Hyperparameter tuning
test_enet <- bayes_hyper_enet(df = x_train,
                              folds = folds,
                              bounds_enet = bounds_enet, 
                              ncores = ncore_cl,
                              iters.n = ncore_cl,
                              iters.k = ncore_cl)

cat("hypertuning elastic net successful!", "\n")

## best parameters for the elastic net regression
test_enet$best_params_enet

## features that were selected by the elastic net regression
test_enet$non_zero_predictors

## was the early stopping (time) triggered?
test_enet$early_stopping_triggered

## total time of the hypertuning
test_enet$time_hypertuning

## Adding covariates back into predictor set if they were 0 in elastic net
predictors_level_1 <- c(setdiff(covariates_full, test_enet$non_zero_predictors),
                        test_enet$non_zero_predictors)

## Removing intercept from predictor space (not needed for rf, svr and xgb)
predictors_level_1 <- setdiff(predictors_level_1, "(Intercept)")                        

## are all covariates in predictors_level_1?
cat("All covariates in predictor set?",
    sum(covariates_full %in% predictors_level_1) - length(covariates_full) == 0,
    "\n")

## Check if intercept is still in predictor space
cat("Intercept still in predictor space: ", "(Intercept)" %in% predictors_level_1, "\n")

## vector for relevant variables in the actual round of machine learning
vars_ML_1 <- c("FISNumber", "FamilyNumber", "QoL_simple", predictors_level_1)


## creating definitive training set for the level 1 models
x_train_ML <- x_train %>%
  select(all_of(vars_ML_1), -contains("Intercept"))

## creating definitive test set for the level 1 models
x_test_ML <- x_test %>%
  select(all_of(vars_ML_1), -contains("Intercept"))

## checking if same columns are contained in training and test set
cat("Column names the same between training and test set: ",
    length(setdiff(colnames(x_train_ML), colnames(x_test_ML))) == 0, "\n")

## check if column names add up
cat("column names adding up: ",
    ncol(x_train_ML) == length(predictors_level_1) + 3, "\n")

##---------------------------------------------------------------

## Model 1 - Random Forest 
## again, creating folds, ensuring families stay together in the 
## Cross validation of the model
set.seed(7)
folds <- groupKFold(group = x_train_ML$FamilyNumber, k = 10)

## setting bounds for the hyperparameter space for random forest
bounds_rf <- list(
  mtry = c(1L, ncol(x_train_ML) - 3L),
  max.depth = c(3L, 30L),
  min.node.size = c(1L, 50L),
  num.trees = c(100L, 1500L)
)

## Running the random forest with bayesian hyperparameter tuning
run_rf <- bayes_hyper_rf(
  df_train = x_train_ML,
  df_test = x_test_ML,
  folds = folds,
  bounds_rf = bounds_rf,
  ncores = ncore_cl,
  iters.n = ncore_cl,
  iters.k = ncore_cl
)

## printing some output from rf process
cat("Best parameters for random forest: ", "\n")
run_rf$best_params_rf
cat("Duration hypertuning random forest", "\n")
run_rf$time_hypertuning
cat("Early stopping triggered", "\n")
run_rf$early_stopping_triggered
cat("Iterations run", "\n")
run_rf$niters
cat("Stopping status", "\n")
run_rf$stopStatus
cat("total time elapsed", "\n")
run_rf$totalTime

cat("hypertuning random forest successful!", "\n")

##-------------------------------------------------------------

## Model 2 - Support Vector Regression (dropped after model A, did not 
## converge, saving budget)

svr <- FALSE
if(svr){
  # Define the search bounds for hyperparameters
  bounds_svr <- list(
    C = c(0.1, 10),            # Range for C
    sigma = c(0.01, 0.1),     # Range for sigma
    degree = c(2, 3),          # Range for degree
    scale = c(0.01, 0.1),     # Range for scale
    method = c(0, 1)           # Encodes categorical: 0 = Radial, 1 = Poly
  )
  
  ## Running support vector regression with bayesian hyperparameter tuning
  run_svr <- bayes_hyper_svr(
    df_train = x_train_ML,
    df_test = x_test_ML,
    folds = folds,
    bounds_svr = bounds_svr,
    ncores = ncore_cl,
    iters.n = ncore_cl,
    iters.k = ncore_cl
  )
  
  ## printing some output from svr process
  cat("Best parameters for svr: ", "\n")
  run_svr$best_params_svr
  cat("Duration hypertuning svr", "\n")
  run_svr$time_hypertuning
  cat("Early stopping triggered", "\n")
  run_svr$early_stopping_triggered
  cat("Iterations run", "\n")
  run_svr$niters
  cat("Stopping status", "\n")
  run_svr$stopStatus
  cat("total time elapsed", "\n")
  run_svr$totalTime
  
  cat("hypertuning support vector regression successful!", "\n")
}
##-------------------------------------------------------------

## Model 3 - XGBoost

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

## Running the xgboost with bayesian hyperparameter tuning
run_xgb <- bayes_hyper_xgb(
  df_train = x_train_ML,
  df_test = x_test_ML,
  folds = folds,
  bounds_xgb = bounds_xgb,
  ncores = ncore_cl,
  iters.n = ncore_cl,
  iters.k = ncore_cl
)

## printing some output from xgb process
cat("Best parameters for xgb: ", "\n")
run_xgb$best_params_xgb
cat("Duration hypertuning xgb", "\n")
run_xgb$time_hypertuning
cat("Early stopping triggered", "\n")  
run_xgb$early_stopping_triggered
cat("Iterations run", "\n")
run_xgb$niters
cat("Stopping status", "\n")
run_xgb$stopStatus
cat("Best parameters for random forest: ", "\n")
run_xgb$totalTime

cat("hypertuning XGBoost successful!", "\n")

## adding flag if prediction is original or bootstrapped
if(iter == 1){
  run_rf$preds_df_rf$original_prediction <- 1
  run_xgb$preds_df_xgb$original_prediction <- 1
} else {
  run_rf$preds_df_rf$original_prediction <- 0
  run_xgb$preds_df_xgb$original_prediction <- 0  
} 
## optional if svr was run
if(svr){
  if(iter == 1){
    run_svr$preds_df_svr$original_prediction <- 1
  } else {
    run_svr$preds_df_svr$original_prediction <- 0
  }
}

# ----------------------------------------------------------------------
# ----- Export ---------------------------------------------------------
# ----------------------------------------------------------------------

## gathering all objects that should be saved into list
## not all elements in workspace, only selected
workspace_objects <- mget(c("covariates_full", "iter", "ncore_cl",
                            "predictors_level_1", "train_ids", "test_ids",
                            "run_rf", "run_xgb"))


# Save the list to an RDS file
filename <- paste0("sensitivity_workspace_model_B_iteration_", iter, ".rds")
saveRDS(
  workspace_objects,
  file = paste0(here::here("data", "intermediate",
                           "workspaces_sensitivity_analysis_MCD", filename)))




## time tracking
t01 <- Sys.time()

cat("duration entire script variable set B, sensitivity analysis: ",
    difftime(t01, t00, unit = "mins"), " minutes")

# eoS

