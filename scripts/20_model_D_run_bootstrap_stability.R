# !/usr/bin/env Rscript
# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-07-14
#
# Script Name: 20_model_D_run_bootstrap_stability.R
#
# Script Description: This script is supposed to run the original 
# and the B = 100
# bootstrapped versions of the ML script to inspect model stability
# The ML part was run on the SNELLIUS cluster, parallelizing a job array for
# running the script 101 times simultaneously
# Goal is to save each output of model predictions and performance 
# in separate file in subdirectory and then to combine them in the script where
# the stability check takes place. Thus one run of this script (with iter = 1)
# Runs all models 1 time with the original train / test split
#
#
# Notes: 20_model_D_run_bootstrap_stability.R runs models on variable set D
# (CBCL items + longitudinal variables + covariates)
#
# Notes: For the full analysis, this script needs to be run twice! First 
# run prepares all B bootstrapped datasets for machine learning and 
# can be run locally, second part performs bootstrapped machine learning
# and should be run by executing the bash script
# "bash_model_D_bootstrap" which runs the script "bash_model_D_run" 
# B times
# results will then be saved in correct subdirectories to continue analysis
#
#


# Set options
t00 <- Sys.time()

options(scipen = 999, expressions = 500000)
## note that 500000 is the absolute max allowed

# install.packages("rprojroot")
# install.packages("here")

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


## loading in covariate names
covariates_names <- readRDS(
  here::here("data", "intermediate", "names_covariates.rds"))
# covariates_names

## loading in rater covariates names
rater_covariates <- readRDS(
  here::here("data", "intermediate", "names_rater_covariates.rds")
)



## one vector with all covariates names
covariates_names <- c(covariates_names, rater_covariates)


##############################################################################

## part 1 (not run on cluster!): preprocess the datasets for the original run
## and all 100 bootstrapped runs, save them as .rds files that
## can later be read in

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# set to data_prepared <- FALSE if data were not yet preprocessed
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
data_prepared <- TRUE


## only execute this when data are not yet preprocessed
if(data_prepared == FALSE){
  iterations <- c(1:101)
  
  ## loading in dataset D
  
  data_model_D_base <- readRDS(
    here::here("data", "intermediate", "data_full_model_D.rds"))
  
  
  ## initiating for loop to prepare the entire set
  
  for (iter in iterations) {
    ## this can be tested and returned on ntr1 server run exiting the script
    iter <- as.numeric(iter)
    
    if (iter > 1) {
      b_iter <- iter - 1
    } else {
      b_iter <- 0
    }
    b_iter <- as.numeric(b_iter)
    cat("Iteration / Index for Bootstrapped dataset: ", b_iter)
    
    ## same train / test split as in variable set A
    ## ids training set
    if (b_iter == 0) {
      train_ids <- readRDS(
        here::here("data", "intermediate", "indices_train.rds"))
    } else {
      train_ids <- readRDS(
        here::here(
          "data", "intermediate", "indices_bootstrap.rds"))[[b_iter]][[1]]
    }
    
    ## ids test set
    if (b_iter == 0) {
      test_ids <- readRDS(
        here::here("data", "intermediate", "indices_test.rds"))
    } else {
      test_ids <- readRDS(
        here::here(
          "data", "intermediate", "indices_bootstrap.rds"))[[b_iter]][[2]]
    }
    
    
    ## save unpreprocessed data for later confounder analysis
    if(iter == 1){
      saveRDS(data_model_D_base,
              file = here::here("data", "intermediate", "data_model_D.rds"))
    }
    
    
    
    ## further distinction: numeric and factor covariates
    num_covariates <- c(grep("time_lag", covariates_names, value = TRUE),
                        grep("age_qol", covariates_names, value = TRUE),
                        rater_covariates)
    
    factor_covariates <- setdiff(covariates_names, num_covariates)
    
    ## check values of the factor covariates
    if(iter == 1){
      for (f_col in factor_covariates) {
       cat("distribution of values for variable ", f_col, "\n")
        print(table(data_model_D_base[[f_col]]))
        cat("\n", "\n")
      }
    }
    
    
    ## converting covariates to factors
    ## (this is done in the function f_conv)
    data_model_D <- f_conv(df = data_model_D_base,
                           covariates = factor_covariates)
    
    
    ## converting columns with multiple
    ## class types to numeric
    data_model_D <- mult_to_numeric(df = data_model_D)
    
    
    ## dummy coding categorical covariates
    data_dummies_D <- dummy_cols(
      data_model_D,
      select_columns = factor_covariates,
      remove_first_dummy = TRUE,
      remove_selected_columns = TRUE,
      ignore_na = TRUE
    )
    
    ## saving unpreprocessed training set for later calculating MCD 
    if(iter == 1){
      data_model_D_train <- data_dummies_D %>%
        filter(FISNumber %in% train_ids)
      
      saveRDS(data_model_D_train,
              here::here("data", "intermediate", "data_model_D_train.rds"))
      
      data_model_D_test <- data_dummies_D %>%
        filter(FISNumber %in% test_ids)
      
      saveRDS(data_model_D_test,
              here::here("data", "intermediate", "data_model_D_test.rds"))
    }
    
    dummy_vars <- setdiff(colnames(data_dummies_D), colnames(data_model_D))
    
    covariates_full <- c(num_covariates, dummy_vars)
    
    ## saving vector of covariate names for this dataset
    if (iter == 1) {
      saveRDS(covariates_full,
              here::here("data", "intermediate", "covariates_full_D.rds"))
    }
    
    covariates_full <- readRDS(
      here::here("data", "intermediate", "covariates_full_D.rds"))
    
    
    ## preprocessing for machine learning
    ## (this is done in the function f_preprocess)
    ## nzv removal, high cor removal, linear combination removal, imputation
    preprocessed_D <- ml_preprocess(
      df = data_dummies_D,
      train_ids = train_ids,
      test_ids = test_ids,
      covariates = covariates_full
    )
    
    ## checking if family is still included in training variables
    "FamilyNumber" %in% colnames(preprocessed_D$x_train_comb)
    
    x_train <- preprocessed_D$x_train_comb
    
    x_test <- preprocessed_D$x_test_comb
    
    ## saving data
    filename_dataset_processed <- paste0("full_prepared_data_D_", iter, ".rds")
    
    ## creating directory for prepared datasets for model D (if not already
    ## created)
    if(!dir.exists(here::here("data",
                              "intermediate",
                              "prep_data_D"))){
      dir.create(here::here("data",
                            "intermediate",
                            "prep_data_D"))
    }
    
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
        "prep_data_D",
        filename_dataset_processed
      )
    )
    cat("data preparation finished for iteration ", iter, "\n", "\n")
  }
  
  t0a <- Sys.time()
  cat("duration data preparation model D all bootstrap: ",
      difftime(t0a, t00, unit = "mins"), " minutes")
  
  stop(paste0("data preparation finished for all runs"))
}

## if data were already prepared: server preparation steps
if(data_prepared) {
  
  # --------------------------------------------------------------
  # ---------- Get Iteration Number ------------------------------
  # --------------------------------------------------------------
  
  #iter <- 1 (for testing purposes)
  iter <- commandArgs(trailingOnly = TRUE)
  iter <- as.numeric(iter)
  if (iter > 1) {
    b_iter <- iter - 1
  } else {
    b_iter <- 0
  }
  b_iter <- as.numeric(b_iter)
  cat("Iteration / Index for Bootstrapped dataset: ", b_iter)
  
}  


##-----------------------------------------------------------------------------
##############################################################################

## Machine learning (run this on cluster / server)


## listing files with the full prepared data
filepath_prep_data <- here::here("data", "intermediate", "prep_data_D")
list_files_prep_data <- grep(".rds", list.files(filepath_prep_data),
                             value = TRUE)

## selecting correct file
filename <- paste0(filepath_prep_data, "/",
                   list_files_prep_data[grep(paste0("_", iter, ".rds"),
                                             list_files_prep_data)]
) 

## loading in data objects for the machine learning models
x_train <- readRDS(filename)[["x_train"]]

x_test <- readRDS(filename)[["x_test"]]

## loading in train and test ids for saving
if(b_iter == 0) {
  train_ids <- readRDS(
    here::here("data", "intermediate", "indices_train.rds"))
} else {
  train_ids <- readRDS(
    here::here(
      "data", "intermediate", "indices_bootstrap.rds"))[[b_iter]][[1]]
}

## ids test set
if(b_iter == 0) {
  test_ids <- readRDS(
    here::here("data", "intermediate", "indices_test.rds"))
} else {
  test_ids <- readRDS(
    here::here(
      "data", "intermediate", "indices_bootstrap.rds"))[[b_iter]][[2]]
}


covariates_full <- readRDS(
  here::here("data", "intermediate", "covariates_full_D.rds"))

## specifying number of cores to be used for parallelization
# (depends on availability on server)
## ncore_cl <- 48
ncore_cl <- 96

## setting seed for replicability
set.seed(iter)

## creating folds so that during training, families stay together
folds <- caret::groupKFold(group = x_train$FamilyNumber, k = 10)

# Define predictors by excluding ID and FamilyNumber and outcome
predictor_vars_enet <- setdiff(names(x_train), c("FISNumber", "FamilyNumber",
                                                 "QoL_simple"))



## Iterating (looping over conditions)
## number of cores that can be requested per node 


###############################################################################

## 0) Elastic Net Regression for variable selection

## defining bounds for hyperparameter tuning elastic net
## (variable selection step)
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
cat("best parameters elastic net: ", "\n")
test_enet$best_params_enet
cat("\n", "\n")

## features that were selected by the elastic net regression
cat("non-zero predictors enet: ", "\n")
test_enet$non_zero_predictors
cat("\n", "\n")

## was the early stopping (time) triggered?
cat("early stopping triggered in elastic net hypertuning: ", "\n")
test_enet$early_stopping_triggered
cat("\n", "\n")

## total time of the hypertuning
cat("total time Bayesian hypertuning elastic net with ",
    ncore_cl, " cores: ", "\n")
test_enet$time_hypertuning
cat("\n", "\n")

## Adding covariates back into predictor set if they were 0 in elastic net
predictors_level_1 <- c(setdiff(covariates_full, test_enet$non_zero_predictors),
                        test_enet$non_zero_predictors)

## Removing intercept from predictor space (not needed for rf, svr and xgb)
predictors_level_1 <- setdiff(predictors_level_1, "(Intercept)")                        

## are all covariates in predictors_level_1?
sum(covariates_full %in% predictors_level_1) - length(covariates_full) == 0

## Check if intercept is still in predictor space
cat("Intercept still in predictor space: ",
    "(Intercept)" %in% predictors_level_1, "\n")

## vector for relevant variables in the actual round of machine learning
vars_ML_1 <- c("FISNumber", "FamilyNumber", "QoL_simple", predictors_level_1)


## creating definitive training set for the level 1 models
x_train_ML <- x_train %>%
  select(all_of(vars_ML_1))

## creating definitive test set for the level 1 models
x_test_ML <- x_test %>%
  select(all_of(vars_ML_1))

## checking if same columns are contained in training and test set
cat("Column names the same between training and test set: ",
    length(setdiff(colnames(x_train_ML), colnames(x_test_ML))) == 0, "\n")

## check if column names add up
cat("column names adding up: ",
    ncol(x_train_ML) == length(predictors_level_1) + 3, "\n")

##-----------------------------------------------------------------------------

## Model 1 - Random Forest 
## again, creating folds, ensuring families stay together in the 
## Cross validation of the model
set.seed(iter)
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
cat("Early stopping triggered in random forest Bayesian hypertuning", "\n")
run_rf$early_stopping_triggered
cat("Iterations run", "\n")
run_rf$niters
cat("Stopping status", "\n")
run_rf$stopStatus
cat("total time elapsed", "\n")
run_rf$totalTime

cat("hypertuning random forest successful!", "\n")

##-------------------------------------------------------------

## Model 2 - Support Vector Regression (dropped after model A to save resources
# as models were not stable)

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
  nrounds = c(50L, 200L)
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
cat("Total time xgb: ", "\n")
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
filename <- paste0("workspace_model_D_iteration_", iter, ".rds")
saveRDS(workspace_objects, file = paste0(here::here("data", "intermediate",
                                                    "bootstrap", filename)))




## time tracking
t01 <- Sys.time()

cat("duration entire script model D iteration ", iter, ": ",
    difftime(t01, t00, unit = "mins"), " minutes")


#timer_total <- proc.time()[3]


# print total time of nodes
#print(paste0("Full Timing Iteration ", iter, ":"))
#proc.time()[3] - timer_total

# eoS



