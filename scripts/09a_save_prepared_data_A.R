# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-03-12
#
# Script Name: 09a_save_prepared_data.R
#
# Script Description: This script is supposed to re-run the data preparation
# for the model A data so that also the complete prepared (imputed, etc.) 
# training and test data are saved (in later models, those data will be saved
# as an additional step in the running script)
#
#
#
#

# --------------------------------------------------------------
# ---------- Get Iteration Number ------------------------------
# --------------------------------------------------------------




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


for(b_iter in 0:100) {
  cat("data preparation iteration ", b_iter, "\n")
  ## readRDS solves the problem!
  if (b_iter == 0) {
    train_ids <- readRDS(
      here::here("data", "intermediate", "indices_train.rds"))
  } else {
    train_ids <- readRDS(here::here(
      "data", "intermediate", "indices_bootstrap.rds"))[[b_iter]][[1]]
  }
  
  
  if (b_iter == 0) {
    test_ids <- readRDS(here::here("data", "intermediate", "indices_test.rds"))
  } else {
    test_ids <- readRDS(here::here("data", "intermediate", "indices_bootstrap.rds"))[[b_iter]][[2]]
  }
  
  ## specifying number of cores to be used for parallelization
  ncore_cl <- 96
  
  
  ## loading in the data
  ## loading in full model_A data (merged together in script 06_b_merge_nonLGM.R)
  ## Still to be renamed to model A
  load(here::here("data", "intermediate", "data_model_0.Rdata"))
  temp <- load(here::here("data", "intermediate", "data_model_0.Rdata"))
  cat("full model_A data loaded in; name of object: ",
      "'",
      temp,
      "'",
      "\n",
      "\n")
  
  
  ## loading in covariate names
  load(here::here("data", "intermediate", "names_covariates.RData"))
  temp <- load(here::here("data", "intermediate", "names_covariates.RData"))
  cat("vector with names of covariates loaded in; name of object: ",
      "'",
      temp,
      "'",
      "\n",
      "\n")
  
  ## loading in rater covariates
  ## (created in script 06_longitudinal_features_no_LGM) and merging them to
  ## covariates names
  load(here::here("data", "intermediate", "names_rater_covariates.Rdata"))
  temp <- load(here::here("data", "intermediate", "names_rater_covariates.Rdata"))
  cat(
    "vector with names of rater covariates loaded in; name of object: ",
    "'",
    temp,
    "'",
    "\n",
    "\n"
  )
  
  ## loading in df with Family Numbers
  load(here::here("data", "intermediate", "FIS_fam_nr.RData"))
  temp <- load(here::here("data", "intermediate", "FIS_fam_nr.RData"))
  cat(
    "saved df with FISNr and FamilyNumber loaded in; name of object: ",
    "'",
    temp,
    "'",
    "\n",
    "\n"
  )
  
  ## one vector with all covariates names
  covariates_names <- c(covariates_names, rater_covariates)
  
  
  ## further distinction: numeric and factor covariates
  num_covariates <- c(
    grep("time_lag", covariates_names, value = TRUE),
    grep("age_qol", covariates_names, value = TRUE),
    rater_covariates
  )
  
  factor_covariates <- setdiff(covariates_names, num_covariates)
  
  
  ## converting covariates to factors
  ## (this is done in the function f_conv)
  data_full_raw <- f_conv(df = data_full_raw, covariates = factor_covariates)
  
  
  ## converting columns with multiple
  ## class types to numeric
  data_full_raw <- mult_to_numeric(df = data_full_raw)
  
  ## dummy coding categorical covariates
  data_dummies_A <- dummy_cols(
    data_full_raw,
    select_columns = factor_covariates,
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE,
    ignore_na = TRUE
  ) ## not own column, but missing
  ## information here will be imputed as well
  
  dummy_vars <- setdiff(colnames(data_dummies_A), colnames(data_full_raw))
  
  covariates_full <- c(num_covariates, dummy_vars)
  
  
  ## preprocessing for machine learning
  ## (this is done in the function f_preprocess)
  ## nzv removal, high cor removal, linear combination removal, imputation
  preprocessed_A <- ml_preprocess(
    df = data_dummies_A,
    train_ids = train_ids,
    test_ids = test_ids,
    covariates = covariates_full
  )
  
  ## checking if family is still included in training variables
  cat(
    "FamilyNumber still in training set: ",
    "FamilyNumber" %in% colnames(preprocessed_A$x_train_comb),
    "\n"
  )
  
  x_train <- preprocessed_A$x_train_comb
  
  x_test <- preprocessed_A$x_test_comb
  
  x_full <- list(x_train = x_train, x_test = x_test)
  
  ## saving x_train and x_test for every run
  run <- b_iter + 1
  filename <- paste0("full_prepared_data_it_", run, ".rds")
  
  ## always saving list with two objects: [[1]] is x_train, [[2]] is x_test
  saveRDS(x_full, file = paste0(here::here(
    "data", "intermediate", "prep_data_A", filename
  )))
  cat("data preparation iteration ", b_iter, " finished", "\n")
  cat("data saved under ", filename, "\n", "\n")
}

t01 <- Sys.time()

cat("duration entire script: ",
    difftime(t01, t00, unit = "mins"), " minutes")