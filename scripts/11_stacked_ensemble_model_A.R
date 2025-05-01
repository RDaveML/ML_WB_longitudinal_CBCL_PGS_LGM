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
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "caret")


## loading in ML custom ML + Hypertuning functions
source(here::here("scripts", "functions", "functions_ml.R"))

## Steps: 

## 1) Loading in original train test split
train_ids <- readRDS(here::here("data", "intermediate", "indices_train.rds"))

test_ids <- readRDS(here::here("data", "intermediate", "indices_test.rds"))

## 2) Preparing data

## loading in data with family numbers (Object is called df_FIS_fam)
load(here::here("data", "intermediate", "FIS_fam_nr.RData"))


## Loading in prediction data from random forest and xbg
pred_rf <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_A_iteration_1.rds"))$run_rf$preds_df_rf 

pred_xgb <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_A_iteration_1.rds"))$run_xgb$preds_df_xgb 

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
model_lm <- train(QoL_simple ~ . - FISNumber - FamilyNumber, 
                  data = data_train, 
                  method = "lm",
                  trControl = trainControl(method = "cv", 
                                           index = folds,
                                           savePredictions = TRUE,
                                           allowParallel = TRUE))

## comparison of model performance

## load in model objects of level one models (original predictions)

## (optional) b) xgboost with Bayesian hypertuning
exclude_vars <-  c("FISNumber", "FamilyNumber", "QoL_simple")
x_train_matrix <- model.matrix(~ ., data = data_train)[, !(colnames(model.matrix(~ ., data = df)) %in% exclude_vars)]
y_train_vector <- data_train$QoL_simple


## 4) Feature importance analysis




















