# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-12-03
#
# Script Name: 08a_ML_model_A
#
# Script Description: This script contains the machine learning preprocessing
# steps and the machine learning modelling for the baseline model (model A)
# of the study "Combining longitudinal change features of
# childhood psychopathology with Polygenic scores in machine learning models
# of adult wellbeing". Model A serves as the baseline model and only contains 
# the raw responses to the childhood abnormal behavior questionnaires (CBCL),
# non LGM longitudinal features (longitudinal mean & SD, RMSSD, autocorrelation,
# autoregression) and the study covariates (sex, SES, time-lag)
#
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
               "stringr", "readxl", "data.table", "caret")


## loading in full model_A data (merged together in script 06_b_merge_nonLGM.R)
load(here::here("data", "intermediate", "data_model_A.Rdata"))
temp <- load(here::here("data", "intermediate", "data_model_A.Rdata"))
cat("full model_A data loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")


## loading in training ids and test ids (split created)
load(here::here("data", "intermediate", "indices_train.RData"))
temp <- load(here::here("data", "intermediate", "indices_train.RData"))
cat("saved indices of participants in training set loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")

load(here::here("data", "intermediate", "indices_test.RData"))
temp <- load(here::here("data", "intermediate", "indices_test.RData"))
cat("saved indices of participants in training set loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")


### NOTE: STILL INSERT the B different splits here: Baseline will be 
## done with split 1, the rest then runs separate baseline preprocessing


## ultimate splitting test and training data
data_train <- data_model_A %>% 
  filter(FISNumber %in% train_ids)

data_test <- data_model_A %>%
  filter(FISNumber %in% test_ids)


## Pre-processing steps

## A) standard ML preprocessing

## i) near-zero variance
nzv_train <- nearZeroVar(data_train)

data_train <- data_train[-nzv_train]


## ii) high correlation (note that all columns must be numeric, df not changed here)
num_data <- data_train[, sapply(data_train, is.numeric)]

high_cor = findCorrelation(cor(num_data,
                               use = "pairwise.complete.obs"),
                           cutoff = .95)

## iii) multicollinearity

## coding linear model where all variables are predictors, then check VIF, 
## if higher than 5 (or 10), remove

## iv) linear dependence

## v) one-hot encoding categorical features

##----------------------------------------------------------------------------


## B) Imputing data

## Note: Outlier removal by means of the Minimum covariance determinant (MCD)
## can only be done with imputed data

## before imputation: Take out FISNR! it should not be part of the KNN procedure
df_FISNr_train <- data_train %>%
  select(FISNumber)

df_FISNr_test <- data_test %>%
  select(FISNumber)

df_A_train <- data_train %>%
  select(-FISNumber)

df_A_test <- data_test %>%
  select(-FISNumber)


## KNN imputation: 
t1 <- Sys.time()

cat("Beginning KNN imputation training data")
k_pad <- round(sqrt(ncol(df_A_train)))
train_pre_obj <- preProcess(df_A_train,
                            method = "knnImpute",
                            k = k_pad)

t2 <- Sys.time()

cat("duration KNN imputation object: ", difftime(t2, t1, unit = "mins"))

df_A_train_imp <- predict(train_pre_obj, df_A_train)

t3 <- Sys.time()

cat("duration KNN imputation train data: ", difftime(t3, t2, unit = "mins"))

df_A_test_imp <- predict(train_pre_obj, df_A_test)

t4 <- Sys.time()

cat("duration KNN imputation test data: ", difftime(t4, t3, unit = "mins"))

sum(colMeans(is.na(df_A_train_imp)) != 0)
sum(colMeans(is.na(df_A_test_imp)) != 0)

##-----------------------------------------------------------------------------

## C) feature selection: Elastic net





