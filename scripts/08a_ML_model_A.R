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


## Pre-processing steps


## A) Imputing data

## Note: Outlier removal by means of the Minimum covariance determinant (MCD)
## can only be done with imputed data






## B) Outlier removal: Minimum covariance determinant (MCD)





## C) ML preprocessing

## i) near-zero variance

## ii) high correlation

## iii) multicollinearity

## iv) linear dependence

## v) one-hot encoding categorical features






