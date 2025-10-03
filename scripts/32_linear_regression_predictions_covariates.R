# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-09-17
#
# Script Name: 32_linear_regression_predictions_covariates.R
#
# Script Description: IN this script, the predictions of the models 
# will be regressed on the covariates to make sure their influence is
# not overly strong
#
#
# Notes: This also needs to be done for all models and algorithms
#
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table")


## idea: turn this into function, lapply over all datasets, covariate vectors
## complete, included and then code linear model

###############################################################################

## Model A

## loading in prediction dfs for all models ran

## loading in covariates for this specific feature set

## joining

## coding linear regression

###############################################################################

## Model B

## loading in prediction dfs for all models ran

## loading in covariates for this specific feature set

## joining

## coding linear regression

###############################################################################


## Model C

## loading in prediction dfs for all models ran

## loading in covariates for this specific feature set

## joining

## coding linear regression

###############################################################################



## Model D

## loading in prediction dfs for all models ran

## loading in covariates for this specific feature set

## joining

## coding linear regression

###############################################################################



## Model E

## loading in prediction dfs for all models ran

## loading in covariates for this specific feature set

## joining

## coding linear regression

###############################################################################


# eoS












