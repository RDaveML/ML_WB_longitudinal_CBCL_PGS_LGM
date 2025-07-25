# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-07-14
#
# Script Name: 22_data_merge_model_D_E.R
#
# Script Description:
# Dataset for the models D and E are created in this script
# data frames of CBCL features, non-LGM longitudinal features (created in 
# script 06b) and LGM-longitudinal features (created in script 07) are 
# joined together resulting in full feature set model D, covariates relevant
# (saved in script 03?!) are checked if they were correctly included
#
# filtered data of model D will then be joined with PGS data to create
# the dataset for model E, covariates will be checked in line with covariate 
# set model C (combination of raw CBCL features and PGS), genetic covariates
# saved in script 08
#
#
# Notes: ## CONTINUE HERE!! Still write explicitly what feature
# sets model D and E are finally
#
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "MplusAutomation", "glue",
               "purrr")

## reading in RDSdata (once not working on server anymore)
LGM_df <- readRDS(here::here("data", "intermediate", "LGM_df.rds")) %>%
  ## Remove all columns that refer to standard errors of estimates
  ## (not-informative, still contained in data file for later inspection,
  ## additional analysis)
  select(-contains("_SE_"))

###############################################################################


## feature set model D


## load in non-LGM features dataframe created in script 06
data_non_LGM <- readRDS(
  here::here("data", "intermediate", "data_model_D_1.rds"))

## merge non_LGM longitudinal df with LGM longitudinal df 
data_full_model_D <- data_non_LGM %>%
  left_join(LGM_df, by = "FISNumber")

dim(data_full_model_D)


## checking again if all covariates for model A are contained in the 
## df (vector saved in script 09)
## adjust this and check back with the original covariate set 
## (probably saved in script 03)
names_covariates_A <- readRDS(
  here::here("data", "intermediate", "names_covariates.rds"))

## also add the longitudinal rater covariates (m_self, sd_self, etc. those were
## saved in script 06)
rater_covariates <- readRDS(
  here::here("data", "intermediate", "names_rater_covariates.rds"))

covariates_model_A <- c(names_covariates_A, rater_covariates)

setdiff(covariates_model_A, colnames(data_full_model_D))
## no differences, all covariates contained in feature set!

saveRDS(data_full_model_D,
        file = here::here("data", "intermediate", "data_full_model_D.rds"))

## data prep model D/E will follow in the respective ML modelling scripts
## (also check covariates, raw item scores, etc.)

###############################################################################

## feature set model E

## loading in training / test indices PGS dataset for filtering
indices_train_PGS <- readRDS(
  here::here("data", "intermediate", "indices_train_PGS.rds"))

indices_test_PGS <- readRDS(
  here::here("data", "intermediate", "indices_test_PGS.rds"))


PGS_data <- readRDS(
  here::here("data", "intermediate", "PGS", "data_PCA_PGS.rds"))


## filtering data model D for subjects that have genetic data (are included in 
## indices_train_PGS or indices_test_PGS)

data_model_E <- data_full_model_D %>%
  filter(FISNumber %in% c(indices_train_PGS, indices_test_PGS))

## loading in genetic data (only those participants that also
## have CBCL items answered, shrunken df saved in script 13)
## and genetic covariates

gen_data <- readRDS(
  here::here("data", "intermediate", "PGS", "data_PGS_model_B.rds")) # %>%
## optional: add prefix GEN_ to all variables that contain genetic information
## for later feature comparison
  ## rename_with(~ paste0("GEN_", .), -FISNumber, -FamilyNumber, -QoL_simple)

## checking if FISNumbers match
setdiff(data_model_E$FISNumber, gen_data$FISNumber)
## no differences, correct individuals in both dataframes

## merging model D data with genetic data
## check which columns exist in both dataframes and need to be matched on 
## as well! 
setdiff(colnames(gen_data),
        setdiff(colnames(gen_data), colnames(data_model_E)))

## "FISNumber", "FamilyNumber", "QoL_simple" exist in both datasets, match on 
## these columns
data_full_model_E <- data_model_E %>%
  left_join(gen_data, by = c("FISNumber", "FamilyNumber", "QoL_simple"))

ncol(gen_data)
ncol(data_model_E) + ncol(gen_data) - 3 == ncol(data_full_model_E) 
## worked out!


gen_covariates <- readRDS(
  here::here("data", "intermediate", "names_genetic_covariates.rds"))

outlier_cols <- readRDS(
  here::here("data", "intermediate", "names_outlier_columns_PGS.rds"))

## checking if all genetic covariates are contained in dataset
setdiff(c(gen_covariates, outlier_cols), colnames(data_full_model_E))
## no differences, all genetic covariates are contained in dataset!

## saving df
saveRDS(data_full_model_E,
        file = here::here("data", "intermediate", "data_full_model_E.rds"))

## eoS

