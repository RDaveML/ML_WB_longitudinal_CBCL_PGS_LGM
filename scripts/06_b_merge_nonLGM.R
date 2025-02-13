# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-12-03
#
# Script Name: 06_b_merge_nonLGM.R
#
# Script Description: In this script, the participant's raw answers to the
# CBCL questions are merged with the non-LGM features calculated in the 
# scripts 06_longitudinal_features_no_LGM and 06_a_autocorrelation
# Necessary covariates will be retained and the complete feature set which
# will be used in the baseline models (model A, no LGM features, no Polygenic scores)
# will be saved
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


## loading in custom functions
source(here::here("scripts", "functions", "functions_longitudinal_features.R"))



## Loading in data and item vectors
## loading in full cleaned dataset (CBLC + IDs + covariates, PGS still missing)
load(here::here("data", "intermediate", "data_full.RData"))
temp <- load(here::here("data", "intermediate", "data_full.RData"))
cat("full raw CBCL data loaded in; name of object: ", "'", temp, "'",
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


## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx")) %>%
  as.data.frame()

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
load(here::here("scripts", "variable_vectors.RData"))
temp <- load(here::here("scripts", "variable_vectors.RData"))
cat("CBCL variable names vectors loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")

## loading in list of CBCL items per question
load(here::here("scripts", "CBCL_questions_list.RData"))
temp <- load(here::here("scripts", "CBCL_questions_list.RData"))
cat("list of CBCL items per question loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")


## loading in CBCL items to be retained and to be dropped
load(here::here("scripts", "CBCL_items_keep"))
load(here::here("scripts", "CBCL_items_drop"))
temp <- load(here::here("scripts", "CBCL_items_drop"))
cat("list of CBCL itemstp be dropped loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")



## Cropping down the CBCL items per question (removing dropped items)
cat("removing items to be dropped",
    "\n", "\n")
for(i in 1:length(CBCL_questions_list)){
  CBCL_questions_list[[i]] <- setdiff(CBCL_questions_list[[i]], CBCL_items_drop)
}


## loading in feature dfs

## longitudinal mean & SD
load(here::here("data", "intermediate", "df_mean_SD_long.Rdata"))
temp <- load(here::here("data", "intermediate", "df_mean_SD_long.Rdata"))
cat("df with longitudinal means and SDs loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")

## RMSSD
load(here::here("data", "intermediate", "df_RMSSD.Rdata"))
temp <- load(here::here("data", "intermediate", "df_RMSSD.Rdata"))
cat("df with RMSSD features loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")

## autocorrelation and autoregression features
load(here::here("data", "intermediate", "data_acf_imp.Rdata"))
temp <- load(here::here("data", "intermediate", "data_acf_imp.Rdata"))
cat("df with autocorrelation and autoregression features loaded in; name of object: ",
    "'", temp, "'", "\n", "\n")


## rater means and sds for every participant
load(here::here("data", "intermediate", "df_rater_covariates.Rdata"))
temp <- load(here::here("data", "intermediate", "df_rater_covariates.Rdata"))
cat("df with mean and sd per rater for every participant; name of object: ",
    "'", temp, "'", "\n", "\n")


## covariates for the analysis (explored and preprocessed in script
## 03_covariates.R)
load(here::here("data", "intermediate", "data_covariates.RData"))
temp <- load(here::here("data", "intermediate", "data_covariates.RData"))
cat("df with pre-registered covariates loaded in; name of object: ",
    "'", temp, "'", "\n", "\n")

## vector of covariate names
load(here::here("scripts", "names_covariates.RData"))
temp <- load(here::here("scripts", "names_covariates.RData"))
cat("vector with covariate names loaded in; name of object: ",
    "'", temp, "'", "\n", "\n")


## filtering covariate data so that same participants remain as in 
## full raw data
data_covariates <- data_covariates %>%
  filter(FISNumber %in% unique(data_full$FISNumber))



## cropping down full data: Only keeping the ID, raw CBCL scores and outcome
## use this for model A as well
data_full_raw <- data_full %>%
  select(FISNumber, FamilyNumber, all_of(CBCL_items_keep), QoL_simple) %>%
  left_join(data_covariates) ## covariates

## saving dataframe with only raw CBCL scores and covariates
filename <- "data_model_0.Rdata"
save(data_full_raw, file = here("data", "intermediate", "data_model_0.Rdata"))
cat("data were saved in file: ", filename)



## Binding together all dfs
data_model_A <- data_full_raw %>% # raw CBCL vars & outcome
  left_join(mean_long_df, by = "FISNumber") %>% ## longitudinal mean & SD
  left_join(rmssd_df1, by = "FISNumber") %>% # RMSSD
  left_join(full_acf_df, by = "FISNumber") %>% ## autocorrelation & autoregression
  left_join(data_rater, by = "FISNumber")



dim(data_model_A)
## 5087 individuals, 2373 features (pre-preprocessing)

## saving dataframe
filename <- "data_model_A.Rdata"
save(data_model_A, file = here("data", "intermediate", "data_model_A.Rdata"))
cat("data were saved in file: ", filename)

cat("end of script")

# end of script
