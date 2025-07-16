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
# load(here::here("data", "intermediate", "data_full.RData"))
data_full <- readRDS(here::here("data", "intermediate", "data_full.rds"))


## loading in training ids and test ids (split created)
# load(here::here("data", "intermediate", "indices_train.RData"))
train_ids <- readRDS(here::here("data", "intermediate", "indices_train.rds"))

# load(here::here("data", "intermediate", "indices_test.RData"))
test_ids <- readRDS(here::here("data", "intermediate", "indices_test.rds"))


## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx")) %>%
  as.data.frame()

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
CBCL_YSR_items_vec <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[1]]

CBCL_items_vec <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[2]]

YSR_items_vec <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[3]]

ea_vars <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[4]]

qol_vars <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[5]]

## loading in list of CBCL items per question
CBCL_questions_list <- readRDS(
  here::here("scripts", "CBCL_questions_list.rds"))


## loading in CBCL items to be retained and to be dropped
CBCL_items_keep <- readRDS(
  here::here("data", "intermediate", "CBCL_items_keep.rds")
)
CBCL_items_drop <- readRDS(
  here::here("data", "intermediate", "CBCL_items_drop.rds")
)



## Cropping down the CBCL items per question (removing dropped items)
cat("removing items to be dropped",
    "\n", "\n")
for(i in 1:length(CBCL_questions_list)){
  CBCL_questions_list[[i]] <- setdiff(CBCL_questions_list[[i]], CBCL_items_drop)
}


## loading in feature dfs

## longitudinal mean & SD
# load(here::here("data", "intermediate", "df_mean_SD_long.Rdata"))
mean_long_df <- readRDS(
  here::here("data", "intermediate", "df_mean_SD_long.rds")) %>%
    rename_with(~ paste0("non_LGM_", .), -FISNumber)
## adding prefix "non_LGM" to all column names for later comparison of 
## feature sets



## RMSSD
# load(here::here("data", "intermediate", "df_RMSSD.Rdata"))
## note: this df you might need to check again because of NaNs
rmssd_df1 <- readRDS(here::here("data", "intermediate", "df_RMSSD.rds")) %>%
  rename_with(~ paste0("non_LGM_", .), -FISNumber)



## autocorrelation and autoregression features
# load(here::here("data", "intermediate", "data_acf_imp.Rdata"))
full_acf_df <- readRDS(
  here::here("data", "intermediate", "data_acf_imp.rds")) %>%
    rename_with(~ paste0("non_LGM_", .), -FISNumber)



## rater means and sds for every participant
# load(here::here("data", "intermediate", "df_rater_covariates.Rdata"))
data_rater <- readRDS(
  here::here("data", "intermediate", "df_rater_covariates.rds"))



## covariates for the analysis (explored and preprocessed in script
## 03_covariates.R)
# load(here::here("data", "intermediate", "data_covariates.RData"))
data_covariates <- readRDS(
  here::here("data", "intermediate", "data_covariates.rds")
  )

## vector of covariate names
# load(here::here("data", "intermediate", "names_covariates.RData"))
covariates_names <- readRDS(
  here::here("data", "intermediate", "names_covariates.rds"))


## filtering covariate data so that same participants remain as in 
## full raw data
data_covariates <- data_covariates %>%
  filter(FISNumber %in% unique(data_full$FISNumber))



## cropping down full data: Only keeping the ID, raw CBCL scores and outcome
## use this for model A as well
data_full_raw <- data_full %>%
  select(FISNumber, FamilyNumber, all_of(CBCL_items_keep), QoL_simple) %>%
  left_join(data_covariates) %>% ## covariates
  left_join(data_rater, by = "FISNumber")

## saving dataframe with only raw CBCL scores and covariates
filename <- "data_model_A.rds"
# save(data_full_raw, file = here("data", "intermediate", "data_model_0.Rdata"))
saveRDS(data_full_raw, here::here("data", "intermediate", "data_model_A.rds"))
cat("data were saved in file: ", filename)



## Binding together all dfs (note; this is then dataframe for model D! Only there 
## longitudinal features are first included)
data_model_D_1 <- data_full_raw %>% # raw CBCL vars & outcome
  left_join(mean_long_df, by = "FISNumber") %>% ## longitudinal mean & SD
  left_join(rmssd_df1, by = "FISNumber") %>% # RMSSD
  left_join(full_acf_df, by = "FISNumber") ## autocorrelation & autoregression


dim(data_model_D_1)
## 5087 individuals, 2373 features (pre-preprocessing)

## saving dataframe
filename <- "data_model_D_1.rds"
# save(data_model_A, file = here("data", "intermediate", "data_model_A.Rdata"))
saveRDS(data_model_D_1, here::here("data", "intermediate", "data_model_D_1.rds"))
cat("data were saved in file: ", filename)

cat("end of script")

# end of script
