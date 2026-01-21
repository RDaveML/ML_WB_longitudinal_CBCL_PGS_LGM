# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-09-17
#
# Script Name: 33_Sensitivity_analysis_MCD.R
#
# Script Description: Coding of the Minimum covariance determinant 
# (MCD), filtering multivariate outliers to create subset of full dataset
# for all variable sets and to later re-run analyses 
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
               "stringr", "readxl", "data.table", "robustbase", "admisc",
               "MASS", "rrcov", "caret", "rrcovHD")

## function to calculate MCD contained here
source(here::here("scripts", "functions", "functions_preprocessing.R"))


## Note: Participants will only be removed from the training set, 
## test set will be the same

## different outliers for all 5 feature sets! 

###############################################################################

## For all datasets: 
# - remove IQR = 0 columns
# - calculate (with robustbase) covmcd
# - calculate mahalanobis based on covmcd (50 and 75)
# - save IDs with highest 5% mcd
# - save a vector of IDs to be removed


## loading in data objects with training sets for ML models 
## (those were created in the respective bootstrapped run files)
filepaths_prep_data <- 
  paste0("A:/ML_WB_longitudinal_CBCL_PGS_LGM/data/intermediate/data_model_", 
         c("A", "B", "C", "D", "E"), "_train.rds")

data_sets_train <- lapply(filepaths_prep_data, function(x){
  ## creates a nameless list with all the datafiles
  readRDS(x)
})

names(data_sets_train) <- paste0("dataset_", c("A", "B", "C", "D", "E"))


t1 <- Sys.time()
## looping custom MCD calculation function with the names of the datasets A-E
ids_mcd_ABCDE <- mapply(
  mcd_5,
  data_sets_train,
  names(data_sets_train),
  seed = 1:length(data_sets_train), ## ensuring reproducibility of sample split
  SIMPLIFY = FALSE
)

names(ids_mcd_ABCDE) <- paste0("names_outliers_MCD_",
                               c("A", "B", "C", "D", "E"))

t2 <- Sys.time()

cat("duration removing mcd outliers (5%): ",
    difftime(t2, t1, unit = "mins"), " minutes")
## NOTE: MASS and dplyr both have a select function!
## indicate package before calling function (dplyr::select)

# eoS




