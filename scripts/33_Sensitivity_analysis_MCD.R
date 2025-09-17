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
# (MCD), filtering multivariate outliers to create subset of full dataset and
# re-run analyses 
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
               "stringr", "readxl", "data.table")


## Note: Participants will only be removed from the training set, 
## test set will be the same

## different outliers for all 5 feature sets! 

###############################################################################

## Model A
## loading in data objects for the machine learning models

## note: re-run model A data prep before this or check in SciStor if it was done
## if not, change script 09 according to script 23 (mdoel D)
filepath_A <- "A:\ML_WB_longitudinal_CBCL_PGS_LGM\data\intermediate\prep_data_A"
x_train_A <- readRDS(paste0(filepath_A, "/", list.files(filepath_A)[[1]]))[["x_train"]]

## Model B 


## Model C


## Model D


## Model E









