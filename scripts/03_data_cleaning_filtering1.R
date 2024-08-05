# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-07-31
#
# Script Name: 03_data_cleaning01_filtering1
#
# Script Description: This script contains the first steps of data cleaning
# for the project: 
## Combining longitudinal change features of childhood psychopathology 
## with Polygenic scores in machine learning models of adult wellbeing
#
# Notes: Filtering procedure may still be adjusted after consultation with the 
# supervisors and after receiving PGS data
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "caret")

## printing and changing working directory if needed
getwd()

here::here()

getwd() == here::here()

## loading in custom functions
source(here::here("scripts", "functions", "functions.R"))


## reading in datafile (if necessary, change filepath to where file is located)
data <- read_sav(here::here("data", "source_raw", "PHE_20240722_4552_YJS.sav")) %>%
  as.data.frame()

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
load(here::here("scripts", "variable_vectors.RData"))

## loading in sumamry dataframe from exploration
load(here::here("scripts", "summary_CBCL.RData"))

## loading in list of CBCL items per question
load(here::here("scripts", "CBCL_questions_list.RData"))

nrow(data)
# Initially 92969 participants in dataset

## Filtering steps: 

# 1) Remove all participants with no outcome
data1 <- data %>% 
  filter(!is.na(levenc8) | !is.na(levenc10) |
         !is.na(levenc12) | !is.na(levenc14))

nrow(data1)
nrow(data) - nrow(data1)
## 39764 participants with wellbeing / QoL data are available
rm(data)

# 2) Filter all participants who did not participate in any YNTR survey
data2 <- data1 %>%
  filter(!is.na(in_YS_3M) | !is.na(in_YS_5) | !is.na(in_YS_7M) |
         !is.na(in_YS_10M) | !is.na(in_YS_12M) | !is.na(in_YS_DHBQ14) | 
         !is.na(in_YS_DHBQ16))

nrow(data2)
nrow(data1) - nrow(data2)
## After second filtering step, still 11169 participants, 28595 were dropped

rm(data1)

## further filtering steps...

## removing participants with only NAs in CBCL variables
all_na_rows <- apply(data2 %>%
  dplyr::select(all_of(CBCL_YSR_items_vec)), 1, function(x) all(is.na(x)))

data3 <- data2[!all_na_rows, ]

nrow(data3)
nrow(data2) - nrow(data3)

rm(data2)

## removing +50% missings
## KNN impute for e.g. covariates and PGSs...

## KNN imputation

data4 <- data3 %>%
  dplyr::select(all_of(CBCL_YSR_items_vec)) %>%
  mutate(across(everything(), as.numeric))

## removing rows with +50% missing
threshold <- 0.5
rows_with_excessive_na <- apply(data4, 1, function(x) mean(is.na(x)) > threshold)
filtered_data4 <- data4[!rows_with_excessive_na, ]

filtered_data4_id <- data3[!rows_with_excessive_na, ] %>%
  dplyr::select(FISNumber)

## removing only NA columns
#all_na_columns <- sapply(data4, function(x) all(is.na(x)))
#data4 <- data4[, !all_na_columns]

## Removing columns with +50% missings to see if that changes anything
col_threshold <- 0.5
cols_with_excessive_na <- sapply(filtered_data4, function(x) mean(is.na(x)) > col_threshold)
filtered_data4 <- filtered_data4[, !cols_with_excessive_na]

## This actually worked! Possible that the list of CBCL question to 
## perform longitudinal modeling on needs to be reduced! 


pre_model <- preProcess(filtered_data4, method = "knnImpute", k = 5,
                        verbose = TRUE)

imputed <- predict(pre_model, newdata = filtered_data4)

## Binding with ID column again
data4 <- bind_cols(filtered_data4_id, imputed)
nrow(data4)
nrow(data3) - nrow(data4)

# [...]


##-----------------------------------------------------------------------------

## Outlier removal: Calculation of the Minimum-covariance determinant (MCD),
## a more robust version of Mahalanobis' distance (Leys et al., 2018)

## For now only do this with the CBCL data, dataset fed will later be adjusted

## For now: Toy data, only two columns to check if original df value 
## and function work in general

library(MASS)
## NOTE: MASS and dplyr both have a select function!
## indicate package before calling function (dplyr::select)


data_mcd_toy <- data2 %>%
  dplyr::select(all_of(CBCL_YSR_items_vec)) %>%
  dplyr::select(1:2)

## IMPORTANT: THOSE functions can only be applied after imputation! Missings
## are not allowed

## Code was taken from Leys et al., 2018

# Creating covariance matrix for MCD («data_mcd» is the matrix containing  
# data with no indicator variable
output50 <- cov.mcd(data_mcd_toy, quantile.used = nrow(data_mcd_toy)* .5)
output75 <- cov.mcd(data_mcd_toy, quantile.used = nrow(data_mcd_toy)* .75)

# Distances from centroid for each matrix
md <- mahalanobis(data_mcd_toy, colMeans(data_mcd_toy), cov(data_mcd_toy))
mhmcd50 <-mahalanobis(data_mcd_toy, output50$center, output50$cov)
mhmcd75 <-mahalanobis(data_mcd_toy, output75$center, output75$cov)

# Detecting outliers for each method
# The index of each detected outlier is recorded for each method for a 
# alpha= .01
# For more than two variables, df of cutoff variable (in bold)
## has to be adjusted

alpha <-.05 ## less conservative than Leys at al

cutoff <- (qchisq(p = 1 - alpha, df = 2)) ## ADJUST THIS! 
names_outliers_MH <- which(md > cutoff)
names_outliers_MCD50 <- which(mhmcd50 > cutoff)
names_outliers_MCD75 <- which(mhmcd75 > cutoff)

# Excluding outliers in a new matrix (here based on MCD75) called data_mcd2
excluded <- names_outliers_MCD75


## later! First check if 10% instead of alpha could be applied
data_mcd2_toy <-data_mcd_toy[-excluded,]








