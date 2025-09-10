# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-09-08
#
# Script Name: 32_demographics_descriptive statistics.R
#
# Script Description:
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


###############################################################################

## 1) full dataset 


## loading in raw dataset A (no genetic data) and covariates A to calculate 
## descriptive statistics
data_full_raw <- readRDS(here::here("data", "intermediate", "data_model_A.rds"))

covariates_names <- readRDS(
  here::here("data", "intermediate", "names_covariates.rds")
)


## number of families 
length(unique(data_full_raw$FamilyNumber))
## 3,250 families

## extract descriptives about the covariates
table(data_full_raw$sex)
table(data_full_raw$twzyg)
table(data_full_raw$ea4fa_agg)
table(data_full_raw$ea4mo_agg)
table(data_full_raw$QoL_indicator)

## descriptives for continuous variables
summary(data_full_raw %>% select(age_qol, time_lag))

## histogram of age
data_full_raw %>%
  ggplot(aes(x = age_qol)) + 
  geom_histogram(bins = round(max(data_full_raw$age_qol, na.rm = TRUE), 1) - 
                        round(min(data_full_raw$age_qol, na.rm = TRUE), 1) + 1)

## histogram of outcome QoL
data_full_raw %>%
  ggplot(aes(x = QoL_simple)) + 
  geom_histogram(bins = 10) + 
  ## setting marks to integers 1- 10





###############################################################################

## 2) participants with genetic data available 

## loading in covariate names
covariates_names <- readRDS(
  here::here("data", "intermediate", "names_covariates.rds"))
covariates_names

## loading in vector with names genetic covariates
gen_covariates <- readRDS(
  here::here("data", "intermediate", "names_genetic_covariates.rds"))

gen_covariates <- c(gen_covariates, "EUR_1KG_Outlier", "NL_Strict_Outlier")

data_model_B_base <- readRDS(here::here("data", "intermediate", "PGS", "data_PGS_model_B.rds"))

table(data_model_B_base$NL_Strict_Outlier)
table(data_model_B_base$EUR_1KG_Outlier)

