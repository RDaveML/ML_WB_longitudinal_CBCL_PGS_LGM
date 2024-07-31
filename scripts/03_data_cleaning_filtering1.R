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
               "stringr", "readxl", "data.table")

## printing and changing working directory if needed
getwd()

here::here()

getwd() == here::here()

## reading in datafile (if necessary, change filepath to where file is located)
data <- read_sav(here::here("data", "source_raw", "PHE_20240722_4552_YJS.sav")) %>%
  as.data.frame()

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
load(here::here("scripts", "variable_vectors.RData"))

## Filtering steps: 

# 1) Remove all participants with no outcome
data <- data %>% 
  filter(!is.na(levenc8) | !is.na(levenc10) |
         !is.na(levenc12) | !is.na(levenc14))

# 2) Filter all participants who did not participate in the YNTR surveys
data <- data %>%
  filter(!is.na(in_YS_3M) | !is.na(in_YS_5) | !is.na(in_YS_7M) |
         !is.na(in_YS_10M) | !is.na(in_YS_12M) | !is.na(in_YS_DHBQ14) | 
         !is.na(in_YS_DHBQ16))


