# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-08-15
#
# Script Name: 06_LGM.R
#
# Script Description: In this script, the filtered CBCL variables 
# (see script 04_data_cleaning_filtering1.R) from the project:
# Combining longitudinal change features of childhood psychopathology 
# with Polygenic scores in machine learning models of adult wellbeing
# will be used for latent growth modeling (LGM) to create 
# features carrying longitudinal information about childhood psychopathology
# these features will later be used in the ML predictor space for the 
# prediction of adult wellbeing (Qualitý of life)
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


## loading in necessary dataset and vectors / tables of variables 
load(here::here("data", "intermediate", "data_LGM.RData"))

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
load(here::here("scripts", "variable_vectors.RData"))

## loading in list of CBCL items per question
load(here::here("scripts", "CBCL_questions_list.RData"))

## loading in CBCL items to be retained and to be dropped
load(here::here("scripts", "CBCL_items_keep"))
load(here::here("scripts", "CBCL_items_drop"))

## First task: Adjust the variable vectors, some of the variables are not 
## in the dataset anymore! 
CBCL_items_LGM <- intersect(CBCL_items_keep, CBCL_items_vec)
YSR_items_LGM <- intersect(CBCL_items_keep, YSR_items_vec)



## Second task: Which of the CBCL longitudinal questions are still eligible for
## longitudinal modeling? Some of the items were eliminated during data cleaning
## In order to do longitudinal modeling, there need to be at least 4 measuring
## points per CBCL question

## counting how many items per question are still available, exclude any 
## CBCL questions with less than 4 measures still available
CBCL_items_table <- CBCL_items_table %>%
  rowwise() %>%
  mutate(n_items_available = sum(
    sapply(across(everything()), function(x) as.character(x) %in% CBCL_items_keep)
  )) %>%
  ungroup() %>%
  filter(n_items_available >= 4)
## 80 questions might still be used for latent growth modeling



## Checking structure of missing:
colMeans(is.na(data_LGM)) %>% as.data.frame() %>% View()


## Now: With one item test it out: Pivot data, calculate all you 
## need for LGM, make the entire Mplus stuff for one item and save output
## write down what the function that does the job needs for arguments
## then lapply or sapply it over the items and then bind together all
## the resulting dfs


