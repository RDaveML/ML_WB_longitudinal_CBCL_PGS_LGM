# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-07-31
#
# Script Name: 04_data_cleaning01_filtering1
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
source(here::here("scripts", "functions", "functions_preprocessing.R"))


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

## loading in covariate data for further filtering
load(here::here("scripts", "data_covariates.RData"))
## loading in covariate names 
load(here::here("scripts", "names_covariates.RData"))

nrow(data)
# Initially 92969 participants in dataset

## filtering data in one compact function, outputting dropped participants 
## after each filtering step
data_filtered <- filter_CBCL(df = data, CBCL_YSR_items_vec = CBCL_YSR_items_vec,
                             data_covariates = data_covariates)


## checking some of the important covariates:
## twin status
table(data_filtered$twzyg, useNA = "ifany")
## no non-twins present in sample

## any missing aggregated variables
table(data_filtered$in_NA_AGG, useNA = "ifany")
## no missings in the aggregated variables


## ---------------------------------------------------------------------------

## recoding items that are on different scale!

CBCL_items_table_y5 <- CBCL_items_table %>%
  filter(!is.na(Age5))

CBCL_items_CBCL103 <- unname(unlist(c(CBCL_items_table_y5[1, 1:7])))
CBCL_items_CBCL50 <- unname(unlist(c(CBCL_items_table_y5[2, 1:7])))
## CBCL question 57 was not asked at age 3
CBCL_items_CBCL57 <- unname(unlist(c(CBCL_items_table_y5[3, 1:7])))[!is.na(
  unname(unlist(c(CBCL_items_table_y5[3, 1:7]))))]

# filtering data for only those variables
data_CBCL_103 <- data_filtered %>% 
  select(all_of(CBCL_items_CBCL103))

for(variable in CBCL_items_CBCL103){
print(prop.table(table(data_CBCL_103 %>% select(variable) %>%
                         filter(!is.na(variable)))))
  
}

data_CBCL_50 <- data_filtered %>% 
  select(all_of(CBCL_items_CBCL50))

for(variable in CBCL_items_CBCL50){
  print(prop.table(table(data_CBCL_50 %>% select(variable) %>%
                           filter(!is.na(variable)))))
  
}

data_CBCL_57 <- data_filtered %>% 
  select(all_of(CBCL_items_CBCL57))

for(variable in CBCL_items_CBCL57){
  print(prop.table(table(data_CBCL_57 %>% select(variable) %>%
                           filter(!is.na(variable)))))
  
}

## Inspection of the frequency distributions suggests the 
## following recoding for the YNTR5 questions: 
## 1-2 to 0; 3-4 to 1; 5 - 2

## Recoding the items 
q5 <- c("q13m5", "q11m5", "q4m5")

data_filtered_recoded <- data_filtered %>%
  mutate(across(all_of(q5), 
         ~ case_when(. == 1 | . == 2 ~ 0,
                     . == 3 | . == 4 ~ 1,
                     . == 5 ~ 2,
                     .default = NA)))

## checking if recoding went properly
table(data_filtered$q13m5, useNA = "ifany")
table(data_filtered_recoded$q13m5, useNA = "ifany")

table(data_filtered$q11m5, useNA = "ifany")
table(data_filtered_recoded$q11m5, useNA = "ifany")

table(data_filtered$q4m5, useNA = "ifany")
table(data_filtered_recoded$q4m5, useNA = "ifany")

## it worked out correctly

## removing intermediary objects
rm(list = c("data_CBCL_103", "data_CBCL_50", "data_CBCL_57"))



## removing CBCL items with more than 50% missings 
col_threshold <- 0.5


data_CBCL <- data_filtered_recoded %>%
  select(all_of(CBCL_YSR_items_vec)) %>% ## also add vector of LGM features here (In case after LGM modeling)
  mutate(across(everything(), as.numeric))

cols_with_excessive_na <- sapply(data_CBCL %>% select(
  all_of(CBCL_YSR_items_vec)),
  function(x) mean(is.na(x)) > col_threshold)

data_CBCL_cols <- data_CBCL[, !cols_with_excessive_na]
## 12 items from CBCL filtered

## vector of CBCL items to retain
CBCL_items_keep <- colnames(data_CBCL_cols)
save(CBCL_items_keep, file = here::here("scripts", "CBCL_items_keep"))

## vector of columns to drop for later
CBCL_items_drop <- setdiff(CBCL_YSR_items_vec, CBCL_items_keep)
save(CBCL_items_drop, file = here::here("scripts", "CBCL_items_drop"))


#-----------------------------------------------------------------------------

## creating full dataset and dataset for LGM modeling
data_full <- data_filtered_recoded %>%
  select(-any_of(CBCL_items_drop))

data_LGM <- data_filtered_recoded %>%
  select(FISNumber, FamilyNumber, QoL_simple, all_of(covariates_names),
         all_of(CBCL_items_keep))

## Saving datasets to continue working with them
save(data_full, file = here::here("data", "intermediate", "data_full.RData"))
save(data_LGM, file = here::here("data", "intermediate", "data_LGM.RData"))


## Note: This is all still without PGS calculations! Add those later in 
## separate preprocessing script and do the row and column filtering 
## accordingly, also left_join with main df then!


#-----------------------------------------------------------------------------

## Re-calculation of the summary df after filtering procedure and column 
## reduction
summary_df_final <- data.frame()

