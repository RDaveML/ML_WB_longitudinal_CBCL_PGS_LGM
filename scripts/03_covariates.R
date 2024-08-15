# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date (begin): 2024-08-05
#
# Script Name: 04_covariates.R
#
# Script Description: This script extracts the pre-registered covariates 
# for the analysis. Those will then be saved and merged to the dataset at a
# later timepoint
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
               "stringr", "readxl", "data.table", "lubridate")

## loading in custom functions
source(here::here("scripts", "functions", "functions_preprocessing.R"))

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
load(here::here("scripts", "variable_vectors.RData"))

## loading in sumamry dataframe from exploration
load(here::here("scripts", "summary_CBCL.RData"))

## loading in list of CBCL items per question
load(here::here("scripts", "CBCL_questions_list.RData"))


data <- read_sav(here::here("data", "source_raw", "PHE_20240722_4552_YJS.sav")) %>%
  as.data.frame()


## Extraction of all pre-registered covariates for the analysis


## First: Applying the same filters as in filtering script
## (03_data_cleaning_filtering1.R)
data1 <- data %>% 
  filter(!is.na(levenc8) | !is.na(levenc10) |
           !is.na(levenc12) | !is.na(levenc14)) %>%
  filter(!is.na(in_YS_3M) | !is.na(in_YS_5) | !is.na(in_YS_7M) |
           !is.na(in_YS_10M) | !is.na(in_YS_12M) | !is.na(in_YS_DHBQ14) | 
           !is.na(in_YS_DHBQ16))


## removing participants with only NAs in CBCL variables
all_na_rows <- apply(data1 %>%
                       dplyr::select(all_of(CBCL_YSR_items_vec)), 1, function(x) all(is.na(x)))

data1 <- data1[!all_na_rows, ]

## saving FISNumber column
data_FIS <- data1 %>%
  select(FISNumber)

## PGS covariates: PCAs + Genotyping platform (dummy coded)

## sex
data_sex <- data1 %>%
  select(sex)

## twin status
data_zyg <- data1 %>%
  select(twzyg)

## ea father + ea mother
data_ea <- data1 %>%
  select(ea4fa_agg, ea4mo_agg)

## calculating the time lag between the last YNTR assessment and the first 
## QoL assessment of participants (relevant covariate in the analysis!)

## first: assigning QoL measure to the first QoL measure taken of participant
## Needs to be earliest assessment of QoL unless this early assessment
## took place before the YNTR 16, then first that came after YNTR 16

## approach: simply calculate the 
## QoL as being the earliest available ANTR measure, 
## then calculate time lag variable, if negative, change the QoL and calculate
## the time lag var again, also make extra variable that indicates which 
## ANTR measure was taken for QoL

## Applying calculate_QoL function to different waves


# Initialize columns with NA
data1 <- data1 %>%
  mutate(QoL_simple = NA_real_, QoL_indicator = NA_character_,
         time_lag = NA_real_)

## Assigning QoL_measure for the first time
data1 <- calculate_qol(data1)

## calculating time_lag for the first time, now there are negatives
data1 <- calculate_time_lag(data1)

sum(data1$time_lag <= 0, na.rm = TRUE)

## Now there are 156 negatives that need to be handled next  

sum(is.na(data1$time_lag))
# 748 participants with no time lag variable

table(data1$QoL_simple, useNA = "ifany")
table(data1$QoL_indicator, useNA = "ifany")


## Adjusting the QoL measure and indicator variable in case time_lag is negative
## or 0 (QoL assessment must not have happened before the last YNTR participation)
data1 <- data1 %>%
  mutate(QoL_simple = case_when(
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc10) ~ levenc10,
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc12) ~ levenc12,
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc14) ~ levenc14,
    QoL_indicator == "ANTR8" & time_lag <= 0 & is.na(levenc14) ~ QoL_simple,
    QoL_indicator == "ANTR10" & time_lag <= 0 & !is.na(levenc12) ~ levenc12,
    QoL_indicator == "ANTR10" & time_lag <= 0 & !is.na(levenc14) ~ levenc14,
    QoL_indicator == "ANTR10" & time_lag <= 0 & is.na(levenc14) ~ QoL_simple,
    QoL_indicator == "ANTR12" & time_lag <= 0 & !is.na(levenc14) ~ levenc14,
    QoL_indicator == "ANTR12" & time_lag <= 0 & is.na(levenc14) ~ QoL_simple,
    .default = QoL_simple
          ),
        QoL_indicator = case_when(
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc10) ~ "ANTR10",
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc12) ~ "ANTR12",
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc14) ~ "ANTR14",
    QoL_indicator == "ANTR8" & time_lag <= 0 & is.na(levenc14) ~ QoL_indicator,
    QoL_indicator == "ANTR10" & time_lag <= 0 & !is.na(levenc12) ~ "ANTR12",
    QoL_indicator == "ANTR10" & time_lag <= 0 & !is.na(levenc14) ~ "ANTR14",
    QoL_indicator == "ANTR10" & time_lag <= 0 & is.na(levenc14) ~ QoL_indicator,
    QoL_indicator == "ANTR12" & time_lag <= 0 & !is.na(levenc14) ~ "ANTR14",
    QoL_indicator == "ANTR12" & time_lag <= 0 & is.na(levenc14) ~ QoL_indicator,
    .default = QoL_indicator
        )
  )

## recalculating time_lag variable, now there should be no more negatives
data1 <- calculate_time_lag(data1)

sum(data1$time_lag <= 0, na.rm = TRUE)
## Now still 33 negatives Those are the cases where there is no other QoL 
## measure available. Those need to be filtered out in the filtering function!

sum(is.na(data1$time_lag))
# 748 participants with no time lag variable

table(data1$QoL_simple, useNA = "ifany")
table(data1$QoL_indicator, useNA = "ifany")


## One more covariate: Age at wellbeing assessment, will also be used for 
## filtering and is an important covariate for the wellbeing
data1 <- data1 %>%
  mutate(age_qol = case_when(
    QoL_indicator == "ANTR8" ~ age8,
    QoL_indicator == "ANTR10" ~ age10,
    QoL_indicator == "ANTR12" ~ age12,
    QoL_indicator == "ANTR14" ~ age14,
    .default = NA_real_
  ))

sum(is.na(data1$age_qol))
## one individual with no age at QoL assessment, should probably be 
## removed as well

table(data1$age_qol, useNA = "ifany")

data1 %>% filter(age_qol < 18) %>% nrow()
## 764 participants where QoL assessment was below 18 years
data1 %>% filter(time_lag <= 0) %>% nrow()
## 33 participants where time_lag < 0, those need to be removed as well


## saving covariates dataframe, later merge it with 
## main data (after all the preprocessing and LGM with left_join)

data_covariates <- bind_cols(data_FIS, 
                             data_sex,
                             data_zyg,
                             data_ea,
                             data1 %>%
                               select(QoL_simple, QoL_indicator,
                                      time_lag, age_qol))

covariates_names <- names(data_covariates)[names(data_covariates) 
                                            %notin% c("FISNumber",
                                                      "QoL_simple")]
## removing FISNumber and the outcome from the covariates names, 
## Which survey surved for outcome might actually be interesting 
## covariate


## saving covariate data and names of covariates to call later in the filtering
## script
save(data_covariates, file = here::here("scripts", "data_covariates.RData"))
save(covariates_names, file = here::here("scripts", "names_covariates.RData"))

## question still where in filtering script to apply filtering
## Decision for now: when filtering (before the 50% filtering, then remove the
## covariates again for MCD calculation?!), later join it again to the MCD filtered
## data with left_join (only keep the rows in data_covariates where FISNumber
## matches with rows in MCD-filtered data!)

## Issue: many NAs also in covariates, imputation not really possible
colMeans(is.na(data_covariates))

## now, only 6% missing in time_lag variable due to ascription of last 
## available YNTR age for difference calculation







