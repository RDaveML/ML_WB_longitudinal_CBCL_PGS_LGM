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


## time lag: This is a bit more complicated (age at first ANTR survey - age at
## last YNTR (ysr16))


## first: assigning QoL measure to the first QoL measure taken of participant
## Tricky: Needs to be earliest assessment of QoL unless this early assessment
## took place before the YNTR 16, then first that came after YNTR 16

## New approach: Instead of 1000 cases, simply calculate the 
## QoL as being the earliest available ANTR measure, 
## then calculate time lag variable, if negative, change the QoL and calculate
## the time lag var again, also make extra variable that indicates which 
## ANTR measure was taken for QoL




## Next: Indicate which measure was taken (copy the same case when but assign
## character string, with the help of this, the difference can be calculated)
data1 <- data1 %>%
  mutate(QoL_simple = case_when(
    !is.na(levenc8) ~ levenc8,
    !is.na(levenc10) ~ levenc10,
    !is.na(levenc12) ~ levenc12,
    !is.na(levenc14) ~ levenc14,
    .default = NA
))

## indicator variable which ANTR wave was used for QoL assessment
data1 <- data1 %>%
  mutate(QoL_indicator = case_when(
    !is.na(levenc8) ~ "ANTR8",
    !is.na(levenc10) ~ "ANTR10",
    !is.na(levenc12) ~ "ANTR12",
    !is.na(levenc14) ~ "ANTR14",
    .default = NA
  ))


## calculating time lag variable
## taking fill-in year instead of age has no effect on missings, 
## thus only taking age as it has better resolution
data1 <- data1 %>%
  mutate(time_lag = case_when(
    QoL_indicator == "ANTR8" ~ age8 - ages16,
    QoL_indicator == "ANTR10" ~ age10 - ages16,
    QoL_indicator == "ANTR12" ~ age12 - ages16,
    QoL_indicator == "ANTR14" ~ age14 - ages16,
    .default = NA
  ))

sum(data1$time_lag <= 0, na.rm = TRUE)

## Now there are negatives that need to be handled next  


## Changing the QoL measure if the earliest QoL measure was filled out before
## ysr 16
data1 <- data1 %>%
  mutate(QoL_simple = case_when(
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc10) ~ levenc10,
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc12) ~ levenc12,
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc14) ~ levenc14,
    QoL_indicator == "ANTR10" & time_lag <= 0 & !is.na(levenc12) ~ levenc12,
    QoL_indicator == "ANTR10" & time_lag <= 0 & !is.na(levenc14) ~ levenc14,
    QoL_indicator == "ANTR12" & time_lag <= 0 & !is.na(levenc14) ~ levenc14,
    .default = QoL_simple
  ))

## re-assign QoL indicator variable
data1 <- data1 %>%
  mutate(QoL_indicator = case_when(
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc10) ~ "ANTR10",
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc12) ~ "ANTR12",
    QoL_indicator == "ANTR8" & time_lag <= 0 & !is.na(levenc14) ~ "ANTR14",
    QoL_indicator == "ANTR10" & time_lag <= 0 & !is.na(levenc12) ~ "ANTR12",
    QoL_indicator == "ANTR10" & time_lag <= 0 & !is.na(levenc14) ~ "ANTR14",
    QoL_indicator == "ANTR12" & time_lag <= 0 & !is.na(levenc14) ~ "ANTR14",
    .default = QoL_indicator
  ))

## recalculating time_lag
data1 <- data1 %>%
  mutate(time_lag = case_when(
    QoL_indicator == "ANTR8" ~ age8 - ages16,
    QoL_indicator == "ANTR10" ~ age10 - ages16,
    QoL_indicator == "ANTR12" ~ age12 - ages16,
    QoL_indicator == "ANTR14" ~ age14 - ages16,
    .default = time_lag
  ))

## checking negatives again
sum(data1$time_lag <= 0, na.rm = TRUE)
data1 %>%
  filter(QoL_indicator == "ANTR8" & time_lag <= 0) %>%
  select(starts_with("levenc"), QoL_simple, QoL_indicator,
         time_lag, age8, ages16)
## Those are the negatives are the cases where only one QoL assessment is 
## available and this one happened before or at the same time 
## of the ysr assessment
## Those need to be discarded as the study is interested in the longitudinal
## effect of childhood psychopathology

## One more covariate: Age at wellbeing assessment, might also be used for 
## filtering and is an important covariate for the wellbeing
data1 <- data1 %>%
  mutate(age_qol = case_when(
    QoL_indicator == "ANTR8" ~ age8,
    QoL_indicator == "ANTR10" ~ age10,
    QoL_indicator == "ANTR12" ~ age12,
    QoL_indicator == "ANTR14" ~ age14,
    .default = NA
  ))

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
## time lag has more than 50% missings! 












