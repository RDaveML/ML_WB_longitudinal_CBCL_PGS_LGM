# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-08-15
#
# Script Name: 05_longitudinal_features_no_LGM.R
#
# Script Description: In this script, the filtered CBCL variables 
# (see script 04_data_cleaning_filtering1.R) from the project:
# Combining longitudinal change features of childhood psychopathology 
# with Polygenic scores in machine learning models of adult wellbeing
# will be used to calculate longitudinal features without the use of 
# latent growth modeling, e.g. the n-th order auto-correlation or the 
# RMSSD
#
#
# Notes: Since no models are calculated here, this step takes place before
# the split of  the dataset in training and test data
#
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "forcats", "purrr")

## python version specified
# use_python("/usr/local/bin/python")

## loading in custom functions
source(here::here("scripts", "functions", "functions_longitudinal_features.R"))
       
       

## Loading in data and item vectors
## loading in full cleaned dataset (CBLC + IDs + covariates, PGS still missing)
load(here::here("data", "intermediate", "data_full.RData"))

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx")) %>%
  as.data.frame()

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
load(here::here("scripts", "variable_vectors.RData"))

## loading in list of CBCL items per question
load(here::here("scripts", "CBCL_questions_list.RData"))

## loading in CBCL items to be retained and to be dropped
load(here::here("scripts", "CBCL_items_keep"))
load(here::here("scripts", "CBCL_items_drop"))

## Cropping down the CBCL items per question (removing dropped items)
for(i in 1:length(CBCL_questions_list)){
  CBCL_questions_list[[i]] <- setdiff(CBCL_questions_list[[i]], CBCL_items_drop)
}

## reordering labels of factors
data_full <- data_full %>%
  mutate(across(all_of(CBCL_items_keep), 
                ~ as.numeric(.)))


#------------------------------------------------------------------------------

## Important step before actual analysis: For trial calculations, permute 
## IDs so one remains blind for data
## adjust this depending on phase 
permute <- FALSE
if(permute){
  data_full <- transform(data_full, FISNumber = sample(FISNumber))
}

save(data_full, file = here("data", "intermediate", "data_full_prep.Rdata"))

#------------------------------------------------------------------------------

## Calculating additional longitudinal features

## per question: only extract FISNumber and the items that belong to
## this specific question, calculate mean, append to df, finished df join with 
## allover data

## Mean of item over time & SD of item over time
## Old code, ineffective
old <- TRUE
if(old == FALSE){
mean_long_df <- data.frame()
for(i in 1:length(CBCL_questions_list)){
  q_df <- data_full %>%
    select(FISNumber, all_of(CBCL_questions_list[[i]]))
  q_df <- q_df %>%
    mutate(q_mean = rowMeans(select(q_df, !FISNumber), na.rm = TRUE)) %>%
    rowwise() %>%
    mutate(q_sd = sd(c_across(-FISNumber), na.rm = TRUE)) %>%
    ungroup()
  
  if(i == 1){
  mean_long_df <- q_df
  } else {
    mean_long_df <- left_join(mean_long_df, q_df, by = "FISNumber")
  }
}

}
# Initialize an empty list to store the results for each iteration
results_list <- vector("list", length(CBCL_questions_list))

# Loop over the CBCL_questions_list using lapply
results_list <- lapply(1:length(CBCL_questions_list), function(i) {
  
  # Select the necessary columns
  #print(i)
  q_df <- data_full %>%
    select(FISNumber, all_of(CBCL_questions_list[[i]]))
  
  # Calculate the row means and row standard deviations
  q_df <- q_df %>%
    mutate(q_mean = rowMeans(select(q_df, -FISNumber), na.rm = TRUE),
           q_sd = apply(select(q_df, -FISNumber), 1, sd, na.rm = TRUE)) %>%
    rename(!!paste0(names(CBCL_questions_list)[[i]], "_q_mean") := q_mean,
           !!paste0(names(CBCL_questions_list)[[i]], "_q_sd") := q_sd) %>%
    select(FISNumber, contains("mean"), contains("sd"))
  
  return(q_df)
})

# Combine all data frames in the list using reduce with left_join
mean_long_df <- Reduce(function(x, y) left_join(x, y, by = "FISNumber"),
                       results_list)


## results in additional 172 variables

## appending to full data
#data_full <- data_full %>%
#  left_join(mean_long_df, by = "FISNumber")

## saving longitudinal mean and SD
save(mean_long_df, file = here("data", "intermediate", "df_mean_SD_long.Rdata"))

##-----------------------------------------------------------------------------

## (autocorrelation and autoregression coefficients calculated separately
## in script 06_a_autocorrelation.R)

##----------------------------------------------------------------------------


## Root mean square of successive differences (RMSSD)
## over all available time points

# Initialize an empty list to store the results for each iteration
## (As above)
results_list_RMSSD <- vector("list", length(CBCL_questions_list))

## function to calculate RMSSD for each participant
cal_RMSSD <- function(measurements){
  succ_diff <- diff(measurements)
  rmssd <- sqrt(mean(succ_diff^2, na.rm = TRUE))
  return(rmssd)
}



# Loop over the CBCL_questions_list using lapply
results_list_RMSSD <- lapply(1:length(CBCL_questions_list), function(i) {
  # Select the necessary columns
  df <- data_full %>%
    select(FISNumber, all_of(CBCL_questions_list[[i]]))
  
  # pivot to long format for the RMSSD calculation
  df_long <- df %>%
    pivot_longer(cols = -FISNumber, names_to = "time", values_to = "value")
  
  ## calculate RMSSD (potentially leave away intermediate object)
  df_rmssd <- df_long %>%
    group_by(FISNumber) %>%
    summarize(RMSSD = cal_RMSSD(value)) %>%
    ## renaming RMSSD column with unique name (combination item name + RMSSD)
    rename(!!paste0(names(CBCL_questions_list)[[i]], "_RMSSD") := RMSSD)
  
  return(df_rmssd)
})

# Combine all data frames in the list using reduce with left_join
rmssd_df1 <- Reduce(function(x, y) left_join(x, y, by = "FISNumber"),
                   results_list_RMSSD)

## most NAs were eliminated, still some NaNs in there, not many though

## appending to full data
# data_full <- data_full %>%
#  left_join(rmssd_df1, by = "FISNumber")

## saving rmssd_df
save(rmssd_df1, file = here("data", "intermediate", "df_RMSSD.Rdata"))

## RMSSD calculation successfully rounded off


## loading autocorrelation features
## (calculated in separate file 05_a_autocorrelation.R)

# List objects in the environment before loading
before_objects <- ls()

# Load the RData file
load(here::here("data", "intermediate", "data_acf_imp_train.Rdata"))
load(here::here("data", "intermediate", "data_acf_imp_test.Rdata"))

# List objects in the environment after loading
after_objects <- ls()

# Show the new objects that were loaded
new_objects <- setdiff(after_objects, before_objects)
print(new_objects)

temp <- load(here::here("data", "intermediate", "data_acf_imp.Rdata"))
cat("autocorrelation df loaded in; name of object: ", "'", temp, "'")


## saving df
dim(data_full)
save(data_full, file = here("data", "intermediate", "df_full_nonLGM.Rdata"))

## end of script, merging together with autocorrelation and autoregression 
## features in separate script



