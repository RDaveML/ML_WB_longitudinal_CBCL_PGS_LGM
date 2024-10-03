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
permute <- TRUE
if(permute){
  data_full <- transform(data_full, FISNumber = sample(FISNumber))
}

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
data_full <- data_full %>%
  left_join(mean_long_df, by = "FISNumber")

##-----------------------------------------------------------------------------

## nth-order autocorrelation (all the way until maximum time-lag)

## Note: Because the measures are not equidistant, the calculation of the
## autocorrelation is a lot more complicated!

## weighted autocorrelation based on intervals between measures instead

## First: calculate the time between the assessments for every participant and
## every timespan between two participants
data_full <- data_full %>%
  mutate(ts_YNTR_3_5 = ageq5 - agem3, # ts stands for timespan
         ts_YNTR_3_7 = agem7 - agem3, ## only very few questions of YNTR age 5 
         ## remained
         ts_YNTR_5_7 = agem7 - ageq5,
         ts_YNTR_7_10 = agem10 - agem7,
         ts_YNTR_10_12 = agem12 - agem10,
         ts_YNTR_12_14 = ages14 - agem12,
         ts_YNTR_14_16 = ages16 - ages14)

ts_YNTR_all <- grep("ts_YNTR", colnames(data_full), value = TRUE)

## if question was asked at every age, ts from 3 to 7 not needed
ts_YNTR_full <- setdiff(ts_YNTR_all, "ts_YNTR_3_7")

## column names in case question does not appear in YNTR5
ts_no_YNTR5 <- setdiff(ts_YNTR_all, c("ts_YNTR_3_5", "ts_YNTR_5_7"))

## column names in case question does not appear in YNTR3
ts_no_YNTR3 <- setdiff(ts_YNTR_all, c("ts_YNTR_3_5", "ts_YNTR_3_7"))

## column names in case question does not appear in YNTR3 AND YNTR 5
ts_no_YNTR3_5 <- setdiff(ts_YNTR_all, c("ts_YNTR_3_5", "ts_YNTR_3_7",
                                         "ts_YNTR_5_7"))

## column names in case question does not appear in YNTR5 AND DHQ 14 AND 16
ts_no_YNTR5_14_16 <- setdiff(ts_YNTR_all, c("ts_YNTR_3_5", "ts_YNTR_5_7",
                                             "ts_YNTR_12_14", "ts_YNTR_14_16"))




## ideally: write own custom function here
autocor <- FALSE
#if(autocor){
n_autocor <- function(CBCL_question, df){
  CBCL_items <- CBCL_questions_list[CBCL_question] %>% unlist %>% unname
  cat("CBCL items: ", CBCL_items, "\n")
  length_series <- length(CBCL_items)
  cat("length series: ", length_series, "\n")
  
  ## preparing: which series do we have? 
  items_names <- CBCL_items_table %>%
    filter(question_number == !!CBCL_question)
  print(items_names)
  
  
  time_intervals <- unlist(case_when(
    is.na(items_names$Age5) &&
      is.na(items_names$Age14) &&
      is.na(items_names$Age16) ~ list(ts_no_YNTR5_14_16),
    is.na(items_names$Age3) && is.na(items_names$Age5) ~ list(ts_no_YNTR3_5),
    is.na(items_names$Age3) ~ list(ts_no_YNTR3),
    is.na(items_names$Age5) ~ list(ts_no_YNTR5),
    .default = list(ts_YNTR_full)
  ))
  
  cat("time intervals: ", time_intervals, "\n")
  
  #acf_values <- numeric(length = length_series - 1)
  
  df_acf <- t(sapply(1:nrow(df), function(x){ 
  row <- df[x, ]
  row_values <- row %>%
    select(all_of(CBCL_items)) %>%
    unlist() %>%
    unname()
  
  acf_values <- numeric(length = length_series - 1)
  
  for(lag in 1:(length_series - 1)){
    #print(lag)
    
    ## something still off with this function, check again how to 
    ## calculate weighted autocorrelation (values are off anyway, 
    ## can not exceed -1 or 1!)
    ## CONTINUE HERE
    weights <- row %>%
      select(any_of(time_intervals[lag:(length_series - 1)]))
    
    acf_values[lag] <- cor(row_values[1:(length_series - lag)],
                           row_values[(lag + 1):length_series],
                           use = "na.or.complete") *
      mean(unlist(unname(weights)), na.rm = TRUE)
    ## mean of the weight according to Rehfeld et al., 2011
  }
  return(t(acf_values))
  
  
  }))
  
  print(head(df_acf))
  
  ## still adjust the column names
  colnames(df_acf)[(ncol(df_acf) - (length_series - 1)):ncol(df_acf)] <-
    paste0(CBCL_question, "_autocor_lag_", c(1:(length_series - 1)))
  print(colnames(df_acf))
  return(df_acf)
}

#df_full <- cbind(df, df_acf)

## check how to join this dataset together with the other arising datasets 
## from the CBCL questions 


#}

## trying out function:
test_autocor <- n_autocor(CBCL_question = names(CBCL_questions_list)[1],
                          df = data_full)







## This function worked to extract all the autocorrelation coefficients
## from a data frame with 5 time points and no missings
## Adjust it for your case and see if it works


## calculate autocorrelations

# Function to calculate autocorrelations and return them as a vector
calc_ar_coeff <- function(series, order.max = 1) {
  tryCatch({
    # Skip series with zero variance
    if (var(series) == 0) {
      return(rep(NA, order.max))  # Return NAs for all lags up to order.max
    }
    
    # Calculate autoregressive coefficients using Yule-Walker method
    ar_model <- ar(as.numeric(series), aic = FALSE, order.max = order.max)
    # If fewer AR coefficients are estimated than requested, pad with NAs
    return(c(ar_model$ar, rep(NA, order.max - length(ar_model$ar))))
  }, error = function(e) {
    return(rep(NA, order.max))  # Return NAs in case of an error
  })
}

autor <- TRUE
if(autor){
results_list_AR <- vector("list", length(CBCL_questions_list))

# Loop over the CBCL_questions_list using lapply
#results_list_AR <- lapply(1:length(CBCL_questions_list), function(i) {
results_list_AR <- lapply(1:length(CBCL_questions_list), function(i) {  
  ## adjust this so it runs over all CBCL questions and remove print statements
  # Select the necessary columns
  #print(i)
  items <- CBCL_questions_list[[i]]
  question <- names(CBCL_questions_list)[i]
  #print(items)
  q_df <- data_full %>%
    select(FISNumber, all_of(items))
  
  ## number of autocorrelation coefficients to be calculated
  
  lags <- c(1:(length(items) - 1))
  
  colnames_lags <- paste0(question, "_ar_lag_", lags)

  
  print(colnames_lags)
    # Get the column data
    #print(head(q_df, 1))
  
  ## calculating ar_df
  ar_df <- q_df %>%
    select(-FISNumber) 
  
  ar_df <- as.data.frame(t(apply(ar_df, 1, calc_ar_coeff,
                                 order.max = max(lags))))
  
  q_df <- q_df %>% 
    select(FISNumber) %>%
    cbind(ar_df)
    #print("renaming done")
    #print(head(q_df))
    #  rename(!!ar_colname := ar_lagx)

  
  return(q_df)
})

full_ar_df <- Reduce(function(x, y) left_join(x, y, by = "FISNumber"),
                results_list_AR)

## Issue: The naming of columns is not correct, instead of the desired 
## _ar_lag columns, they all have the same name and are thus 
## continuously misnamed

}

data_full <- data_full %>%
  left_join(full_ar_df, by = "FISNumber")

## Autocorrelation works, however very high number of NAs, should probably be 
## done after imputation

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
data_full <- data_full %>%
  left_join(rmssd_df, by = "FISNumber")

## RMSSD calculation successfully rounded off

