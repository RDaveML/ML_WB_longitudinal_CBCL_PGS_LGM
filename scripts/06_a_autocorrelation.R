# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-11-08
#
# Script Name: 05_a_autocorrelation.R
#
# Script Description: In this script, the filtered CBCL variables 
# (see script 04_data_cleaning_filtering1.R) from the project:
# Combining longitudinal change features of childhood psychopathology 
# with Polygenic scores in machine learning models of adult wellbeing
# are filled up to one measure per year using mean imputation and 
# knn imputation to calculate the autocorrelation features with 
# the acf function which assumes that there are equidistant measures
# At teh end, those features will be saved into a separate feature 
# set. This imputation will not be used anywhere else and is only 
# applied to enable the calculation of the autocorrelation features
#
#
# Notes: Since no models are calculated here, this step takes place before
# the split of  the dataset in training and test data
#
#

## restart calculation CONTINUE HERE

## tracking time info
t1 <- Sys.time()

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "forcats", "purrr", "VIM",
               "parallel", "doParallel", "caret")



##-----------------------------------------------------------------------------


## Imputation knn, only for autocorrelation: 

## steps:

## 1) load in data 
load(here("data", "intermediate", "data_full_prep.Rdata"))

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


## filtering out the CBCL_items that were dropped previously
CBCL_questions_list <- lapply(CBCL_questions_list,
                              function(x) setdiff(x, CBCL_items_drop))


##-----------------------------------------------------------------------------



## 2) filter so that only CBCL variables are still part of it

## 3) pad up so that for every CBCL question, it is as if there was 
## one measurement every year (from 3 til 16 or from 5 to 16 or from 7 to 16), 
## depending on question

# Initialize an empty list to store the results for each iteration
results_list <- vector("list", length(CBCL_questions_list))

#test_test <- data_full[1:10,] ## still adjust this to get full data



# Loop over the CBCL_questions_list using lapply
results_list <- lapply(1:length(CBCL_questions_list), function(i) {
# results_list <- lapply(1:2, function(i) {
  
  # Select the necessary columns
  #print(i)
  CBCL_question <- paste0(names(CBCL_questions_list)[[i]])
  #print(CBCL_question)
  
   q_df <- data_full %>%
#  q_df <- test_test %>% ## still adjust this for full data
    select(FISNumber, all_of(CBCL_questions_list[[i]]))
  
  data_id <- q_df %>%
    select(FISNumber)
  
  
  #print(colnames(q_df))
  
  #q_cols <- colnames(q_df)[-1]
  #print(q_cols)
  
  #ages_asked <- as.numeric(str_extract(q_cols, "\\d+$"))
  #print(ages_asked)
  #full_ages <- seq(min(ages_asked), max(ages_asked))
  
  # Find the missing elements
  #missing_ages <- setdiff(full_ages, ages_asked)
  #print(missing_ages)
  #var_missing <- paste0("imp_var_age", missing_ages)
  #print(var_missing)
  
  data_long <- q_df %>%
    pivot_longer(cols = -FISNumber, names_to = "variable", values_to = "measurement") %>%
    mutate(age = as.numeric(gsub(".*?(\\d+)$", "\\1", variable))) %>%
    select(FISNumber, age, measurement)
  
  
  # create variables to fill up measurements on 1 every year
  data_comp <- data_long %>%
    group_by(FISNumber) %>%
    complete(age = full_seq(age, 1))
  
  
  ## pivoting back to wide data
  data_wide <- data_comp %>%
    pivot_wider(names_from = age,
                values_from = measurement,
                names_prefix = paste0(CBCL_question, "_age_")) %>%
    ungroup()
  
  
  ## identifying the artificial columns (all NA values)
  all_na_cols <- names(colMeans(is.na(data_wide))[colMeans(is.na(data_wide)) == 1])
  median_cols <- setdiff(colnames(data_wide), c("FISNumber", all_na_cols))
  #print(median_cols)
  
  ## inserting median for all NA columns (KNN won't impute columns with only NA)
  continue = TRUE
  if(continue){      
  data_wide <- data_wide %>%
    rowwise() %>%
    mutate(
      median_to_imp = round(median(c_across(all_of(median_cols)), na.rm = TRUE))) %>%
    
    ## note: Rounding down so values stay in scale!
    mutate(across(all_of(all_na_cols), ~ median_to_imp)) %>%
    ungroup() %>%
    select(-median_to_imp)
  }
  return(data_wide)
  
})
  

# Combine all data frames in the list using reduce with left_join
pad_autocor_df <- Reduce(function(x, y) left_join(x, y, by = "FISNumber"),
                         results_list)


## result is dataframe that treats all CBCL questions as if there was
## 1 measurement each year


## saving workspace in case knn crashes
save.image(here("data", "intermediate", "workspace_05a_KNN_acf.Rdata"))

save(pad_autocor_df, file = here::here("data", "intermediate", "pad_autocor_df.RData"))
t2 <- Sys.time()

cat("Time elapsed padding data: ", t2 - t1)

## took 35 minutes to pad up the data

## This worked! Now, knn imputation

##-----------------------------------------------------------------------------

## loading in pad_autocor_df
load(here("data", "intermediate", "pad_autocor_df.RData"))

## loading in training and test set for KNN imputation first on training 
## set, then on test set
load(here("data", "intermediate", "train_data.RData"))

load(here("data", "intermediate", "test_data.RData"))

## saving unique FISNumbers
indices_train <- unique(train_data$FISNumber)

indices_test <- unique(test_data$FISNumber)

## removing train and test data
rm("train_data")
rm("test_data")

colnames(pad_autocor_df)

## splitting padded df into train and test data
pad_autocor_df_train <- pad_autocor_df %>%
  filter(FISNumber %in% indices_train)

pad_autocor_df_test <- pad_autocor_df %>%
  filter(FISNumber %in% indices_test)


t3 <- Sys.time()

## before imputation: Take out FISNR! it should not be part of the KNN procedure
df_FISNr_train <- pad_autocor_df_train %>%
  select(FISNumber)

df_FISNr_test <- pad_autocor_df_test %>%
  select(FISNumber)

pad_autocor_df_train <- pad_autocor_df_train %>%
  select(-FISNumber) %>%
  as.data.frame()

pad_autocor_df_test <- pad_autocor_df_test %>%
  select(-FISNumber) %>%
  as.data.frame()

## KNN imputation: 
cat("Beginning KNN imputation training data")
k_pad <- round(sqrt(ncol(pad_autocor_df_train)))
train_pre_obj <- preProcess(pad_autocor_df_train,
                            method = "knnImpute",
                            k = k_pad)

pad_autocor_df_train2 <- predict(train_pre_obj, pad_autocor_df_train)

sum(colMeans(is.na(pad_autocor_df_train2)) != 0)

t4 <- Sys.time()

cat("KNN preprocessing for training data finished after ", t4 - t3, "\n", 
    "with k = ", k_pad)

## Issue is that the original scale (0, 1, 2) cannot be retained
## thus, autocorrelation features are calculated on the imputed values where 
## also the scale is dodged


pad_autocor_df_test2 <- predict(train_pre_obj, pad_autocor_df_test)




sum(colMeans(is.na(pad_autocor_df_test2)) != 0)
## imputation worked, no more missings

t5 <- Sys.time()

cat("KNN preprocessing for test data finished after ", t5 - t3, "\n", 
    "with k = ", k_pad)

## re-merge with FISNr
pad_autocor_imputed_train <- cbind(df_FISNr_train, pad_autocor_df_train2)
pad_autocor_imputed_test <- cbind(df_FISNr_test, pad_autocor_df_test2)

## Note: This imputation code can perfectly be used later when 
## imputing the actual dataset

## Next, the autocorrelation can be calculated


##-----------------------------------------------------------------------------

## Acf feature calculation

## re-merge training and test data (row wise calculation of autocorrelation)

pad_autocor_imputed <- rbind(pad_autocor_imputed_train, pad_autocor_imputed_test)

## Calculate Autocorrelation with lags as specified,
## per question, thus first filtering for CBCL question
## length of lags is dynamically dependent on how many "measures" there are


## separate FISNr to not include it in the calculation and later merge it back with
FISNr <- pad_autocor_imputed %>% select(FISNumber)



# Function to calculate autocorrelations for a single row
row_acf <- function(row_data, max_lag) {
  # Handle cases with no variation (all values identical)
  # note: in a time series with no variation, the acf is not 
  # actually defined mathematically, however, I assign it here
  # a value of one to signalize that it correlates perfectly with itself
  if (var(row_data) == 0) {
    return(rep(1, max_lag + 1))  # Return 1 for all lags
  } else {
  
  # Compute autocorrelation using acf function
  acf_values <- acf(row_data, plot = FALSE, lag.max = max_lag)$acf
  
  # Return as a numeric vector
  return(as.numeric(acf_values))
  }
}


## loop over all CBCL questions in df

#results_list_acf <- lapply(1:2, function(i) {
results_list_acf <- lapply(1:length(CBCL_questions_list), function(i) {
  
  
  CBCL_question <- paste0(names(CBCL_questions_list)[[i]])
  #print(CBCL_question)
  
  ## selecting only for a specific CBCL question the variables
  
  ## creating dynamic regex to ensure that only questions that are specifically 
  ## associated with the specified CBCL question are extracted (e.g. when the CBCL question
  ## is CBCL_1; do only extract CBCL_1 variables and not also CBCL_100)
  regex_CBCL <- paste0("^", CBCL_question, "(_|$)")
  #print(regex_CBCL)
  acf_df <- pad_autocor_imputed %>%
    select(matches(regex_CBCL))
  
  
  ## check if selection was correct
  #print(colnames(acf_df))
  
  ## next: calculating autocorrelation with all possible lags! 
  
  ## initiating what the maximum lag is depending on how many columns
  length_lag <- length(colnames(acf_df)) - 1 
  #print(length_lag)
  
  
  ## calculation: calculating acf values rowwise based on time series
  acf_df <- acf_df %>%
    rowwise() %>%
    mutate(
      acf = list(row_acf(row_data = c_across(everything()), max_lag = length_lag))  # Compute ACF for the row
    )  %>%
    ## unnest the list so that all elements of the list become columns
    unnest_wider(acf, names_sep = "_")
  
  ## assigning the lag colnames (lag0 - lag.max)
  colnames(acf_df)[(ncol(acf_df) / 2 + 1):ncol(acf_df)] <- 
    paste0(CBCL_question, "_acf_lag_", c(0:length_lag))
  #print(colnames(acf_df))
  
  ## re-binding with FISNr, selecting only acf value columns, dropping
  ## acf lag0 (always 1 by definition)
  acf_df <- cbind(FISNr, acf_df) %>% 
    select(FISNumber, contains("acf")) %>%
    select(!contains("lag_0"))
  
  
  return(acf_df)

}) # eoF

## merging together df with all acf values for all CBCL questions
full_acf_df <- Reduce(function(x, y) left_join(x, y, by = "FISNumber"),
                      results_list_acf)
  
## This worked

##-----------------------------------------------------------------------------

## calculation of autoregression coefficients

# Function to calculate autoregression values for a single row
row_ar <- function(row_data, max_lag) {
  # Handle cases with no variation (all values identical)
  # note: in a time series with no variation, the acf is not 
  # actually defined mathematically, however, I assign it here
  # a value of one to signalize that it correlates perfectly with itself
  if (var(as.numeric(row_data)) == 0) {
    return(rep(1, max_lag))  # Return 1 for all lags
  } else {
    
    # Compute autocorrelation using acf function
    ar_values <- ar(as.numeric(row_data), aic = FALSE, order.max = max_lag)$ar
    
    # Return as a numeric vector
    return(as.numeric(ar_values))
  }
}

## loop over all CBCL questions in df

#results_list_ar <- lapply(1:2, function(i) {
results_list_ar <- lapply(1:length(CBCL_questions_list), function(i) {
  
  
  CBCL_question <- paste0(names(CBCL_questions_list)[[i]])
  #print(CBCL_question)
  
  ## selecting only for a specific CBCL question the variables
  
  ## creating dynamic regex to ensure that only questions that are specifically 
  ## associated with the specified CBCL question are extracted (e.g. when the CBCL question
  ## is CBCL_1; do only extract CBCL_1 variables and not also CBCL_100)
  regex_CBCL <- paste0("^", CBCL_question, "(_|$)")
  #print(regex_CBCL)
  ar_df <- pad_autocor_imputed %>%
    select(matches(regex_CBCL))
  
  
  ## check if selection was correct
  #print(colnames(ar_df))
  
  ## next: calculating autocorrelation with all possible lags! 
  
  ## initiating what the maximum lag is depending on how many columns
  length_lag <- length(colnames(ar_df)) - 1 
  #print(length_lag)
  
  
  ## calculation: calculating acf values rowwise based on time series
  ar_df <- ar_df %>%
    rowwise() %>%
    mutate(
      ar = list(row_ar(row_data = c_across(everything()), max_lag = length_lag))
      # Compute AR coeff for the row
    )  %>%
    ## unnest the list so that all elements of the list become columns
    unnest_wider(ar, names_sep = "_")
  
  ## assigning the lag colnames (lag0 - lag.max)
  colnames(ar_df)[(ncol(ar_df) -length_lag + 1):ncol(ar_df)] <- 
    paste0(CBCL_question, "_ar_order_coef_", c(1:length_lag))
  #print(colnames(ar_df))
  
  ## re-binding with FISNr, selecting only acf value columns, dropping
  ## acf lag0 (always 1 by definition)
  ar_df <- cbind(FISNr, ar_df) %>% 
    select(FISNumber, contains("ar"))# %>%
    #select(!contains("lag_0"))
  
  
  return(ar_df)
  
}) # eoF

## merging together df with all acf values for all CBCL questions
full_ar_df <- Reduce(function(x, y) left_join(x, y, by = "FISNumber"),
                      results_list_ar)

## This worked



##-----------------------------------------------------------------------------

## Merging with acf df 

print(ncol(full_acf_df))


full_acf_df <- full_acf_df %>%
  left_join(full_ar_df, by = "FISNumber")

## saving full acf_df
save(full_acf_df, file = here("data", "intermediate", "data_acf_imp.Rdata"))


print(ncol(full_acf_df))

t6 <- Sys.time()

cat("elapsed time KNN and acf and ar: ", t6-t3)

## splitting up into training and test data again
full_acf_df_train <- full_acf_df %>% 
  filter(FISNumber %in% indices_train)

full_acf_df_test <- full_acf_df %>% 
  filter(FISNumber %in% indices_test)


## 6) save feature set with (only) all autocorrelation features
save(full_acf_df_train, file = here("data", "intermediate", "data_acf_imp_train.Rdata"))
save(full_acf_df_test, file = here("data", "intermediate", "data_acf_imp_test.Rdata"))


## 7) merge autocorrelation set back with full Non-Lgm feature data
## (in script 06_longitudinal_features_no_LGM or later)


cat("Script finished running", "\n")

## 

## splitting up into training and test data again

## for security: saving workspace
# save.image(here("data", "intermediate", "workspace_06a_KNN_acf.Rdata"))

## end of script

## To do still: change annotations and print stuff in script, 

##-----------------------------------------------------------------------------