# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-11-08
#
# Script Name: 06b_autocorrelation.R
#
# Script Description: In this script, the filtered CBCL variables 
# (see script 04_data_cleaning_filtering1.R) from the project:
# Combining longitudinal change features of childhood psychopathology 
# with Polygenic scores in machine learning models of adult wellbeing
# are filled up to one measure per year using mean imputation and 
# knn imputation to calculate the autocorrelation features with 
# the acf function which assumes that there are equidistant measures
# At the end, those features will be saved into a separate feature 
# set. This imputation will not be used anywhere else and is only 
# applied to enable the calculation of the autocorrelation features
#
#
#
#

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
# load(here("data", "intermediate", "data_full_prep.Rdata"))
data_full <- readRDS(here::here("data", "intermediate", "data_full_prep.rds"))

CBCL_items_table <- read_excel(
  here::here("doc", "CBCL_table_t_per_item.xlsx")) %>%
    as.data.frame()

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
CBCL_YSR_items_vec <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[1]]

CBCL_items_vec <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[2]]

YSR_items_vec <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[3]]

ea_vars <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[4]]

qol_vars <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[5]]


## loading in list of CBCL items per question
CBCL_questions_list <- readRDS(
  here::here("data", "intermediate", "CBCL_questions_list.rds"))

## loading in CBCL items to be retained and to be dropped
CBCL_items_keep <- readRDS(
  here::here("data", "intermediate", "CBCL_items_keep.rds")
)
CBCL_items_drop <- readRDS(
  here::here("data", "intermediate", "CBCL_items_drop.rds")
)


## filtering out the CBCL_items that were dropped previously
CBCL_questions_list <- lapply(CBCL_questions_list,
                              function(x) setdiff(x, CBCL_items_drop))


##-----------------------------------------------------------------------------

## create artificial additional measures 
## so that for every CBCL question, it is as if there was 
## one measurement every year (from 3 til 16 or from 5 to 16 or from 7 to 16), 
## depending on question

# Initialize an empty list to store the results for each iteration
results_list <- vector("list", length(CBCL_questions_list))


# Loop over the CBCL_questions_list using lapply
results_list <- lapply(1:length(CBCL_questions_list), function(i) {
  
  # Select the necessary columns
  CBCL_question <- paste0(names(CBCL_questions_list)[[i]])
  
   q_df <- data_full %>%
    select(FISNumber, all_of(CBCL_questions_list[[i]]))
  
  data_id <- q_df %>%
    select(FISNumber)
  
  
  data_long <- q_df %>%
    pivot_longer(
      cols = -FISNumber, names_to = "variable", values_to = "measurement") %>%
    mutate(age = as.numeric(gsub(".*?(\\d+)$", "\\1", variable))) %>%
    select(FISNumber, age, measurement)
  
  
  # create variables to create scenario with 1 measurement every year
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
  all_na_cols <- names(
    colMeans(is.na(data_wide))[colMeans(is.na(data_wide)) == 1])
  median_cols <- setdiff(colnames(data_wide), c("FISNumber", all_na_cols))

  ## inserting median for all NA columns (KNN won't impute columns with only NA)
  continue = TRUE
  if(continue){      
  data_wide <- data_wide %>%
    rowwise() %>%
    mutate(
      median_to_imp = round(
        median(c_across(all_of(median_cols)), na.rm = TRUE))) %>%
    
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


## saving df in case knn crashes

saveRDS(pad_autocor_df,
        here::here("data", "intermediate", "pad_autocor_df.rds"))
t2 <- Sys.time()

cat("Time elapsed padding data: ", t2 - t1)

## took 35 minutes to pad up the data

##-----------------------------------------------------------------------------

## KNN imputation

## loading in pad_autocor_df
pad_autocor_df <- readRDS(
  here::here("data", "intermediate", "pad_autocor_df.rds"))


## loading in train and test IDs
indices_train <- readRDS(
  here::here("data", "intermediate", "indices_train.rds"))

indices_test <- readRDS(
  here::here("data", "intermediate", "indices_test.rds"))

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

## Note that the original scale (0, 1, 2) cannot be retained
## (KNN automatically scales)
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

## Next, the autocorrelation can be calculated


##-----------------------------------------------------------------------------

## Autocorrelation (Acf) feature calculation

## re-merge training and test data (row wise calculation of autocorrelation)

pad_autocor_imputed <- rbind(pad_autocor_imputed_train,
                             pad_autocor_imputed_test)

## Calculate Autocorrelation with lags as specified,
## per question, thus first filtering for CBCL question
## length of lags is dynamically dependent on how many "measures" there are


## separate FISNr to not include it in the calculation
## and later merge it back with
FISNr <- pad_autocor_imputed %>% select(FISNumber)



# Function to calculate autocorrelations for a single row
row_acf <- function(row_data, max_lag) {
  # Handle cases with no variation (all values identical)
  # note: in a time series with no variation, the acf is not 
  # actually defined mathematically, however, here
  # a value of 1 is assigned to signalize that it correlates
  # perfectly with itself
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
  
  ## selecting only for a specific CBCL question the variables
  ## creating dynamic regex to ensure that only questions that are specifically 
  ## associated with the specified CBCL question are extracted 
  ## (e.g. when the CBCL question
  ## is CBCL_1; do only extract CBCL_1 variables and not also CBCL_100)
  regex_CBCL <- paste0("^", CBCL_question, "(_|$)")
  
  acf_df <- pad_autocor_imputed %>%
    select(matches(regex_CBCL))
  
  ## next: calculating autocorrelation with all possible lags! 
  
  ## initiating what the maximum lag is depending on how many columns
  length_lag <- length(colnames(acf_df)) - 1 
  
  ## calculation: calculating acf values rowwise based on time series
  acf_df <- acf_df %>%
    rowwise() %>%
    mutate(
      acf = list(row_acf(row_data = c_across(everything()),
                         max_lag = length_lag))  # Compute ACF for the row
    )  %>%
    ## unnest the list so that all elements of the list become columns
    unnest_wider(acf, names_sep = "_")
  
  ## assigning the lag colnames (lag0 - lag.max)
  colnames(acf_df)[(ncol(acf_df) / 2 + 1):ncol(acf_df)] <- 
    paste0(CBCL_question, "_acf_lag_", c(0:length_lag))
  
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
  
##-----------------------------------------------------------------------------

## calculation of autoregression (ar) coefficients

# Function to calculate autoregression values for a single row
row_ar <- function(row_data, max_lag) {
  # Handle cases with no variation (all values identical)
  # note: in a time series with no variation, the ar is not 
  # actually defined mathematically, here, value of 1 assigned
  # to signalize max autoregression
  if (var(as.numeric(row_data)) == 0) {
    return(rep(1, max_lag))  # Return 1 for all lags
  } else {
    
    # Compute autoregression coefficients up til max lag depending 
    # on length of the series
    ar_values <- ar(as.numeric(row_data), aic = FALSE, order.max = max_lag)$ar
    
    # Return as a numeric vector
    return(as.numeric(ar_values))
  }
}

## loop over all CBCL questions in df

#results_list_ar <- lapply(1:2, function(i) {
results_list_ar <- lapply(1:length(CBCL_questions_list), function(i) {
  
  
  CBCL_question <- paste0(names(CBCL_questions_list)[[i]])

  ## selecting only for a specific CBCL question the variables
  
  ## creating dynamic regex to ensure that only questions that are specifically 
  ## associated with the specified CBCL question are extracted (e.g. when the CBCL question
  ## is CBCL_1; do only extract CBCL_1 variables and not also CBCL_100)
  regex_CBCL <- paste0("^", CBCL_question, "(_|$)")

  ar_df <- pad_autocor_imputed %>%
    select(matches(regex_CBCL))
  
  
  ## check if selection was correct

  ## next: calculating autocorrelation with all possible lags! 
  
  ## initiating what the maximum lag is depending on how many columns
  length_lag <- length(colnames(ar_df)) - 1 
  #print(length_lag)
  
  
  ## calculation: calculating ar values rowwise based on time series
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
  
  ## re-binding with FISNr, selecting only ar value columns
  ar_df <- cbind(FISNr, ar_df) %>% 
    select(FISNumber, contains("ar"))
  
  
  return(ar_df)
  
}) # eoF

## merging together df with all acf values for all CBCL questions
full_ar_df <- Reduce(function(x, y) left_join(x, y, by = "FISNumber"),
                      results_list_ar)

##-----------------------------------------------------------------------------

## Merging with acf df 

print(ncol(full_acf_df))


full_acf_df <- full_acf_df %>%
  left_join(full_ar_df, by = "FISNumber")

## saving full acf_df, will be merged with other longitudinal variables
# in script 06c_merge_nonLGM.R
saveRDS(full_acf_df, here::here("data", "intermediate", "data_acf_imp.rds"))


print(ncol(full_acf_df))

t6 <- Sys.time()

cat("elapsed time KNN and acf and ar: ", t6-t3)

## splitting up into training and test data again
full_acf_df_train <- full_acf_df %>% 
  filter(FISNumber %in% indices_train)

full_acf_df_test <- full_acf_df %>% 
  filter(FISNumber %in% indices_test)


## 6) save feature set with (only) all autocorrelation features
saveRDS(full_acf_df_train,
        here::here("data", "intermediate", "data_acf_imp_train.rds"))

saveRDS(full_acf_df_test,
        here::here("data", "intermediate", "data_acf_imp_test.rds"))


cat("Script finished running", "\n")

## eoS