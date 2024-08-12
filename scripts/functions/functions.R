# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-07-31
#
# Script Name: functions.R
#
# Script Description: This script contains custom functions written for 
# the project:
## Combining longitudinal change features of childhood psychopathology 
## with Polygenic scores in machine learning models of adult wellbeing
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


## negation operator
`%notin%` <- Negate(`%in%`)

## For in between checks of workspace occupation
workspace.size <- function() {
  ws <- sum(sapply(ls(envir=globalenv()), function(x)object.size(get(x))))
  class(ws) <- "object_size"
  ws
}


## Data filtering function that does not create intermediate objects and
## puts out at every step how many participants were dropped
filter_CBCL <- function(df, CBCL_YSR_items_vec, data_covariates){
  cat("initial sample size: ", nrow(df), "\n", "\n")
  
  ## first filtering step: remove all participants with no outcome
  ## (no QoL measure in any of the ANTR waves)
  cat("Removing participants with no QoL outcome", "\n", "\n")
  
  data1 <- df %>%
    filter(!is.na(levenc8) | !is.na(levenc10) |
             !is.na(levenc12) | !is.na(levenc14))
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(df), "\n",
      "Sample size after filtering: ", nrow(data1), "\n",
      "participants dropped: ", nrow(df) - nrow(data1), "\n", "\n")
  
  ## second filtering step: Filtering all participants who did not participate 
  ## in any YNTR survey of interest
  cat("Removing participants who did not participate in YNTR", "\n", "\n")
  
  data2 <- data1 %>%
    filter(!is.na(in_YS_3M) | !is.na(in_YS_5) | !is.na(in_YS_7M) |
             !is.na(in_YS_10M) | !is.na(in_YS_12M) | !is.na(in_YS_DHBQ14) | 
             !is.na(in_YS_DHBQ16))
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(data1), "\n",
      "Sample size after filtering: ", nrow(data2), "\n",
      "participants dropped: ", nrow(data1) - nrow(data2), "\n", "\n")
  
  ## third filtering step: removing participants with only NAs in CBCL variables
  cat("Removing participants with only NAs in CBCL variables", "\n", "\n")
  
  ## note: CBCL_YSR_items_vec was loaded in before in script, needs to be 
  ## assigned when calling function
  all_na_rows <- apply(data2 %>%
                         dplyr::select(all_of(CBCL_YSR_items_vec)),
                       1,
                       function(x) all(is.na(x)))
  data3 <- data2[!all_na_rows, ]
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(data2), "\n",
      "Sample size after filtering: ", nrow(data3), "\n",
      "participants dropped: ", nrow(data2) - nrow(data3), "\n", "\n")
  
  ## filtering out participants where QoL was assessed before ysr16
  cat("Removing participants where only QoL assessment happened before ysr16",
      "\n", "\n")
  ## joining with covariate data
  data4 <- data3 %>%
    left_join(data_covariates, by = c("FISNumber", "sex", "twzyg",
                                      "ea4fa_agg", "ea4mo_agg")) %>%
    filter(time_lag > 0 | is.na(time_lag)) ## only keeping participants where 
    ## time lag is positive or NA (no infor on time of filling out)
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(data3), "\n",
      "Sample size after filtering: ", nrow(data4), "\n",
      "participants dropped: ", nrow(data3) - nrow(data4), "\n", "\n")
  
  
  ## Optional: filtering out participants where QoL was assessed before age 18
  cat("Removing participants where QoL assessment happened before age18",
      "\n", "\n")
  
  data5 <- data4 %>%
    filter(age_qol >= 18 | is.na(age_qol))
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(data4), "\n",
      "Sample size after filtering: ", nrow(data5), "\n",
      "participants dropped: ", nrow(data4) - nrow(data5), "\n", "\n")

  
  ## removing participants with more than 50% missings in the CBCL items
  cat("Removing participants with more than 50% missings in CBCL items",
      "\n", "\n")
  
  threshold <- 0.5
  data_50_perc <- data5 %>%
    dplyr::select(all_of(CBCL_YSR_items_vec))
  rows_with_excessive_na <- apply(data_50_perc,
                                  1, function(x) mean(is.na(x)) > threshold)
 data6 <- data5[!rows_with_excessive_na, ]
 
 ## outputting updated sample size
 cat("Sample size before filtering: ", nrow(data5), "\n",
     "Sample size after filtering: ", nrow(data6), "\n",
     "participants dropped: ", nrow(data5) - nrow(data6), "\n", "\n")
  
  ## returning final df (which will then be given to perform KNN imputation)
 cat("Returning final data frame after filtering operations with sample size: ",
     nrow(data6))
 return(data6) 
}


