# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-09-09
#
#
# Script Name: functions_longitudinal_features.R
#
# Script Description: This script contains custom 
# functions coding the non-LGM longitudinal features (mean, SD, autocorrelation,
# autoregression, RMSSD) for the project
# Combining longitudinal change features of childhood psychopathology 
# with Polygenic scores in machine learning models of adult wellbeing
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



## calculating the weighted autocorrelation values of all possible lags
## of a CBCL question time series for each participant 
## Since the timepoints are not equidistant, weighted autocorrelation instead
## of standard correlation needs to be calculated
calculate_weighted_autocorrelation <- function(time_series, timestamps, max_lag, sigma) {
  
  # Initialize autocorrelation vector
  autocorrelations <- numeric(length = max_lag)
  
  # Loop over possible lags
  for (lag in 1:max_lag) {
    
    # Initialize weighted sums
    num <- 0
    denom <- 0
    
    # Loop over pairs of points
    for (i in 1:(length(time_series) - lag)) {
      for (j in (i + lag):length(time_series)) {
        
        # Check for missing data in the time series
        if (is.na(time_series[i]) || is.na(time_series[j])) {
          next  # Skip this pair if either point is missing
        }
        
        # Time difference between the two points
        delta_t <- timestamps[j] - timestamps[i]
        
        # Check for missing or invalid timestamps
        if (is.na(delta_t)) {
          next  # Skip if the time difference is missing or invalid
        }
        
        # Apply Gaussian kernel to weigh the pairs based on time difference
        weight <- exp(- (delta_t^2) / (2 * sigma^2))
        
        # Accumulate weighted sums if weight is valid
        if (!is.na(weight)) {
          num <- num + weight * (time_series[i] - mean(time_series, na.rm = TRUE)) * (time_series[j] - mean(time_series, na.rm = TRUE))
          denom <- denom + weight
        }
      }
    }
    
    # Compute autocorrelation at this lag if there are valid weights
    if (denom != 0) {
      autocorrelations[lag] <- num / denom
    } else {
      autocorrelations[lag] <- NA  # Return NA if no valid pairs at this lag
    }
  }
  
  return(autocorrelations)
  
  ## still adjust this part! 
  colnames(df_acf)[(ncol(df_acf) - (length_series - 1)):ncol(df_acf)] <-
    paste0(CBCL_question, "_autocor_lag_", c(1:(length_series - 1)))
}
  


## Autoregression: calculating all possible autoregression coefficients 
## if there are sufficient data points and variability 
## return NA otherwise

ar_cust <- function(x, lag){
  x_use <- x[!is.na(x)]
  if(length(x_use) < (lag + 1) || length(x) < lag){
    ar_result <- NA
  } else if(sd(x_use) == 0){
    ar_result = NA
  } else {
    ar_result <- ar(x_use, aic = FALSE, order.max = lag, na.action = na.pass)$ar
  }
  return(ar_result)
}


## Root mean square of successive differences (RMSSD)
## over all available time points

## function to calculate RMSSD for each participant
cal_RMSSD <- function(measurements){
  succ_diff <- diff(measurements)
  rmssd <- sqrt(mean(succ_diff^2, na.rm = TRUE))
  return(rmssd)
}




