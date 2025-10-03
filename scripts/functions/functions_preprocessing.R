# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-07-31
#
# Script Name: functions_preprocessing.R
#
# Script Description: This script contains custom preprocessing 
# functions written for the project:
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
               "stringr", "readxl", "data.table", "purrr")


## negation operator
`%notin%` <- Negate(`%in%`)

## For in between checks of workspace occupation
workspace.size <- function() {
  ws <- sum(sapply(ls(envir=globalenv()), function(x)object.size(get(x))))
  class(ws) <- "object_size"
  ws
}



## Function to assign QoL variable and calculating time lag the correct way
## QoL needs to be assessed after last YNTR participation and participants 
## need to be at least 18 years old at age of QoL, will then be filtered
calculate_qol <- function(data) {
  data %>%
    mutate(
      ## QoL measure should be the earliest available measure
      QoL_simple = case_when(
        !is.na(levenc8) ~ levenc8,
        !is.na(levenc10) ~ levenc10,
        !is.na(levenc12) ~ levenc12,
        !is.na(levenc14) ~ levenc14,
        ## default will a priori be assigned to NA
        .default = QoL_simple
      ),
      ## creating indicator which QoL measure was taken
      QoL_indicator = case_when(
        !is.na(levenc8) ~ "ANTR8",
        !is.na(levenc10) ~ "ANTR10",
        !is.na(levenc12) ~ "ANTR12",
        !is.na(levenc14) ~ "ANTR14",
        .default = QoL_indicator
      )
    )
}

## function to calculate time_lag between QoL assessment and latest available
## YNTR assessment - will be used to update QoL in case it happened before YNTR
calculate_time_lag <- function(data) {
  data %>%
    mutate(
      time_lag = case_when(
        QoL_indicator == "ANTR8" ~ ifelse(!is.na(ages16), age8 - ages16,
                                          ifelse(!is.na(ages14), age8 - ages14,
                                                 ifelse(!is.na(agem12), age8 - agem12,
                                                        ifelse(!is.na(agem10), age8 - agem10,
                                                               NA_real_)))),
        QoL_indicator == "ANTR10" ~ ifelse(!is.na(ages16), age10 - ages16,
                                           ifelse(!is.na(ages14), age10 - ages14,
                                                  ifelse(!is.na(agem12), age10 - agem12,
                                                         ifelse(!is.na(agem10), age10 - agem10,
                                                                NA_real_)))),
        QoL_indicator == "ANTR12" ~ ifelse(!is.na(ages16), age12 - ages16,
                                           ifelse(!is.na(ages14), age12 - ages14,
                                                  ifelse(!is.na(agem12), age12 - agem12,
                                                         ifelse(!is.na(agem10), age12 - agem10,
                                                                NA_real_)))),
        QoL_indicator == "ANTR14" ~ ifelse(!is.na(ages16), age14 - ages16,
                                           ifelse(!is.na(ages14), age14 - ages14,
                                                  ifelse(!is.na(agem12), age14 - agem12,
                                                         ifelse(!is.na(agem10), age14 - agem10,
                                                                NA_real_)))),
        TRUE ~ NA_real_
      )
    )
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
  
  ## filtering out participants where QoL was assessed before last YNTR participation
  cat("Removing participants where only QoL assessment happened before last YNTR participation",
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
  ## or where no information about age at 
  cat("Removing participants where QoL assessment happened before age18 or no info",
      "\n", "\n")
  
  data5 <- data4 %>%
    filter(age_qol >= 18 & !is.na(age_qol))
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(data4), "\n",
      "Sample size after filtering: ", nrow(data5), "\n",
      "participants dropped: ", nrow(data4) - nrow(data5), "\n", "\n")
  
  
  
  ## removing participants with only NAs in YSR items
  cat("Removing participants with only NAs in YSR items",
      "\n", "\n")
  
  
  data_YSR <- data5 %>%
    dplyr::select(any_of(YSR_items_vec))
  rows_with_allna_YSR <- apply(data_YSR,
                               1, function(x) sum(!is.na(x)) == 0)
  
  data6 <- data5[!rows_with_allna_YSR, ]
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(data5), "\n",
      "Sample size after filtering: ", nrow(data6), "\n",
      "participants dropped: ", nrow(data5) - nrow(data6), "\n", "\n")
  
  
  

  
  ## removing participants with more than 50% missings in the CBCL items
  cat("Removing participants with more than 50% missings in CBCL items",
      "\n", "\n")
  
  threshold <- 0.5
  data_50_perc <- data6 %>%
    dplyr::select(all_of(CBCL_YSR_items_vec))
  rows_with_excessive_na <- apply(data_50_perc,
                                  1, function(x) mean(is.na(x)) > threshold)
 data7 <- data6[!rows_with_excessive_na, ]
 
 ## outputting updated sample size
 cat("Sample size before filtering: ", nrow(data6), "\n",
     "Sample size after filtering: ", nrow(data7), "\n",
     "participants dropped: ", nrow(data6) - nrow(data7), "\n", "\n")
  
 
 cat("Returning final data frame after filtering operations with sample size: ",
     nrow(data7))
 
 ## returning final df (which will then be given to perform KNN imputation)
 return(data7) 
}


## Alternative filtering function: participant with age at QoL assessment
## < 18 are NOT dropped
filter_CBCL_2 <- function(df, CBCL_YSR_items_vec, data_covariates){
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
  
  ## filtering out participants where QoL was assessed before last YNTR participation
  cat("Removing participants where only QoL assessment happened before last YNTR participation",
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
  ## or where no information about age at 
  cat("Removing participants with no info on age QoL assessment",
      "\n", "\n")
  
  data5 <- data4 %>%
    filter(!is.na(age_qol))
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(data4), "\n",
      "Sample size after filtering: ", nrow(data5), "\n",
      "participants dropped: ", nrow(data4) - nrow(data5), "\n", "\n")
  
  
  ## removing participants with only NAs in YSR items
  cat("Removing participants with only NAs in YSR items",
      "\n", "\n")
  
  
  data_YSR <- data5 %>%
    dplyr::select(any_of(YSR_items_vec))
  rows_with_allna_YSR <- apply(data_YSR,
                               1, function(x) sum(!is.na(x)) == 0)
  
  data6 <- data5[!rows_with_allna_YSR, ]
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(data5), "\n",
      "Sample size after filtering: ", nrow(data6), "\n",
      "participants dropped: ", nrow(data5) - nrow(data6), "\n", "\n")
  
  
  ## removing participants with more than 50% missings in the CBCL items
  cat("Removing participants with more than 50% missings in CBCL items",
      "\n", "\n")
  
  threshold <- 0.5
  data_50_perc <- data6 %>%
    dplyr::select(all_of(CBCL_YSR_items_vec))
  rows_with_excessive_na <- apply(data_50_perc,
                                  1, function(x) mean(is.na(x)) > threshold)
  data7 <- data6[!rows_with_excessive_na, ]
  
  ## outputting updated sample size
  cat("Sample size before filtering: ", nrow(data6), "\n",
      "Sample size after filtering: ", nrow(data7), "\n",
      "participants dropped: ", nrow(data6) - nrow(data7), "\n", "\n")
  
  
  cat("Returning final data frame after filtering operations with sample size: ",
      nrow(data7))

  ## returning final df (which will then be given to perform KNN imputation)
  return(data7) 
}

###############################################################################

## function that saves IDs of 5% top mcd outliers from the training set
## for sensitivity analysis, these can then later be removed

## default alpha = .75, performs better than .5 if less than N x 1/4 outliers
## (Leys et al., 2018)

mcd_5 <- function(data, dataset_name, alpha_mcd = 0.75, filter_cutoff = FALSE,
                  threshold = 0.05, seed = NULL){
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  cat("random seed for this dataset: ", seed, "\n")
  
  cat("name of the dataset: ", dataset_name, "\n")
  filename_outliers <- paste0("outlierIDs_MCD_", dataset_name, ".rds")
  
  ## saving IDs
  data_mcd_ID <- data %>%
    dplyr::select(FISNumber)
  
  ## selecting only predictor variables
  data_mcd <- data %>%
    dplyr::select(-FISNumber, -FamilyNumber, -QoL_simple)
  
  cat("Number of columns before removing IQR = 0 cols: ", ncol(data_mcd), "\n")
  
  ## deleting columns with IQR = 0
  data_mcd <- data_mcd[, sapply(data_mcd, function(col) IQR(col, na.rm = TRUE) > 0),
                       drop = FALSE]
  
  cat("Number of columns remaining: ", ncol(data_mcd), "\n")
  
  ## removing highly correlated variables to avoid eigenvalue issues
  cor_mat <- cor(data_mcd, use = "pairwise.complete.obs")
  high_corr <- caret::findCorrelation(cor_mat, cutoff = 0.95)
  if (length(high_corr) > 0) {
    data_mcd <- data_mcd[, -high_corr, drop = FALSE]
    cat("Dropped", length(high_corr), "highly correlated variables\n")
  }
  
  # cat("Number of columns remaining: ", ncol(data_mcd), "\n")
  cat("Number of columns remaining for MCD: ", ncol(data_mcd), "\n")
  
  ## (This is only possible with non NaN data)
  ## removing linear combination variables
  #combos <- findLinearCombos(as.matrix(data_mcd))$remove
  
  #if(!is.null(combos)){
      
  #  data_mcd <- data_mcd[-combos]
  #}
  
  
  # Creating covariance matrix for MCD («data_mcd» is the matrix containing  
  # data with no indicator variable
  

  # alpha controls the fraction used
  
  ## ensure that all eigenvalues are positive, 
  ## if not, first calculate PCA on the data and calculate mcd on the 
  ## PCA data
  
  ## also if number of mcd columns is too high
  # if(!all(eigen(output_mcd$cov)$values > 0) | ncol(data_mcd) > 250){
  # if(!all(eigen(output_mcd$cov)$values > 0)){
  if(ncol(data_mcd) > 250){
    
    PCA_approach <- TRUE
    
    ## mean imputing before calculating PCA (This is only to determine the 
    ## outliers, the actual data will not bet touched)
    for(j in seq_len(ncol(data_mcd))){
      data_mcd[is.na(data_mcd[, j]), j] <- mean(data_mcd[, j], na.rm = TRUE)
    }
    
    # 1) scale the data (important for PCA when variables have different units)
    X <- scale(data_mcd, center = TRUE, scale = TRUE)
    
    # 2) PCA
    pca <- prcomp(X, center = FALSE, scale = FALSE)
    
    # 3) choose number of PCs to keep
    #    a) keep PCs with non-negligible variance:
    eps <- 1e-8
    k_nonzero <- sum(pca$sdev > eps)
    
    #    b) or use cumulative variance threshold (80% to not have excessive 
    # high number of PCs)
    cumvar <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
    k_80 <- which(cumvar >= 0.80)[1]   # first index reaching >=980%
    
    # pick k = min(k_nonzero, k_80, nrow(X)-1)
    # setting hard cap at 250 PCAs
    k <- min(k_nonzero, ifelse(is.na(k_80), k_nonzero, k_80), 250, nrow(X)-1)
    
    if(k == 250){
      cat("Cap of 250 PCs applied", "\n")
    }
    
    pcs <- pca$x[, 1:k, drop = FALSE]
    
    cat("Number of PCAs to calculate MCD on: ", ncol(pcs), "\n")
    
    # 4) run MCD on the reduced data
    mcd <- covMcd(pcs, alpha = alpha_mcd)  # robust center and covariance in PC space
    
    # 5) compute mahalanobis distances in PC space
    # If mcd$cov is fine (invertible), this works:
    mhmcd <- mahalanobis(pcs, mcd$center, mcd$cov)
    
  } else {
    # Distances from centroid for matrix
    PCA_approach <- FALSE
    #output_mcd <- rrcov::CovMcd(data_mcd, alpha = alpha_mcd)
    output_mcd <- robustbase::covMcd(data_mcd, alpha = alpha_mcd)
    mhmcd <- mahalanobis(data_mcd, output_mcd$center, output_mcd$cov)
  }
  
  cat("PCA carried out: ", PCA_approach, "\n")
  
  ## optional: Instead of fixed quota, flag all IDs that fall below
  ## significance threshold
  
  if(filter_cutoff){
    cutoff <- (qchisq(p = 1 - threshold, df = ncol(data_mcd))) ## ADJUST THIS! 
    names_outliers_MCD <- which(mhmcd > cutoff)
    
    saveRDS(names_outliers_MCD,
            file = here::here("data", "intermediate", filename_outliers))
  } else {
    
    names_outliers_MCD <- cbind(data.frame(mhmcd), data_mcd_ID) %>% 
      # rowid_to_column() %>%
      arrange(desc(mhmcd)) %>%
      head(0.05 * length(mhmcd)) %>%
      dplyr::select(FISNumber) %>%
      pull()
    
    saveRDS(names_outliers_MCD,
            file = here::here("data", "intermediate", filename_outliers))
    
    cat(length(names_outliers_MCD),
        " participants removed from training set ", dataset_name, "\n")
    ## Thus instead of cutoff only discard the top 5%, based on the mhmcd75!
  }
  
  return(names_outliers_MCD)
  
}


