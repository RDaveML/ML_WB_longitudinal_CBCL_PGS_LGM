# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-09-17
#
# Script Name: 33_Sensitivity_analysis_MCD.R
#
# Script Description: Coding of the Minimum covariance determinant 
# (MCD), filtering multivariate outliers to create subset of full dataset and
# re-run analyses 
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
               "stringr", "readxl", "data.table", "robustbase", "admisc",
               "MASS", "rrcov", "caret", "rrcovHD")

source(here::here("scripts", "functions", "functions_preprocessing.R"))


## Note: Participants will only be removed from the training set, 
## test set will be the same

## different outliers for all 5 feature sets! 

###############################################################################

## For all datasets: Write function that
# - removes IQR = 0 columns
# - calculates (with robustbase) covmcd
# - calculates mahalanobis based on covmcd (50 and 75)
# - saves IDs with highest 5% mcd
# - saves a vector of IDs to be removed


## Model A
## loading in data objects for the machine learning models

## note: re-run model A data prep before this or check in SciStor if it was done
## if not, change script 09 according to script 23 (mdoel D)
#filepath_A <- "A:/ML_WB_longitudinal_CBCL_PGS_LGM/data/intermediate/prep_data_A"
filepaths_prep_data <- 
  paste0("A:/ML_WB_longitudinal_CBCL_PGS_LGM/data/intermediate/data_model_", 
         c("A", "B", "C", "D", "E"), "_train.rds")

# files_datasets <- c(
#  "data_model_A.rds",
  
#)

data_sets_train <- lapply(filepaths_prep_data, function(x){

  ## creates a nameless list with all the datafiles
  readRDS(x)
  
  # x_train_A <- readRDS(paste0(filepath_A, "/", list.files(filepath_A)[[1]]))[["x_train"]]
})

names(data_sets_train) <- paste0("dataset_", c("A", "B", "C", "D", "E"))

#data_model_A <- data_sets_train[[1]]
#outliers_A <- mcd_5(data = data_model_A,
#                    dataset_name = names(data_sets_train)[1])

#data_model_B <- data_sets_train[[2]]
#outliers_B <- mcd_5(data = data_model_B,
#                    dataset_name = names(data_sets_train)[2], seed = 2)


t1 <- Sys.time()
## looping with the names of the datasets A-E
## PCAs might still need a variance filter, implement this in function
## (with hints already received), 
## CONTINUE HERE!!!
ids_mcd_ABCDE <- mapply(
  mcd_5,
  data_sets_train,
  names(data_sets_train),
  seed = 1:length(data_sets_train), ## ensuring reproducibility of sample split
  SIMPLIFY = FALSE
)

names(ids_mcd_ABCDE) <- paste0("names_outliers_MCD_",
                               c("A", "B", "C", "D", "E"))

t2 <- Sys.time()

cat("duration removing mcd outliers (5%): ",
    difftime(t2, t1, unit = "mins"), " minutes")
## NOTE: MASS and dplyr both have a select function!
## indicate package before calling function (dplyr::select)

old <- FALSE

if(old){
  
  ## saving IDs
  data_mcd_ID_A <- x_train_A %>%
    dplyr::select(FISNumber)
  
  data_mcd <- x_train_A %>% ## selecting all predictive varibales
    dplyr::select(-FISNumber, -FamilyNumber, -QoL_simple)
  
  ## Since many of the variables have IQR = 0, they don't help in distinguishing
  ## outliers, thus identify outliers only based on the columns 
  
  ## saving order of columns (will be applied to final dataframe later so that it
  ## has the same order of columns)
  column_order <- names(data_mcd)
  
  
  ## deleting columns that have IQR = 0
  ncol(data_mcd)
  
  ## still adjust the names of the MCD intermediary datasets, potentially also
  ## write this into a custom function
  
  ## Those columns might also need to be removed in advance! IN this case, adjust 
  ## names afterwards
  
  data_mcd1 <- data_mcd[, sapply(data_mcd, function(col) IQR(col) > 0)]
  
  ncol(data_mcd1)
  ## only 223/359 columns still remaining for calculation of mcd
  
  ## Code was taken from Leys et al., 2018
  
  # Creating covariance matrix for MCD («data_mcd» is the matrix containing  
  # data with no indicator variable
  # output50 <- cov.mcd(data_mcd1, quantile.used = nrow(data_mcd1)* .5)
  ## If column has IQR 0! Not possible to calculate this
  ## Might happen that system is exactly singular (Not in this case)
  ## With high number of variables, calculating this takes long to run! 
  
  output50 <- covMcd(data_mcd1, alpha = 0.5)  # alpha controls the fraction used
  
  output75 <- covMcd(data_mcd1, alpha = 0.75)
  
  ## with two columns that do have IQR != 0, this worked
  
  
  # Distances from centroid for each matrix
  md <- mahalanobis(data_mcd1, colMeans(data_mcd1), cov(data_mcd1))
  mhmcd50 <- mahalanobis(data_mcd1, output50$center, output50$cov)
  mhmcd75 <- mahalanobis(data_mcd1, output75$center, output75$cov)
  
  eigen(output50$cov)$values   # should all be > 0
  eigen(output75$cov)$values
  
  ## eigenvalues of mhmcd50 are not all > 0, therefore use .75 as sample fraction
  
  # Detecting outliers for each method
  # The index of each detected outlier is recorded for each method for a 
  # alpha= .01
  # For more than two variables, df of cutoff variable (in bold)
  ## has to be adjusted
  
  ## IF filtering should be done on cutoff instead of fixed 5% of the sample
  filter_cutoff <- FALSE
  
  if(filter_cutoff){
    alpha <-.05 ## less conservative than Leys at al
    
    cutoff <- (qchisq(p = 1 - alpha, df = ncol(data_mcd1))) ## ADJUST THIS! 
    names_outliers_MH <- which(md > cutoff)
    names_outliers_MCD50 <- which(mhmcd50 > cutoff)
    names_outliers_MCD75 <- which(mhmcd75 > cutoff)
    ## here instead of threshold, just use top 10%?
    length(names_outliers_MCD50)
    length(names_outliers_MCD75)
    
    ## might be quite high when using the cutoff! 
    ## Indeed, if using the cutoff, 480 outliers (out of 4070) when applying .50
    ## threshold, even more (1011) when applying .75 threshold
  }  
    
    ## those vectors give the row numbers of IDs in the original dataframe, use 
    ## for filtering
  if(all(eigen(output50$cov)$values > 0)){
    names_outliers_MCD50_5 <- cbind(data.frame(mhmcd50), data_mcd_ID_A) %>% 
      # rowid_to_column() %>%
      arrange(desc(mhmcd50)) %>%
      head(0.05 * length(mhmcd50)) %>%
      dplyr::select(FISNumber) %>%
      pull()
    
    saveRDS(names_outliers_MCD50_5,
            file = here::here("data", "intermediate", "outlierIDs_MCD_A.rds"))
    
  }
  
  names_outliers_MCD75_5 <- cbind(data.frame(mhmcd75), data_mcd_ID_A) %>% 
    # rowid_to_column() %>%
    arrange(desc(mhmcd75)) %>%
    head(0.05 * length(mhmcd75)) %>%
    dplyr::select(FISNumber) %>%
    pull()
  
  saveRDS(names_outliers_MCD75_5,
          file = here::here("data", "intermediate", "outlierIDs_MCD_A.rds"))
  
  cat(length(names_outliers_MCD75_5),
      " participants removed from training set model A", "\n")
  ## Thus instead of cutoff only discard the top 5%, based on the mhmcd75!
  
}

# eoS




