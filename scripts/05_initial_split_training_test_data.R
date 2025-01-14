# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-08-16
#
# Script Name: 06_split_training_test_data.R
#
# Script Description: In this script, the original split of the 
# data in training and test data is created
#
#
# Notes: Family members need to stay in the same sample
# Later, when collapsing the samples again in order to perform
# the bootstrap assessment of machine learning model stability. this 
# also needs to be implemented when looping over the B bootstrapped samples
#
#   PGS is still missing! 
#
#


# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table")


## loading in full cleaned dataset (CBLC + IDs + covariates, PGS still missing)
load(here::here("data", "intermediate", "data_full.RData"))

## initial step: saving df with FISNr and FamilyNumber (needed later when
## creating training folds
df_FIS_fam <- data_full %>%
  select(FISNumber, FamilyNumber)
save(df_FIS_fam, file = here::here("data", "intermediate", "FIS_fam_nr.RData"))


#------------------------------------------------------------------------------

## Important step before actual analysis: For trial calculations, permute 
## IDs so one remains blind for data, once creating ML models, permute 
## first, copy this code and execute it
permute <- FALSE
if(permute){
  data_full <- transform(data_full, FISNumber = sample(FISNumber))
}

#------------------------------------------------------------------------------

## Initializing the test data as the full dataset
## Train and test indices instead of subsetting dataframes repeatedly

## Note: This is the baseline train-test split that will be used for the GMM, 
## the feature selection and the baseline ML modeling against which the bootstrapped
## results will be compared

seed <- 2911
set.seed(seed)

family_ids <- unique(data_full$FamilyNumber)
family_sizes <- table(data_full$FamilyNumber)
total_rows <- nrow(data_full)

# Initialize empty indices for training data
train_indices <- integer(0)

# Keep a set of family IDs to sample from
remaining_family_ids <- family_ids

# While loop to accumulate training data until it's about 80% of the data
while (length(train_indices) < 0.8 * total_rows) {
  # Sample a random family ID from remaining families
  family_id <- sample(remaining_family_ids, 1)
  
  # Get indices for this family
  family_indices <- which(data_full$FamilyNumber == family_id)
  
  # Append these indices to the training indices
  train_indices <- c(train_indices, family_indices)
  
  # Remove the chosen family ID from the remaining families
  remaining_family_ids <- setdiff(remaining_family_ids, family_id)
}

# Use the indices to create train and test datasets
train_data <- data_full[train_indices, ]
test_data <- data_full[-train_indices, ]

train_ids <- train_data %>%
  select(FISNumber) %>%
  pull()

test_ids <- test_data %>%
  select(FISNumber) %>%
  pull()


## saving training and test data
save(train_data, file = here::here("data", "intermediate", "train_data.RData"))

save(test_data, file = here::here("data", "intermediate", "test_data.RData"))

## saving training and test ids 
save(train_ids, file = here::here("data", "intermediate", "indices_train.RData"))

save(test_ids, file = here::here("data", "intermediate", "indices_test.RData"))




## This splitting needs to happen 100 times for the bootstrapping assessment
## of model stability! 

## save all 100 splits in an object, use the first for the baseline ML run 
## and the other ones to see if model objects were stable

## Bootstrapping: 

B <- 100

boot_inds <- vector("list", length = B)


## Indices of different runs

for(b in 1:length(boot_inds)){
  seed <- b
  set.seed(b)
  family_ids <- unique(data_full$FamilyNumber)
  family_sizes <- table(data_full$FamilyNumber)
  total_rows <- nrow(data_full)

  # Initialize empty indices for training data
  train_indices <- integer(0)

  # Keep a set of family IDs to sample from
  remaining_family_ids <- family_ids

  # While loop to accumulate training data until it's about 80% of the data
  while (length(train_indices) < 0.8 * total_rows) {
    # Sample a random family ID from remaining families
    family_id <- sample(remaining_family_ids, 1)
  
    # Get indices for this family
    family_indices <- which(data_full$FamilyNumber == family_id)
  
    # Append these indices to the training indices
    train_indices <- c(train_indices, family_indices)
  
    # Remove the chosen family ID from the remaining families
    remaining_family_ids <- setdiff(remaining_family_ids, family_id)
  }

  # Use the indices to create train and test datasets
  train_ids <- data_full[train_indices, ] %>%
    select(FISNumber) %>%
    pull()
  
  test_ids <- data_full[-train_indices, ] %>%
    select(FISNumber) %>%
    pull()

  boot_inds[[b]] <- list(train_ids, test_ids, seed)
  
}

## saving training and test ids for all B bootstrapped samples
save(boot_inds, file = here::here("data", "intermediate", "indices_bootstrap.RData"))

## note: Do entire split again after outlier removal, throw out 
## bootstrapping part here, only do it in later script

## CONTINUE HERE!

## end of script






