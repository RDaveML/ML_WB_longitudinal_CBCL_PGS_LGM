# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-05-13
#
# Script Name: 12_split_training_test_data.R
#
# Script Description: In this script, the split into training and test data
# is made for all models (sets B, C, E)
# that include PGS (different sample than used for model A!)
#
#
# Notes: Family members need to stay in the same sample
# Later, when collapsing the samples again in order to perform
# the bootstrap assessment of machine learning model stability. this 
# also needs to be implemented when looping over the B bootstrapped samples
#
#
#


# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table")


## loading in full PGS dataset (with covariates)
data_full_PGS <- readRDS(
  file = here::here("data", "intermediate", "PGS", "data_PCA_PGS.rds"))

## loading in family df 
df_FIS_fam <- readRDS(
  here::here("data", "intermediate", "PGS", "FIS_fam_nr.rds"))

## loading in QoL outcome data
data_outcome <- readRDS(
  here::here("data", "intermediate", "data_outcome.rds")) %>%
  mutate(QoL_simple = as.numeric(QoL_simple))

## joining dfs (the sample of 5087 is the sample with QoL outcome)
data_PGS_FIS <- df_FIS_fam %>%
  right_join(data_outcome, by = "FISNumber") %>%
  inner_join(data_full_PGS, by = "FISNumber")

## saving the dataframe with shrunken down sample
if(!dir.exists(here::here("data", "intermediate", "PGS"))){
  dir.create(here::here("data", "intermediate", "PGS"))
}

saveRDS(data_PGS_FIS, here::here("data", "intermediate", "PGS",
                                 "data_PGS_model_B.rds"))

data_PGS_fam <- data_PGS_FIS %>%
  select(FISNumber, FamilyNumber)

## saving initial split (used for the main analysis)
saveRDS(data_PGS_fam, here::here("data", "intermediate", "PGS",
                                 "data_fam_PGS.rds"))


## Updated sample size: Only 2656 participants have PGS AND QoL outcome


#------------------------------------------------------------------------------

## Initializing the test data as the full dataset
## Train and test indices instead of subsetting dataframes repeatedly

## Note: This is the baseline train-test split that will be used for the Latent
## growth modeling, 
## the feature selection and the baseline ML modeling against which the
## bootstrapped results will be compared

seed <- 2911
set.seed(seed)

family_ids <- unique(data_PGS_FIS$FamilyNumber)
family_sizes <- table(data_PGS_FIS$FamilyNumber)
total_rows <- nrow(data_PGS_FIS)

# Initialize empty indices for training data
train_indices <- integer(0)

# Keep a set of family IDs to sample from
remaining_family_ids <- family_ids

# While loop to accumulate training data until it's about 80% of the data
while (length(train_indices) < 0.8 * total_rows) {
  # Sample a random family ID from remaining families
  family_id <- sample(remaining_family_ids, 1)
  
  # Get indices for this family
  family_indices <- which(data_PGS_FIS$FamilyNumber == family_id)
  
  # Append these indices to the training indices
  train_indices <- c(train_indices, family_indices)
  
  # Remove the chosen family ID from the remaining families
  remaining_family_ids <- setdiff(remaining_family_ids, family_id)
}

# Use the indices to create train and test datasets
train_data <- data_PGS_FIS[train_indices, ]
test_data <- data_PGS_FIS[-train_indices, ]

train_ids <- train_data %>%
  select(FISNumber) %>%
  pull()

test_ids <- test_data %>%
  select(FISNumber) %>%
  pull()


## saving training and test ids 
saveRDS(train_ids,
        file = here::here("data", "intermediate", "indices_train_PGS.rds"))
saveRDS(test_ids,
        file = here::here("data", "intermediate", "indices_test_PGS.rds"))


###############################################################################

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
  family_ids <- unique(data_PGS_FIS$FamilyNumber)
  family_sizes <- table(data_PGS_FIS$FamilyNumber)
  total_rows <- nrow(data_PGS_FIS)
  
  # Initialize empty indices for training data
  train_indices <- integer(0)
  
  # Keep a set of family IDs to sample from
  remaining_family_ids <- family_ids
  
  # While loop to accumulate training data until it's about 80% of the data
  while (length(train_indices) < 0.8 * total_rows) {
    # Sample a random family ID from remaining families
    family_id <- sample(remaining_family_ids, 1)
    
    # Get indices for this family
    family_indices <- which(data_PGS_FIS$FamilyNumber == family_id)
    
    # Append these indices to the training indices
    train_indices <- c(train_indices, family_indices)
    
    # Remove the chosen family ID from the remaining families
    remaining_family_ids <- setdiff(remaining_family_ids, family_id)
  }
  
  # Use the indices to create train and test datasets
  train_ids <- data_PGS_FIS[train_indices, ] %>%
    select(FISNumber) %>%
    pull()
  
  test_ids <- data_PGS_FIS[-train_indices, ] %>%
    select(FISNumber) %>%
    pull()
  
  boot_inds[[b]] <- list(train_ids, test_ids, seed)
  
}

## saving training and test ids for all B bootstrapped samples
saveRDS(boot_inds,
        file = here::here("data", "intermediate", "indices_bootstrap_PGS.rds"))

## end of script






