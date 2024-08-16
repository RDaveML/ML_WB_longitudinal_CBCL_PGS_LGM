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


## Creating the split while still ensuring that participants from the same
## family stay together

## idea: create empty data frame, randomly choose participants from the
## same family and attach it to the training set, 
## stop once nrow(training) >= 0.8 * nrow(total), then assign all IDs that 
## are not yet in the training set to the test set, check if sample sizes 
## are correct, before: set seed! Ask Dirk if anything special regarding random
## seeds needs to be coded before hand! 

train_data <- data.frame()
## initializaing test data as full dataset which will continuously be shrunken
## down by transferring data into the traning set
## what remains will be the ultimate test set
testdata <- data_full

## setting seed (check again if there needs to be something special done before!)
set.seed(1608)

while(nrow(train_data) < 0.8 * nrow(data_full)){
  
  ## 1) sampling a random family id that is contained in the test set
  family_id <- sample(unique(testdata$FamilyNumber), 1)
  
  ## 2) create a family data set (filtering test data for only this family ID)
  family_data <- testdata %>%
    filter(FamilyNumber == family_id)
  
  ## 3) append those family data to the training set
  train_data <- rbind(train_data, family_data)
  
  ## 4) update test set (the family is removed)
  testdata <- testdata %>%
    filter(FamilyNumber != family_id)
}
rm(family_data)
rm(family_id)

nrow(train_data)
nrow(testdata)
## takes very long time to sample! 

## saving training and test data
save(train_data, file = here::here("data", "intermediate", "train_data.RData"))

save(testdata, file = here::here("data", "intermediate", "test_data.RData"))









