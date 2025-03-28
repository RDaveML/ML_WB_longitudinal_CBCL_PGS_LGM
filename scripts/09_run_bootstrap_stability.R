# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-03-12
#
# Script Name: 09_run_bootstrap_stability.R
#
# Script Description: This script is supposed to run the original and the B = 100
# bootstrapped versions of the ML script to inspect model stability
# It will be given to the SNELLIUS cluster, parallelizing over 101 SNELLIUS nodes
# Goal is to save each output of model predictions and performance 
# in separate file in subdirectory and then to combine them in the script where
# the stability check takes place
#
#
# Notes:
#
#

# --------------------------------------------------------------
# ---------- Get Iteration Number ------------------------------
# --------------------------------------------------------------

# !/usr/bin/env Rscript
iter <- commandArgs(trailingOnly=TRUE) ## use this as index for the datasets!
iter <- 1
## this can be tested and returned on ntr1 server run exiting the script 

if(iter > 1){
  b_iter <- iter - 1
} else {
  b_iter <- iter
}
print(b_iter)
b_iter <- as.numeric(b_iter)
print(b_iter)
cat("Iteration / Index for Bootstrapped dataset: ", b_iter)

test <- TRUE
if(test){
  stop("Iteration print works / does not work")
}


## keeping track of time
t1 <- Sys.time()



# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")

## here add packages that need to be installed manually / dependencies, etc.

## For foreach export
packages_used <- c("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
                   "stringr", "readxl", "data.table")

## loading libraries
## Need to be separately installed a priori in cluster R 
pacman::p_load(packages_used)

## Sourcing custom functions
## (files need to be located in same directory, also on cluster)

## Loading in test / train indices that are correct for the current B
## loading in training ids and test ids (split created)
## NOTE: CHECK AND MAKE SURE THAT THE SPLIT SCRIPT MAKES IN TOTAL
## 101 SPLITS, THAT WILL BE SAVED AS A LIST, HERE THEN INDEX ONLY
## PIECE OF LIST THAT BELONGS TO CURRENT ITERATION, LIST NEEDS TO BE SAVED 
## WITH saveRDS function

## Find out if you can also load in only a slice of a list, otherwise load in
## entire list and slice the object here in R

## readRDS solves the problem!
if(b_iter == 1){
  train_ids <- readRDS(here::here("data", "intermediate", "indices_train.rds"))
} else {
  train_ids <- readRDS(
    here::here("data", "intermediate", "indices_bootstrap.rds"))[[b_iter]][[1]]
}


if(b_iter == 1){
  test_ids <- readRDS(here::here("data", "intermediate", "indices_test.rds"))
} else {
  test_ids <- readRDS(
    here::here("data", "intermediate", "indices_bootstrap.rds"))[[b_iter]][[2]]
}

## Iterating (looping over conditions)
## number of cores that can be requested per node 
## on genoa needs to be divisible by 16

## here: insert code loading in, type conversion, KNN, enet, rf, svr, xgb
## always with predictions

#cluster <- 64
#cl <- makeCluster(cluster, outfile="")
#registerDoParallel(cl)

timer_total <- proc.time()[3]


# print total time of nodes
print(paste0("Full Timing Iteration ", iter, ":"))
proc.time()[3] - timer_total

stopCluster(cl)

# ----------------------------------------------------------------------
# ----- Export ---------------------------------------------------------
# ----------------------------------------------------------------------

## gathering all objects into list
workspace_objects <- mget(ls())

# Save the list to an RDS file
filename <- paste0("workspace_model_A_iteration_", iter, ".rds")
saveRDS(workspace_objects, file = paste0(here::here("data", "intermediate", filename)))

# Save (with saveRDS)
saveRDS(output_test_iter, file = here::here(paste0("mHMM_TestSim_Iter", iter,
                                                   ".RDS")))


## Time tracking in output
t2 <- Sys.time()

## printing output to console 
print(t2 - t1)





