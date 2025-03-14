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
print(iter)
iter <- as.numeric(iter)
print(iter)
cat("Iteration / Index for Bootstrapped dataset: ", iter)

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
train_ids <- readRDS(here::here("data", "intermediate", "indices_train.rds"))[[iter]]

test_ids <- readRDS(here::here("data", "intermediate", "indices_test.rds"))[[iter]]



## Iterating (looping over conditions)
## number of cores that can be requested per node 
## on genoa needs to be divisible by 16, rendering 7 cores idle
## (57 design configurations in total)

cluster <- 64
cl <- makeCluster(cluster, outfile="")
registerDoParallel(cl)

timer_total <- proc.time()[3]

set.seed(iter)

output_test_iter <- foreach(conf = 1:nrow(configs_1234),
                            .export = c("configs_1234", "list_dkl_configs"),
                            .packages = packages_used,
                            .verbose = FALSE,
                            ## It occurred during testruns that mHMM function
                            ## failed due to 
                            ## system being singular or negative probability,
                            ## errorhandling = pass lets parallel loop continue
                            ## to run
                            ## in some instances, there will be no result then
                            ## still needs to be addressed in analysis 
                            ## scripts
                            .errorhandling = "pass") %dopar% {
                              
                              sim_recover_mHMM(n_obs = configs_1234[conf, "N"], t = 200,
                                               Dkl = configs_1234[conf, "Dkl"],
                                               emiss_distr_config = list_dkl_configs[[conf]],
                                               p = configs_1234[conf, "p"],
                                               S = configs_1234[conf, "C"],
                                               gamma_self = configs_1234[conf, "gamma_self"],
                                               var_gamma = configs_1234[conf, "opt_var_gamma"],
                                               J = 2000, ## fewer number of iterations
                                               burn_in = 1000)
                              
                            }


# print total time of nodes
print(paste0("Full Timing Iteration ", iter, ":"))
proc.time()[3] - timer_total

stopCluster(cl)

# ----------------------------------------------------------------------
# ----- Export ---------------------------------------------------------
# ----------------------------------------------------------------------

# Save
saveRDS(output_test_iter, file = here::here(paste0("mHMM_TestSim_Iter", iter,
                                                   ".RDS")))


## Time tracking in output
t2 <- Sys.time()

## printing output to console 
print(t2 - t1)





