# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-09-09
#
# Script Name: 0x_grid_conditions_LGM
#
# Script Description: This script contains the conditions 
# that should be checked when comparing different latent growth curve models
# in the script 07_LGM.R
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



classes <- c(2:5)

curve <- c("linear", "quadratic")

starting_values <- c()

## after talk with Conor, check which conditions also need to be altered









