# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-08-15
#
# Script Name: 05_longitudinal_features_no_LGM.R
#
# Script Description: In this script, the filtered CBCL variables 
# (see script 04_data_cleaning_filtering1.R) from the project:
# Combining longitudinal change features of childhood psychopathology 
# with Polygenic scores in machine learning models of adult wellbeing
# will be used to calculate longitudinal features without the use of 
# latent growth modeling, e.g. the n-th order auto-correlation or the 
# RMSSD
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