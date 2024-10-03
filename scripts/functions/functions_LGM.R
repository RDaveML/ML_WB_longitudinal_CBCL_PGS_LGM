# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-08-16
#
# Script Name: functions_LGM.R
#
# Script Description: This script contains custom 
# functions of the latent growth modeling part written for the project:
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


LGM_preprocess <- function(df, CBCL_question){
  
  
}


LGM_reshape <- function(df, CBCL_question) {
  

}
  

LGM_DV_calculation <- function(df, CBCL_question){
  
  
}


LGM_estimation_grid <- function(df, configuration_grid){
  
  
  
}


