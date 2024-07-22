## Initial data exploration 
## Project Combining longitudinal change features of childhood psychopathology 
## with Polygenic scores in machine learning models of adult wellbeing

## Author: Dave Leitritz

## start date: 2024-07-22

## end date: 


## installing / loading libraries
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr")

## changing format of printed number
options(scipen = 999)

## printing and changing working directory if needed
getwd()

## reading in datafile
data <- read_sav(here::here("data", "source_raw", "PHE_20240722_4552_YJS.sav"))

## object size, dimensions, volume of dataset
format(object.size(data), units = "Gb")
## 0.4 GB
dim(data)
## 92969 records, 526 variables

## saving for later comparisons
nrow_data <- nrow(data)
ncol_data <- ncol(data)

## Missings? 
colMeans(is.na(data))
rowMeans(is.na(data))
## Issue, very many missings, maybe threshold needs to be raised














