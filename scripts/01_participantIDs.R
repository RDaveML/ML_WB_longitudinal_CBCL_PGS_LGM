# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email: d.m.leitritz@vu.nl
#   
# Date: 2024-07-22
#
# 
#
# Script Name: 01_participantIDs.R
# 
# Script Description: 
#
# Preparation: Writing participant IDs to file to use later
# 
#
# Notes: Being part of a national prospective cohort study (NTR), 
# (a) our data cannot be made publicly available for privacy reasons but are available
# for legitimate researchers via their data access procedure
# ([https://tweelingenregister.vu.nl/information_for_researchers/working-with-ntr-data]
# (https://tweelingenregister.vu.nl/information_for_researchers/working-with-ntr-data))
# and (b) our sample will, due to the longitudinal data collection procedures,
# partly overlap with previous publications.
#

## installing / loading libraries
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr")

options(scipen = 999)

## creating directory for raw data, save raw dataset there manually
if(!dir.exists(here::here("data", "source_raw"))){
  dir.create(here::here("data", "source_raw"))
}


## reading in raw data (for replication efforts contact Netherlands twin
## register data managers)
data <- read_sav(here::here("data", "source_raw", "PHE_20240722_4552_YJS.sav"))

length(unique(data$FISNumber)) == nrow(data)
## no duplicates, all IDs can be extracted by simply selecting the column

## extracting participant IDs (FIS-number)
participant_IDs <- data %>% select(FISNumber) %>%
## formatting IDs to characters 
  mutate(FISNumber = as.character(FISNumber))

## writing IDs to file
if(!dir.exists(here::here("data", "intermediate"))){
  dir.create(here::here("data", "intermediate"))
}
write.table(participant_IDs, file = here::here("data", "intermediate",
                                               "participantIDs.txt"),
            sep = "\t", row.names = FALSE)