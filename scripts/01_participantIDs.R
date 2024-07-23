## Participant ID file

## Project Combining longitudinal change features of childhood psychopathology 
## with Polygenic scores in machine learning models of adult wellbeing

## Author: Dave Leitritz

## start date: 2024-07-2

## end date: 


## installing / loading libraries
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr")

options(scipen = 999)

## reading in data
data <- read_sav(here::here("data", "source_raw", "PHE_20240722_4552_YJS.sav"))

length(unique(data$FISNumber)) == nrow(data)
## no duplicates, all IDs can be extracted by simply selecting the column

## extracting participant IDs (FIS-number)
participant_IDs <- data %>% select(FISNumber) %>%
## formatting IDs to characters because excel cannot handle too many digits
  mutate(FISNumber = as.character(FISNumber))

## writing IDs to file
write.table(participant_IDs, file = here::here("data", "intermediate",
                                               "participantIDs.txt"),
            sep = "\t", row.names = FALSE)