# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email: d.m.leitritz@vu.nl
#   
# Date: 2024-07-22
#
# end date: 
#
# Script Name: 02_data_exploration.R
# 
# Script Description:
#
## Initial data exploration 
## Project Combining longitudinal change features of childhood psychopathology 
## with Polygenic scores in machine learning models of adult wellbeing
#
#
# Notes:
#
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "") ## This is a helper when excuting the 
## script! Console also prints options
options(scipen = 999)


## installing / loading libraries
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table")

## printing and changing working directory if needed
getwd()

here::here()

getwd() == here::here()

## reading in datafile
data <- read_sav(here::here("data", "source_raw", "PHE_20240722_4552_YJS.sav")) %>%
  as.data.frame()

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

# Next step: checking the codes of the variables





##------------------------------------------------------------------------------
# Preparation steps
##

## creating vectors of variable names for more easy looping later

## which variables are what?

## CBCL / YSR variables
CBCL_YSR_items <- read_excel("C:/Users/qrq337/OneDrive - Vrije Universiteit Amsterdam/Documents/VU/PhD_Machine_Learning_x_Well-being/01_PROJECTS/Project01_longitudinal_machine_learning/Table_S1_Pre-reg_table_CBCL_features_GMM.xlsx",
                              col_names = FALSE)[-1,]

## setting column names
colnames(CBCL_YSR_items) <- CBCL_YSR_items[1,]

# removing row that still contains column names
CBCL_YSR_items <- CBCL_YSR_items[-1,]


## starting the filtering
CBCL_YSR_items <- CBCL_YSR_items %>%
  filter(Inclusion == "yes") %>%
  select(contains("age"))

CBCL_YSR_items_vec <- vector()

for(i in 1:nrow(CBCL_YSR_items)){
  ## making one vector out of all cells that are not NA
  ## by this, all item names are saved in one vector
  CBCL_YSR_items_vec <- c(CBCL_YSR_items_vec, 
                          CBCL_YSR_items[i,][!is.na(CBCL_YSR_items[i,])])
}

## Double check: Old versions of CBCL questions as documented in NTR doc)
## are they contained in the excel table?

old_items <- c("q51om3", "q79om3", # YS3
               "q2om7", "q4om7", "q5om7", "q28om7", "q78om7", "q99om7", # YS7
               "q2om10", "q4om10", "q5om10", "q28om10", "q108om10", "q99om10", # YS10)
               "q2om12", "q4om12", "q5om12", "q28om12", "q128om12", "q99om12", # YS12)
               "q99oysr14", "q99oysr16") # YSR 14 & YSR 16


old_items_keep <- vector()
for(i in 1:length(old_items)){
  print(old_items[i])
  print(old_items[i] %in% CBCL_YSR_items_vec)
  cat("\n")
  if(old_items[i] %in% CBCL_YSR_items_vec){
    old_items_keep <- c(old_items_keep, old_items[i])
  }
}

sort(old_items_keep)

## Only questions 5 and 99 still in the excel sheet, 99 might 
## be need to kicked out because it's entirely different content between the 
## old and the new version
## q5om needs to be kicked out because YSR question does not exist in showcase
## Questions q113ysr14; q113ysr16; q116ysr14 and q116ysr14 do not exist in 
## showcase! remove those

no_data_items <- c(old_items_keep, "q113ysr14", "q113ysr16",
                   "q116ysr14", "q116ysr16")

CBCL_YSR_items_vec <- setdiff(CBCL_YSR_items_vec, no_data_items)

## still needs to be separated into CBCL and YSR items and given to vector

## df
CBCL_items_df <- CBCL_YSR_items %>% select(Age3:Age12)

YSR_items_df <- CBCL_YSR_items %>% select(Age14:Age16)

## vectors (appending all item names from questions asked in the CBCL)
## age3 - age12
CBCL_items_vec <- c(CBCL_items_df %>% select(1) %>% pull(),
                    CBCL_items_df %>% select(2) %>% pull(),
                    CBCL_items_df %>% select(3) %>% pull(),
                    CBCL_items_df %>% select(4) %>% pull(),
                    CBCL_items_df %>% select(5) %>% pull())

## removing NAs 
CBCL_items_vec <- na.omit(CBCL_items_vec)

YSR_items_vec <- c(YSR_items_df %>% select(1) %>% pull(),
                   YSR_items_df %>% select(2) %>% pull())

## removing NAs
YSR_items_vec <- na.omit(YSR_items_vec)

## variables of educational attainment: mother, father, family
## all on 4-point scale, higher N, also reliability info, number of reports,
## for overall: age at time of report

ea_vars <- sort(c("ea4_agg", "ea4_age_agg", "ea4_info_agg", "ea4_n_agg", # overall
             "ea4mo_agg", "ea4mo_info_agg", "ea4mo_n_agg", # mother
             "ea4fa_agg", "ea4fa_info_agg", "ea4fa_n_agg")) # father


## Outcome! QoL / wellbeing variables:
## Cantril ladder at ANTR waves 8, 10, 12, 14

qol_vars <- c("levenc8", "levenc10", "levenc12", "levenc14")

inspect <- FALSE

## visually inspecting unique values of variables for abnormalities
if(inspect == TRUE){
for(col in 3:131){
  cat("Variable", "'", colnames(data)[col], "'", "unique values: ",
      sort(unique(data[, col])), "\n", "\n")
}

for(col in 132:262){
  cat("Variable", "'", colnames(data)[col], "'", "unique values: ",
      sort(unique(data[, col])), "\n", "\n")
}


for(col in 263:393){
  cat("Variable", "'", colnames(data)[col], "'", "unique values: ",
      sort(unique(data[, col])), "\n", "\n")
}


for(col in 394:ncol(data)){
  cat("Variable", "'", colnames(data)[col], "'", "unique values: ",
      sort(unique(data[, col])), "\n", "\n")
}
}
  
## tabulations: variables 3-14 inform about attributes of participants related
## to genome, family, sex , sibling status
for(col in 3:14){
  cat("frequencies variable", "'", colnames(data)[col], "'", "\n",
      paste(names(table(data[,col], useNA = "ifany")),
            table(data[,col], useNA = "ifany"),
            sep = ": ", collapse = "\n"),
      "\n\n")
}

## Inspecting codes

## Variables spybs14/16 (truant) are not coded 0-2, check codes and
## distributions extracting together with CBCL items same question
data_truant <- data %>%
  select(q101m7, q101m10, q101m12, spybs14, spybs16)

table(data_truant$q101m7, useNA = "ifany")
table(data_truant$q101m10, useNA = "ifany")
table(data_truant$q101m12, useNA = "ifany")
table(data_truant$spybs14, useNA = "ifany")
table(data_truant$spybs16, useNA = "ifany")
## Some recoding should happen with those variables, alternatively discard
## truant item


# Read the file
lines <- readLines(here::here("data", "source_raw", "NTR_4552_vallabels.txt"))

labels_table <- data.frame(variable = character(), labels = character(),
                           stringsAsFactors = FALSE)

# Process each line
for (line in lines) {
  # Use regular expressions to extract the variable name and labels
  variable_name <- str_extract(line, "^[^,]+")
  labels <- str_extract(line, "\\{.*\\}")
  
  # Remove any surrounding whitespace
  variable_name <- str_trim(variable_name)
  labels <- str_trim(labels)
  
  # Add the extracted information to the data frame
  labels_table <- rbind(labels_table, data.frame(variable = variable_name,
                                                 labels = labels,
                                                 stringsAsFactors = FALSE))
}

## writing to csv_file
# write.csv(labels_table, here::here("data", "intermediate", "labels.csv"),
#          row.names = FALSE, col.names = FALSE)


## Not all variables in the data are contained in the label table
setdiff(colnames(data), labels_table$variable)
## doesn't matter, it's only the questions from the YsR that are not listed

# Inspecting codes ea variables(educational attainment)
ea_labels <- labels_table %>%
  filter(variable %in% ea_vars)

## unique codes that appear in the ea_vars
for(ea in ea_vars){
  cat("Variable", "'", ea, "'", "unique values: ",
      sort(unique(data %>% select(ea) %>% pull())), "\n", "\n")
}

## age at filling out survey variables, check distributions
data_age_vars <- data %>%
  select(contains("age"))


## Next: containment variables: Did participate fill out survey wave?

## Variables that indicate if participants filled out a survey
data_in_vars <- data %>% 
  select(FISNumber, starts_with("in_")) %>%
  mutate(n_missing_surveys = rowSums(is.na(.)))

## frequencies 
for(col in 2:ncol(data_in_vars)){
  cat("frequencies variable", "'", colnames(data_in_vars)[col], "'", "\n",
      paste(names(table(data_in_vars[,col], useNA = "ifany")),
            table(data_in_vars[,col], useNA = "ifany"),
            sep = ": ", collapse = "\n"),
      "\n\n")
}

## NA proportions
colMeans(is.na(data_in_vars)) ## Very high proportions of missings in YSR 

## filtering step (this might be written into separate script
## where data cleaning eventually takes place)

## 1) Filtering out all participants who have no outcome (did not participate
## in any of the ANTR surveys)
data_ANTR_participate <- data %>%
  select(FISNumber, starts_with("in_AS")) %>%
  mutate(n_QoL = rowSums(!is.na(.)) - 1) ## ensuring FISnumber is not counted

table(data_ANTR_participate$n_QoL)
## 50375 participants have not a single QoL measure!
## apply this filter before anything else! 




