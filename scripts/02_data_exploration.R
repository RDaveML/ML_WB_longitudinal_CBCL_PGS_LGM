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
               "stringr", "readxl", "data.table", "psych")

## printing and changing working directory if needed
getwd()

here::here()

getwd() == here::here()

## negation operator
`%notin%` <- Negate(`%in%`)

## reading in datafile (if necessary, change filepath to where file is located)
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
## Still adjust this filename!!
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

## saving all vectors of variable names to re-use in later scripts
save(CBCL_YSR_items_vec, CBCL_items_vec, YSR_items_vec, ea_vars, qol_vars, 
     file = here::here("scripts", "variable_vectors.RData"))

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


# Read the file (change filepath if necessary to where file is located)
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
## change filepath if necessary
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


#-----------------------------------------------------------------------------------------

## filtering out participants with no outcome, then calculate again how
## many participants did surveys

data_with_QoL <- data %>%
  filter(!is.na(in_AS_8) | !is.na(in_AS_10) | !is.na(in_AS_12) | !is.na(in_AS_14))



data_with_QoL2 <- data %>%
  filter(!is.na(levenc8) | !is.na(levenc10) | !is.na(levenc12) | !is.na(levenc14))
## There also seems to be difference here, not all participants in ANTR surveys filled out 
## QoL measure CL

nrow(data) - nrow(data_with_QoL2)

## Containment variables again (after filtering process)
## Variables that indicate if participants filled out a survey
data_in_vars2 <- data_with_QoL2 %>% 
  select(FISNumber, starts_with("in_")) %>%
  mutate(n_missing_surveys = rowSums(is.na(.)))

## frequencies 
for(col in 2:ncol(data_in_vars2)){
  cat("frequencies variable", "'", colnames(data_in_vars2)[col], "'", "\n",
      paste(names(table(data_in_vars2[,col], useNA = "ifany")),
            table(data_in_vars2[,col], useNA = "ifany"),
            sep = ": ", collapse = "\n"),
      "\n\n")
}

## NA proportions
colMeans(is.na(data_in_vars2)) ## Very high proportions of missings in YSR 

## filtering step (this might be written into separate script
## where data cleaning eventually takes place)

## 1) Filtering out all participants who have no outcome (did not participate
## in any of the ANTR surveys)
data_ANTR_participate2 <- data_with_QoL2 %>%
  select(FISNumber, starts_with("in_AS")) %>%
  mutate(n_QoL = rowSums(!is.na(.)) - 1) ## ensuring FISnumber is not counted

table(data_ANTR_participate2$n_QoL)


## Next: Reliability inspection of ea variables
data_ea <- data_with_QoL2 %>%
  select(FISNumber, starts_with("ea4"))

for(col in 2:ncol(data_ea)){
  cat("frequencies variable", "'", colnames(data_ea)[col], "'", "\n",
      paste(names(table(data_ea[,col], useNA = "ifany")),
            table(data_ea[,col], useNA = "ifany"),
            sep = ": ", collapse = "\n"),
      "\n\n")
}

## Next: Age distribution variables - What is distribution of ages 
## at every timepoint survey was filled out?
## Tables but also plot distributions
## Are there outliers? Inspect those cases
## age at filling out survey variables, check distributions
data_age_vars <- data_with_QoL2 %>%
  select(FISNumber, contains("age")) %>%
  select(!(ea4_age_agg))

ncol(data_age_vars)

## tables of distribution of age when survey was administered
for(col in 2:ncol(data_age_vars)){
  cat("frequencies variable", "'", colnames(data_age_vars)[col], "'", "\n",
      paste(names(table(data_age_vars[,col], useNA = "ifany")),
            table(data_age_vars[,col], useNA = "ifany"),
            sep = ": ", collapse = "\n"),
      "\n\n")
}

## plots of age distribution
ANTR_age_cols <- c("age8", "age10", "age12", "age14")
plot_age_list <- vector('list', ncol(data_age_vars) - 1)
for(i in 1:(ncol(data_age_vars) - 1)) {
  col_name <- names(data_age_vars)[i + 1]  # Get the column name
  if(col_name %in% ANTR_age_cols){
    age_desired <- mean(data_age_vars[, i + 1], na.rm = TRUE)
    subtitle <- "ANTR wave,
horizontal line indicates mean age of participants"
  } else {
    age_desired <- as.numeric(regmatches(col_name,
                                         gregexpr("[0-9]+", col_name)))
    subtitle <- "YNTR wave,
horizontal line indicates age when survey was supposed to take place"
  }
  plot_age_list[[i]] <- ggplot(data_age_vars, aes(x = .data[[col_name]])) +
    geom_histogram() + 
    geom_vline(xintercept = age_desired, linewidth = 2) + 
    labs(subtitle = subtitle)
}
plot_age_list[[1]]
plot_age_list[[2]]
plot_age_list[[3]]
plot_age_list[[4]]
plot_age_list[[5]]
plot_age_list[[6]]
plot_age_list[[7]]
plot_age_list[[8]]
plot_age_list[[9]]
plot_age_list[[10]]
plot_age_list[[11]]

#------------------------------------------------------------------------------

## For all participants: In how many YNTR surveys did they participate
## Calculating how many people have responses in YNTR surveys
data_YNTR_participate2 <- data_with_QoL2 %>% 
  select(FISNumber, starts_with("in_YS")) %>%
  select(!(in_YS_DHBQ18)) %>%
  mutate(n_missing_surveys = rowSums(is.na(.))) %>%
  mutate(perc_surveys_missing = n_missing_surveys / 7)

table(data_YNTR_participate2$n_missing_surveys)
## We see that is is also an issue that a lot of participants from the ANTR who
## have QoL measures did not participate in the YNTR surveys

## calculate percentage score of missing surveys per participant
table(round(data_YNTR_participate2$perc_surveys_missing, 2))

## Calculation how many measurements per CBCL / YSR question there are
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

## This is still to be continued! 

##-----------------------------------------------------------------------------

## 50% filtering applied to a) columns b) rows for the CBCL / YSR questions
data_CBCL_filter <- data_with_QoL2 %>%
  select(FISNumber, all_of(CBCL_YSR_items_vec)) %>%
  mutate(var_mis = rowSums(is.na(.))) %>%
  mutate(miss50 = ifelse(var_mis - 2 > 0.5 * (ncol(.) - 2),
                  0, 1))

table(data_CBCL_filter$var_mis)

table(data_CBCL_filter$miss50)
## When applying the 50% filter to only the CBCL items, 33209 participants
## need to be discarded, 6555 participants still left!

colMeans(is.na(data_CBCL_filter))[colMeans(is.na(data_CBCL_filter)) > 0.5]
colMeans(is.na(data_CBCL_filter))[colMeans(is.na(data_CBCL_filter)) > 0.5] %>%
  length()
colMeans(is.na(data_CBCL_filter))
colMeans(is.na(data_CBCL_filter)) %>%
  length()

## Huge issue: all CBCL features in the dataset have more than 50% missing! 
## filter needs to be more permissive

## Only take participants with only one or two missing surveys? 

## Trying again when removing those participants with only missing YNTR surveys
data_CBCL_filter2 <- data_CBCL_filter %>%
  filter(var_mis != 451) ## filter out all those who have 0 answers to CBCL

table(data_CBCL_filter2$var_mis)

table(data_CBCL_filter2$miss50)

colMeans(is.na(data_CBCL_filter2))[colMeans(is.na(data_CBCL_filter2)) > 0.5]
colMeans(is.na(data_CBCL_filter2))[colMeans(is.na(data_CBCL_filter2)) > 0.5] %>%
  length()
colMeans(is.na(data_CBCL_filter2))
colMeans(is.na(data_CBCL_filter2)) %>%
  length()


data_not_CBCL <- data_with_QoL2 %>%
  select(!any_of(CBCL_YSR_items_vec))

workspace.size <- function() {
  ws <- sum(sapply(ls(envir=globalenv()), function(x)object.size(get(x))))
  class(ws) <- "object_size"
  ws
}

workspace.size()
## At this point objects in working environment almost sum up to 1GB,
## make sure to clean unnecessary objects in the process (objects can always
## be saved and loaded back in later)

#------------------------------------------------------------------------------

## Summary statistics and distribution plots
## Based participants with at least one QoL measure and at least one YNTR
## participation

## Basic summary function looping over variables
summary_df <- data.frame()
for (col_name in names(data_CBCL_filter2
     [names(data_CBCL_filter2) %notin% c("FISNumber", "var_mis", "miss50")])) {
  cat("Summary of", col_name, ":\n")
  print(describe(data_CBCL_filter2[[col_name]]))
  cat("\n")
  summary_var <- as.data.frame(describe(data_CBCL_filter2[[col_name]])) 
  summary_var <- rownames_to_column(summary_var)
  summary_var[1,1] <- col_name
  names(summary_var)[1] <- "variable"
  summary_df <- rbind(summary_df, summary_var)
  ## only variable name still missing
}

## This summary df can be used to identify variables with suspicious 
## distributions! 
summary_df <- summary_df %>%
  mutate(perc_answers = n / nrow(data_CBCL_filter2)) %>%
  mutate(perc_missing_answers = 1 - perc_answers)

## Continue here! check the summary statistics, calculate additional according
## plot distributions, make correlation plots

