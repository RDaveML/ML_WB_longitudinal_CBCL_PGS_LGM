# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-07-31
#
# Script Name: 04_data_cleaning01_filtering1
#
# Script Description: This script contains the first steps of data cleaning
# for the project: 
## Combining longitudinal change features of childhood psychopathology 
## with Polygenic scores in machine learning models of adult wellbeing
#
# Steps:
# - filtering out all participants that do not match criteria
#       (Check script and paper for exact steps)
# - Re-coding CBCL items that are not on the 0-2 scale
# - Imputing age variables
# - 
#
# Notes: 
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "caret", "psych")

## printing and changing working directory if needed
getwd()

here::here()

getwd() == here::here()

## loading in custom functions
source(here::here("scripts", "functions", "functions_preprocessing.R"))


## reading in datafile (if necessary, change filepath to where file is located)
data <- read_sav(
  here::here("data", "source_raw", "PHE_20240722_4552_YJS.sav")) %>%
    as.data.frame()

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

## loading in vectors of variable names for filtering and selecting
CBCL_YSR_items_vec <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[1]]

CBCL_items_vec <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[2]]

YSR_items_vec <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[3]]

ea_vars <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[4]]

qol_vars <- readRDS(
  here::here("data", "intermediate", "variable_vectors.rds"))[[5]]

## loading in summary dataframe from exploration
summary_df <- readRDS(here::here("data", "intermediate", "summary_CBCL.rds"))

## loading in list of CBCL items per question
CBCL_questions_list <- readRDS(
  here::here("data", "intermediate", "CBCL_questions_list.rds"))

## loading in covariate data for further filtering
data_covariates <- readRDS(
  here::here("data", "intermediate", "data_covariates.rds")
  )
## loading in covariate names 
names_covariates <- readRDS(
  here::here("data", "intermediate", "names_covariates.rds")
)

nrow(data)
# Initially 92969 participants in dataset

## filtering data in one compact function, outputting dropped participants 
## after each filtering step


data_filtered <- filter_CBCL(df = data, CBCL_YSR_items_vec = CBCL_YSR_items_vec,
                             data_covariates = data_covariates)


## checking some of the important covariates:
## twin status
table(data_filtered$twzyg, useNA = "ifany")
## no non-twins present in sample

## any missing aggregated variables
table(data_filtered$in_NA_AGG, useNA = "ifany")
## no missings in the aggregated variables


## ---------------------------------------------------------------------------

## recoding items that are on different scale!

CBCL_items_table_y5 <- CBCL_items_table %>%
  filter(!is.na(Age5))

CBCL_items_CBCL103 <- unname(unlist(c(CBCL_items_table_y5[1, 1:7])))
CBCL_items_CBCL50 <- unname(unlist(c(CBCL_items_table_y5[2, 1:7])))
## CBCL question 57 was not asked at age 3
CBCL_items_CBCL57 <- unname(unlist(c(CBCL_items_table_y5[3, 1:7])))[!is.na(
  unname(unlist(c(CBCL_items_table_y5[3, 1:7]))))]

# filtering data for only those variables
data_CBCL_103 <- data_filtered %>% 
  select(all_of(CBCL_items_CBCL103))

for(variable in CBCL_items_CBCL103){
print(prop.table(table(data_CBCL_103 %>% select(variable) %>%
                         filter(!is.na(variable)))))
  
}

data_CBCL_50 <- data_filtered %>% 
  select(all_of(CBCL_items_CBCL50))

for(variable in CBCL_items_CBCL50){
  print(prop.table(table(data_CBCL_50 %>% select(variable) %>%
                           filter(!is.na(variable)))))
  
}

data_CBCL_57 <- data_filtered %>% 
  select(all_of(CBCL_items_CBCL57))

for(variable in CBCL_items_CBCL57){
  print(prop.table(table(data_CBCL_57 %>% select(variable) %>%
                           filter(!is.na(variable)))))
  
}

## Inspection of the frequency distributions suggests the 
## following recoding for the YNTR5 questions: 
## 1-2 to 0; 3-4 to 1; 5 - 2

## Recoding the items 
q5 <- c("q13m5", "q11m5", "q4m5")

data_filtered_recoded <- data_filtered %>%
  mutate(across(all_of(q5), 
         ~ case_when(. == 1 | . == 2 ~ 0,
                     . == 3 | . == 4 ~ 1,
                     . == 5 ~ 2,
                     .default = NA)))

## checking if recoding went properly
table(data_filtered$q13m5, useNA = "ifany")
table(data_filtered_recoded$q13m5, useNA = "ifany")

table(data_filtered$q11m5, useNA = "ifany")
table(data_filtered_recoded$q11m5, useNA = "ifany")

table(data_filtered$q4m5, useNA = "ifany")
table(data_filtered_recoded$q4m5, useNA = "ifany")

## it worked out correctly

## removing intermediate objects
rm(list = c("data_CBCL_103", "data_CBCL_50", "data_CBCL_57"))

#----------------------------------------------------------------------------

## Imputing age variables: If individual has no value for age at assessment
## CBCL, impute with the mean

age_CBCL_cols <- c("agem3", "ageq5", "agem7", "agem10", "agem12",
                   "ages14", "ages16")

in_vars_CBCL <- c("in_YS_3M", "in_YS_5", "in_YS_7M", "in_YS_10M", "in_YS_12M",
                  "in_YS_DHBQ14", "in_YS_DHBQ16")

means_age_cols <- numeric(length = length(age_CBCL_cols))

for(var in 1:length(means_age_cols)){
  means_age_cols[var] <- data_filtered_recoded %>%
    select(!!age_CBCL_cols[var]) %>%
    pull() %>%
    mean(na.rm = TRUE)
}

for(i in 1:length(means_age_cols)){
    ## fill with function so that NAs where participants did actually 
    ## participante are imputed with mean age
    age_col <- sym(age_CBCL_cols[i])
    in_vars_col <- sym(in_vars_CBCL[i])
    print(age_col)
    print(in_vars_col)
    data_filtered_recoded <- data_filtered_recoded %>%
      mutate(!!age_col := ifelse(is.na(!!age_col) & !is.na(!!in_vars_col),
                                means_age_cols[i], 
                                !!age_col))
}

## This has almost no influence, still a lot of missings in age


## removing CBCL items with more than 50% missings 
col_threshold <- 0.5


data_CBCL <- data_filtered_recoded %>%
  select(all_of(CBCL_YSR_items_vec)) %>% 
  mutate(across(everything(), as.numeric))

cols_with_excessive_na <- sapply(data_CBCL %>% select(
  all_of(CBCL_YSR_items_vec)),
  function(x) mean(is.na(x)) > col_threshold)

data_CBCL_cols <- data_CBCL[, !cols_with_excessive_na]
## 12 items from CBCL filtered

## vector of CBCL items to retain
CBCL_items_keep <- colnames(data_CBCL_cols)
saveRDS(CBCL_items_keep,
        file = here::here("data", "intermediate", "CBCL_items_keep.rds"))

## vector of columns to drop for later
CBCL_items_drop <- setdiff(CBCL_YSR_items_vec, CBCL_items_keep)
saveRDS(CBCL_items_drop,
        file = here::here("data", "intermediate", "CBCL_items_drop.rds"))


##-----------------------------------------------------------------------------

## recalculating summary data frame with filtered dataset
## Summary statistics and distribution plots
## Based participants with at least one QoL measure and at least one YNTR
## participation

## Basic summary function looping over variables outputting basic
## summary statistics
summary_df <- data.frame()
for (col_name in names(data_CBCL_cols)) {
  #cat("Summary of", col_name, ":\n")
  #print(describe(data_CBCL_filter2[[col_name]]))
  #cat("\n")
  summary_var <- as.data.frame(describe(data_CBCL_cols[[col_name]], 
                                        IQR = TRUE)) 
  summary_var <- rownames_to_column(summary_var)
  summary_var[1,1] <- col_name
  names(summary_var)[1] <- "variable"
  summary_df <- rbind(summary_df, summary_var)
  ## only variable name still missing
}

## This summary df can be used to identify variables with suspicious 
## distributions!
## Adding further variables
summary_df <- summary_df %>%
  mutate(perc_answers = n / nrow(data_CBCL_cols)) %>%
  mutate(perc_missing_answers = 1 - perc_answers) %>%
  mutate(VC = sd / mean) %>%
  mutate(variance = sd^2)

## Reading in CBCL items overview table
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

## Attaching question labels to summary df

## creating dataframe with itemcode and question
CBCL_ages <- grep("Age", names(CBCL_items_table), value = TRUE)
CBCL_item_question_age <- data.frame()
for(row in 1:nrow(CBCL_items_table)){  
  for(col in CBCL_ages) {
    variable <- as.character(CBCL_items_table[row, col])
    question <- CBCL_items_table[row, "question_number"]
    age <- col
    set <- unlist(unname(c(variable, question, age)))
    # concatenating all information together, turning it into vector instead of 
    # list
    CBCL_item_question_age <- rbind(CBCL_item_question_age, set)
  }
}
names(CBCL_item_question_age) <- c("variable", "question_number", "age")

## attach to summary df
summary_df <- summary_df %>%
  left_join(CBCL_item_question_age %>% filter(!is.na(variable)),
            by = "variable")

## changing column order so question appears in front
summary_df <- summary_df[, c(1, 2, 20, 21, 3:5, 19, 6:18)]

## saving summary dataframe
saveRDS(summary_df, here::here("data", "intermediate", "summary_CBCL.rds"))

##-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------

## creating full dataset and dataset for LGM modeling
data_full <- data_filtered_recoded %>%
  select(-any_of(CBCL_items_drop))

data_LGM <- data_filtered_recoded %>%
  select(FISNumber, FamilyNumber, QoL_simple, all_of(covariates_names),
         all_of(CBCL_items_keep))

## Saving datasets to continue working with them
saveRDS(data_full, here::here("data", "intermediate", "data_full.rds"))
saveRDS(data_LGM, here::here("data", "intermediate", "data_LGM.rds"))

## Note: PGS will be calculated at later step


## eoS 


