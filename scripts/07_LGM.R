# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-08-15
#
# Script Name: 07_LGM.R
#
# Script Description: In this script, the filtered CBCL variables 
# (see script 04_data_cleaning_filtering1.R) from the project:
# Combining longitudinal change features of childhood psychopathology 
# with Polygenic scores in machine learning models of adult wellbeing
# will be used for latent growth modeling (LGM) to create 
# features carrying longitudinal information about childhood psychopathology
# these features will later be used in the ML predictor space for the 
# prediction of adult wellbeing (Qualitý of life)
#
#
# Notes: This steps needs to happen after the splitting of the dataset
# in training and test data
#
#

# Set options
cat("SETTING OPTIONS... /n/n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "MplusAutomation", "glue")


## custom preparation functions
source(here::here("scripts", "functions", "functions_ml.R"))

## setting working directory
setwd(here::here())

  ## creating the directory "cprobabilities" within current directory 
  ## if it does not yet exist
  if(!dir.exists(here::here("mplus_files", "cprobabilities"))){
    dir.create(here::here("mplus_files", "cprobabilities"))
  }

## loading in necessary dataset and vectors / tables of variables

## loading in training set
# load(here::here("data", "intermediate", "train_data.RData"))
train_data <- readRDS(here::here("data", "intermediate", "train_data.rds"))

## loading in test set
# load(here::here("data", "intermediate", "test_data.RData"))
test_data <- readRDS(here::here("data", "intermediate", "test_data.rds"))

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx")) %>%
  as.data.frame()

## loading in dataframe where CBCL items are connected to age variables and
## combining it properly with age vars and CBCL question to use during variable
## selection later
CBCL_age_df <- as.data.frame(t(readRDS(here::here("data", "intermediate", "CBCL_age_df.RDS"))))
colnames(CBCL_age_df) <- CBCL_age_df["question_number", ]
CBCL_age_df <- CBCL_age_df[-nrow(CBCL_age_df), ]
colnames(CBCL_age_df)[ncol(CBCL_age_df)] <- "age_var"
## CONTINE HERE!! 
## Code a function that given an input item from the CBCL questions list 
## outputs the age variables that are associated with the question codes

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
# load(here::here("scripts", "variable_vectors.RData"))
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

## loading in list of CBCL items per question
# load(here::here("scripts", "CBCL_questions_list.RData"))
CBCL_questions_list <- readRDS(
  here::here("scripts", "CBCL_questions_list.rds"))

## note that this still contains the CBCL items to be dropped

## loading in CBCL items to be retained and to be dropped
load(here::here("scripts", "CBCL_items_keep"))
load(here::here("scripts", "CBCL_items_drop"))

CBCL_items_keep <- readRDS(
  here::here("data", "intermediate", "CBCL_items_keep.rds")
)
CBCL_items_drop <- readRDS(
  here::here("data", "intermediate", "CBCL_items_drop.rds")
)

#------------------------------------------------------------------------------

## Important step before actual analysis: For trial calculations, permute 
## IDs so one remains blind for data
permute <- FALSE
if(permute){
  train_data <- transform(train_data, FISNumber = sample(FISNumber))
}

#------------------------------------------------------------------------------



## First task: Adjust the variable vectors, some of the variables are not 
## in the dataset anymore! 
CBCL_items_LGM <- intersect(CBCL_items_keep, CBCL_items_vec)
YSR_items_LGM <- intersect(CBCL_items_keep, YSR_items_vec)



## Second task: Which of the CBCL longitudinal questions are still eligible for
## longitudinal modeling? Some of the items were eliminated during data cleaning
## In order to do longitudinal modeling, there need to be at least 4 measuring
## points per CBCL question

## counting how many items per question are still available, exclude any 
## CBCL questions with less than 4 measures still available
CBCL_items_table <- CBCL_items_table %>%
  rowwise() %>%
  mutate(n_items_available = sum(
    sapply(across(everything()), function(x) as.character(x) %in% CBCL_items_keep)
  )) %>%
  ungroup() %>%
  filter(n_items_available >= 4)
## 80 questions might still be used for latent growth modeling
## (If IQR = 0 columns are not removed)

## shrinking down: table to contain only the variable names and the question
## names
CBCL_items_table_reduced <- CBCL_items_table %>%
  select(starts_with("Age"), question_number)

## adjusting CBCL_questions_list (filtering for only valid CBCL questions 
## that can be used for longitudinal modeling)
CBCL_items_valid <- unique(CBCL_items_table_reduced$question_number)

## selecting only the elements of the list that are contained in CBCL_items_valid
CBCL_questions_list <- CBCL_questions_list[CBCL_items_valid]


## Checking structure of missing:
# colMeans(is.na(data_LGM)) %>% as.data.frame() %>% View()

## calculate per question: How many measures does a participant have 
## for this specific question?
n_m_df <- data.frame()
step <- 1
for(question in unique(CBCL_items_table_reduced$question_number)){
  items_question <- CBCL_items_table_reduced %>%
    filter(question_number == question) %>%
    select(-question_number) %>%
    as.character() %>%
    na.omit()
  
  data_question <- train_data %>%
    select(FISNumber, all_of(items_question)) %>%
    # rowwise() %>%
    mutate(n_measures = rowSums(!is.na(.)) - 1) %>%
    select(FISNumber, n_measures)
  
  q_col <- paste0("n_measures_", question)
  
  colnames(data_question) <- c("FISNumber", q_col)
  
  if(step == 1){
  n_m_df <- data_question
  } else {
    n_m_df <- n_m_df %>%
      left_join(data_question, by = "FISNumber")
  }
  step <- step + 1
  train_data <- train_data %>%
    left_join(data_question, by = "FISNumber")
  
}

## same for test data
n_m_df <- data.frame()
step <- 1
for(question in unique(CBCL_items_table_reduced$question_number)){
  items_question <- CBCL_items_table_reduced %>%
    filter(question_number == question) %>%
    select(-question_number) %>%
    as.character() %>%
    na.omit()
  
  data_question <- test_data %>%
    select(FISNumber, all_of(items_question)) %>%
    # rowwise() %>%
    mutate(n_measures = rowSums(!is.na(.)) - 1) %>%
    select(FISNumber, n_measures)
  
  q_col <- paste0("n_measures_", question)
  
  colnames(data_question) <- c("FISNumber", q_col)
  
  if(step == 1){
    n_m_df <- data_question
  } else {
    n_m_df <- n_m_df %>%
      left_join(data_question, by = "FISNumber")
  }
  step <- step + 1
  test_data <- test_data %>%
    left_join(data_question, by = "FISNumber")
  
}


## Next: Binding this to data


##----------------------------------------------------------------------------


## parallelizing data preparation and LGM modelling per question 
## Do this in two blocks so it can run at 48 cores at ntr-compute1
ncore_ntr <- 48

## integrating parallelization
cl <- makeCluster(ncores)
## cluster of 64 on ntrcompute-2
registerDoParallel(cl)
clusterExport(cl, c("CBCL_age_df", 
                    "CBCL_questions_list", 
                    "CBCL_items_table_reduced",
                    "train_data", 
                    "test_data",
                    "LGM_preprocess",
                    "LCGA_1_4_CBCL"),
              envir = environment())

invisible(clusterEvalQ(cl,expr= {
  library(MplusAutomation)
  library(here)
  library(dplyr)
  library(tidyverse)
  library(stringr)
  library(data.table)
  
}))
invisible(clusterEvalQ(cl, ls()))

LGM_full_CBCL <- parLapply(
  cl, names(CBCL_questions_list), function(x) {
    ## data prep for training and test set
    train_data_question <-
      LGM_preprocess(
        CBCL_age_df = CBCL_age_df,
        CBCL_question = x,
        df = train_data,
        questions_list = CBCL_questions_list,
        items_table = CBCL_items_table_reduced
      )
    
    test_data_question <-
      LGM_preprocess(
        CBCL_age_df = CBCL_age_df,
        CBCL_question = x,
        df = test_data,
        questions_list = CBCL_questions_list,
        items_table = CBCL_items_table_reduced
      )
    
    ## calculating and selecting 1-4 class models for each question,
    ## creating output dataframe
    LGM_df_question <-
      LCGA_1_4_CBCL(df_train = train_data_question,
                    df_test = test_data_question,
                    CBCL_question = x)
    
    return(LGM_df_question)
  })
stopCluster(cl)

## set working directory back to parent directory (was affected by the LGM
## function)
setwd(here::here())


## make one dataframe out of all df_out_CBCL dfs from the list
## LGM_full_CBCL

## Important: To be able to separate from the non-LGM longitudinal
## features later, give all variables (except for FISNumber)
## the prefix (or suffix) LGM_ (_LGM)


## load in non-LGM features dataframe created in script 06

## give all variables (except for FISNumber)
## the prefix (or suffix) non_LGM_ (_non_LGM)

## merge non_LGM longitudinal df with LGM longitudinal df 


saveRDS(df_full_longitudinal,
        file = here::here("data", "intermediate", "df_full_longitudinal"))

## saving data model D will follow in the ML modelling script (also add
## covariates, raw item scores, etc., basically merge data_model_A with 
## df_full_longitudinal)






