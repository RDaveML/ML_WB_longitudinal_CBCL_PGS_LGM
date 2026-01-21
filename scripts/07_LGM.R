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
# features carrying latent longitudinal information about 
# childhood psychopathology
# these features will later be used in the ML predictor space for the 
# prediction of adult wellbeing (Qualitý of life)
#
#
# Notes: The modelling is first carried out on the training set. The 
# resulting models are then applied to the test set to generate the same 
# features in the test set
#
# LGM models can be 2-4 class Latent growth analysis models, or, if 
# fit criteria indicate that there are no latent classes, 
# multilevel LGM models where every individual has their own 
# latent slope and intercept

# Note: Script was ran on server (ntr-compute1), using 48 cores in parallel

# Set options
cat("SETTING OPTIONS... /n/n", sep = "")
options(scipen = 999)

t00 <- Sys.time()

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
packages_load <- c("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
                   "stringr", "readxl", "data.table", "MplusAutomation", "glue",
                   "purrr", "parallel", "doParallel", "foreach")

pacman::p_load(char = packages_load)

## custom preparation functions
source(here::here("scripts", "functions", "functions_ml.R"))
source(here::here("scripts", "functions", "functions_LGM.R"))

## setting working directory
setwd(here::here())

## creating the directories for mplus files
## within current directory 
## if it does not yet exist

if(!dir.exists(here::here("mplus_files"))){
  dir.create(here::here("mplus_files"))
  dir.create(here::here("mplus_files", "cprobabilities"))
}

## loading in necessary dataset and vectors / tables of variables
data_full <- readRDS(here::here("data", "intermediate", "data_full.rds"))

## loading in train and test IDs
indices_train <- readRDS(
  here::here("data", "intermediate", "indices_train.rds"))

indices_test <- readRDS(
  here::here("data", "intermediate", "indices_test.rds"))

## train set
train_data <- data_full %>%
  filter(FISNumber %in% indices_train)

## test set
test_data <- data_full %>%
  filter(FISNumber %in% indices_test)

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(
  here::here("doc", "CBCL_table_t_per_item.xlsx")) %>%
    as.data.frame()

## loading in dataframe where CBCL items are connected to age variables and
## combining it properly with age vars and CBCL question to use during variable
## selection later
CBCL_age_df <- as.data.frame(
  t(readRDS(here::here("data", "intermediate", "CBCL_age_df.RDS")))
  )
colnames(CBCL_age_df) <- CBCL_age_df["question_number", ]
CBCL_age_df <- CBCL_age_df[-nrow(CBCL_age_df), ]
colnames(CBCL_age_df)[ncol(CBCL_age_df)] <- "age_var"


## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
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
CBCL_questions_list <- readRDS(
  here::here("data", "intermediate", "CBCL_questions_list.rds"))

## note that this still contains the CBCL items to be dropped

CBCL_items_keep <- readRDS(
  here::here("data", "intermediate", "CBCL_items_keep.rds")
)
CBCL_items_drop <- readRDS(
  here::here("data", "intermediate", "CBCL_items_drop.rds")
)

#------------------------------------------------------------------------------

## First task: Adjust the variable vectors, some of the variables are not 
## in the dataset anymore (were removed in filtering procedure)! 
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
    sapply(
      across(everything()), function(x) as.character(x) %in% CBCL_items_keep)
  )) %>%
  ungroup() %>%
  filter(n_items_available >= 4)
## 80 questions might still be used for latent growth modeling

## shrinking down: table to contain only the variable names and the question
## names
CBCL_items_table_reduced <- CBCL_items_table %>%
  select(starts_with("Age"), question_number)

## adjusting CBCL_questions_list (filtering for only valid CBCL questions 
## that can be used for longitudinal modeling)
CBCL_items_valid <- unique(CBCL_items_table_reduced$question_number)

## selecting only the elements of the list
## that are contained in CBCL_items_valid
CBCL_questions_list <- CBCL_questions_list[CBCL_items_valid]


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

## recoding haven_labelled variables in the raw data

labelled_count <- 0
for(col in 1:ncol(train_data)){
  if("haven_labelled" %in% class(train_data[, col])){
    labelled_count <- labelled_count + 1
  }
}
cat(labelled_count, " columns haven_labelled, those need to be converted")

## converting have_labelled columns to numeric
train_data <- mult_to_numeric(df = train_data)

test_data <- mult_to_numeric(df = test_data)

##----------------------------------------------------------------------------

## parallelizing data preparation and LGM modelling per question 
## Note: This was here done in two blocks 
## analysis was run on 48 cores (about half of the available cores on the 
## ntr-compute1 server of the NTR)
ncore_ntr <- 48

## integrating parallelization
cl <- makeCluster(ncore_ntr)
## cluster of 48 on ntr-compute1
registerDoParallel(cl)
clusterExport(cl, c("CBCL_age_df", 
                    "CBCL_questions_list", 
                    "CBCL_items_table_reduced",
                    "train_data", 
                    "test_data",
                    "age_var_select",
                    "LGM_preprocess",
                    "generate_fixed_syntax_with_classes",
                    "LGM_1_4_CBCL",
                    "f_conv"), ## custom function from ml functions
              envir = environment())

invisible(clusterEvalQ(cl,expr= {
  library(MplusAutomation)
  library(here)
  library(dplyr)
  library(tidyverse)
  library(stringr)
  library(data.table)
  library(parallel)
  library(doParallel)

  ## export entire set of functions
}))

## do this in two blocks, 48 cores available on ntr-compute1
LGM_full_CBCL_1 <- parLapply(
  cl, names(CBCL_questions_list)[1:48], function(x) { 
    tryCatch({
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
        LGM_1_4_CBCL(df_train = train_data_question,
                    df_test = test_data_question,
                    CBCL_question = x)
      
      return(LGM_df_question)}, error = function(e) {
        message(sprintf("Error in processing question '%s': %s", x, e$message))
        return(NULL)
      })
  })
stopCluster(cl)

## assigning names according to question labels
names(LGM_full_CBCL_1) <- names(CBCL_questions_list)[1:48]


cat("\nFirst half of LGM questions done, now second half\n")


## set working directory back to parent directory (was affected by the LGM
## function)
setwd(here::here())

## second part of the LGM, remaining CBCL questions
ncore_ntr <- length(CBCL_questions_list) - 48

## integrating parallelization
cl <- makeCluster(ncore_ntr)
## cluster of 64 on ntrcompute-2
registerDoParallel(cl)
clusterExport(cl, c("CBCL_age_df", 
                    "CBCL_questions_list", 
                    "CBCL_items_table_reduced",
                    "train_data", 
                    "test_data",
                    "age_var_select",
                    "LGM_preprocess",
                    "generate_fixed_syntax_with_classes",
                    "LGM_1_4_CBCL",
                    "f_conv"),
              envir = environment())

invisible(clusterEvalQ(cl,expr= {
  library(MplusAutomation)
  library(here)
  library(dplyr)
  library(tidyverse)
  library(stringr)
  library(data.table)
  library(foreign)
  library(readr)
  library(readxl)
  library(glue)
  library(purrr)
}))
invisible(clusterEvalQ(cl, ls()))

LGM_full_CBCL_2 <- parLapply(
  cl, names(CBCL_questions_list)[49:length(CBCL_questions_list)], function(x) {
     tryCatch({
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
        LGM_1_4_CBCL(df_train = train_data_question,
                      df_test = test_data_question,
                      CBCL_question = x)
      
      return(LGM_df_question)}, error = function(e) {
        message(sprintf("Error in processing question '%s': %s", x, e$message))
        return(NULL)
      })
  })
stopCluster(cl)

names(LGM_full_CBCL_2) <- names(
  CBCL_questions_list)[49:length(CBCL_questions_list)]

## set working directory back to parent directory (was affected by the LGM
## function)
setwd(here::here())


## appending both lists
LGM_full_CBCL <- c(LGM_full_CBCL_1, LGM_full_CBCL_2)

## saving LGM df for subsequent analyses
saveRDS(
  LGM_full_CBCL, file = here::here("data", "intermediate", "LGM_full_CBCL.rds"))

cat("LGM df saved\n")

## removing all sub-elements of the list where $df_out_CBCL is NULL
LGM_full_CBCL <- LGM_full_CBCL[!sapply(LGM_full_CBCL, function(x) {
  is.null(x) || is.null(x$df_out_CBCL)
})]

## saving CBCL_questions where outcome dataframe was NULL
CBCL_questions_null <- setdiff(names(CBCL_questions_list), 
                               names(LGM_full_CBCL))

## make one dataframe out of all df_out_CBCL dfs that are contained in the 
## sub elements of the list LGM_full_CBCL
## all dfs should be joined together by the indicator variable "FISNumber"
LGM_df <- Reduce(function(x, y) left_join(x, y, by = "FISNumber"),
                 lapply(LGM_full_CBCL, function(df) {
                  df$df_out_CBCL
                  })) %>% 
  ## renaming: To be able to separate from the non-LGM longitudinal
  ## features later, all variables (except for FISNumber)
  ## get the prefix (or suffix) 'LGM_ (_LGM)'
  rename_with(~ paste0("LGM_", .), -FISNumber) %>%
  mutate(FISNumber = as.numeric(as.character(FISNumber)))
  ## numeric to align with FISNumber
  ## in other data parts
  ## saving LGM df

## saving data frame that resulted from the latent growth modelling on all
## eligible CBCL items
saveRDS(LGM_df, file = here::here("data", "intermediate", "LGM_df.rds"))

## saving CBCL_questions where outcome dataframe was NULL
saveRDS(CBCL_questions_null, file = here::here(
  "data", "intermediate", "CBCL_questions_null.rds"))


t01 <- Sys.time()

cat("duration entire script (running LGM on all CBCL_question, merging dfs): ",
    difftime(t01, t00, unit = "mins"), " minutes")

## eoS

###############################################################################










