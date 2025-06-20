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

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx")) %>%
  as.data.frame()

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

## Next: Binding this to data


## Now: With one item test it out: Pivot data, calculate all you 
## need for LGM, make the entire Mplus stuff for one item and save output
## write down what the function that does the job needs for arguments
## then lapply or sapply it over the items and then bind together all
## the resulting dfs


## Important: before any analyses, permute Family IDs and FISNumbers so
## you remain blind to the actual content of the data! 


##----------------------------------------------------------------------------

## LGM part: Bottom-up from simpler to more complex models

## Note: this was moved to 07_old_LGM.R


## data preparation for LGM in Mplus

## sampling test Question
test_q <- na.omit(as.character(sample_n(CBCL_items_table_reduced, 1)))

# question name
test_q_name <- test_q[length(test_q)]

var_names_test <- unlist(unname(CBCL_questions_list[test_q_name]))

## issue here: getting the number of measures also included in the select statement! 

test_df <- train_data %>%
  select(FISNumber, FamilyNumber, twzyg, any_of(var_names_test),
         #matches(paste0(test_q, "\\b"))
         ) %>%
  rename("FISNr" = FISNumber, "FamNr" = FamilyNumber,
  ## renaming the variable that contains the string "n_measures" to "n_t"
         "n_t" = paste0("n_measures_", test_q_name))
## renaming so that variable names are max 8 characters long

colnames(test_df)[colnames(test_df) %in% var_names_test] <- 
  paste0("t", 1:length(var_names_test))


## Add number of family members for each participant
count_fam <- test_df[,c("FamNr","FISNr")] %>%
  count(FamNr) %>%
  rename("fam_count" = n)

test_df <- merge(test_df, count_fam, by = "FamNr")

saveRDS(test_df, here::here("mplus_files", "test_df_CBCL.rds"))
test_df <- readRDS(here::here("mplus_files", "test_df_CBCL.rds"))

## conversion of columns to proper factors 
test_df <- f_conv(df = test_df,
                  covariates = c("FamNr", "FISNr", "twzyg",
                                 paste0("t", 1:length(var_names_test))))


classes <- c(1:4)

list_models <- vector("list")

data_files_before <- list.files(here::here("mplus_files"))

for(class_nr in classes){

  if(class_nr == 1){
    ## most easy case: code model with one class, then 2-4 classes
    title_string <- paste0(class_nr, "-class model CBCL_question ", test_q_name)
    
    ## CONTINUE HERE!!!
    variable_string <- gsub("\n", "", paste0("usevar = t1",
                              "-",
                              paste0("t", length(var_names_test)),
                              " FamNr;",
                              "\n",
                              "categorical = t1",
                              "-",
                              paste0("t", length(var_names_test)),
                              ";",
                              "\n",
                              "cluster = FamNr;" ##classes = c(2);
                              ))
    analysis_string <-gsub("\n", "", paste0(
    "type = complex;
     estimator = mlr;
     link = probit;
     ALGORITHM = INTEGRATION;"
    ))
    
    if(length(var_names_test) == 4){ 
      model_string <- gsub("\n", "", paste0(
     "i by t1@0 t2* t3* t4@1;
      s by t1@0 t2* t3* t4@1;
      ![t1$1@0 t2$1@0 t3$1@0 t4$1@0] (thr1);
      ![t1$2*1 t2$2*1 t3$2*1 t4$2*1] (thr2);
      ! Freely estimate means of latent intercept and slope
      [i];  
      [s];
      ! Freely estimate variances of intercept and slope
      i*;  
      s*;
      i WITH s@0;"
      ))
    } else if(length(var_names_test) == 5){ 
      model_string <- gsub("\n", "", paste0(  
     "i by t1@0 t2* t3* t4* t5@1;
      s by t1@0 t2* t3* t4* t5@1;
      ![t1$1@0 t2$1@0 t3$1@0 t4$1@0 t5$1@0] (thr1);
      ![t1$2*1 t2$2*1 t3$2*1 t4$2*1 t5$2*1] (thr2);
      ! Freely estimate means of latent intercept and slope
      [i];  
      [s];
      ! Freely estimate variances of intercept and slope
      i*;  
      s*;
      i WITH s@0;"
      ))
    } else if(length(var_names_test) == 6){ 
      model_string <-
     "i by t1@0 t2* t3* t4* t5* t6@1;
      s by t1@0 t2* t3* t4* t5* t6@1;
      ![t1$1@0 t2$1@0 t3$1@0 t4$1@0 t5$1@0 t6$1@0] (thr1);
      ![t1$2*1 t2$2*1 t3$2*1 t4$2*1 t5$2*1 t6$2*1] (thr2);
      ! Freely estimate means of latent intercept and slope
      [i];  
      [s];
      ! Freely estimate variances of intercept and slope
      i*;  
      s*;
      i WITH s@0;"  
    } else {
      model_string <- gsub("\n", "", paste0(
     "i by t1@0 t2* t3* t4* t5* t6* t7@1;
      s by t1@0 t2* t3* t4* t5* t6* t7@1;
      ![t1$1@0 t2$1@0 t3$1@0 t4$1@0 t5$1@0 t6$1@0 t7$1@0] (thr1);
      ![t1$2*1 t2$2*1 t3$2*1 t4$2*1 t5$2*1 t6$2*1 t7$2*1] (thr2);
      ! Freely estimate means of latent intercept and slope
      [i];  
      [s];
      ! Freely estimate variances of intercept and slope
      i*;  
      s*;
      i WITH s@0;"
      ))
    }
    
    ## this model string works!
    
    output_string <- "standardized tech1 tech4 tech10;"
    
    model_1_class <- mplusObject(
      TITLE = title_string,
      VARIABLE = variable_string,
      ANALYSIS = analysis_string,
      MODEL = model_string,
      OUTPUT = output_string,
      ## here still add which outputs are actually relevant
      usevariables = colnames(test_df), # alternative tech1 tech8;
      rdata = test_df
    )
    
    ## try this with one test df where you save the data before! 
    ## CONTINUE HERE!!!
    
    ## Issue: Model seems to be with correct syntax but takes forever to run
    
    fit_model_1_class <- mplusModeler(model_1_class,
                                dataout = here("mplus_files",
                                               paste0("model_", class_nr, "_class_", test_q_name, ".dat")),
                                # note: data needs to be given to model! 
                                # only solution seems to be to directly delete it 
                                # afterwards!
                                modelout = here("mplus_files",
                                                paste0("model_", class_nr, "_class_", test_q_name, ".inp")),
                                check = TRUE, run = TRUE, hashfilename = FALSE,
                                Mplus_command = "C:/Program Files/Mplus/Mplus.exe")
    ## note: if you run this on server, the filepath to the Mplus command also
    ## needs to be changed!
    ## CONTINUE HERE!!!
    
    ## removing the datafile that was newly created 
    data_files_after <- setdiff(list.files(here::here("mplus_files")), 
                                data_files_before)
    
    ## selecting .dat file contained in data_files_after
    dat_file <- data_files_after[grep("\\.dat$", data_files_after)]
    
    ## removing the .dat file from the directory 
    cat("Removing .dat file", "\n")
    file.remove(here("mplus_files", dat_file))
    
    ## saving name of .out file that was newly created
    out_file <- data_files_after[grep("\\.out$", data_files_after)]
    
    ## changing name of the newly created .out file in the directory
    ## to "out_file_new.out"
    cat("Renaming .out file", "\n")
    file.rename(here("mplus_files", out_file), 
                here("mplus_files",
                     paste0("model_", class_nr, "_class_", test_q_name, ".out")
                )
    )
    
    ## saving model fit to list
    list_models <- append(list_models, fit_model_1_class)
    ## still rename the saved element with the name of the CBCL question
    ## currently being iterated
  }
  
  else {
    ## here code mixture model with 2-4 classes (automatic starting values,
    ## use the material from the SEM classes)
    
    
  }
  
  
  
  
  
  ## idea: instead of loading all the model output into R, only read in AICs, 
  ## then select lowest, only load in this model with the readModels command
}


dummy_model_Mplus <- mplusObject(
  TITLE = title,
  VARIABLE = 
    "usevar = t1-t5 FamNr;
     categorical = t1-t5;
     cluster = FamNr;
     classes = c(2);",
  ANALYSIS = 
    "type = mixture complex;
     algorithm = integration;
     processors = 7;
     convergence = 0.01;
     miterations = 500;",
  MODEL = 
    "%overall%
   b0 by t1@1 t2@1 t3@1 t4@1 t5@1;
   b1 by t1@0 t2@1 t3@2 t4@3 t5@4;
   ![t1@0 t2@0 t3@0 t4@0 t5@0];
   !t1* t2* t3* t4* t5*
   b0*1;
   b1*.2;", ## no specification of starting values for the latent 
  ## classes
  OUTPUT = "sampstat standardized tech1 tech4 tech8;",
  usevariables = colnames(test_df), # alternative tech1 tech8;
  rdata = test_df[1:500,]
)

## Issue: Model seems to be with correct syntax but takes forever to run

fit_fam_mix <- mplusModeler(model_fam_mix,
                            dataout = here("mplus_files", "model_base.dat"),
                            # note: data needs to be given to model! 
                            # only solution seems to be to directly delete it 
                            # afterwards!
                            modelout = here("mplus_files", "model_fam_mix.inp"),
                            check = TRUE, run = TRUE, hashfilename = FALSE,
                            Mplus_command = "C:/Program Files/Mplus/Mplus.exe")







