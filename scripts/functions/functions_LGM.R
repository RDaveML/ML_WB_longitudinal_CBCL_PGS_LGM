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
               "stringr", "readxl", "data.table", "MplusAutomation", "glue")


## this function takes a dataframe and a CBCL question from a list 
## as input, filters the dataframe for only the relevant items 
## so that only those can be used for latent growth modeling
LGM_preprocess <- function(CBCL_question, df, questions_list, items_table){
  
  ## setting name of the question 
  question <- na.omit(as.character(filter(items_table, question_number == CBCL_question)))
  q_name <- question[length(question)]
  var_names_LGM <- unlist(unname(questions_list[q_name]))

  data_LGM <- df %>%
  select(FISNumber, FamilyNumber, twzyg, any_of(question),
         matches(paste0(question, "\\b"))) %>%
  rename("FISNr" = FISNumber, "FamNr" = FamilyNumber,
  ## renaming the variable that contains the string "n_measures" to "n_t"
         "n_t" = paste0("n_measures_", q_name))
## renaming so that variable names are max 8 characters long

colnames(data_LGM)[colnames(data_LGM) %in% var_names_LGM] <- 
  paste0("t", 1:length(var_names_LGM))


## Add number of family members for each participant
count_fam <- data_LGM[,c("FamNr","FISNr")] %>%
  count(FamNr) %>%
  rename("fam_count" = n)

data_LGM <- merge(data_LGM, count_fam, by = "FamNr")

## conversion of columns to proper factors 
data_LGM <- f_conv(df = data_LGM,
                  covariates = c("FamNr", "FISNr", "twzyg",
                                 paste0("t", 1:length(var_names_LGM))))

return(data_LGM)

## next up: testing this with various CBCL questions (run a few tests below)
  
}

LGM_preprocess <- function(CBCL_question, df, questions_list, items_table){
  
  ## setting name of the question 
  question <- na.omit(as.character(filter(items_table, question_number == CBCL_question)))
  q_name <- question[length(question)]
  var_names_LGM <- unlist(unname(questions_list[q_name]))

  data_LGM <- df %>%
  select(FISNumber, FamilyNumber, twzyg, any_of(question),
         matches(paste0(question, "\\b"))) %>%
  rename("FISNr" = FISNumber, "FamNr" = FamilyNumber,
  ## renaming the variable that contains the string "n_measures" to "n_t"
         "n_t" = paste0("n_measures_", q_name))
## renaming so that variable names are max 8 characters long

colnames(data_LGM)[colnames(data_LGM) %in% var_names_LGM] <- 
  paste0("t", 1:length(var_names_LGM))


## Add number of family members for each participant
count_fam <- data_LGM[,c("FamNr","FISNr")] %>%
  count(FamNr) %>%
  rename("fam_n" = n)

data_LGM <- merge(data_LGM, count_fam, by = "FamNr")

## conversion of columns to proper factors 
data_LGM <- f_conv(df = data_LGM,
                  covariates = c("FamNr", "FISNr", "twzyg",
                                 paste0("t", 1:length(var_names_LGM))))

return(data_LGM)

## still one more thing: the name of the question should be kept in the loop
## over all CBCL questions because it needs to appear in the model title 
## and also in the features that will be created from the modeling!

## next up: testing this with various CBCL questions (run a few tests below)

  
}


## cleaning up the directory after every Mplus model run: Mplus by default saves
## .dat, .inp and .out file, removing .dat file, renaming .out file 
## if it should be saved

## I might not need this, only if I want to clean up the mplus_files directory
file_clean_mplus <- function(files_before, files_after, class_nr = NULL,
                             question_name){
  ## removing the datafile that was newly created 
  data_files_after <- setdiff(files_after, data_files_before)
  
  ## selecting .dat file contained in data_files_after
  dat_file <- data_files_after[grep("\\.dat$", data_files_after)]
  
  ## removing the .dat file from the directory 
  cat("Removing .dat file", "\n")
  file.remove(here("mplus_files", dat_file))
  
  ## saving name of .out file that was newly created
  #out_file <- data_files_after[grep("\\.out$", data_files_after)]
  
  ## changing name of the newly created .out file in the directory
  ## to "out_file_new.out"
  #cat("Renaming .out file", "\n")
  #file.rename(here("mplus_files", out_file), 
  #            here("mplus_files",
  #                 paste0("model_", class_nr, "_class_", question_name, ".out")
  #            )
  #)
  
  return(invisible(NULL))
}
  
LGM_1_CBCL <- function(df, CBCL_question){
  
  var_names_LGM <- unlist(unname(CBCL_questions_list[CBCL_question]))
  title_string <- paste0("1-class model CBCL_question ", CBCL_question)
  
  ## CONTINUE HERE!!!
  variable_string <- paste0(
      "usevar = t1","-", paste0("t", length(var_names_LGM))," FamNr;\n",
      "categorical = t1", "-", paste0("t", length(var_names_LGM),";\n"),
      "cluster = FamNr;" ##classes = c(2);
    )
  
  analysis_string <- paste0(
    "type = complex;\n",
    "estimator = mlr;\n",
    "link = probit;\n",
    "algorithm = integration;"
  )
  
  if(length(var_names_test) == 4){ 
    model_string <- paste0(
      "i by t1@0 t2* t3* t4@1;\n",
      "s by t1@0 t2* t3* t4@1;\n", 
      "! Fix one threshold to identify location (this is the anchor);\n",
      "[t1$1@-1];\n",
      "! Constrain second thresholds equal across time (partial invariance);\n",
      "[t1$2] (th2);\n",
      "[t2$2] (th2);\n",
      "[t3$2] (th2);\n",
      "[t4$2] (th2);\n",
      "! Let remaining first thresholds be freely estimated;\n",
      "[t2$1];\n",
      "[t3$1];\n",
      "[t4$1];\n",
      "! Freely estimate latent means;\n",
      "[i];\n",
      "[s];\n",
      "i WITH s@0;"
    )
  } else if(length(var_names_test) == 5){ 
    model_string <- paste0(
      "i by t1@0 t2* t3* t4* t5@1;\n",
      "s by t1@0 t2* t3* t4* t5@1;\n", 
      "! Fix one threshold to identify location (this is the anchor);\n",
      "[t1$1@-1];\n",
      "! Constrain second thresholds equal across time (partial invariance);\n",
      "[t1$2] (th2);\n",
      "[t2$2] (th2);\n",
      "[t3$2] (th2);\n",
      "[t4$2] (th2);\n",
      "[t5$2] (th2);\n",
      "! Let remaining first thresholds be freely estimated;\n",
      "[t2$1];\n",
      "[t3$1];\n",
      "[t4$1];\n",
      "[t5$1];\n",
      "! Freely estimate latent means;\n",
      "[i];\n",
      "[s];\n",
      "i WITH s@0;"
    )
  } else if(length(var_names_test) == 6){ 
    model_string <- paste0(
      "i by t1@0 t2* t3* t4* t5* t6@1;\n",
      "s by t1@0 t2* t3* t4* t5* t6@1;\n", 
      "! Fix one threshold to identify location (this is the anchor);\n",
      "[t1$1@-1];\n",
      "! Constrain second thresholds equal across time (partial invariance);\n",
      "[t1$2] (th2);\n",
      "[t2$2] (th2);\n",
      "[t3$2] (th2);\n",
      "[t4$2] (th2);\n",
      "[t5$2] (th2);\n",
      "[t6$2] (th2);\n",
      "! Let remaining first thresholds be freely estimated;\n",
      "[t2$1];\n",
      "[t3$1];\n",
      "[t4$1];\n",
      "[t5$1];\n",
      "[t6$1];\n",
      "! Freely estimate latent means;\n",
      "[i];\n",
      "[s];\n",
      "i WITH s@0;"
    )
  } else {
    model_string <- paste0(
      "i by t1@0 t2* t3* t4* t5* t6* t7@1;\n",
      "s by t1@0 t2* t3* t4* t5* t6* t7@1;\n", 
      "! Fix one threshold to identify location (this is the anchor);\n",
      "[t1$1@-1];\n",
      "! Constrain second thresholds equal across time (partial invariance);\n",
      "[t1$2] (th2);\n",
      "[t2$2] (th2);\n",
      "[t3$2] (th2);\n",
      "[t4$2] (th2);\n",
      "[t5$2] (th2);\n",
      "[t6$2] (th2);\n",
      "[t7$2] (th2);\n",
      "! Let remaining first thresholds be freely estimated;\n",
      "[t2$1];\n",
      "[t3$1];\n",
      "[t4$1];\n",
      "[t5$1];\n",
      "[t6$1];\n",
      "[t7$1];\n",
      "! Freely estimate latent means;\n",
      "[i];\n",
      "[s];\n",
      "i WITH s@0;"
    )
  }
  
  output_string <- "standardized tech1 tech4 tech10;"
  ## adapt if other output features needed
  
  model_1_class <- mplusObject(
    TITLE = title_string,
    VARIABLE = variable_string,
    ANALYSIS = analysis_string,
    MODEL = model_string,
    OUTPUT = output_string,
    ## here still add which outputs are actually relevant
    usevariables = colnames(var_names_LGM), # alternative tech1 tech8;
    rdata = df
  )
  
  fit_model_1_class <- mplusModeler(
    model_1_class,
    dataout = here(
      "mplus_files",
      paste0("model_1_class_", CBCL_question, ".dat")
    ),
    # note: data needs to be given to model!
    # only solution seems to be to directly delete it
    # afterwards!
    modelout = here(
      "mplus_files",
      paste0("model_1_class_", CBCL_question, ".inp")
    ),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE,
    Mplus_command = "C:/Program Files/Mplus/Mplus.exe"
  )
  
  
}
## Great! This function works!

## Next: Code LCGA function 2-4 classes after you saw the 2-3 class model being
## replicated correctly, then go further in workflow of looped function in the 
## 07 script
## The functions I need are `createMixtures` and `runModels`

GMM_2_4_CBCL <- function(df, CBCL_question){
  
  var_names_LGM <- unlist(unname(CBCL_questions_list[CBCL_question]))
  #title_string <- paste0(class_nr, "-class model CBCL_question ", CBCL_question)
  
  ## CONTINUE HERE!!!
  variable_string <- paste0(
    "usevar = t1","-", paste0("t", length(var_names_LGM))," FamNr;\n",
    "categorical = t1", "-", paste0("t", length(var_names_LGM),";\n"),
    "cluster = FamNr;"
  )
  
  ## Here still find out how this behaves when the model is run by
  ## createMixtures and runModels
  savedata_string <- paste0("
          SAVE = CPROBABILITIES;\n", 
          "FILE = probs_", CBCL_question, ".dat;")
  
  analysis_string <- paste0(
    "type = complex;\n",
    "estimator = mlr;\n",
    "link = probit;\n",
    "algorithm = integration;\n"
  )
  
  if(length(var_names_test) == 4){ 
    model_string <- paste0(
      "i by t1@0 t2* t3* t4@1;\n",
      "s by t1@0 t2* t3* t4@1;\n", 
      "! Fix one threshold to identify location (this is the anchor);\n",
      "[t1$1@-1];\n",
      "! Constrain second thresholds equal across time (partial invariance);\n",
      "[t1$2] (th2);\n",
      "[t2$2] (th2);\n",
      "[t3$2] (th2);\n",
      "[t4$2] (th2);\n",
      "! Let remaining first thresholds be freely estimated;\n",
      "[t2$1];\n",
      "[t3$1];\n",
      "[t4$1];\n",
      "! Freely estimate latent means;\n",
      "[i];\n",
      "[s];\n",
      "i WITH s@0;"
    )
  } else if(length(var_names_test) == 5){ 
    model_string <- paste0(
      "i by t1@0 t2* t3* t4* t5@1;\n",
      "s by t1@0 t2* t3* t4* t5@1;\n", 
      "! Fix one threshold to identify location (this is the anchor);\n",
      "[t1$1@-1];\n",
      "! Constrain second thresholds equal across time (partial invariance);\n",
      "[t1$2] (th2);\n",
      "[t2$2] (th2);\n",
      "[t3$2] (th2);\n",
      "[t4$2] (th2);\n",
      "[t5$2] (th2);\n",
      "! Let remaining first thresholds be freely estimated;\n",
      "[t2$1];\n",
      "[t3$1];\n",
      "[t4$1];\n",
      "[t5$1];\n",
      "! Freely estimate latent means;\n",
      "[i];\n",
      "[s];\n",
      "i WITH s@0;"
    )
  } else if(length(var_names_test) == 6){ 
    model_string <- paste0(
      "i by t1@0 t2* t3* t4* t5* t6@1;\n",
      "s by t1@0 t2* t3* t4* t5* t6@1;\n", 
      "! Fix one threshold to identify location (this is the anchor);\n",
      "[t1$1@-1];\n",
      "! Constrain second thresholds equal across time (partial invariance);\n",
      "[t1$2] (th2);\n",
      "[t2$2] (th2);\n",
      "[t3$2] (th2);\n",
      "[t4$2] (th2);\n",
      "[t5$2] (th2);\n",
      "[t6$2] (th2);\n",
      "! Let remaining first thresholds be freely estimated;\n",
      "[t2$1];\n",
      "[t3$1];\n",
      "[t4$1];\n",
      "[t5$1];\n",
      "[t6$1];\n",
      "! Freely estimate latent means;\n",
      "[i];\n",
      "[s];\n",
      "i WITH s@0;"
    )
  } else {
    model_string <- paste0(
      "i by t1@0 t2* t3* t4* t5* t6* t7@1;\n",
      "s by t1@0 t2* t3* t4* t5* t6* t7@1;\n", 
      "! Fix one threshold to identify location (this is the anchor);\n",
      "[t1$1@-1];\n",
      "! Constrain second thresholds equal across time (partial invariance);\n",
      "[t1$2] (th2);\n",
      "[t2$2] (th2);\n",
      "[t3$2] (th2);\n",
      "[t4$2] (th2);\n",
      "[t5$2] (th2);\n",
      "[t6$2] (th2);\n",
      "[t7$2] (th2);\n",
      "! Let remaining first thresholds be freely estimated;\n",
      "[t2$1];\n",
      "[t3$1];\n",
      "[t4$1];\n",
      "[t5$1];\n",
      "[t6$1];\n",
      "[t7$1];\n",
      "! Freely estimate latent means;\n",
      "[i];\n",
      "[s];\n",
      "i WITH s@0;"
    )
  }
  
  output_string <- "standardized tech1 tech4 tech10;"
  ## adapt if other output features needed
  
  createMixtures(classes = 2:4, filename_stem = CBCL_question,                                    
                 rdata = df,
                 ANALYSIS = analysis_string,   
                 VARIABLE = variable_string)  
  
  
  ## Continue here, first only check if create mixtures runs successfully
  
  ## title string still needs to be adjusted
  
  ## you can very well do 1-4 classes
  
  ## tech output options (use function help)
  
  ## also include the file_clean_mplus function in this function! Can be coded 
  ## inside, define list_before and list_after inside this function

  
  
}


LCGA_1_4_CBCL <- function(df, CBCL_question){
  
  ## create subdirectory for this specific CBCL question where is 
  ## being worked in 
  
  dir_CBCL_question <- here::here("mplus_files", CBCL_question)
  if(!dir.exists(here::here("mplus_files", CBCL_question))){
    dir.create(here::here("mplus_files", CBCL_question))
  }
  
  ## change to the subdirectory
  setwd(dir_CBCL_question)
  
  ## create list of files in the subdirectory (repeat this later before 
  ## the file decluttering)
  
  var_names_LGM <- unlist(unname(CBCL_questions_list[CBCL_question]))
  title_string <- paste0("class model CBCL_question_", CBCL_question)
  
  variable_string <- paste0(
    "usevar = t1","-", paste0("t", length(var_names_LGM))," FamNr;\n",
    "categorical = t1", "-", paste0("t", length(var_names_LGM),";\n"),
    "cluster = FamNr;\n"
  )
  
  ## Here still find out how this behaves when the model is run by
  ## createMixtures and runModels
  savedata_string <- paste0("
          SAVE = CPROBABILITIES;\n", 
                            "FILE = probs_", CBCL_question, ".dat;")
  
  analysis_string <-   analysis_string <- paste0(
    "type = mixture complex;\n",
    "estimator = mlr;\n",
    "link = probit;\n",
    "!algorithm = integration;\n"
  )
  
  
  if(length(var_names_test) == 4){ 
    model_string <- paste0(
      "i by t1@0 t2* t3* t4@1;\n",
      "s by t1@0 t2* t3* t4@1;\n")
    ## adjust this depending on output of time varying analysis
  } else if(length(var_names_test) == 5){ 
    model_string <- paste0(
      "i by t1@0 t2* t3* t4* t5@1;\n",
      "s by t1@0 t2* t3* t4* t5@1;\n")
  } else if(length(var_names_test) == 6){ 
    model_string <- paste0(
      "i by t1@0 t2* t3* t4* t5* t6@1;\n",
      "s by t1@0 t2* t3* t4* t5* t6@1;\n")
  } else {
    model_string <- paste0(
      "i by t1@0 t2* t3* t4* t5* t6* t7@1;\n",
      "s by t1@0 t2* t3* t4* t5* t6* t7@1;\n")
  }
  
  output_string <- paste0(
    "tech1 tech4 tech7 tech8 tech14;\n")
  ## adapt if other output features needed
  
  ## likely not needed anymore since I work in dedicated directory
  ## files_before_mixture <- list.files()
  
  ## still add saving the output, for now, I just want to find out 
  ## if the decluttering works
  classes <- 1:4
  mix_out_console <- capture.output(
  createMixtures(classes = classes, filename_stem = CBCL_question,                                    
                 rdata = data_test_LGM_1, ## df
                 VARIABLE = variable_string,
                 ANALYSIS = analysis_string,
                 model_overall =  model_string,   
                 OUTPUT = output_string,
                 SAVEDATA = paste0(
                   "FILE = {filename_stem}_{C}.dat;\n",
                   "SAVE = CPROBABILITIES;"),
                 quiet = FALSE) 
  )
  ## run results in error:
  ## Error in check_mixtures(modelList) : 
    ## mixtureSummaryTable requires a list of mixture models as its first argument.
  ## also the .dat files for the class assignments and probabilities 
  ## are not created
  
  ## removing the newly created data file, it is not needed anymore
  file.remove(grep(list.files(), 
                   pattern = paste0("data_", CBCL_question),
                   value = TRUE))
  
  ## move all .dat files in current directory that contain
  ## CBCL_question and a number in their filename connected by an 
  ## underscore and move them to the directory "cprobabilities"
  list_dat_files <- grep(list.files(), 
               pattern = paste0(CBCL_question, "_\\d+\\.dat$"),
               value = TRUE)
  
  list_inp_files <- grep(list.files(), 
                         pattern = paste0(CBCL_question, "_\\d+\\.inp$"),
                         value = TRUE)
  
  list_out_files <- grep(list.files(), 
                         pattern = paste0(CBCL_question, "_\\d+\\.out$"),
                         value = TRUE)
  
  
  ## here insert the model selection part!
  
  ## reading output from the mixture modeling
  ## it is always the last classes + 1 lines
  length_mix_out_console <- length(mix_out_console)
  mix_sum_table <- 
    mix_out_console[
      (length(mix_out_console) - length(classes)):length(mix_out_console)]
  mix_sum_table
  ## turn this into dataframe (first line should be column names)
  ## CONTINUE HERE!! 
  ## before continuing, change working directory!
  
  ## alternative might be mixtureSummaryTable() function?
  
  ## for now: Only use minimal group size and BIC as filters for choosing the 
  ## model!
  
  ## expand the dataframe with the filenames
  mix_sum_table$dat_file <- list_dat_files
  mix_sum_table$inp_file <- list_inp_files
  mix_sum_table$out_file <- list_out_files
  
  ## for now: Only use minimal group size and BIC as filters for choosing the 
  ## model!
  
  
  ## adapt the list so that the correct file is chosen (setdiff)
  ## this comes only after the model selection!
  file.copy(from = paste0(getwd(), "/", list_dat_files),
               to = paste0(here::here("mplus_files", "cprobabilities"), "/",
                           list_dat_files) 
              )
  
  file.remove(from = paste0(getwd(), "/", list_dat_files)) 

  
  ## Next steps (track those in Obsidian!): 
  
  ## make function so that for every CBCL question, an own directory is created
  ## move all output files that were created there
  
  ## - file moving and cleaning works with 1 example?
  
  ## access createModels output that was written to file:
  ## get df with model info, extract AIC / BIC, entropy, minimal group size
  
  ## choose model with optimal fit, delete .inp, .out and probabilities.dat for
  ## all other models
  
  ## read the output of the model that was best
  
  ## save all parameters
  
  ## code new model: exactly the same as the .inp for the optimal model, 
  ## but on test data instead of training data and all parameter values are
  ## fixed to the ones from the optimal model
  
  ## make sure that all those operations are happening inside the specific 
  ## subdirectory
  
  ## combine model output and probabilities file to new dataframe that contains 
  ## FISNumbers 
  ## append to large overall dataframe (reduce function)
  
  ## cleaning function: several files are not needed anymore, remove them
  
  
  
}


LGM_DV_calculation <- function(df, CBCL_question){
  
  
}


LGM_estimation_grid <- function(df, configuration_grid){
  
  
  
}

## ----------------------------------------------------------------------------

## testing area (leave this in!)
data_test_LGM_80 <- LGM_preprocess(
  CBCL_question = names(CBCL_questions_list)[[80]],
  df = train_data,
  questions_list = CBCL_questions_list,
  items_table = CBCL_items_table_reduced)

View(data_test_LGM_80)


class_1_test_1 <- LGM_1_CBCL(data_test_LGM_1, names(CBCL_questions_list)[[1]])
## works! means and variances of intercepts are estimated!
class_1_test_80 <- LGM_1_CBCL(data_test_LGM_80, names(CBCL_questions_list)[[80]])

LCGA_test_1 <- LCGA_2_4_CBCL(data_test_LGM_1, names(CBCL_questions_list)[[1]])

## question: Can you suppress the printing output of the mPlusmodeler function?





