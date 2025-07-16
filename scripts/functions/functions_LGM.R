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

source(here::here("scripts", "functions", "functions_ml.R"))


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

## function to select the correct age variables for a specific CBCL question
## returns character vector of correct 
## CBCL_age_df is loaded into Script 07 before executing
age_var_select <- function(CBCL_age_df, CBCL_question){
  age_vars <- CBCL_age_df %>%
    select(all_of(c(CBCL_question, "age_var"))) %>%
    filter(!is.na(!!sym(CBCL_question))) %>%
    select(age_var) %>%
    pull()
  return(age_vars)
}

## Data preprocessing function: variables that belong to specific
## CBCL question are selected plus relevant age variables and data is being
## proprocessed for Mplus modeling
LGM_preprocess <- function(CBCL_age_df, 
                           CBCL_question, df, questions_list, items_table){
  
  ## setting name of the question 
  question <- na.omit(as.character(filter(items_table, question_number == CBCL_question)))
  q_name <- question[length(question)]
  var_names_LGM <- unlist(unname(questions_list[q_name]))
  age_vars_LGM <- age_var_select(CBCL_age_df = CBCL_age_df,
                                 CBCL_question = CBCL_question)

  data_LGM <- df %>%
  select(FISNumber, FamilyNumber, twzyg, any_of(question),
         matches(paste0(question, "\\b")), all_of(age_vars_LGM)) %>%
  rename("FISNr" = FISNumber, "FamNr" = FamilyNumber,
  ## renaming the variable that contains the string "n_measures" to "n_t"
         "n_t" = paste0("n_measures_", q_name))

  ## Here, it also needs to be added which of the age variables need to be 
  ## kept!
  
  ## check here which variables are the correct ones and if I need to 
  ## calculate age via the birthdate and the invulate
    
  ## renaming so that variable names are max 8 characters long

  ## Ordinal items (to t)  
  colnames(data_LGM)[colnames(data_LGM) %in% var_names_LGM] <- 
    paste0("t", 1:length(var_names_LGM))

  ## age variables (to ts, TimeScores)
  ## age variables will be used as time score variables
  colnames(data_LGM)[colnames(data_LGM) %in% age_vars_LGM] <- 
    paste0("ts", 1:length(age_vars_LGM))


## Add number of family members for each participant
count_fam <- data_LGM[,c("FamNr","FISNr")] %>%
  count(FamNr) %>%
  rename("fam_n" = n)

data_LGM <- merge(data_LGM, count_fam, by = "FamNr")

## conversion of columns to proper factors 
data_LGM <- f_conv(df = data_LGM,
                  covariates = c("FamNr", "FISNr", "twzyg",
                                 paste0("t", 1:length(var_names_LGM))))

data_LGM <- data_LGM %>%
  mutate(across(starts_with("ts"), ~ .x / 3))


return(data_LGM)

## next up: testing this with various CBCL questions (run a few tests below)

  
}


## cleaning up the directory after every Mplus model run: Mplus by default saves
## .dat, .inp and .out file, removing .dat file, renaming .out file 
## if it should be saved

## I might not need this, only if I want to clean up the mplus_files directory
file_clean_mplus <- function(files_before, files_after, question_name){
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
  


## Function that extracts parameters from an LCGA model and 
## creates model syntax with those parameters fixed 
generate_fixed_syntax_with_classes <- function(params) {
  fixed_lines <- list()
  
  # Separate out shared parameters (OVERALL block) vs class-specific
  if ("LatentClass" %in% names(params)) {
    params_overall <- params[is.na(params$LatentClass), ]
    params_class <- params[!is.na(params$LatentClass), ]
  } else {
    params_overall <- params
    params_class <- NULL
  }
  
  ### Helper to create fixed syntax from a subset of parameters
  create_lines <- function(param_subset) {
    lines <- c()
    
    for (i in seq_len(nrow(param_subset))) {
      row <- param_subset[i, ]
      header <- row$paramHeader
      param <- row$param
      est <- round(row$est, 5)
      
      # Skip latent class probabilities (e.g., Means for C1#1, etc.)
      if (header == "Means" && grepl("^C\\d+#\\d+", param)) next
      
      if (header == "Thresholds") {
        lines <- c(lines, paste0("[", param, "@", est, "]",";"))
      } else if (header %in% c("I.BY", "S.BY")) {
        factor_name <- sub("\\.BY", "", header)
        lines <- c(lines, paste0(factor_name, " BY ", param, " @", est, ";"))
      } else if (header %in% c("Means", "Intercepts")) {
        lines <- c(lines, paste0("[", param, "@",  est, "]",";"))
      } else if (header == "Variances") {
        lines <- c(lines, paste0(param, " @", est, ";"))
      } else if (header == "Regressions") {
        lines <- c(lines, paste0(param, " ON ", row$param2, " @", est, ";"))
      } else if (header == "Residual.Variances") {
        lines <- c(lines, paste0(param, " @", est, ";"))
      } else if (header == "Loadings") {
        lines <- c(lines, paste0(row$paramHeader, " BY ", param, " @", est, ";"))
      }
    }
    
    return(lines)
  }
  
  # OVERALL block (parameters shared across classes)
  overall_lines <- create_lines(params_overall)
  if (length(overall_lines) > 0) {
      fixed_lines[["%OVERALL%"]] <- overall_lines
  }
  
  # Class-specific blocks
  if (!is.null(params_class)) {
    class_labels <- sort(unique(params_class$LatentClass))
    
    for (class_label in class_labels) {
      class_data <- params_class[params_class$LatentClass == class_label, ]
      class_lines <- create_lines(class_data)
      block_name <- paste0("%C#", class_label, "%")
      fixed_lines[[block_name]] <- class_lines
    }
  }
  
  # Collapse all blocks into one Mplus-compatible string
  syntax_out <- unlist(
    lapply(names(fixed_lines), function(block) {
      c(block, fixed_lines[[block]], "")  # add empty line for spacing
    })
  )

  ## remove the line that read "%OVERALL%" from syntax_out
  ## if "LatentClass" is not in params
  if (!"LatentClass" %in% names(params)) {
    syntax_out <- syntax_out[!grepl("^%OVERALL%", syntax_out)]
  } 
  
  return(paste(syntax_out, collapse = "\n"))
}


LGM_1_4_CBCL <- function(df_train, df_test, CBCL_question){
  
  
  ## the name of the question should be kept in the loop
  ## over all CBCL questions because it needs to appear in the model title 
  ## and also in the features that will be created from the modeling!
  
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
    "usevar = t1","-", paste0("t", length(var_names_LGM)),
    " ts1","-", paste0("ts", length(var_names_LGM)), " FamNr;\n",
    "categorical = t1", "-", paste0("t", length(var_names_LGM),";\n"),
    "TSCORES = ts1", "-", paste0("ts", length(var_names_LGM), ";\n"),
    "cluster = FamNr;\n"
  )
  
  ## Here still find out how this behaves when the model is run by
  ## createMixtures and runModels
  savedata_string <- paste0("
          SAVE = CPROBABILITIES;\n", 
          "FILE = probs_", CBCL_question, ".dat;")
  
  analysis_string <- paste0(
    "type = mixture random complex;\n",
    "estimator = mlr;\n",
    "link = probit;\n",
    "algorithm = integration;\n"
  )
  
  
  
  if(length(var_names_LGM) == 4){ 
    model_string <- paste0(
      "i s | t1-t4 AT ts1-ts4;\n",
      "i@0;\n",
      "s@0;\n",
      "i WITH s@0;\n")
    ## adjust this depending on output of time varying analysis
  } else if(length(var_names_LGM) == 5){ 
    model_string <- paste0(
      "i s | t1-t5 AT ts1-ts5;\n",
      "i@0;\n",
      "s@0;\n",
      "i WITH s@0;\n")
  } else if(length(var_names_LGM) == 6){ 
    model_string <- paste0(
      "i s | t1-t6 AT ts1-ts6;\n",
      "i@0;\n",
      "s@0;\n",
      "i WITH s@0;\n")
  } else if(length(var_names_LGM) == 7){
    model_string <- paste0(
      "i s | t1-t7 AT ts1-ts7;\n",
      "i@0;\n",
      "s@0;\n",
      "i WITH s@0;\n")
  } else {
    cat(paste0("CBCL question ",
               CBCL_question,
               " skipped, incorrect number of measures"))
    return(list(df_CBCL_out <- NULL,
                n_class = NULL))
  }
  
  output_string <- paste0(
    "tech1 tech7 tech8;\n")
  ## adapt if other output features needed
  
  ## likely not needed anymore since I work in dedicated directory
  ## files_before_mixture <- list.files()
  
  ## still add saving the output, for now, I just want to find out 
  ## if the decluttering works
  classes <- 1:4
  mix_out_console <- capture.output(
  createMixtures(classes = classes, filename_stem = CBCL_question,                                    
                 rdata = df_train,
                 VARIABLE = variable_string,
                 ANALYSIS = analysis_string,
                 model_overall =  model_string,   
                 OUTPUT = output_string,
                 SAVEDATA = paste0(
                   "FILE = {filename_stem}_{C}_train_cprobs.dat;\n",
                   "SAVE = CPROBABILITIES;"),
                #SAVEDATA = paste0(
                #   "FILE = {filename_stem}_{C}.dat;\n",
                #   "SAVE = CPROBABILITIES;"),
                 quiet = FALSE), 
  file = paste0(getwd(), "/", "output_LCGA.txt"))
  
  
  ## removing the newly created data file, it is not needed anymore
  file.remove(grep(pattern = paste0("data_", CBCL_question),
                   list.files(),
                   value = TRUE))
  
  ## move all .dat files in current directory that contain
  ## CBCL_question and a number in their filename connected by an 
  ## underscore and move them to the directory "cprobabilities"
  dat_files <- grep(pattern = "train_cprobs.dat$",
                    list.files(),
                    value = TRUE)
  
  inp_files <- grep(pattern = "class.inp$",
                    list.files(),
                    value = TRUE)
  
  ## .out files somehow always make the CBCL question lowercase, 
  ## take this into account here
  out_files <- grep(pattern = "class.out$",
                    list.files(),
                    value = TRUE)
  
  
  ## here insert the model selection part!
  
  ## reading output from the mixture modeling
  ## it is always the last classes + 1 lines
  #length_mix_out_console <- length(mix_out_console)
  #mix_sum_table <- 
  #  mix_out_console[
  #    (length(mix_out_console) - length(classes)):length(mix_out_console)]
  # mix_sum_table
  ## turn this into dataframe (first line should be column names)

  ## mix_sum_table is now 5 lines of characters. It should be a dataframe
  ## where the first line is the column names and the rest are the values
  #df_mix_sum_table <- read.table(text = mix_sum_table[2:length(mix_sum_table)],
  #                               header = FALSE,
  #                               stringsAsFactors = FALSE)
  
  ## CONTINUE HERE!!! find out how to turn this into proper table
  ## then this should work because the parallel worker always reads within
  ## a specific directory an actually existing file, the console output
  ## is then irrelevant!
  #df_mix_sum_table <- data.frame()
  
  ## reading in summary output from file where it was saved, 
  ## only read in lines that refer to the models
  lines_mix_sum <- readLines("output_LCGA.txt")
  #length(lines_mix_sum)
  ## save line number of the line that contains both 
  ## "Title" and "Classes"
  lines_mix_sum <- lines_mix_sum[
    grep("Title.*Classes", lines_mix_sum):length(lines_mix_sum)]
  
  ## Re-appending truncated column back to table like output
  ## function written by Chat-GPT
  reconstruct_table <- function(lines) {
    # Step 1: Identify the header and data rows
    header_line <- lines[1]
    
    # Step 2: Identify where the split column starts
    split_col_header_index <- which(grepl("^\\s*max_prob\\s*$", lines))
    if (length(split_col_header_index) == 0) stop("No split column header found.")
    
    # Step 3: Extract main data and split column values
    main_data_lines <- lines[2:(split_col_header_index - 1)]
    split_col_lines <- lines[(split_col_header_index + 1):length(lines)]
    
    # Step 4: Extract only the value part from split_col_lines
    split_values <- sapply(strsplit(split_col_lines, "\\s+"), function(x) tail(x, 1))
    
    # Step 5: Combine values with each main data line
    combined_lines <- mapply(function(main, val) paste(main, val), main_data_lines, split_values)
    
    # Step 6: Add a placeholder column name for the row index
    full_header <- paste("Row", header_line, "max_prob")
    
    # Step 7: Combine header and data
    final_lines <- c(full_header, combined_lines)
    return(final_lines)
  }
  
  
  
  # Usage
  fixed_lines <- reconstruct_table(lines_mix_sum)
  
  # Print to confirm structure
  #cat(paste(fixed_lines, collapse = "\n"))
  
  # Normalize and parse to data.frame
  #fixed_lines_clean <- gsub("^\\s+|\\s+$", "", fixed_lines)
  #fixed_lines_clean <- gsub("\\s{2,}", " ", fixed_lines_clean)
  #df_mix_sum_table <- read.table(
  #  text = fixed_lines_clean, header = TRUE, stringsAsFactors = FALSE)
  
  df_mix_sum_table <- read.table(
    text = fixed_lines, header = TRUE, stringsAsFactors = FALSE)
  
  # Show result
  #View(df_mix_sum_table)
  
  
  #lines_mix_sum <- 
  #  lines_mix_sum[(length(lines_mix_sum) - 
   #                     length(classes) + 1):length(lines_mix_sum)]
  #df_mix_sum_table
  #for(line in 1:length(lines_mix_sum)){
      ## steps to do with each line of lines_mix_sum:
  ## 1) separate all elements that are separated by space into distinct elements of a vector
  ## 2) add each vector as new line of the data frame
  #  line_split <- strsplit(lines_mix_sum[line], " ")[[1]]
    ## remove empty elements from line_split
  #  line_split <- line_split[line_split != ""]
    
    ## drop unnecessary first element
  #  line_split <- line_split[-1]
    
    ## add the line to the dataframe
  #  df_mix_sum_table <- rbind(df_mix_sum_table, line_split)
  #}
  
  
  df_mix_sum_table <- df_mix_sum_table %>%
    mutate(Title = paste(df_mix_sum_table$Row, df_mix_sum_table$Title,
                         sep = " ")) %>%
    select(-Row)
  
  ## expand the dataframe with the filenames
  df_mix_sum_table$dat_file <- dat_files
  df_mix_sum_table$inp_file <- inp_files
  df_mix_sum_table$out_file <- out_files
  ## save the model summary table in directory!
  ## CONTINUE HERE!!!
  saveRDS(df_mix_sum_table, file = paste0("summary_LCGA_mixture_", 
                                          CBCL_question, ".rds"))

  
  ## Entropy should be higher than .60 
  ## (middle ground, Bleidorn et al., 2009)
  model_opt <- df_mix_sum_table %>%
    filter(min_N >= 0.1 * nrow(df_train)) %>%
    filter(is.na(Entropy) | Entropy >= 0.6)
  ## discarding models with entropy < .6
    
  model_opt <- model_opt %>%
    filter(BIC == min(model_opt$BIC)) ## selecting model with lowest BIC
  
  opt_n_class <- model_opt$Classes
  
  ## optional: If optimal level is 1-class LCGA keep files (to be coded here
  ## if necessary)
  
  
  ## insert here: block that codes multilevel model
  ## LGM with individual variation allowed on both training and test set?!
  if(opt_n_class == 1){
    
    ## delete files of previous modelling, keeping console output file
    ## and summary table of mixture modelling and 1-class modelling Mplus files
    files_opt_model <- c(model_opt$inp_file,
                         model_opt$out_file)
    
    file.remove(from = paste0(getwd(), "/", 
                              setdiff(list.files(),
                                      c(files_opt_model,
                                        "output_LCGA.txt",
                                        paste0("summary_LCGA_mixture_",
                                               CBCL_question, ".rds")))))
    
        title_string_LGM <- paste0("Multilevel LGM model CBCL_question_",
                                   CBCL_question)
    
    ## adapt model and analysis string to create Multilevel LGM
    ## (every individual has own slope and intercept, no mixture)
    analysis_string_LGM <- paste0(
      "type = random complex;\n",
      "estimator = mlr;\n",
      "link = probit;\n",
      "algorithm = integration;\n"
    )
    
    if(length(var_names_LGM) == 4){ 
      model_string_LGM <- paste0(
        "i s | t1-t4 AT ts1-ts4;\n")
      ## adjust this depending on output of time varying analysis
    } else if(length(var_names_LGM) == 5){ 
      model_string_LGM <- paste0(
        "i s | t1-t5 AT ts1-ts5;\n")
    } else if(length(var_names_LGM) == 6){ 
      model_string_LGM <- paste0(
        "i s | t1-t6 AT ts1-ts6;\n")
    } else if(length(var_names_LGM) == 7){
      model_string_LGM <- paste0(
        "i s | t1-t7 AT ts1-ts7;\n")
    } else {
      cat(paste0("CBCL question ",
                 CBCL_question,
                 " skipped, incorrect number of measures, returning NULL"))
      return(list(df_CBCL_out = NULL,
                  n_class = NULL))
    }
    
    ## code model training set
    model_LGM_train <- mplusObject(
      TITLE = paste0(title_string_LGM, " (training set)"),
      VARIABLE = variable_string,
      ANALYSIS = analysis_string_LGM,
      MODEL = model_string_LGM,
      OUTPUT = output_string,
      SAVEDATA = paste0(
        "FILE = ", CBCL_question, "_", "I_S_scores_train.dat;\n",
        "SAVE = FSCORES;"),
      usevariables = colnames(var_names_LGM),
      rdata = df_train
    )
    
    fit_LGM_train <- mplusModeler(
      model_LGM_train,
      dataout = paste0("model_train_LGM_", CBCL_question, ".dat"),
      # note: data needs to be given to model!
      # only solution seems to be to directly delete it
      # afterwards!
      modelout = paste0("model_train_LGM_", CBCL_question, ".inp"),
      check = TRUE,
      run = TRUE,
      hashfilename = FALSE,
      Mplus_command = detectMplus() #"C:/Program Files/Mplus/Mplus.exe"
    )

    
    ## extract model parameters to fix them in test set
    model_LGM_train_info <- readModels(target = getwd(), what = "all")
    
    par_fixed_LGM_test <- paste0(model_string_LGM, "\n",
                                 generate_fixed_syntax_with_classes(
                                 model_LGM_train_info$parameters$unstandardized)
                                )

    ## special case: It can occur that the value 2 does not occur in a specific
    ## variable in the test set!
    
    ## in this case:
    ## check all variables in df_test where the column name
    ## begins with "t" followed by a number
    ## if not, save them to a vector
    ## In the next step, remove the lines from par_fixed_LGM_test that 
    ## refer to the threshold to the value 2 (e.g. [T1$2@4.284]; should be 
    ## removed if T1$2 does not occur in the test set)
    var_names_test <- grep("^t\\d+$", colnames(df_test), value = TRUE)
    
    ## check if the value 2 occurs in the test set
    values_2_test <- sapply(var_names_test, function(x) {
      any(df_test[[x]] == 2)
    })
    ## set the first letter of the names of values_2_test to uppercase
    names(values_2_test) <- toupper(var_names_test)

    ## disassembling par_fixed_LGM_test
    par_fixed_LGM_test <- unlist(strsplit(par_fixed_LGM_test, "\n"))
    
    ## remove the lines that refer to the threshold to the value 2
    ## if the variable does not occur in the test set
    ## the lines in par_fixed_LGM_test where these two conditions are both TRUE:
    ## A) a variable is contained where values_2_test is NA
    ## and
    ## B) a dollar sign is followed by a "2"
    ## should be removed from par_fixed_LGM_test
    par_fixed_LGM_test <- par_fixed_LGM_test[!grepl(
      paste0("(", paste(names(values_2_test[is.na(values_2_test)]), collapse = "|"), 
             ")\\$2"), par_fixed_LGM_test)]


    ## this removes all lines that refer to the threshold to the value 2 if that 
    ## value does not occur in the test set
    
    ## now re-join the lines to a single string
    par_fixed_LGM_test <- paste(par_fixed_LGM_test, collapse = "\n")
    cat(par_fixed_LGM_test)


    ## code model test set
    model_LGM_test <- mplusObject(
      TITLE = paste0(title_string_LGM, " (test set)"),
      VARIABLE = variable_string,
      ANALYSIS = analysis_string_LGM,
      MODEL = par_fixed_LGM_test,
      OUTPUT = output_string,
      SAVEDATA = paste0(
        "FILE = ", CBCL_question, "_", "I_S_scores_test.dat;\n",
        "SAVE = FSCORES;"),
      usevariables = colnames(var_names_LGM),
      rdata = df_test
    )
    
    fit_LGM_test <- mplusModeler(
      model_LGM_test,
      dataout = paste0("model_test_LGM_", CBCL_question, ".dat"),
      # note: data needs to be given to model!
      # only solution seems to be to directly delete it
      # afterwards!
      modelout = paste0("model_test_LGM_", CBCL_question, ".inp"),
      check = TRUE,
      run = TRUE,
      hashfilename = FALSE,
      Mplus_command = detectMplus() #"C:/Program Files/Mplus/Mplus.exe"
    )
    
    ## reading in model output model test set
    model_test_output_file <- grep(".out",
                                   grep("test", list.files(), value = TRUE),
                                   value = TRUE)
    
    model_LGM_test_info <- readModels(target = model_test_output_file,
                                      what = "all")
    
    ## assembling output dfs
    ## training data
    df_out_CBCL_train <- cbind(select(df_train, FISNr),
                               model_LGM_train_info$savedata) %>%
      select(FISNr, I, I_SE, S, S_SE)
    
    
    df_out_CBCL_test <- cbind(select(df_test, FISNr),
                               model_LGM_test_info$savedata) %>%
      select(FISNr, I, I_SE, S, S_SE)
    
    ## merging training and test dataframes
    df_out_CBCL <- rbind(df_out_CBCL_train,
                         df_out_CBCL_test) %>%
      ## all column names except for "FISNr" should be extended with the value
      ## of CBCL_question
      rename_with(~ paste0(., "_", CBCL_question), -FISNr) %>%
      rename("FISNumber" = FISNr)
    
    
    ## saving dataframe with information about Intercept and slope in  
    ## directory of the respective CBCL question
    saveRDS(df_out_CBCL, file = paste0("outcome_df_LGM_", CBCL_question, ".rds"))
    
    return(list(df_out_CBCL = df_out_CBCL,
                n_class = opt_n_class))
    
###############################################################################    
    
    ## Alternative case: Best model is actually multiclass model
  } else {
  
      ## here delete all files not needed anymore
      #dat_files
      #inp_files
      #out_files
      
      files_opt_model <- c(model_opt$dat_file, 
                           model_opt$inp_file,
                           model_opt$out_file)
      
      ## adapt the list so that the correct file is chosen (setdiff)
      ## this comes only after the model selection!
      
      ## delete all files in the directory except the ones associated with the
      ## optimal model, the output of the LCGA and the 
      ## summary table of the LCGA models
      files_delete <- setdiff(list.files(), c(files_opt_model,
                                              "output_LCGA.txt",
                                              paste0("summary_LCGA_mixture_", 
                                                     CBCL_question, ".rds")))
                              
      file.remove(from = paste0(getwd(), "/", files_delete)) 
      
      
      file.copy(from = paste0(getwd(), "/", model_opt$dat_file),
                to = paste0(here::here("mplus_files", "cprobabilities"), "/",
                            model_opt$dat_file),
                overwrite = TRUE
      )
      ## note: The .dat file cannot be deleted if the the readModels function
      ## is supposed to be used!
      
      model_opt_info <- readModels(target = getwd(), what = "all")
      cat("savedata file present: ", "savedata" %in% names(model_opt_info), "\n")
  
      par_fixed_test <- paste0("%OVERALL%\n",
                               model_string,
                               "\n",
                               generate_fixed_syntax_with_classes(
                                 model_opt_info$parameters$unstandardized)
                               )
      
      
      ## Now: estimate the exact same model with all important 
      ## parameters fixed to the values from the model estimated on the training set
      variable_string_test <- paste0(variable_string,
                                     "classes = c(", opt_n_class, ");\n")
      
      data_filename <- paste0("model_test_set_", CBCL_question, ".dat")
      
      model_test_set <- mplusObject(
        TITLE = paste0(title_string, " (test set)"),
        VARIABLE = variable_string_test,
        ANALYSIS = analysis_string,
        MODEL = par_fixed_test,
        OUTPUT = output_string,
        SAVEDATA = paste0(
          "FILE = ", CBCL_question, "_", opt_n_class, "_test_cprobs.dat;\n",
          "SAVE = CPROBABILITIES;"),
        usevariables = colnames(var_names_LGM), # alternative tech1 tech8;
        rdata = df_test
      )
      
      
      fit_model_test_set <- mplusModeler(
        model_test_set,
        dataout = paste0("model_test_set_", CBCL_question, ".dat"),
        # note: data needs to be given to model!
        # only solution seems to be to directly delete it
        # afterwards!
        modelout = paste0("model_test_set_", CBCL_question, ".inp"),
        check = TRUE,
        run = TRUE,
        hashfilename = FALSE,
        Mplus_command = detectMplus() #"C:/Program Files/Mplus/Mplus.exe"
      )
      
      
      ## appending the information of the latent classes (slope, intercept)
      parameters_model <- model_opt_info$parameters$unstandardized %>%
        filter(paramHeader == "Means" & param %in% c("I", "S")) %>%
        as.data.frame()
      
      ## creating latentClass indicator depending on model
      l_class_ind <- unique(parameters_model$LatentClass)
      
      ## re-appending FISNumbers to probabilities dataframe and creating output 
      ## df of the modeling to save for this CBCL question, 
      
      ## here is where it goes wrong, the savedata file is not read in properly
      df_out_CBCL_train <- cbind(select(df_train, FISNr),
                                 model_opt_info$savedata) %>%
        select(FISNr, starts_with("CPROB"), C1) %>%
        mutate(I_C1 = NA, S_C1 = NA)
      
      for(cl in l_class_ind){
        df_out_CBCL_train <- df_out_CBCL_train %>%
          mutate(I_C1 = ifelse(C1 == cl,
                               as.numeric(
                                 select(
                                   filter(parameters_model,
                                          LatentClass == cl & param == "I"),
                                   est)
                                 ),
                               I_C1),
                 S_C1 = ifelse(C1 == cl,
                               as.numeric(
                                 select(
                                   filter(parameters_model,
                                          LatentClass == cl & param == "S"),
                                   est)
                                 ),
                               S_C1)
          )
      }
      
      ## next up: Code this as well into test set, merge, change column
      ## names so they also contain the CBCL_question string
      
      ## reading in model output model test set
      model_test_output_file <- grep(".out",
                                     grep("test", list.files(), value = TRUE),
                                     value = TRUE)
      
      model_opt_test_info <- readModels(target = model_test_output_file,
                                        what = "all")
      
      parameters_model_test <- model_opt_test_info$parameters$unstandardized %>%
        filter(paramHeader == "Means" & param %in% c("I", "S"))
      
      df_out_CBCL_test <- cbind(select(df_test, FISNr),
                                model_opt_test_info$savedata) %>%
        select(FISNr, starts_with("CPROB"), C) %>%
        ## note: here, latent class assignment variable ("C") is labelled
        ## differently than in the training model df ("C1")
        mutate(I_C1 = NA, S_C1 = NA) %>%
        rename("C1" = C)
      
      for(cl in l_class_ind){
        df_out_CBCL_test <- df_out_CBCL_test %>%
          mutate(I_C1 = ifelse(C1 == cl,
                               as.numeric(
                                 select(
                                   filter(parameters_model_test,
                                          LatentClass == cl & param == "I"),
                                   est)
                               ),
                               I_C1),
                 S_C1 = ifelse(C1 == cl,
                               as.numeric(
                                 select(
                                   filter(parameters_model_test,
                                          LatentClass == cl & param == "S"),
                                   est)
                               ),
                               S_C1)
          )
      }
      # View(df_out_CBCL_test)
      
      ## merging training and test dataframes
      df_out_CBCL <- rbind(df_out_CBCL_train,
                           df_out_CBCL_test) %>%
        ## all column names except for "FISNr" should be extended with the value
        ## of CBCL_question
        rename_with(~ paste0(., "_", CBCL_question), -FISNr) %>%
        rename("FISNumber" = FISNr)
      
      ## since the correlation between the model information features is 
      ## almost perfect, they will be removed in the ML preprocessing
      ## therefore, dataframe with this information will be saved in the 
      ## directory of the respective CBCL question
      saveRDS(df_out_CBCL, file = paste0("outcome_df_LGM_", CBCL_question, ".rds"))
      cprob_cols <- grep("CPROB", colnames(df_out_CBCL), value = TRUE)
      
      ## select minimum value of the column means of the the columns that 
      ## contain "CPROB" in their name and save the column name of this column
      min_prob_col <- cprob_cols[which.min(
        apply(df_out_CBCL[, c(cprob_cols)], 2, min))]  
      
      ## de-selecting group probability column with lowest average and 
      ## intercept and slope columns since they perfectly correlate
      df_out_CBCL <- df_out_CBCL %>%
        select(-all_of(min_prob_col), -contains("C1"))
      
      
      ## saving cprobs file of the test set to additional folder as well
      out_cprobs_file <- grep("test_cprobs", list.files(), value = TRUE)
      file.copy(from = paste0(getwd(), "/", out_cprobs_file),
                to = paste0(here::here("mplus_files", "cprobabilities"), "/",
                            out_cprobs_file),
                overwrite = TRUE
      )
      # setwd(dir_CBCL_question)
      
      }
  return(list(df_out_CBCL = df_out_CBCL,
              n_class = opt_n_class))

}

## Next steps (track those in Obsidian!): 

## make function so that for every CBCL question, an own directory is created
## move all output files that were created there

## - file moving and cleaning works with 1 example?

## make sure that all those operations are happening inside the specific 
## subdirectory

## combine model output and probabilities file to new dataframe that contains 
## FISNumbers 
## append to large overall dataframe (reduce function, this does not happen 
## inside this function!)

## cleaning function: several files are not needed anymore, remove them

LGM_DV_calculation <- function(df, CBCL_question){
  
  
}


LGM_estimation_grid <- function(df, configuration_grid){
  
  
  
}

## ----------------------------------------------------------------------------

## testing area (leave this in!)
source_tests <- FALSE ## important to not accidently also source those objects
if(source_tests){
  data_train_LGM_1 <- LGM_preprocess(
    CBCL_age_df = CBCL_age_df,
    CBCL_question = names(CBCL_questions_list)[[1]],
    df = train_data,
    questions_list = CBCL_questions_list,
    items_table = CBCL_items_table_reduced)
  
  data_test_LGM_1 <- LGM_preprocess(
    CBCL_age_df = CBCL_age_df,
    CBCL_question = names(CBCL_questions_list)[[1]],
    df = test_data,
    questions_list = CBCL_questions_list,
    items_table = CBCL_items_table_reduced)
  
  View(data_train_LGM_1)
  
  data_train_LGM_97 <- LGM_preprocess(
    CBCL_age_df = CBCL_age_df,
    CBCL_question = names(CBCL_questions_list)[[80]],
    df = train_data,
    questions_list = CBCL_questions_list,
    items_table = CBCL_items_table_reduced)
  
  data_test_LGM_97 <- LGM_preprocess(
    CBCL_age_df = CBCL_age_df,
    CBCL_question = names(CBCL_questions_list)[[80]],
    df = test_data,
    questions_list = CBCL_questions_list,
    items_table = CBCL_items_table_reduced)
  
  system.time({LCGA_test_1 <-
    LGM_1_4_CBCL(df_train = data_train_LGM_1,
                  df_test = data_test_LGM_1,
                  CBCL_question = names(CBCL_questions_list)[[1]])
  })
  ## function works!!!
  
  
  system.time({LCGA_test_2 <- lapply(names(CBCL_questions_list)[c(2, 80)], function(x){
    ## data prep for training and test set
    train_data_question <- 
    LGM_preprocess(CBCL_age_df = CBCL_age_df,
                   CBCL_question = x,
                   df = train_data,
                   questions_list = CBCL_questions_list,
                   items_table = CBCL_items_table_reduced)
  
    test_data_question <- 
    LGM_preprocess(CBCL_age_df = CBCL_age_df,
                   CBCL_question = x,
                   df = test_data,
                   questions_list = CBCL_questions_list,
                   items_table = CBCL_items_table_reduced)
    
    LGM_df_question <-
    LGM_1_4_CBCL(df_train = train_data_question,
                  df_test = test_data_question,
                  CBCL_question = x)
    
    return(LGM_df_question)
    })
})

## CONTINUE HERE!!!
## This now works! Next week: Send to server and paralellize, essentially 
## run script 07 took 63 - 71 minutes

## reduce df to 1 big df
}

## question: Can you suppress the console printing output of the 
## mPlusmodeler function?
## CONTINUE HERE!!!




