# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-04-15
#
# Script Name: 08_PCA_PRS_NTR.R
#
# Script Description:
## This script is supposed to execute the following steps
## (in the following order):
## Unzip .zip file in the directory 
## /data/dleitritz/p01_CBCL_PGS_LGM/data/intermediate/PGS
## for every .sav file in the unzipped directory:
##  1. Read in the .sav file
##  2. Perform specific calculations:
##  Calculate a principle component analysis (PCA) on all columns that contain
##  "PGS" in their name
##  3. Save the values of the 1st Principle component together with the ID and
##  all specified covariates, the column name of the first principal_component
##  should consist out of the character string "PC1_" and the phenotype which
##  is contained in the filename of the .sav file
## 
## after all calculations are done, merge all dataframes on the ID columns,
## named "FISNumber" finally, save the dataset as a .csv file in the directory 
## /data/dleitritz/p01_CBCL_PGS_LGM/data/intermediate/PGS
#
#
# Notes:
# this script was run on the compute server ntrcompute2 where the source file 
# was located. For data access, contact ntr data access comittee
# Script assumes that .zip file is located in file path
#
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)



# Load necessary libraries
pacman::p_load("dplyr", "haven", "foreign", "here", "readr",
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "fastDummies", "RANN",
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest", "psych")

# Unzip the .zip file in the current directory
## The zip files are in the directory 
## /data/dleitritz/p01_CBCL_PGS_LGM/data/intermediate/PGS
unzip_dir <- "data/intermediate/PGS/unzipped_files"
# Directory containing zip files
zip_dir <- "/data/dleitritz/p01_CBCL_PGS_LGM/data/intermediate/PGS"
zip_file <- list.files(path = zip_dir, pattern = "\\.zip$", full.names = TRUE)
if (length(zip_file) == 1) {
    unzip(zip_file, exdir = unzip_dir)
} else {
    stop("No .zip file found or multiple .zip files detected.")
}

# List all .sav files in the unzipped directory
sav_files <- list.files(unzip_dir, pattern = "\\.sav$", full.names = TRUE)

## run once with TRUE to save name vectors of genetic covariates and outlier
## columns
testing_PCA <- FALSE

if(testing_PCA){
## sandboxing here with only the first element from sav_files
  test_file <- sav_files[1]
## loading in data
  data <- read.spss(test_file, to.data.frame = TRUE, use.value.labels = FALSE)
  print(colnames(data))
  str(data)

## check genetic outliers (variable: EUR_1KG)
  table(data$EUR_1KG_Outlier)
## make table of proportions of genetic outliers
  table(data$EUR_1KG_Outlier) / nrow(data)

## check for missing values
  sum(colMeans(is.na(data)))
## only column SEX_add1 has missings (all na!)

## saving genetic covariate names in vector
  gen_covariates <- c("PLD_AXIOM", "PLD_GSA", 
                      grep("PC",
                           grep("1KG", colnames(data), value = TRUE),
                           value = TRUE))

## saving names of outlier columns in vector
  outlier_cols <- c("EUR_1KG_Outlier", "NL_Strict_Outlier")

## saving names of genetic covariates and outlier columns 
## invectors for later 
  saveRDS(gen_covariates,
          file = here::here("data", "intermediate",
                          "names_genetic_covariates.rds"))

  saveRDS(outlier_cols,
          file = here::here("data", "intermediate",
                          "names_outlier_columns_PGS.rds"))

## Check: Are genetic covariates equal across files?
  data1 <- read.spss(sav_files[2], to.data.frame = TRUE,
                     use.value.labels = FALSE)

## checking if column PC1_1KG has exactly the same values in both datasets
  all(data$PC1_1KG == data1$PC1_1KG)
## checking if column FISNumber has exactly the same values in both datasets
  all(data$FISNumber == data1$FISNumber)


## Selecting the score columns for PCA
  score_cols <- grep("SCORE", colnames(data), value = TRUE)

## carry out PCA (pca function from psych package)
  pca_PGS <- pca(data[, score_cols])
  pca_PGS <- pca_PGS$scores[, 1]

  data.frame(scores = pca_PGS) %>%
      ggplot(aes(x = scores)) + 
      geom_histogram()

## garbage collection
  gc()

}

# Function to perform specific calculations on a .sav file
process_sav_pca <- function(file_path) {
    data <- read.spss(file_path, to.data.frame = TRUE)
    
    ## needed: rename column 'FISnumber' to 'FISNumber'
    colnames(data)[colnames(data) == "FISnumber"] <- "FISNumber"

    ## saving genetic covariate names in vector
    gen_covariates <- c("PLD_AXIOM", "PLD_GSA", 
                        grep("PC",
                            grep("1KG", colnames(data), value = TRUE),
                                 value = TRUE))
    
    ## saving names of outlier columns in vector
    outlier_cols <- c("EUR_1KG_Outlier", "NL_Strict_Outlier")

                                 

    ## also taking covariates and outlier columns in first iteration
    if(file_path == sav_files[1]){
       PCA_PGS_data <- data %>%
        select(FISNumber, all_of(c(gen_covariates, outlier_cols)),
               contains("SCORE"))
        ## otherwise only FISNumber and SCORE columns
    } else {
       PCA_PGS_data <- data %>%
        select(FISNumber, contains("SCORE"))
    }

    ## carrying out PCA (without intermediate objects, only scores of interest)
    pca_PGS <- pca(PCA_PGS_data %>%
       select(contains("SCORE")))$scores[, 1]
    
    ## obtaining the phenotype from the filename:
    ## This should be always the part of the filename that comes between 
    ## the string 'NTR-DSR-4552_' and the next underscore in the filename
    phenotype <- str_extract(file_path, "(?<=NTR-DSR-4552_)[^_]+")
    
    ## Still address this issue: ADHD, AlzheimersDisease and Smoking 
    ## exist several times! Find out which phenotypes those exactly are!
    if(file_path == sav_files[1]){
        data_out <- PCA_PGS_data %>%
            select(FISNumber, all_of(c(gen_covariates, outlier_cols)))
        data_out$PC1_PGS <- pca_PGS
        colnames(data_out)[colnames(data_out) == "PC1_PGS"] <- 
            paste0("PC1_PGS_", phenotype)
    } else {
        data_out <- PCA_PGS_data %>%
            select(FISNumber)
        data_out$PC1_PGS <- pca_PGS
        ## renaming the column with the PGS score to include the phenotype,
        ## since ADHD, AlzheimersDisease and Smoking occur multiple times in
        ## file names, they are handled separately
        colnames(data_out)[colnames(data_out) == "PC1_PGS"] <- 
            case_when(
    file_path == "data/intermediate/PGS/unzipped_files/NTR-DSR-4552_ADHD_PMID32026073_MRG18_PedMergedWithScores.sav"
        ~ "PC1_PGS_ADHD_childhood",
    file_path == "data/intermediate/PGS/unzipped_files/NTR-DSR-4552_ADHD_PMID36702997_MRG18_PedMergedWithScores.sav"
        ~ "PC1_PGS_ADHD",
    file_path == "data/intermediate/PGS/unzipped_files/NTR-DSR-4552_AlzheimersDisease_PMID24162737_MRG18_PedMergedWithScores.sav"
        ~ "PC1_PGS_ADHD_AlzheimersDisease_Lambert_2013",
    file_path == "data/intermediate/PGS/unzipped_files/NTR-DSR-4552_AlzheimersDisease_PMID35379992_MRG18_PedMergedWithScores.sav"
        ~ "PC1_PGS_ADHD_AlzheimersDisease_Bellenguez_2022",
    file_path == "data/intermediate/PGS/unzipped_files/NTR-DSR-4552_Smoking_CigarettesPerDay_PMID30643251_MRG18_PedMergedWithScores.sav" 
        ~ "PC1_PGS_SmokingCigarettesPerDay",
    ## in the GWAS reference PMID31427789, smoking was coded categorically
    ## (0 = never, 1 = previous, 2 = current)
    file_path == "data/intermediate/PGS/unzipped_files/NTR-DSR-4552_Smoking_PMID31427789_MRG18_PedMergedWithScores.sav"
        ~ "PC1_PGS_SmokingCategorical",
    file_path == "data/intermediate/PGS/unzipped_files/NTR-DSR-4552_Smoking_SmokingCessation_PMID30643251_MRG18_PedMergedWithScores.sav"
        ~ "PC1_PGS_SmokingCessation",
    file_path == "data/intermediate/PGS/unzipped_files/NTR-DSR-4552_Smoking_SmokingInitiation_PMID30643251_MRG18_PedMergedWithScores.sav"
        ~ "PC1_PGS_SmokingInitiation", 
    .default = paste0("PC1_PGS_", phenotype)  
            )
    }

    return(data_out)
}

# Process each .sav file and store results
full_data_PCA_PGS <- lapply(sav_files, process_sav_pca)

# Merge all dataframes on the ID column "FISNumber"
merged_data <- Reduce(function(x, y) merge(x, y, by = "FISNumber", all = TRUE),
                      full_data_PCA_PGS)

# head(merged_data)
# colnames(merged_data)
# table(merged_data$EUR_1KG_Outlier) / nrow(merged_data)
# ~3.7% EUR outliers
# table(merged_data$NL_Strict_Outlier) / nrow(merged_data)
# ~5% NL outliers
# sum(colMeans(is.na(merged_data)))
# No missing values

## saving data
saveRDS(merged_data,
        file = here::here("data", "intermediate", "PGS", "data_PCA_PGS.rds"))



# eoS