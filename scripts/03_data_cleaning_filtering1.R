# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-07-31
#
# Script Name: 03_data_cleaning01_filtering1
#
# Script Description: This script contains the first steps of data cleaning
# for the project: 
## Combining longitudinal change features of childhood psychopathology 
## with Polygenic scores in machine learning models of adult wellbeing
#
# Notes: Filtering procedure may still be adjusted after consultation with the 
# supervisors and after receiving PGS data
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "caret")

## printing and changing working directory if needed
getwd()

here::here()

getwd() == here::here()

## loading in custom functions
source(here::here("scripts", "functions", "functions.R"))


## reading in datafile (if necessary, change filepath to where file is located)
data <- read_sav(here::here("data", "source_raw", "PHE_20240722_4552_YJS.sav")) %>%
  as.data.frame()

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
load(here::here("scripts", "variable_vectors.RData"))

## loading in sumamry dataframe from exploration
load(here::here("scripts", "summary_CBCL.RData"))

## loading in list of CBCL items per question
load(here::here("scripts", "CBCL_questions_list.RData"))

## loading in covariate data for further filtering
load(here::here("scripts", "data_covariates.RData"))
## loading in covariate names 
load(here::here("scripts", "names_covariates.RData"))

nrow(data)
# Initially 92969 participants in dataset

## filtering data in one compact function, outputting dropped participants 
## after each filtering step
data_filtered <- filter_CBCL(df = data, CBCL_YSR_items_vec = CBCL_YSR_items_vec,
                             data_covariates = data_covariates)


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

## removing intermediary objects
rm(list = c("data_CBCL_103", "data_CBCL_50", "data_CBCL_57"))

#-----------------------------------------------------------------------------

## KNN imputation


## Preparation and execution of KNN imputation

## Select only CBCL item columns for KNN Imputation procedure
## Remember to later join back with the full column CBCL data
## when removing the outliers!!! 


## In the following lines, the names of the dffs still nede to be adjusted

## Removing columns with +50% missings to see if that changes anything
col_threshold <- 0.5

## keeping Ids
data_ids_filtered <- data_filtered_recoded %>%
  select(FISNumber)

## preparing the filtering
data_CBCL <- data_filtered_recoded %>%
  select(all_of(CBCL_YSR_items_vec)) %>%
  mutate(across(everything(), as.numeric))

cols_with_excessive_na <- sapply(data_CBCL %>% select(
  all_of(CBCL_YSR_items_vec)),
                                 function(x) mean(is.na(x)) > col_threshold)

data_CBCL_cols <- data_CBCL[, !cols_with_excessive_na]
## 12 items from CBCL filtered

CBCL_items_keep <- colnames(data_CBCL_cols)

## vector of columns to drop for later
CBCL_items_drop <- setdiff(CBCL_YSR_items_vec, CBCL_items_keep)


## This actually worked! Possible that the list of CBCL question to 
## perform longitudinal modeling on needs to be reduced! 


## Alternative KNN imputation with the vim package
library(VIM)

data_imputed_vim <- kNN(
  data_CBCL_cols,
  variable = colnames(data_CBCL_cols),
  k = 5,
  dist_var = colnames(data_CBCL_cols),
  weights = NULL,
  numFun = median,
  catFun = maxCat,
  makeNA = NULL,
  NAcond = NULL,
  impNA = TRUE,
  donorcond = NULL,
  mixed = vector(),
  mixed.constant = NULL,
  trace = FALSE,
  imp_var = TRUE,
  imp_suffix = "imp",
  addRF = FALSE,
  onlyRF = FALSE,
  addRandom = FALSE,
  useImputedDist = TRUE,
  weightDist = FALSE,
  methodStand = "range",
  ordFun = medianSamp
)
## takes very long to run! 

##---------------------------------------------------------------------------



pre_model <- preProcess(data_CBCL_cols, method = "knnImpute", k = 5,
                        verbose = TRUE)

print(pre_model$mean)
print(pre_model$std)

imputed <- predict(pre_model, newdata = data_CBCL_cols)
## Imputation worked but the values are strange now
for(col in 1:220){
  cat("variable: ", colnames(imputed[col]), "\n",
      min(imputed[, col]), "\n",
      max(imputed[, col]), "\n", "\n")
}

for(col in 221:439){
  cat("variable: ", colnames(imputed[col]), "\n",
      min(imputed[, col]), "\n",
      max(imputed[, col]), "\n", "\n")
}

## particularly questions 101 and 105 seem to be very off (Truant and taking
## drugs), furthermore 97 and 91 (threatens other people, talks suicide)

## Binding with ID column again
data_imputed <- bind_cols(data_ids_filtered, imputed)
nrow(data_imputed)
nrow(data_CBCL_cols) - nrow(data_imputed)

# [...]


##-----------------------------------------------------------------------------



## Outlier removal: Calculation of the Minimum-covariance determinant (MCD),
## a more robust version of Mahalanobis' distance (Leys et al., 2018)

## For now only do this with the CBCL data, dataset fed will later be adjusted

## For now: Toy data, only two columns to check if original df value 
## and function work in general

library(MASS)

t1 <- Sys.time()
## NOTE: MASS and dplyr both have a select function!
## indicate package before calling function (dplyr::select)

## saving IDs
data_mcd_toy_ID <- data_imputed %>%
  dplyr::select(FISNumber)

data_mcd_toy <- data_imputed %>%
  dplyr::select(any_of(CBCL_items_keep))

## Since many of the variables have IQR = 0, they don't help in distinguishing
## outliers, thus identify outliers only based on the columns 

## saving order of columns (will be applied to final dataframe later so that it
## has the same order of columns)
column_order <- names(data_mcd_toy)


## deleting columns that have IQR = 0
ncol(data_mcd_toy)

## still adjust the names of the MCD intermediary datasets, potentially also
## write this into a custom function

data_mcd_toy1 <- data_mcd_toy[, sapply(data_mcd_toy, function(col) IQR(col) > 0)]

ncol(data_mcd_toy1)
## only 159 columns still remaining for calculation of mcd


## IMPORTANT: The MCD functions can only be applied after imputation! Missings
## are not allowed

## Code was taken from Leys et al., 2018

# Creating covariance matrix for MCD («data_mcd» is the matrix containing  
# data with no indicator variable
output50 <- cov.mcd(data_mcd_toy1, quantile.used = nrow(data_mcd_toy1)* .5)
## If column has IQR 0! Not possible to calculate this
## Might happen that system is exactly singular (Not in this case)
## With high number of variables, calculating this takes long to run! 

output75 <- cov.mcd(data_mcd_toy1, quantile.used = nrow(data_mcd_toy1)* .75)

## with two columns that do have IQR != 0, this worked


# Distances from centroid for each matrix
md <- mahalanobis(data_mcd_toy1, colMeans(data_mcd_toy1), cov(data_mcd_toy1))
mhmcd50 <- mahalanobis(data_mcd_toy1, output50$center, output50$cov)
mhmcd75 <- mahalanobis(data_mcd_toy1, output75$center, output75$cov)

# Detecting outliers for each method
# The index of each detected outlier is recorded for each method for a 
# alpha= .01
# For more than two variables, df of cutoff variable (in bold)
## has to be adjusted

alpha <-.05 ## less conservative than Leys at al

cutoff <- (qchisq(p = 1 - alpha, df = ncol(data_mcd_toy1))) ## ADJUST THIS! 
names_outliers_MH <- which(md > cutoff)
names_outliers_MCD50 <- which(mhmcd50 > cutoff)
names_outliers_MCD75 <- which(mhmcd75 > cutoff)
## here instead of threshold, just use top 10%?
length(names_outliers_MCD50)
length(names_outliers_MCD75)
class(mhmcd75)

## those vectors give the row numbers of IDs in the original dataframe, use 
## for filtering
names_outliers_MCD50_5 <- data.frame(mhmcd50) %>% 
  rowid_to_column() %>%
  arrange(desc(mhmcd50)) %>%
  head(0.05 * length(mhmcd50)) %>%
  dplyr::select(rowid) %>%
  pull()

names_outliers_MCD75_5 <- data.frame(mhmcd75) %>% 
  rowid_to_column() %>%
  arrange(desc(mhmcd75)) %>%
  head(0.05 * length(mhmcd75)) %>%
  dplyr::select(rowid) %>%
  pull()

## might be quite high when using the cutoff! 
## Indeed, if using the cutoff, 3114 outliers (out of 6552) when applying .75
## threshold, even more (4060) when applying .50 threshold

## Thus instead of cutoff only discard the top 5%

# Excluding outliers in a new matrix (here based on MCD75) called data_mcd2
excluded <- names_outliers_MCD75_5


## dropping the outliers according to top 5% MCD

data_mcd2_toy1 <- data_mcd_toy1[-excluded, ]
data_mcd2_toy_ID <- data.frame(FISNumber = data_mcd_toy_ID[-excluded, ])

t2 <- Sys.time()

print(t2 - t1)
  
## binding ID and data
data_cleaned_CBCL <- bind_cols(data_mcd2_toy_ID, data_mcd2_toy1)
nrow(data_mcd2_toy1)
nrow(data_mcd2_toy_ID)
nrow(data_cleaned)

## This ensures that there are still 5957 participants in the data (if only
## removing the mcd75 top 5% filter to the CBCL data!)


## Joining with the variables that were not part of the MCD calculation

## still check how exactly this needs to be set so that the previously excluded
## columns go back to the dataframe also make an order of the columns before the 
## deletion and apply again after the merge so that the dataframe has 
## the same column order 
data_cleaned <- bind_cols(data_cleaned_CBCL, data_mcd_toy[-excluded,
                              sapply(data_mcd_toy,
                                     function(col) IQR(col) <= 0)])

## changing column order
data_cleaned <- data_cleaned[, c("FISNumber", column_order)]

dropped_cols <- setdiff(CBCL_YSR_items_vec, names(data_cleaned))
## The CBCL Items ultimately dropped from the analysis, the same as 
## CBCL_items_drop

## Still figure out what is with the KNN imputed values!


## Next step: matching it with the covariate data with left_join 
## or with rest of data that was cut out at the beginning of the imputation 
## procedure



