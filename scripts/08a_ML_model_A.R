# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-12-03
#
# Script Name: 08a_ML_model_A
#
# Script Description: This script contains the machine learning preprocessing
# steps and the machine learning modelling for the baseline model (model A)
# of the study "Combining longitudinal change features of
# childhood psychopathology with Polygenic scores in machine learning models
# of adult wellbeing". Model A serves as the baseline model and only contains 
# the raw responses to the childhood abnormal behavior questionnaires (CBCL),
# non LGM longitudinal features (longitudinal mean & SD, RMSSD, autocorrelation,
# autoregression) and the study covariates (sex, SES, time-lag)
#
#
# Notes:
#
#

## getting complete duration
t00 <- Sys.time()

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")

## expressions = 50000 to ensure very large paste length
options(scipen = 999, expressions = 50000)


# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "fastDummies")


## loading in full model_A data (merged together in script 06_b_merge_nonLGM.R)
load(here::here("data", "intermediate", "data_model_A.Rdata"))
temp <- load(here::here("data", "intermediate", "data_model_A.Rdata"))
cat("full model_A data loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")


## loading in training ids and test ids (split created)
load(here::here("data", "intermediate", "indices_train.RData"))
temp <- load(here::here("data", "intermediate", "indices_train.RData"))
cat("saved indices of participants in training set loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")

load(here::here("data", "intermediate", "indices_test.RData"))
temp <- load(here::here("data", "intermediate", "indices_test.RData"))
cat("saved indices of participants in training set loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")

## loading in df with FISNr and FamilyNumber
load(here::here("data", "intermediate", "FIS_fam_nr.RData"))
temp <- load(here::here("data", "intermediate", "FIS_fam_nr.RData"))
cat("saved df with FISNr and FamilyNumber loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")

## loading in covariate names
load(here::here("scripts", "names_covariates.RData"))
temp <- load(here::here("scripts", "names_covariates.RData"))
cat("vector with names of covariates loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")


## inspecting classes and unique values of covariates
for(covariate in covariates_names){
  print(covariate); print(class(data_model_A[, covariate]))
  print(unique(data_model_A[, covariate]))
  }

## further distinction: numeric and factor covariates
num_covariates <- c(grep("time_lag", covariates_names, value = TRUE),
                    grep("age_qol", covariates_names, value = TRUE))

factor_covariates <- setdiff(covariates_names, num_covariates)

## factor conversion of factor_covariates
for(covariate in factor_covariates){
  data_model_A[, covariate] <- as.factor(data_model_A[, covariate])
}

## check if conversion worked
for(covariate in factor_covariates){
  print(covariate); print(class(data_model_A[, covariate]))
}

## worked! All are factors

## Counting factor columns
vars_mult_class <- vector()
vars_factor <- vector()
length_factor_vars <- 0
length_mult_class <- 0
for(var in 1:ncol(data_model_A)){
  variable <- data_model_A[, var]
  if(length(class(variable)) > 1) {
    cat("multiclass variable; variable ", colnames(data_model_A[var]),
        " is class: ", class(variable), "\n", "\n")
    vars_mult_class <- c(vars_mult_class, colnames(data_model_A[var]))
    length_mult_class <- length_mult_class + 1
  }
  else if(class(variable) == "factor"){
    vars_factor <- c(vars_factor, colnames(data_model_A[var]))
    length_factor_vars <- length_factor_vars + 1
  } else {
    next
  }
}
print(vars_factor)
print(length_factor_vars)

## Dummy coding of factor variables and saving the names of the resulting 
## variables + the numeric covariates so they can be held out later
## when filtering and imputing

data_dummies <- dummy_cols(data_model_A,
                           select_columns = vars_factor,
                           remove_first_dummy = TRUE,
                           remove_selected_columns = TRUE,
                           ignore_na = TRUE) ## not own column, but missing
## information here will be imputed as well

dummy_vars <- setdiff(colnames(data_dummies), colnames(data_model_A))

covariates_full <- c(num_covariates, dummy_vars)

one_hot <- FALSE ## change this to True when running the entire script
## again on cluster

if(one_hot){
  
  #factor_cols <- names(data_model_A)[sapply(data_model_A, is.factor)]
  
  ## explicitly coding missing as factor level (recode back later!)
  data_model_A[vars_factor] <- lapply(data_model_A[vars_factor], function(col) {
    if (is.factor(col)) {
      levels(col) <- c(levels(col), "Missing") # Add "Missing" as a level
    }
    replace(col, is.na(col), "Missing")       # Replace NA with "Missing"
  })
  
  one_hot_list <- lapply(vars_factor, function(factor_var) {
    # Create one-hot encoding for the current factor
    mat <- model.matrix(~ . + 0, data = data_model_A[, factor_var, drop = FALSE])
    
    # Update column names to include the original variable name
    colnames(mat) <- paste0(factor_var, levels(data_model_A[[factor_var]]))
    
    return(mat)
  })
  
  one_hot_encoded <- do.call(cbind, one_hot_list)
  
  names_dummy <- colnames(one_hot_encoded)
  
  # Remove the original factor column from the dataset
  data_model_A <- data_model_A %>% select(-all_of(vars_factor))
  
  # [, !names(x_train_comb) %in% factor_cols]
  
  # Combine the one-hot encoded columns with the rest of the dataset
  data_model_A <- cbind(data_model_A, one_hot_encoded)
  
  ## appending names of newly created dummy columns with numeric covariates 
  ## resulting in a vector that can be used to prevent these variables 
  ## from being excluded when ML preprocessing, they should remain in the model
  names_covariates <- c(names_dummy, num_covariates)
  
  print(names_covariates)

}

## Now. all factor covariates are dummy coded (no level dropped), 
## and there is a vector of all covariates names that are still in the
## dataset

##-----------------------------------------------------------------------------

### NOTE: STILL INSERT the B different splits here: Baseline will be 
## done with split 1, the rest then runs separate baseline preprocessing

##-----------------------------------------------------------------------------


## ultimate splitting test and training data
data_train <- data_dummies %>% 
  filter(FISNumber %in% train_ids)

data_test <- data_dummies %>%
  filter(FISNumber %in% test_ids)

## Rearranging Columns (ID, outcome and covariates in beginning)
## Should they be avoided in feature selection?
data_train <- data_train %>%
  select(FISNumber, QoL_simple, all_of(covariates_full), everything())

data_test <- data_test %>%
  select(FISNumber, QoL_simple, all_of(covariates_full), everything())

## Pre-processing steps

## saving IDs

df_FISNr_train <- data_train %>%
  select(FISNumber)

df_FISNr_test <- data_test %>%
  select(FISNumber)

## saving outcome 
y_train <- data_train %>% select(QoL_simple)

## type conversion
y_train$QoL_simple <- as.numeric(y_train$QoL_simple)

y_test <- data_test %>% select(QoL_simple)

y_test$QoL_simple <- as.numeric(y_test$QoL_simple)

## A) standard ML preprocessing

## note: here, covariates need to be held out! 

data_covariates_train <- data_train %>%
  select(FISNumber, all_of(covariates_full))

data_covariates_test <- data_test %>%
  select(FISNumber, all_of(covariates_full))

## append this back to the dataset after removing columns!


## For preprocessing: Remove all columns that are not supposed to be removed 
## by the preprocessing steps, based on the training data, later
## filter test data with remaining variables and append FISNumber again when 
## needed
data_train <- data_train %>%
  select(-all_of(covariates_full), -FISNumber, -QoL_simple)


## i) near-zero variance
nzv_train <- nearZeroVar(data_train)

ncol(data_train)

data_train <- data_train[-nzv_train]

cat(length(nzv_train), " columns removed (near zero variance)", "\n")

ncol(data_train)


## ii) high correlation (note that all columns must be numeric, df not changed here)
## Note: here no non-numeric column, still kept in in case changes

num_data <- data_train[, sapply(data_train, is.numeric)]

high_cor <- findCorrelation(cor(num_data,
                               use = "pairwise.complete.obs"),
                           cutoff = .95)

num_data <- num_data[-high_cor]

cat(length(high_cor), " columns removed (high correlation; .95)", "\n")

## re-appending non-numeric columns (here: none)
data_train <- cbind(data_train[, !sapply(data_train, is.numeric)], num_data)

## iii) linear dependence

# Create a preprocessing object for median imputation
## note: This is only to find linear combiations of columns which are intended 
## to be removed! 
num_data <- data_train[, sapply(data_train, is.numeric)]

preProcessObj <- preProcess(num_data, method = "medianImpute")

# Apply the median imputation to the dataset
num_data_imputed <- predict(preProcessObj, num_data)
## some columns ended up in the ignore part, median imputing those manually
for (col in preProcessObj$method$ignore) {
  # For columns with constant values or high NAs, fill with median or a default value
  num_data_imputed[, col] <- median(num_data_imputed[, col], na.rm = TRUE)
}

combos <- findLinearCombos(as.matrix(num_data_imputed))$remove


## conditional in case there were any non-numeric columns
if(ncol(num_data_imputed) == ncol(data_train)){

  data_train <- data_train[-combos]

} else {
    num_data_imputed <- num_data_imputed[-combos]
    data_train <- cbind(
      data_train[, sapply(data_train, is.numeric)], data_train %>%
      select(all_of(colnames(num_data_imputed)))) 
  }

cat(length(combos), " columns removed (linear combinations)", "\n")

cat(ncol(data_train), " remaining columns in training set", "\n")


## (iv) multicollinearity)

## feature selection by means of elastic net also takes care of 
## removing highly collinear variables


## selecting variables in test set that are also contained in training set
data_test <- data_test %>%
  select(all_of(colnames(data_train)))


## v) recoding features

## first: Listing types of features in dataset
vars_num <- vector()
vars_char <- vector()
vars_fact <- vector()
vars_int <- vector()
vars_bool <- vector()
vars_other <- vector()
vars_mult_class <- vector()
length_num <- 0
length_char <- 0
length_fact <- 0
length_int <- 0
length_bool <- 0
length_other <- 0
length_mult_class <- 0

for(var in 1:ncol(data_train)){
  variable <- data_train[, var]
  #print(colnames(data_train)[var])
  #print(class(variable))
  #print(length(class(variable)))
  if(length(class(variable)) > 1) {
    cat("multiclass variable; variable ", colnames(data_train[var]),
        " is class: ", class(variable), "\n", "\n")
    vars_mult_class <- c(vars_mult_class, colnames(data_train[var]))
    length_mult_class <- length_mult_class + 1
  } else if(class(variable) == "numeric"){
    vars_num <- c(vars_num, colnames(data_train[var]))
    length_num <- length_num + 1
  } else if(class(variable) == "character"){
    vars_char <- c(vars_char, colnames(data_train[var]))
    length_char <- length_char + 1
  } else if(class(variable) == "integer"){
    vars_int <- c(vars_int, colnames(data_train[var]))
    length_int <- length_int + 1
  } else if(class(variable) == "logical") {
    vars_bool <- c(vars_bool, colnames(data_train[var]))
    length_bool <- length_bool + 1
  } else {
    cat("other variable detected; variable ", colnames(data_train[var]),
        " is class: ", class(variable), "\n", "\n")
    vars_other <- c(vars_other, colnames(data_train[var]))
    length_other <- length_other + 1
  }
}

length_num
length_char
length_int
length_bool
length_other
length_mult_class
vars_mult_class

## all the multilabel classes are likely numeric
## those can be recoded 

## printing unique values of multiclass variables
for(var in vars_mult_class){
  print(var)
  print(unique(data_train[[var]]))
}

## all those variabels can be recoded to numeric variables
data_train <- data_train %>%
  mutate_at(vars_mult_class, as.numeric)

## no more multiclass variables

## same for test data (At some point filter test set to contain only variables
## that are also in training set and do the type conversion as well)
data_test <- data_test %>%
  mutate_at(vars_mult_class, as.numeric)

##----------------------------------------------------------------------------


## B) Imputing data

## Note: Outlier removal by means of the Minimum covariance determinant (MCD)
## can only be done with imputed data and will only be done as sensitivity
## analysis after obtaining stable ML models

## before imputation: Take out FISNR! it should not be part of the KNN procedure

## Reminder that those datasets already exist
df_FISNr_train <- df_FISNr_train

df_FIS_fam <- df_FIS_fam

## Rejoining with covariates 




df_A_train <- data_covariates_train %>%
  cbind(data_train)
  

df_A_test <- data_covariates_test %>%
  cbind(data_test)

## IMPORTANT: KNN imputation needs to happen without outcome

## Needs to evaluate to FALSE
if("QoL_simple" %in% colnames(df_A_train) |
   "QoL_simple" %in% colnames(df_A_test)) {
    stop("Error: Outcome must be removed before KNN imputation. Script stopped")
}


## KNN imputation: 
t1 <- Sys.time()

x_train <- df_A_train %>%
  select(-FISNumber)

## was created previously
y_train <- y_train


x_test <- df_A_test %>%
  select(-FISNumber)

y_test <- y_test

cat("Beginning KNN imputation training data")
k_pad <- round(sqrt(ncol(x_train)))
train_pre_obj <- preProcess(x_train,
                            method = "knnImpute",
                            k = k_pad)

t2 <- Sys.time()

cat("duration KNN imputation object: ", difftime(t2, t1, unit = "mins"))

x_train_imp <- predict(train_pre_obj, x_train)

t3 <- Sys.time()

cat("duration KNN imputation train data: ", difftime(t3, t2, unit = "mins"))

x_test_imp <- predict(train_pre_obj, x_test)

t4 <- Sys.time()

cat("duration KNN imputation test data: ", difftime(t4, t3, unit = "mins"))

sum(colMeans(is.na(x_train_imp)) != 0)
sum(colMeans(is.na(x_test_imp)) != 0)
## columns in test set are those which are not yet removed

##-----------------------------------------------------------------------------



## C) feature selection: Elastic net

## reappending y to x (outcome to training set), for caret functions, also 
## family for doing the family split!
x_train_comb <- cbind(df_FISNr_train, y_train, x_train_imp) %>%
  left_join(df_FIS_fam, by = "FISNumber")

x_test_comb <- cbind(df_FISNr_test, y_test, x_test_imp) %>%
  left_join(df_FIS_fam, by = "FISNumber")


## Now begin with the actual ML : Find functions to do glm with optimized 
## CV alpha and lambda! 

## also: Implement Bayesian optimization of the hypertuning parameters 
## alpha and lambda

## Also remember that you don't want the FISnr as a predictor! 

## No more preprocessing needed, KNN impute already centered and scaled

## train control object: Adaptive cross-validation, see e.g. Habets et al., 2023
## Kuhn: caret package
# Define the train control with adaptive cross-validation
## 10-fold CV

# Create custom 10-fold cross-validation keeping families together
set.seed(7)
folds <- groupKFold(group = x_train_comb$FamilyNumber, k = 10)

adaptControl <- trainControl(method = "adaptive_cv",
                             number = 10, repeats = 10,
                             adaptive = list(min = 5, alpha = 0.05, 
                                             method = "gls", complete = FALSE),
                             search = "random",
                             index = folds)




# Define predictors by excluding ID and FamilyNumber and outcome
predictor_vars <- setdiff(names(x_train_comb), c("FISNumber", "FamilyNumber",
                                                 "QoL_simple"))

# Create a formula dynamically

## Note: Seems like a lot of raw answers seem to get eliminated during the 
## preprocessing!

formula <- as.formula(paste("QoL_simple ~", paste(predictor_vars, collapse = " + ")))
## Note that this changes later since it will be re-assigned

# Train an elastic net regression model

## Note: This is adaptive hypertuning, what I want is Bayesian hypertuning

## here: still adjust names and later also work in the random seeds at a 
## tune length of e.g. 1000 (see Habets et al.)
t0_el <- Sys.time()
glm_adapt <- train(formula, ## use of formula here as FISNr and Family number needed to be excluded!
                   data = x_train_comb,
                   method = "glmnet",  # Elastic net regression model
                   trControl = adaptControl, 
                   metric = "RMSE", # Metric for regression
                   tuneLength = 15, # Number of random hyperparameter settings
                   verbose = TRUE)

# Output the best model and parameters
print(glm_adapt)
glm_adapt$bestTune

t1_el <- Sys.time()

cat("duration adaptive hypertuning with length 15: ",
    difftime(t1_el, t0_el, unit = "mins"), " minutes")

## Next: extract non-zero coefficients predictors, also performance on test set
## and training set

## -----------------------------------------------------------------------


## I should also parallelize the bayesian process as I do in XGBoost

## Comparison with Bayesian hypertuning:
t0_bayes <- Sys.time()

## defining function that gives out the needed parameters

# Define the objective function
elastic_net_bayes <- function(alpha, lambda) {
  
  ## ChatGPT suggestion to track iterations with no progress
  # Static variables to track best score and stagnant iterations
  if (!exists("bestScore", envir = .GlobalEnv)){
    assign("bestScore", -Inf, envir = .GlobalEnv)
    }
  if (!exists("noImprovementCount", envir = .GlobalEnv)){
    assign("noImprovementCount", 0, envir = .GlobalEnv)
  }
  
  # Train the model using caret with glmnet
  model <- train(formula, 
                 data = x_train_comb,
                 method = "glmnet",
                 trControl = trainControl(method = "cv",
                                          number = 10,
                                          index = folds,# 10-fold CV
                                          allowParallel = FALSE),
                 ## setting to FALSE because otherwise conflicts with later
                 ## parallelization
                 # preProc = c("center", "scale"),
                 tuneGrid = data.frame(alpha = alpha, lambda = lambda))
  
  score <- -min(model$results$RMSE)  # Negative RMSE
  # Function finds maxima so inverting the output!
  
  # Debug: Print the parameters and score
  
  ## extracting optimal alpha and lambda from model
  alpha <- model$bestTune$alpha
  lambda <- model$bestTune$lambda
  cat("Alpha:", alpha, "Lambda:", lambda, "Score:", score, "\n")
  
  ## checking if score actually improved significantly
  ## this is supposed to take up functionality of the otherHalting argument
  ## except that function does not stop immediately if there is no improvement
  
  minUtility <- 0.001  # Define threshold for improvement
  if (score - get("bestScore", envir = .GlobalEnv) < minUtility) {
    assign("noImprovementCount",
           get("noImprovementCount",
               envir = .GlobalEnv) + 1, envir = .GlobalEnv)
  } else {
    assign("noImprovementCount", 0, envir = .GlobalEnv)  # Reset counter if improvement is significant
    assign("bestScore", score, envir = .GlobalEnv)  # Update best score
  }
  
  # Stop if no improvement for 5 consecutive iterations (Or other amount of iterations)
  if (get("noImprovementCount", envir = .GlobalEnv) >= 5) {
    stop("Early stopping: No improvement in 5 consecutive iterations")
  }
  
  return(list(Score = score))  # Ensure proper list format
  ## here add other components that should be included in the final summary
  ## table provided by the bayesOpt function
}

## Run bayesian optimization

# Set the search bounds for hyperparameters
bounds <- list(alpha = c(0, 1), lambda = c(0.001, 1))

# Run Bayesian optimization


set.seed(64)

## To parallelize
cl <- makeCluster(parallel::detectCores() - 1)
## cl <- makeClusterr(64)
## cluster of 64 on ntrcompute-2
registerDoParallel(cl)
clusterExport(cl,c('formula', 'folds', 'x_train_comb', 'elastic_net_bayes'),
              envir = globalenv())
clusterEvalQ(cl,expr= {
  library(glmnet)
  library(caret)
  library(dplyr)
})
clusterEvalQ(cl, ls())

tWithPar <- system.time(
  opt_results <- bayesOpt(
    FUN = elastic_net_bayes,
    bounds = bounds,
    initPoints = 5,
    ## initPoints must be greater than the number of FUN inputs
    iters.n = (parallel::detectCores() - 1)*2,
    ## iters.n = 64*2,
    iters.k = (parallel::detectCores() - 1)*2,
    ## iters.k = 64*2,
    ## otherHalting = list(timeLimit = 30000, minUtility = NULL)
    parallel = TRUE,
    verbose = 1
  )
)


stopCluster(cl)
registerDoSEQ()


# set.seed(7)
# opt_results <- bayesOpt(FUN = elastic_net_bayes, bounds = bounds,
#                        initPoints = 5, iters.n = 10)

# View the best parameters
print(opt_results)

# Extract best parameters

## This is the command you were looking for! 
data.frame(getBestPars(opt_results))
best_params <- getBestPars(opt_results)


t1_bayes <- Sys.time()

cat("duration bayesian hypertuning: ",
    difftime(t1_bayes, t0_bayes, unit = "mins"), " minutes")


# Train the final model using the optimal parameters
model_bayes <- train(formula, 
                     data = x_train_comb,
                     method = "glmnet",
                     trControl = trainControl(method = "none"),  # No CV for the final model
                     # preProc = c("center", "scale"),
                     tuneGrid = data.frame(alpha = best_params$alpha,
                                           lambda = best_params$lambda))

## model now runs, check again if it still works if you adjust the optimization
## function to explicitly include the lambda and alpha! Might want to adjust this 
## depending on the model you are running! 



t2_bayes <- Sys.time()

cat("duration model training after bayesian hypertuning: ",
    difftime(t2_bayes, t1_bayes, unit = "mins"), " minutes")

## CONTINUE HERE!!

## evaluating both models on test set and check if performance is
## significantly different (You have this somewhere in your resources)

## adaptive tuning
predictions_adapt <- predict(glm_adapt, newdata = x_test_comb)

## Bayesian hypertuning
predictions_bayes <- predict(model_bayes, newdata = x_test_comb)

## extracting measures (note: both arguments need to be vectors!)
perf_measures_adapt <- postResample(pred = predictions_adapt,
                                    obs = y_test$QoL_simple)
print(perf_measures_adapt)
perf_measures_bayes <- postResample(predictions_bayes,
                                    obs = y_test$QoL_simple)
print(perf_measures_bayes)


## compare outcomes (statistically compare the performances)
t.test(perf_measures_adapt, perf_measures_bayes, paired = TRUE)

## t-test indicates no significant difference: 

## for this elastic net modeling, both hypertuning approaches seem to work 
## equally well, now, extracting the coefficients, see which model gives 
## more feature selection


## extracting features with non-zero coefficients

final_model_adapt <- glm_adapt$finalModel

# cv tuned lambda
best_lambda_adapt <- glm_adapt$bestTune$lambda

# Extract the coefficients for tuned lambda
coef_matrix_adapt <- coef(final_model_adapt, s = best_lambda_adapt)

# Convert to df
coef_df_adapt <- as.data.frame(as.matrix(coef_matrix_adapt))
coef_df_adapt$Predictor <- rownames(coef_df_adapt)
rownames(coef_df_adapt) <- NULL


# Filter for non-zero coefficients
non_zero_coef_adapt <- coef_df_adapt[coef_df_adapt[, 1] != 0, ]

# Get the unique original column names for predictors with non-zero coefficients
non_zero_predictors_adapt <- unique(non_zero_coef_adapt$Predictor)

# Display the predictors with non-zero coefficients
non_zero_predictors_adapt

cat("count of non-0 coefficient predictors from elastic net with
    adaptive hypertuning: ", length(non_zero_predictors_adapt) - 1)


## same for bayesian optimized tuned model
final_model_bayes <- model_bayes$finalModel

# cv tuned lambda
best_lambda_bayes <- final_model_bayes$lambdaOpt

# Extract the coefficients for tuned lambda
coef_matrix_bayes <- coef(final_model_bayes, s = best_lambda_bayes)

# Convert to df
coef_df_bayes <- as.data.frame(as.matrix(coef_matrix_bayes))
coef_df_bayes$Predictor <- rownames(coef_df_bayes)
rownames(coef_df_bayes) <- NULL


# Filter for non-zero coefficients
non_zero_coef_bayes <- coef_df_bayes[coef_df_bayes[, 1] != 0, ]

non_zero_predictors_bayes <- unique(non_zero_coef_bayes$Predictor)

# Display the predictors with non-zero coefficients
# non_zero_predictors_bayes <- non_zero_coef_bayes$Predictor
non_zero_predictors_bayes

cat("count of non-0 coefficient predictors from elastic net with
    bayesian hypertuning: ", length(non_zero_predictors_bayes) - 1)

intersect(non_zero_predictors_adapt, non_zero_predictors_bayes)
intersect_count <- 0
for(predictor in non_zero_predictors_bayes){
  if(predictor %in% non_zero_predictors_adapt){
    cat(predictor, " contained in non zero predictors adapt", "\n", "\n")
    intersect_count <- intersect_count + 1
  }
}
intersect_count

## features remaining after adaptive hypertuning: 39 (without covariates)

## features remaining after Bayesian hypertuning: 38 (without covariates)

predictors_A_adapt <- grep("(Intercept)",
                          non_zero_predictors_adapt,
                          value = TRUE, invert = TRUE)


predictors_A_bayes <- grep("(Intercept)",
                          non_zero_predictors_bayes,
                          value = TRUE, invert = TRUE)


## Here, add the covariates back if they were eliminated! (not simply append the
## covariates vector, but only those that were removed (with setdiff or so))

predictors_A_bayes <- c(setdiff(covariates_full, predictors_A_bayes),
                        predictors_A_bayes)

sum(covariates_full %in% predictors_A_bayes) - length(covariates_full) == 0
## should evaluate to TRUE, all covariates contained in predictor set


save(predictors_A_bayes,
     file = here::here("data", "intermediate", "predictors_model_A.RData"))

## Predictor set saved: Next up: training the level one models to check for
## stability


## ----------------------------------------------------------------------------

## Next: On reduced training set (only features in non-zero predictors), train 
## the baseline models 

## vector for relevant variables in the actual round of machine learning
vars_ML <- c("FISNumber", "FamilyNumber", "QoL_simple", predictors_A_bayes)



## creating definitive training set for the level 1 models (formula will eliminate)
## the ID variables and outcome from predictors
x_train_ML <- x_train_comb %>%
  select(all_of(vars_ML))

x_test_ML <- x_test_comb %>%
  select(all_of(vars_ML))

## check if column names add up
ncol(x_train_ML) == length(predictors_A_bayes) + 3

## Column transformation (covariates might still be non-numeric columns)

## Note: Write this into a function

## x_train_ML <- mult_col_conversion(x_train_ML)


## first: Listing types of features in dataset
vars_num <- vector()
vars_char <- vector()
vars_fact <- vector()
vars_int <- vector()
vars_bool <- vector()
vars_other <- vector()
vars_mult_class <- vector()
length_num <- 0
length_char <- 0
length_fact <- 0
length_int <- 0
length_bool <- 0
length_other <- 0
length_mult_class <- 0

for(var in 1:ncol(x_train_ML)){
  variable <- x_train_ML[, var]
  #print(colnames(data_train)[var])
  #print(class(variable))
  #print(length(class(variable)))
  if(length(class(variable)) > 1) {
    cat("multiclass variable; variable ", colnames(x_train_ML[var]),
        " is class: ", class(variable), "\n", "\n")
    vars_mult_class <- c(vars_mult_class, colnames(x_train_ML[var]))
    length_mult_class <- length_mult_class + 1
  } else if(class(variable) == "numeric"){
    vars_num <- c(vars_num, colnames(x_train_ML[var]))
    length_num <- length_num + 1
  } else if(class(variable) == "character"){
    vars_char <- c(vars_char, colnames(x_train_ML[var]))
    length_char <- length_char + 1
  } else if(class(variable) == "integer"){
    vars_int <- c(vars_int, colnames(x_train_ML[var]))
    length_int <- length_int + 1
  } else if(class(variable) == "logical") {
    vars_bool <- c(vars_bool, colnames(x_train_ML[var]))
    length_bool <- length_bool + 1
  } else {
    cat("other variable detected; variable ", colnames(x_train_ML[var]),
        " is class: ", class(variable), "\n", "\n")
    vars_other <- c(vars_other, colnames(x_train_ML[var]))
    length_other <- length_other + 1
  }
}

length_num
length_char
length_int
length_bool
length_other
length_mult_class
vars_mult_class

## all the multilabel classes are likely numeric
## those can be recoded 

## printing unique values of multiclass variables
for(var in vars_mult_class){
  print(var)
  print(unique(x_train_ML[[var]]))
}

## all those variabels can be recoded to numeric variables
x_train_ML <- x_train_ML %>%
  mutate_at(vars_mult_class, as.numeric)

## no more multiclass variables

## same for test data (At some point filter test set to contain only variables
## that are also in training set and do the type conversion as well)
x_test_ML <- x_test_ML %>%
  mutate_at(vars_mult_class, as.numeric)



# Define predictors by excluding ID and FamilyNumber and outcome
predictor_vars <- setdiff(names(x_train_ML), c("FISNumber", "FamilyNumber",
                                               "QoL_simple"))
# Create a formula dynamically
formula <- as.formula(paste("QoL_simple ~", paste(predictor_vars, collapse = " + ")))

# Create custom 10-fold cross-validation keeping families together
set.seed(7)
folds <- groupKFold(group = x_train_ML$FamilyNumber, k = 10)
## seed kept equal, folds are exactly the same compared to 
## elastic net



## A) Random forest (with Bayesian parameter hypertuning)


## hypertuning parameters that need to be tuned (as pre-registered):

## -	Number of trees: num.trees
## -	Maximum number of features to consider at each split: mtry
## -	Maximum depth of a tree: max.depth
## -	Minimum number of samples required to split a node: min.node.size
## -	Minimum number of samples required at each leaf node (in ranger the same
## as Minimum number of samples required to split a node)

t0_bayes_rf <- Sys.time()

## defining function that gives out the needed parameters
# Define the objective function for Bayesian optimization
rf_bayes <- function(mtry, max.depth, min.node.size, num.trees) {
  
  # Convert parameters to integers
  mtry <- as.integer(mtry)
  max.depth <- as.integer(max.depth)
  min.node.size <- as.integer(min.node.size)
  num.trees <- as.integer(num.trees)
  
  ## track iterations and progress
  if (!exists("bestScore", envir = .GlobalEnv)){
    assign("bestScore", -Inf, envir = .GlobalEnv)
  }
  if (!exists("noImprovementCount", envir = .GlobalEnv)){
    assign("noImprovementCount", 0, envir = .GlobalEnv)
  }
  
  ## initiating cross-validation by hand
  # Initialize an empty vector to store RMSE for each fold
  
  cv_rmse_rf <- numeric(length(folds))
  
  # Loop over each fold
  for (i in seq_along(folds)) {
    # Get the training and validation indices
    indices_x_train_ML <- unlist(folds[-i]) # All except the current fold
    indices_val <- folds[[i]]          # Current fold for validation
    
    # Split the data into training and validation sets
    train_data <- x_train_ML[indices_x_train_ML, ]
    val_data <- x_train_ML[indices_val, ]
    
    # Train the ranger model
    model <- ranger(
      formula = formula,
      data = train_data,
      mtry = mtry,
      max.depth = max.depth,
      min.node.size = min.node.size,
      num.trees = num.trees
    )
    
    # Make predictions on the validation set
    predictions <- predict(model, data = val_data)$predictions
    
    # Calculate RMSE for the current fold
    true_values <- val_data[[all.vars(formula)[1]]] # Extract target variable
    fold_rmse <- sqrt(mean((predictions - true_values)^2))
    
    # Store the RMSE
    cv_rmse_rf[i] <- fold_rmse
  }
  
  # Calculate overall cross-validated RMSE
  mean_cv_rmse <- mean(cv_rmse_rf)
  
  
  ## check if this works
  
  ## ADJUST!!! ## Try running again after SVR finished
  #model <- train(
  #  formula = formula,
  #  data = x_train_ML,
  #  method = "ranger",
  #  trControl = trainControl(
  #    method = "cv",
  #    number = 10,
  #    index = folds # Ensure families stay together
  #  ),
  #  tuneGrid = expand.grid(
  #    mtry = mtry,
  #    splitrule = "variance", # no variation of splitrule
      #num.trees = num.trees,
      #max.depth = max.depth,
  #    min.node.size = min.node.size 
  #  ),
  #  num.trees = num.trees,
  #  max.depth = max.depth
  #)
  
  score <- -min(mean_cv_rmse)

  
  ## Cross-validation was built in by hand
  
  # Calculate predictions and RMSE
  #predictions <- model$predictions
  #actuals <- x_train_ML$QoL_simple  # Extract target variable
  #rmse <- sqrt(mean((actuals - predictions)^2))
  
  ## Early stopping if after 5 iterations no progress
  minUtility <- 0.001  # Define threshold for improvement
  if (score - get("bestScore", envir = .GlobalEnv) < minUtility) {
    assign("noImprovementCount",
           get("noImprovementCount",
               envir = .GlobalEnv) + 1, envir = .GlobalEnv)
  } else {
    assign("noImprovementCount", 0, envir = .GlobalEnv)  # Reset counter if improvement is significant
    assign("bestScore", score, envir = .GlobalEnv)  # Update best score
  }
  
  # Stop if no improvement for 5 consecutive iterations (Or other amount of iterations)
  if (get("noImprovementCount", envir = .GlobalEnv) >= 5) {
    stop("Early stopping: No improvement in 5 consecutive iterations")
  }
  
  
  # score <- -min(model$results$RMSE)  # Negative RMSE
  # Function finds maxima so inverting the output!
  
  # Return negative RMSE (Bayesian optimization minimizes the score)
  # return(list(Score = -rmse))
  return(list(Score = score))
}

# Define the search bounds for hyperparameters

## NOTE: When the bayesian hypertuning of the elastic net has been
## adjusted, you might end up with a different number of features and then
## also adjust the bounds, this holds for all bounds!

## CONTINUE HERE

bounds_rf <- list(
  mtry = c(1L, ncol(x_train_ML) - 3L),
  max.depth = c(3L, 30L),
  min.node.size = c(1L, 50L),
  num.trees = c(100L, 1500L)
)

set.seed(64)

## To parallelize
cl <- makeCluster(parallel::detectCores() - 1)
## cl <- makeClusterr(64)
## cluster of 64 on ntrcompute-2
registerDoParallel(cl)
clusterExport(cl,c('formula', 'folds', 'x_train_ML', 'rf_bayes'),
              envir = globalenv())
clusterEvalQ(cl,expr= {
  library(ranger)
  library(caret)
  library(dplyr)
})
clusterEvalQ(cl, ls())

tWithPar_rf <- system.time(
  opt_results_rf <- bayesOpt(
    FUN = rf_bayes,
    bounds = bounds_rf,
    initPoints = 5,
    ## initPoints must be greater than the number of FUN inputs
    iters.n = (parallel::detectCores() - 1)*2,
    ## iters.n = 64*2,
    iters.k = (parallel::detectCores() - 1)*2,
    ## iters.k = 64*2,
    otherHalting = list(timeLimit = 6000),
    parallel = TRUE,
    verbose = 1,
    acq = "ei"
  )
)


stopCluster(cl)
registerDoSEQ()


# set.seed(7)
# opt_results <- bayesOpt(FUN = elastic_net_bayes, bounds = bounds,
#                        initPoints = 5, iters.n = 10)

# Run Bayesian optimization
#set.seed(7)
#opt_results_rf <- bayesOpt(
#  FUN = rf_bayes,
#  bounds = bounds_rf,
#  initPoints = 5,
#  iters.n = 10,
#  acq = "ei"
#)

# View the best parameters
print(opt_results_rf)

tWithPar_rf

# Extract the best parameters
best_params_rf <- getBestPars(opt_results_rf)
print(best_params_rf)

tune_grid_rf <- data.frame(
  ## note: tuneGrid only accepts mtry, min.node.size and splitrule for rf
  mtry = best_params_rf$mtry,
  # max.depth = best_params_rf$max.depth,
  min.node.size = best_params_rf$min.node.size,
  # num.trees = best_params_rf$num.trees,
  splitrule = "variance"
)


## Run bayesian optimization


t1_bayes_rf <- Sys.time()

cat("duration bayesian hypertuning (random forest): ",
    difftime(t1_bayes_rf, t0_bayes_rf, unit = "mins"), " minutes")


## alternative: adaptive hypertuning, code might be reused, now silenced

adapt <- FALSE
if(adapt){
t1_adapt_rf <- Sys.time()

adaptControl_rf <- trainControl(method = "adaptive_cv",
                                number = 10, repeats = 10, ## 10-fold CV
                                adaptive = list(min = 5, alpha = 0.05, 
                                                method = "gls",
                                                complete = FALSE),
                                search = "random",
                                index = folds)

model_adapt_rf <- train(formula, 
                        data = x_train_ML,
                        method = "ranger",
                        trControl = adaptControl_rf, 
                        metric = "RMSE", # Metric for regression
                        tuneLength = 15, # Number of random hyperparameter settings
                        verbose = TRUE)

t2_adapt_rf <- Sys.time()

cat("duration model training (rf) after adaptive hypertuning: ",
    difftime(t2_adapt_rf, t1_adapt_rf, unit = "mins"), " minutes")


model_adapt_rf$bestTune

}



# Train the final model using the optimal parameters
## I can still train the final model with caret! 
model_bayes_rf <- train(formula, 
                        data = x_train_ML,
                        method = "ranger",
                        trControl = trainControl(method = "none"),
                        # No CV for the final model
                        tuneGrid = tune_grid_rf,
                        ## this needs to added separately in caret / ranger
                        max.depth = best_params_rf$max.depth, 
                        num.trees = best_params_rf$num.trees
                        ###...
                        )


t2_bayes_rf <- Sys.time()

cat("duration model training (random forest) after bayesian hypertuning: ",
    difftime(t2_bayes_rf, t1_bayes_rf, unit = "mins"), " minutes")

## model run! now extract measures!

## CONTINUE HERE!!!




##-----------------------------------------------------------------------------

## B) Support vector regression (with bayesian parameter hypertuning)

## Idea for checking 1 categorical tuning parameter: run model with the 
## grid of continuous parameters for all options of the categorical parameter
## then, save only the best performing, in the end, also save the categorical
## parameter alongside the optimized score!

## parameters to hypertune:
## -	C parameter (penalty for each misclassified datapoint)
## -	Kernel function (transformation method to allow linear separation of data points)
## -	Gamma parameter (similarity radius)


t0_bayes_svr <- Sys.time()


# Training control
#train_control_svr <- trainControl(
#  method = "cv",
#  number = 10,
#  verboseIter = TRUE,
#  allowParallel = FALSE,
#  index = folds ## making sure the family split is still applied 
#)

## defining function that gives out the needed parameters
# define the objective function for Bayesian optimization
svr_bayes <- function(C, sigma, degree, scale, method) {
  
  ## track iterations and progress
  if (!exists("bestScore", envir = .GlobalEnv)){
    assign("bestScore", -Inf, envir = .GlobalEnv)
  }
  if (!exists("noImprovementCount", envir = .GlobalEnv)){
    assign("noImprovementCount", 0, envir = .GlobalEnv)
  }
  
  ## Rounding degree to integer
  degree <- as.integer(ifelse(degree < 2.5, 2, 3))
  
  # Dynamically select parameters based on the method
  method <- ifelse(method < 0.5, "svmRadial", "svmPoly")
  ## also use this later to select the method
  
  if(method == "svmRadial") {
    tune_grid <- expand.grid(
      C = C,
      sigma = sigma
    )
    model_method <- "svmRadial"
  } else if (method == "svmPoly") {
    tune_grid <- expand.grid(
      C = C,
      degree = degree,
      scale = scale
    )
    model_method <- "svmPoly"
  }

  
  
  # Train the model
  # set.seed(7)
  model <- train(
    formula,
    data = x_train_ML,
    method = model_method,
    trControl = trainControl(
      method = "cv",
      number = 10,
      verboseIter = TRUE,
      allowParallel = FALSE,
      index = folds ## making sure the family split is still applied 
    ),
    tuneGrid = tune_grid
  )
  
  score <- -min(model$results$RMSE)
  
  ## Early stopping if after 5 iterations no progress
  minUtility <- 0.001  # Define threshold for improvement
  if (score - get("bestScore", envir = .GlobalEnv) < minUtility) {
    assign("noImprovementCount",
           get("noImprovementCount",
               envir = .GlobalEnv) + 1, envir = .GlobalEnv)
  } else {
    assign("noImprovementCount", 0, envir = .GlobalEnv)  # Reset counter if improvement is significant
    assign("bestScore", score, envir = .GlobalEnv)  # Update best score
  }
  
  # Stop if no improvement for 5 consecutive iterations (Or other amount of iterations)
  if (get("noImprovementCount", envir = .GlobalEnv) >= 5) {
    stop("Early stopping: No improvement in 5 consecutive iterations")
  }
  
  
  # Return negative RMSE (Bayesian optimization minimizes the score)
  return(list(Score = score))
  
}

# Define the search bounds for hyperparameters
bounds_svr <- list(
  C = c(0.1, 10),            # Range for C
  sigma = c(0.01, 0.1),     # Range for sigma
  degree = c(2, 3),          # Range for degree
  scale = c(0.01, 0.1),     # Range for scale
  method = c(0, 1)           # Encodes categorical: 0 = Radial, 1 = Poly
)



set.seed(64)


## To parallelize
cl <- makeCluster(parallel::detectCores() - 1)
## cl <- makeCluster(64)
## cluster of 64 on ntrcompute-2
registerDoParallel(cl)
clusterExport(cl,c('formula', 'folds', 'x_train_ML', 'svr_bayes',
                   #'train_control_svr', 
                   'bounds_svr'),
              envir = globalenv())
clusterEvalQ(cl,expr= {
  library(caret)
  library(dplyr)
})
clusterEvalQ(cl, ls())

tWithPar_svr <- system.time(
  opt_results_svr <- bayesOpt(
    FUN = svr_bayes,
    bounds = bounds_svr,
    initPoints = 10,
    ## initPoints must be greater than the number of FUN inputs
    iters.n = (parallel::detectCores() - 1)*2,
    ## iters.n = 64*2,
    # iters.n = 5,
    iters.k = (parallel::detectCores() - 1)*2,
    ## iters.k = 64*2,
    #otherHalting = list(timeLimit = 12000),
    parallel = TRUE,
    verbose = 1,
    acq = "ei"
  )
)


stopCluster(cl)
registerDoSEQ()

# Run Bayesian optimization
#set.seed(7)
#opt_results_svr <- bayesOpt(
#  FUN = svr_bayes,
#  bounds = bounds_svr,
#  initPoints = 10,
#  iters.n = 30,
#  acq = "ei",
#  verbose = TRUE
#)

# View the best parameters
print(opt_results_svr)

# Extract the best parameters
best_params_svr <- getBestPars(opt_results_svr)
print(best_params_svr)


t1_bayes_svr <- Sys.time()

cat("duration bayesian hypertuning (support vector regression): ",
    difftime(t1_bayes_svr, t0_bayes_svr, unit = "mins"), " minutes")


## train final model

if(best_params_svr$method < 0.5) {
  method_svr <- "svmRadial"
  tune_grid_svr <- data.frame(
    C = best_params_svr$C, ## optimal C
    sigma = best_params_svr$sigma ## optimal sigma
  )
  ## here also add other parameters (or not)
} else {
  method_svr <- "svmPoly"
  tune_grid_svr <- data.frame(
    C = best_params_svr$C, ## optimal C
    degree = round(best_params_svr$degree), ## optimal degree, round to integer
    scale = best_params_svr$scale # ## optimal scale,
  )
  ## here also add other parameters
}


model_bayes_svr <- train(formula, 
                        data = x_train_ML,
                        method = method_svr,
                        trControl = trainControl(method = "none"),
                        # No CV for the final model
                        tuneGrid = tune_grid_svr #,
                        ## ...
                        )


t2_bayes_svr <- Sys.time()

cat("duration model training (support vector regression)", "\n", 
    "after bayesian hypertuning: ",
    difftime(t2_bayes_svr, t1_bayes_svr, unit = "mins"), " minutes")


## extract output

## CONTINUE HERE!!!


##-----------------------------------------------------------------------------


t0_bayes_xgb <- Sys.time()

## C) XGBoost (with bayesian parameter hypertuning)

## note: default option of booster argument ("gbtree") will be used

## parameters to hypertune:
## Tree-specific:

  ## -	Number of trees:
    ## num_parallel_tree

  ## -	Maximum depth of a tree: 
    ## max_depth

  ## -	Minimum sum of instance weight required to create new node in a tree: 
    ## min_child_weight

  ## -	Percentage of cases (rows) used for each tree construction:
    ## subsample

  ## -	Percentage of predictors (columns) used for each tree construction:
    ## colsample_bytree

## Learning task-specific (controlling the overall behavior and the learning process of the model):

  ## -	Learning rate eta (step size shrinkage used in updates to prevent overfitting):
    ## eta

  ## -	Gamma (minimum loss reduction required to make a further partition on a leaf node of the tree:
    ## gamma

  ## -	Lambda (L2 regularization term on weights)
    ## lambda

  ## -	Alpha (L1 regularization term on weights):
    ## alpha
         

xgb_adapted <- TRUE

#?xgb.cv
#?xgboost
## this code now needs to be adapted to run the bayesian hypertuning for 
## an XGBoost model!

## one hot encode factor columns (might be placed earlier!)
# Identify factor columns

## NOTE: PLACE THIS AT BEGINNING OF ML part!
## That also spares you the hickups with the factor earlier where you had
## to remove a root

one_hot <- FALSE

if(one_hot){
factor_cols <- names(x_train_ML)[sapply(x_train_ML, is.factor)]

one_hot_encoded <- model.matrix(~ . - 1, data = x_train_ML[, factor_cols, drop = FALSE])

# Remove the original factor column from the dataset
x_train_ML <- x_train_ML[, !names(x_train_ML) %in% factor_cols]

# Combine the one-hot encoded columns with the rest of the dataset
x_train_ML <- cbind(x_train_ML, one_hot_encoded)
}


xgb_bayes <- function(num_parallel_tree,
                      max_depth,
                      min_child_weight,
                      subsample,
                      colsample_bytree,
                      eta,
                      gamma,
                      lambda,
                      alpha) {
  ## track iterations and progress
  if (!exists("bestScore", envir = .GlobalEnv)) {
    assign("bestScore", -Inf, envir = .GlobalEnv)
  }
  if (!exists("noImprovementCount", envir = .GlobalEnv)) {
    assign("noImprovementCount", 0, envir = .GlobalEnv)
  }
  
  
  ## converting all integer inputs to integers.
  num_parallel_tree <- round(num_parallel_tree)
  max_depth <- round(max_depth)
  min_child_weight <- round(min_child_weight)
  
  
  ## NOW IT IS CORRECT!! CONTINUE HERE!!!
  
  columns_exclude <- c("FamilyNumber", "FISNumber", "QoL_simple")
  
  ## making sure that only predictor columns are contained in training set!
  dtrain <- xgboost::xgb.DMatrix(as.matrix(x_train_ML %>%
                                             select(-all_of(columns_exclude))),
                                 label = as.matrix(x_train_ML$QoL_simple))
  
  
  
  Pars <- list(
    booster = "gbtree",
    ## using default option, no variation of this parameter
    num_parallel_tree = num_parallel_tree,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    eta = eta,
    gamma = gamma,
    lambda = lambda,
    alpha = alpha,
    objective = 'reg:squarederror',
    ## default option, sensible?
    eval_metric = "rmse"
  )
  
  xgbcv <- xgb.cv(
    params = Pars,
    data = dtrain,
    nround = 100,
    folds = folds,
    early_stopping_rounds = 100,
    maximize = TRUE,
    verbose = 1
  )
  
  Score = -min(xgbcv$evaluation_log$test_rmse_mean)
  nrounds = xgbcv$best_iteration
  
  ## Early stopping if after 5 iterations no progress
  minUtility <- 0.001  # Define threshold for improvement
  if (Score - get("bestScore", envir = .GlobalEnv) < minUtility) {
    assign("noImprovementCount",
           get("noImprovementCount", envir = .GlobalEnv) + 1,
           envir = .GlobalEnv)
  } else {
    assign("noImprovementCount", 0, envir = .GlobalEnv)  # Reset counter if improvement is significant
    assign("bestScore", Score, envir = .GlobalEnv)  # Update best score
  }
  
  # Stop if no improvement for 5 consecutive iterations (Or other amount of iterations)
  if (get("noImprovementCount", envir = .GlobalEnv) >= 5) {
    stop("Early stopping: No improvement in 5 consecutive iterations")
  }
  
  return(list(
    Score = Score,
    nrounds = nrounds
  ))
  
}

#------------------------------------------------------------------------------#
#### Bounds
#------------------------------------------------------------------------------#


## This is to be worked out: How to sensibly set the bounds?
bounds_xgb <- list(
  num_parallel_tree = c(1L, 100L),
  max_depth = c(3L, 6L),
  min_child_weight = c(5L, 10L),
  subsample = c(0.1, 1),
  colsample_bytree = c(0.5, 1),
  eta = c(0.01, 0.3),
  gamma = c(0, 5),
  lambda = c(0, 10),
  alpha = c(0, 10)
)


#------------------------------------------------------------------------------#
#### To run in parallel
#------------------------------------------------------------------------------#

## Still check how exactly this works


cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)
clusterExport(cl, c('folds', 'x_train_ML', 'bounds_xgb', 'xgb_bayes'))
clusterEvalQ(cl, expr = {
  library(xgboost)
  library(caret)
  library(dplyr)
})
clusterEvalQ(cl, ls())

tWithPar <- system.time(
  opt_results_xgb <- bayesOpt(
    FUN = xgb_bayes,
    bounds = bounds_xgb,
    initPoints = 10,
    ## initPoints must be greater than the number of FUN inputs
    iters.n = (parallel::detectCores() - 1) * 2,
    iters.k = (parallel::detectCores() - 1) * 2,
    parallel = TRUE,
    verbose = 1
  )
)


stopCluster(cl)
registerDoSEQ()


#------------------------------------------------------------------------------#
#### Printing results
#------------------------------------------------------------------------------#
opt_results_xgb$scoreSummary
getBestPars(opt_results_xgb)



## CONTINUE HERE

## testing, run this again, here is where it goes wrong with the as.matrix part
# model_test_xgb <- xgb_bayes(10, 5, 7, 0.8, 0.9, 0.1, 2, 1, 1)


# View the best parameters
print(opt_results_xgb)

# Extract the best parameters
best_params_xgb <- getBestPars(opt_results_xgb)
print(best_params_xgb)


t1_bayes_xgb <- Sys.time()

cat("duration bayesian hypertuning (XGBoost): ",
    difftime(t1_bayes_xgb, t0_bayes_xgb, unit = "mins"), " minutes")

## took ~400 minutes! (without parallelization)

## took 400 minutes (with parallelization)

## train final model

## CONTINUE HERE (2025-02-11)


## Next: Train final model

## Converting training data to xgb Matrix
columns_exclude <- c("FamilyNumber", "FISNumber", "QoL_simple")

## making sure that only predictor columns are contained in training set!
dtrain_xgb <- xgboost::xgb.DMatrix(as.matrix(x_train_ML %>%
                                             select(-all_of(columns_exclude))),
                               label = as.matrix(x_train_ML$QoL_simple))

dtest_xgb <- xgboost::xgb.DMatrix(as.matrix(x_test_comb %>%
                                               select(-all_of(columns_exclude))),
                                   label = as.matrix(x_train_ML$QoL_simple))

par_xgb <- list(
  booster = "gbtree",
  num_parallel_tree = best_params_xgb$num_parallel_tree,
  max_depth = best_params_xgb$max_depth,
  min_child_weight = best_params_xgb$min_child_weight,
  subsample = best_params_xgb$subsample,
  colsample_bytree = best_params_xgb$colsample_bytree,
  eta = best_params_xgb$eta,
  gamma = best_params_xgb$gamma,
  lambda = best_params_xgb$lambda,
  alpha = best_params_xgb$alpha
)

watchlist <- list(train = dtrain_xgb, eval = dtest)

## caret does not allow for the hyperparameter tuning as wished,
## final model trained with xgb
model_bayes_xgb <- xgboost(
  data = dtrain_xgb,
  nrounds = 100,
  verbose = 2
)

## How is it here with the nrounds? When setting to 500, the train-rmse 
## keeps diminishing, but isn't that simply overfitting?


t2_bayes_xgb <- Sys.time()

cat("duration model training (XGBoost)", "\n", 
    "after bayesian hypertuning: ",
    difftime(t2_bayes_xgb, t1_bayes_xgb, unit = "mins"), " minutes")


## How to do this is in your vault where you inspect Bayesian hypertuning
## there is an XGBoost example

t01 <- Sys.time()


cat("duration entire script: ",
    difftime(t01, t00, unit = "mins"), " minutes")

##-----------------------------------------------------------------------------

## Bootstrapping assessment of model stability










