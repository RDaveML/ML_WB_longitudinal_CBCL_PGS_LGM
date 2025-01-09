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

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")

## expressions = 50000 to ensure very large paste length
options(scipen = 999, expressions = 50000)


# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization")


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


### NOTE: STILL INSERT the B different splits here: Baseline will be 
## done with split 1, the rest then runs separate baseline preprocessing


## ultimate splitting test and training data
data_train <- data_model_A %>% 
  filter(FISNumber %in% train_ids)

data_test <- data_model_A %>%
  filter(FISNumber %in% test_ids)

## Rearranging Columns (ID, outcome and covariates in beginning)
data_train <- data_train %>%
  select(FISNumber, QoL_simple, sex, twzyg, ea4fa_agg, ea4mo_agg, QoL_indicator,
         time_lag, age_qol, everything())

data_test <- data_test %>%
  select(FISNumber, QoL_simple, sex, twzyg, ea4fa_agg, ea4mo_agg, QoL_indicator,
         time_lag, age_qol, everything())


## Pre-processing steps

## saving outcome 
outcome_QoL <- data_train %>% select(QoL_simple)

data_train$QoL_indicator <- factor(data_train$QoL_indicator)

## A) standard ML preprocessing

## i) near-zero variance
nzv_train <- nearZeroVar(data_train)

data_train <- data_train[-nzv_train]

## re-adding outcome in case removed
if("QoL_simple" %in% colnames(data_train)){
  data_train <- data_train
} else { 
  data_train <- cbind(data_train, outcome_QoL)
}

## ii) high correlation (note that all columns must be numeric, df not changed here)
num_data <- data_train[, sapply(data_train, is.numeric)]

high_cor <- findCorrelation(cor(num_data,
                               use = "pairwise.complete.obs"),
                           cutoff = .95)

data_train <- data_train[-high_cor]

if("QoL_simple" %in% colnames(data_train)){
  data_train <- data_train
} else { 
  data_train <- cbind(data_train, outcome_QoL)
}

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

data_train <- data_train[-combos]

if("QoL_simple" %in% colnames(data_train)){
  data_train <- data_train
} else { 
  data_train <- cbind(data_train, outcome_QoL)
}


## iv) multicollinearity
vars_no_id <- colnames(data_train)[-1]
## feature selection by means of elastic net also takes care of 
## removing highly collinear variables



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

## same for test data
data_test <- data_test %>%
  mutate_at(vars_mult_class, as.numeric)

##----------------------------------------------------------------------------


## B) Imputing data

## Note: Outlier removal by means of the Minimum covariance determinant (MCD)
## can only be done with imputed data and will only be done as sensitivity
## analysis after obtaining stable ML models

## before imputation: Take out FISNR! it should not be part of the KNN procedure
df_FISNr_train <- data_train %>%
  select(FISNumber)

df_FISNr_test <- data_test %>%
  select(FISNumber)

df_A_train <- data_train %>%
  select(-FISNumber)

df_A_test <- data_test %>%
  select(-FISNumber)

## IMPORTANT: Do KNN IMPUTATION AGAIN! WITHOUT THE OUTCOME! Calculate anew
## and safe the workspace again


## KNN imputation: 
t1 <- Sys.time()

x_train <- df_A_train %>%
  select(-QoL_simple)

y_train <- df_A_train %>%
  select(QoL_simple)

x_test <- df_A_test %>%
  select(-QoL_simple)

y_test <- df_A_test %>%
  select(QoL_simple)

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

## reappending y to x (outcome to training set), for caret functions
x_train_comb <- cbind(y_train, x_train_imp)

x_test_comb <- cbind(y_test, x_test_imp)


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
adaptControl <- trainControl(method = "adaptive_cv",
                             number = 10, repeats = 10,
                             adaptive = list(min = 5, alpha = 0.05, 
                                             method = "gls", complete = FALSE),
                             search = "random")

set.seed(7)

# Train an elastic net regression model

## Note: This is adaptive hypertuning, what I want is Bayesian hypertuning

## here: still adjust names and later also work in the random seeds at a 
## tune length of e.g. 1000 (see Habets et al.)
t0_el <- Sys.time()
glm_adapt <- train(QoL_simple ~ ., data = x_train_comb,
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
    difftime(t1_el, t0_el, unit = "mins"))

## Next: extract non-zero coefficients predictors, also performance on test set
## and training set

## -----------------------------------------------------------------------



## Comparison with Bayesian hypertuning:
t0_bayes <- Sys.time()

## defining function that gives out the needed parameters

# Define the objective function
elastic_net_bayes <- function(alpha, lambda) {
  
  # Train the model using caret with glmnet
  model <- train(QoL_simple ~ ., 
                 data = x_train_comb,
                 method = "glmnet",
                 trControl = trainControl(method = "cv", number = 10), # 10-fold CV
                 # preProc = c("center", "scale"),
                 tuneGrid = data.frame(alpha = alpha, lambda = lambda))
  
  score <- -min(model$results$RMSE)  # Negative RMSE
  # Function finds maxima so inverting the output!
  
  # Debug: Print the parameters and score
  
  ## extracting optimal alpha and lambda from model
  alpha <- model$bestTune$alpha
  lambda <- model$bestTune$lambda
  cat("Alpha:", alpha, "Lambda:", lambda, "Score:", score, "\n")
  
  return(list(Score = score))  # Ensure proper list format
  ## here add other components that should be included in the final summary
  ## table provided by the bayesOpt function
}

## Run bayesian optimization

# Set the search bounds for hyperparameters
bounds <- list(alpha = c(0, 1), lambda = c(0.001, 1))

# Run Bayesian optimization
set.seed(7)
opt_results <- bayesOpt(FUN = elastic_net_bayes, bounds = bounds,
                        initPoints = 5, iters.n = 10)

# View the best parameters
print(opt_results)


# Extract best parameters

## This is the command you were looking for! 
data.frame(getBestPars(opt_results))
best_params <- getBestPars(opt_results)


t1_bayes <- Sys.time()

cat("duration bayesian hypertuning: ",
    difftime(t1_bayes, t0_bayes, unit = "mins"))


# Train the final model using the optimal parameters
final_model_bayes <- train(QoL_simple ~ ., 
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
    difftime(t2_bayes, t1_bayes, unit = "mins"))




## evaluating both models on test set and check if performance is
## significantly different (You have this somewhere in your resources)

## adaptive tuning
predictions_adapt <- predict(glm_adapt, newdata = x_test_comb)

## Bayesian hypertuning
predictions_bayes <- predict(final_model_bayes, newdata = x_test_comb)

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

## NEXT UP: compare the CIs

## CONTINUE HERE