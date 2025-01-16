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
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC")


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
x_train_comb <- cbind(df_FISNr_train, y_train, x_train_imp) %>%
  left_join(df_FIS_fam, by = "FISNumber")

x_test_comb <- cbind(y_test, x_test_imp) %>%
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
folds <- groupKFold(group = x_train_comb$FamilyNumber, k = 10)

adaptControl <- trainControl(method = "adaptive_cv",
                             number = 10, repeats = 10,
                             adaptive = list(min = 5, alpha = 0.05, 
                                             method = "gls", complete = FALSE),
                             search = "random",
                             index = folds)

set.seed(7)


# Define predictors by excluding ID and FamilyNumber and outcome
predictor_vars <- setdiff(names(x_train_comb), c("FISNumber", "FamilyNumber",
                                                 "QoL_simple"))

# Create a formula dynamically
formula <- as.formula(paste("QoL_simple ~", paste(predictor_vars, collapse = " + ")))

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

## Comparison with Bayesian hypertuning:
t0_bayes <- Sys.time()

## defining function that gives out the needed parameters

# Define the objective function
elastic_net_bayes <- function(alpha, lambda) {
  
  # Train the model using caret with glmnet
  model <- train(formula, 
                 data = x_train_comb,
                 method = "glmnet",
                 trControl = trainControl(method = "cv",
                                          number = 10,
                                          index = folds), # 10-fold CV
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

## Adressing the issue that there are factors in the dataset, those need
## to keep their original name, otherwise, the vector cannot be used for
## filtering the training set for the non-0 predictors
factor_predictors <- names(x_train_comb)[sapply(x_train_comb, is.factor)]

# Add an extra column to track the original predictor name
coef_df_adapt$OriginalPredictor <- coef_df_adapt$Predictor

# Check if the predictor is a factor level and strip the factor level part
coef_df_adapt$OriginalPredictor <- sapply(coef_df_adapt$Predictor, function(predictor) {
  # Check if the predictor matches any factor column name pattern
  match <- factor_predictors[sapply(factor_predictors, function(factor) startsWith(predictor, factor))]
  if (length(match) > 0) {
    # If matched, return the original factor column name
    return(match[1])
  } else {
    # If not a factor, return the predictor as is
    return(predictor)
  }
})


# Filter for non-zero coefficients
non_zero_coef_adapt <- coef_df_adapt[coef_df_adapt[, 1] != 0, ]

# Get the unique original column names for predictors with non-zero coefficients
non_zero_predictors_adapt <- unique(non_zero_coef_adapt$OriginalPredictor)

# Display the predictors with non-zero coefficients
non_zero_predictors_adapt

cat("count of non-0 coefficient predictors from elastic net with
    adaptive hypertuning: ", length(non_zero_predictors_adapt))


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

## Adressing the issue that there are factors in the dataset, those need
## to keep their original name, otherwise, the vector cannot be used for
## filtering the training set for the non-0 predictors
factor_predictors <- names(x_train_comb)[sapply(x_train_comb, is.factor)]

# Add an extra column to track the original predictor name
coef_df_bayes$OriginalPredictor <- coef_df_bayes$Predictor

# Check if the predictor is a factor level and strip the factor level part
coef_df_bayes$OriginalPredictor <- sapply(coef_df_bayes$Predictor, function(predictor) {
  # Check if the predictor matches any factor column name pattern
  match <- factor_predictors[sapply(factor_predictors, function(factor) startsWith(predictor, factor))]
  if (length(match) > 0) {
    # If matched, return the original factor column name
    return(match[1])
  } else {
    # If not a factor, return the predictor as is
    return(predictor)
  }
})




# Filter for non-zero coefficients
non_zero_coef_bayes <- coef_df_bayes[coef_df_bayes[, 1] != 0, ]

# Get the unique original column names for predictors with non-zero coefficients
non_zero_predictors_bayes <- unique(non_zero_coef_bayes$OriginalPredictor)

# Display the predictors with non-zero coefficients
# non_zero_predictors_bayes <- non_zero_coef_bayes$Predictor
non_zero_predictors_bayes

cat("count of non-0 coefficient predictors from elastic net with
    bayesian hypertuning: ", length(non_zero_predictors_bayes))

intersect(non_zero_predictors_adapt, non_zero_predictors_bayes)
intersect_count <- 0
for(predictor in non_zero_predictors_bayes){
  if(predictor %in% non_zero_predictors_adapt){
    cat(predictor, " contained in non zero predictors adapt", "\n", "\n")
    intersect_count <- intersect_count + 1
  }
}
intersect_count

## features remaining after adaptive hypertuning: 40

## features remaining after Bayesian hypertuning: 23

predictors_A_adapt <- grep("(Intercept)",
                          non_zero_predictors_adapt,
                          value = TRUE, invert = TRUE)


predictors_A_bayes <- grep("(Intercept)",
                          non_zero_predictors_bayes,
                          value = TRUE, invert = TRUE)


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

## check if column names add up
ncol(x_train_ML) == length(predictors_A_bayes) + 3

# Define predictors by excluding ID and FamilyNumber and outcome
predictor_vars <- setdiff(names(x_train_ML), c("FISNumber", "FamilyNumber",
                                               "QoL_simple"))
# Create a formula dynamically
formula <- as.formula(paste("QoL_simple ~", paste(predictor_vars, collapse = " + ")))

# Create custom 10-fold cross-validation keeping families together
set.seed(7)
folds <- groupKFold(group = x_train_ML$FamilyNumber, k = 10)



## A) Random forest (with Bayesian parameter hypertuning)
set.seed(7)


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
  
  # Train the random forest model
  model <- ranger(
    formula = formula,
    data = x_train_ML,
    mtry = mtry,
    max.depth = max.depth,
    min.node.size = min.node.size,
    num.trees = num.trees
  )
  
  ## STILL BUILD IN CROSS VALIDATION!!!!
  
  # Calculate predictions and RMSE
  predictions <- model$predictions
  actuals <- x_train_ML$QoL_simple  # Extract target variable
  rmse <- sqrt(mean((actuals - predictions)^2))
  
  # Return negative RMSE (Bayesian optimization minimizes the score)
  return(list(Score = -rmse))
}

# Define the search bounds for hyperparameters
bounds_rf <- list(
  mtry = c(1L, ncol(x_train_ML) - 3L),
  max.depth = c(3L, 30L),
  min.node.size = c(1L, 50L),
  num.trees = c(100L, 1500L)
)

# Run Bayesian optimization
set.seed(7)
opt_results_rf <- bayesOpt(
  FUN = rf_bayes,
  bounds = bounds_rf,
  initPoints = 5,
  iters.n = 10,
  acq = "ei"
)

# View the best parameters
print(opt_results_rf)

# Extract the best parameters
best_params_rf <- getBestPars(opt_results_rf)
print(best_params_rf)


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



## CONTINUE HERE!!!

# Train the final model using the optimal parameters
## I can still train the final model with caret! 
model_bayes_rf <- train(formula, 
                        data = x_train_ML,
                        method = "ranger",
                        trControl = trainControl(method = "none"),
                        # No CV for the final model
                        splitrule = "variance",
                        ## here, insert input from the optimal parameters!
                        ...)


t2_bayes_rf <- Sys.time()

cat("duration model training (random forest) after bayesian hypertuning: ",
    difftime(t2_bayes_rf, t1_bayes_rf, unit = "mins"), " minutes")





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


## Run again, CONTINUE HERE!!!

t0_bayes_svr <- Sys.time()


# Training control
train_control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = FALSE
)

## defining function that gives out the needed parameters
# define the objective function for Bayesian optimization
svr_bayes <- function(C, sigma, degree, scale, method) {
  
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
  set.seed(7)
  model <- train(
    formula,
    data = x_train_ML,
    method = model_method,
    trControl = train_control,
    tuneGrid = tune_grid
  )
  
  score <- -min(model$results$RMSE)
  
  # Return negative RMSE (Bayesian optimization minimizes the score)
  return(list(Score = score))
  
}

# Define the search bounds for hyperparameters
bounds_svr <- list(
  C = c(0.1, 10),            # Range for C
  sigma = c(0.001, 0.1),     # Range for sigma
  degree = c(2, 4),          # Range for degree
  scale = c(0.001, 0.1),     # Range for scale
  method = c(0, 1)           # Encodes categorical: 0 = Radial, 1 = Poly
)

# Run Bayesian optimization
set.seed(7)
opt_results_svr <- bayesOpt(
  FUN = svr_bayes,
  bounds = bounds_svr,
  initPoints = 10,
  iters.n = 30,
  acq = "ei",
  verbose = TRUE
)

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
  ## here also add other parameters
} else {
  method_svr <- "svmPoly"
  ## here also add other parameters
}


##-----------------------------------------------------------------------------


## C) XGBoost (with bayesian parameter hypertuning)

## How to do this is in your vault where you inspect Bayesian hypertuning
## there is an XGBoost example















