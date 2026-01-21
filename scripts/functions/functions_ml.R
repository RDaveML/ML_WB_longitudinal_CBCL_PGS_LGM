## functions to automatize ML procedure with different datasets 
## and different models 


## negation operator
`%notin%` <- Negate(`%in%`)

# options(scipen = 999, expressions = 50000)

## converting categorical covariates to factors
f_conv <- function(df, covariates){
  data_converted <- df
  for(covariate in covariates){
    data_converted[, covariate] <- as.factor(data_converted[, covariate])
  }
  return(data_converted)
}

#############################################################################

## function to list types of variables in dataframe
class_info <- function(df){
  arg_name <- deparse(substitute(df))
  print(paste("The argument name is:", arg_name))
  
  ## saving variables of different types and counting
  vars_num <- vector()
  vars_char <- vector()
  vars_factor <- vector()
  vars_int <- vector()
  vars_bool <- vector()
  vars_other <- vector()
  vars_mult_class <- vector()
  length_num <- 0
  length_char <- 0
  length_factor_vars <- 0
  length_int <- 0
  length_bool <- 0
  length_other <- 0
  length_mult_class <- 0
  
  for(var in 1:ncol(df)){
    variable <- df[, var]
    if(length(class(variable)) > 1) {
      vars_mult_class <- c(vars_mult_class, colnames(df[var]))
      length_mult_class <- length_mult_class + 1
    } else if(class(variable) == "numeric"){
      vars_num <- c(vars_num, colnames(df[var]))
      length_num <- length_num + 1
    } else if(class(variable) == "character"){
      vars_char <- c(vars_char, colnames(df[var]))
      length_char <- length_char + 1
    } else if(class(variable) == "integer"){
      vars_int <- c(vars_int, colnames(df[var]))
      length_int <- length_int + 1
    } else if(class(variable) == "logical") {
      vars_bool <- c(vars_bool, colnames(df[var]))
      length_bool <- length_bool + 1
    } else if(class(variable) == "factor"){
      vars_factor <- c(vars_factor, colnames(df[var]))
      length_factor_vars <- length_factor_vars + 1
    } else {
      vars_other <- c(vars_other, colnames(df[var]))
      length_other <- length_other + 1
    }
  }
  return(list(df_name = arg_name,
         count_classes = list(length_num = length_num,
                              length_char = length_char,
                              length_factor_vars = length_factor_vars,
                              length_int = length_int,
                              length_bool = length_bool,
                              length_other = length_other,
                              length_mult_class = length_mult_class),
         types_vectors = list(vars_num = vars_num,
                              vars_char = vars_char,
                              vars_factor = vars_factor,
                              vars_int = vars_int,
                              vars_bool = vars_bool,
                              vars_other = vars_other,
                              vars_mult_class = vars_mult_class)
            )
         )
         
}

##############################################################################

## converting columns in dataset that had multiple column types to
## purely numeric, important preprocessing function, will enable 
## processing of data with specific R functions smoothly
mult_to_numeric <- function(df){
  
  vars_mult_class <- vector()
  length_mult_class <- 0
  
  for(var in 1:ncol(df)){
    variable <- df[, var]
    if(length(class(variable)) > 1) {
      vars_mult_class <- c(vars_mult_class, colnames(df[var]))
      length_mult_class <- length_mult_class + 1
    } else {
      next
    }
  }

  if(length_mult_class > 0){
    
    df_converted <- df %>%
      mutate_at(vars_mult_class, as.numeric)
  } else {
    df_converted <- df
  }
  
  return(df_converted)
}


## ML preprocessing function taking current dataset and train and test split 
## will be looped over all B = 100 bootstrapped datasets,
## takes as input dataset, train IDs, test IDs and vector of covariate names
ml_preprocess <- function(df, train_ids, test_ids, covariates){
  
  df_FIS_fam <- df %>%
    select(FISNumber, FamilyNumber)
  
  ## train test split
  data_train <- df %>% 
    filter(FISNumber %in% train_ids)
  
  data_test <- df %>%
    filter(FISNumber %in% test_ids)
  
  ## re-arranging columns
  data_train <- data_train %>%
    select(FISNumber, QoL_simple, all_of(covariates), everything())
  
  data_test <- data_test %>%
    select(FISNumber, QoL_simple, all_of(covariates), everything())
  
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
    select(FISNumber, all_of(covariates))
  
  data_covariates_test <- data_test %>%
    select(FISNumber, all_of(covariates))
  
  ## For preprocessing: Remove all columns that are not supposed to be removed 
  ## by the preprocessing steps, based on the training data, later
  ## filter test data with remaining variables and append FISNumber again when 
  ## needed
  data_train <- data_train %>%
    select(-all_of(covariates), -FISNumber, -QoL_simple, -FamilyNumber)
  

  ## i) near-zero variance removal
  nzv_train <- nearZeroVar(data_train)
  
  
  if (length(nzv_train) != 0) {
    data_train <- data_train[, -nzv_train, drop = FALSE]
  }
  
  
  cat(length(nzv_train), " columns removed (near zero variance)", "\n")
  
  cat(ncol(data_train), " columns remaining in training set", "\n")
  
  
  ## ii) high correlation removal
  ## (note that all columns must be numeric, df not changed here)
  ## Note: here no non-numeric column, still kept in in case changes
  num_data <- data_train[, sapply(data_train, is.numeric)]
  
  high_cor <- findCorrelation(cor(num_data,
                                  use = "pairwise.complete.obs"),
                              cutoff = .95)
  
  if(length(high_cor) != 0){
  num_data <- num_data[-high_cor]
  
  cat(length(high_cor), " columns removed (high correlation; .95)", "\n")
  cat(ncol(data_train), " columns remaining in training set", "\n")
  
  ## re-appending non-numeric columns (here: none)
  data_train <- cbind(data_train[, !sapply(data_train, is.numeric)], num_data)
  } else {
    cat(length(high_cor), " columns removed (high correlation; .95)", "\n")
    cat(ncol(data_train), " columns remaining in training set", "\n")
    data_train <- data_train
  }
  
  ## iii) linear dependence (note: does not allow NAs, in between imputation)
  ## executed prior
  
  # Create a preprocessing object for median imputation
  ## note: This is only to find linear combiations of columns which are intended 
  ## to be removed! Will not be actual imputation of data for ML
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
  
  if(!is.null(combos)){
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
  } else {
    
    data_train <- data_train
    
    cat(length(combos), " columns removed (linear combinations)", "\n")
    
    cat(ncol(data_train), " remaining columns in training set", "\n")
    
  }
  
  ## (feature selection by means of elastic net also takes care of 
  ## removing highly collinear variables, thus additional step taking care of 
  ## multicollinear variables not needed)
  
  
  ## selecting variables in test set that are also contained in training set
  data_test <- data_test %>%
    select(all_of(colnames(data_train)))
  
  ##----------------------------------------------------------------------------
  
  
  ## B) Imputing data (ML does not allow for NAs)
  
  ## before imputation: Take out FISNR! it should not be part of the KNN procedure
  
  ## Rejoining with covariates 
  df_train <- data_covariates_train %>%
    cbind(data_train)
  
  
  df_test <- data_covariates_test %>%
    cbind(data_test)
  
  ## IMPORTANT: KNN imputation needs to happen without outcome
  
  ## Needs to evaluate to FALSE
  if("QoL_simple" %in% colnames(df_train) |
     "QoL_simple" %in% colnames(df_test)) {
    stop("Error: Outcome must be removed before KNN imputation. Script stopped")
  }
  
  
  ## KNN imputation: 
  t1 <- Sys.time()
  
  x_train <- df_train %>%
    select(-FISNumber)
  
  ## was created previously
  y_train <- y_train
  
  
  x_test <- df_test %>%
    select(-FISNumber)
  
  y_test <- y_test
  
  cat("Beginning KNN imputation training data", "\n")
  ## determining k (square root of number of predictors, following 
  ## e.g. Pelt et al., 2024)
  k_pad <- round(sqrt(ncol(x_train)))
  train_pre_obj <- preProcess(x_train,
                              method = "knnImpute",
                              k = k_pad)
  
  t2 <- Sys.time()
  
  cat("duration KNN imputation object: ", difftime(t2, t1, unit = "mins"), "\n")
  
  x_train_imp <- predict(train_pre_obj, x_train)
  
  t3 <- Sys.time()
  
  cat("duration KNN imputation train data: ",
      difftime(t3, t2, unit = "mins"), "\n")
  
  ## running imputation on test data, solely informed by training data, no
  ## leakage
  x_test_imp <- predict(train_pre_obj, x_test)
  
  t4 <- Sys.time()
  
  cat("duration KNN imputation test data: ",
      difftime(t4, t3, unit = "mins"), "\n")
  
  ## check if there are still missing values
  sum(colMeans(is.na(x_train_imp)) != 0)
  if(sum(colMeans(is.na(x_train_imp)) != 0)){
    cat("The following columns still contain NAs: ", "\n", 
        colnames(x_train_imp)[which(colMeans(is.na(x_train_imp)) != 0)])
    stop("Error: Still columns with missings")
  }
  sum(colMeans(is.na(x_test_imp)) != 0)
  ## columns in test set are those which are not yet removed
  
  ## reappending y to x (outcome to training set), for caret functions, also 
  ## family for doing the family split!
  x_train_comb <- cbind(df_FISNr_train, y_train, x_train_imp) %>%
    left_join(df_FIS_fam, by = "FISNumber")
  
  x_test_comb <- cbind(df_FISNr_test, y_test, x_test_imp) %>%
    left_join(df_FIS_fam, by = "FISNumber")
  
  ## returning all objects needed, can straight be passed to ML pipeline
  
  return(list(df_FISNr_train = df_FISNr_train,
              df_FISNr_test = df_FISNr_test,
              x_train_comb = x_train_comb,
              x_test_comb = x_test_comb,
              y_train = y_train,
              y_test = y_test,
              data_covariates_train = data_covariates_train,
              data_covariates_test = data_covariates_test))
}

##-----------------------------------------------------------------------------

## Functions that carry out bayesian hypertuning of tuning parameters 
## for different ML models. The tuned parameters are then saved in the
## script where ML modelling is done and passed to the ultimate model 


## Bayesian Hypertuning: First function that integrates hypertuning 
## for elastic net, then for rf, SVr and XGB

## takes as input dataset, folds object (family members need to stay together
## during cross-validation!), manually defined bounds for the parameter
## search space, number of cores to use during hypertuning
## and number of times function is run after initialization (iters.n) 
## and number of times to sample functions at each optimization step (iter.k)
## iters.k must be max = iters.n
bayes_hyper_enet <- function(df, folds, bounds_enet,
                             ncores = parallel::detectCores() - 2,
                             iters.n = 10,
                             iters.k = 10){
  
  ## making sure bounds is a list containing parameters "alpha" and "lambda"
  if(!is.list(bounds_enet) | 
     length(
       setdiff(
         c("alpha", "lambda"), names(bounds_enet)
         )
       ) != 0){
   stop("bounds_enet must be a list with elements alpha and lambda")
  }
  
  ## name to export to cluster
  df_name <- deparse(substitute(df))
  
  ## excluding variables from being predictors
  ## creating x and y to avoid problems with formula object
  exclude_vars <-  c("FISNumber", "FamilyNumber", "QoL_simple")
  x_train_matrix <- model.matrix(~ ., data = df)[
    , !(colnames(model.matrix(~ ., data = df)) %in% exclude_vars)]
  
  ## saving outcome vector
  y_train_vector <- df$QoL_simple
  
  
  # Initialize shared variable for best result
  ## these are set globally! So that they are available to the 
  ## elastic_net_bayes function
  best_result_so_far <- NULL
  early_stopping_triggered <- FALSE
  
  
  # Define the objective function
  elastic_net_bayes <- function(alpha, lambda) {
    
    # Prevent further iterations if early stopping was triggered
    # (stored globally)
    if (early_stopping_triggered) {
      return(list(Score = best_result_so_far$Score))
      # Return best result found so far
    }
    
    # Train model with 10 fold CV using sampled hypertuning parameters
    model <- train(x = x_train_matrix, 
                   y = y_train_vector,  
                   method = "glmnet",
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            index = folds, 
                                            allowParallel = FALSE),
                   tuneGrid = data.frame(alpha = alpha, lambda = lambda))
    
    score <- -min(model$results$RMSE)  # Negative RMSE as outcome 
    ## since bayesOpt function maximizes
    
    
    # Check for early stopping, otherwise update best score list
    minUtility <- 0.001
    if (!is.null(best_result_so_far) && 
        (score - best_result_so_far$Score < minUtility)) {
          best_result_so_far$noImprovementCount <<- 
            best_result_so_far$noImprovementCount + 1
    } else {
        best_result_so_far <<- 
          list(Score = score, alpha = alpha, lambda = lambda,
               noImprovementCount = 0)
    }
    
    # Flag early stopping if no improvement for 5 iterations
    if (best_result_so_far$noImprovementCount >= 5) {
      early_stopping_triggered <<- TRUE
      # Set flag to prevent further evaluations
    } # to global environment
    
    ## Note: This ensures that upon running again, no further model will 
    ## be trained if there was no improvement previously
    ## bayesOpt will still run all iterations, but it will only always return 
    ## the earlier score, saving a lot of time
    
    return(list(Score = best_result_so_far$Score))
    # Always return the best score found
  }
  
  
  ## integrating parallelization
  cl <- makeCluster(ncores)
  
  ## (study was run in parallel with 96 cores on computational cluster SNELLIUS)
  
  registerDoParallel(cl)
  clusterExport(cl, c("folds", df_name, "elastic_net_bayes", "x_train_matrix",
                      "y_train_vector",
                      'best_result_so_far',
                      'early_stopping_triggered'),
                envir = environment())
  ## Manually export the formula because `clusterExport` struggles with formulas
  
  ## suppress printing content that is being exported
  invisible(clusterEvalQ(cl,expr= {
    library(glmnet)
    library(caret)
    library(dplyr)
  }))
  invisible(clusterEvalQ(cl, ls()))
  
  ## running optimization function, bayesOpt uses the above defined 
  ## training function
  tWithPar <- system.time(
    opt_results_enet <- bayesOpt(
      FUN = elastic_net_bayes,
      bounds = bounds_enet,
      initPoints = 5,
      ## initPoints must be greater than the number of FUN inputs
      iters.n = iters.n,
      iters.k = iters.k,
      ## optional: Specify other stopping triggers
      ## otherHalting = list(timeLimit = 30000, minUtility = NULL)
      parallel = TRUE,
      verbose = 1
    )
  )
  
  ## stopping cluster again
  stopCluster(cl)
  registerDoSEQ()
  
  ## print best results
  print(opt_results_enet)

  print(tWithPar)
  
  ## extracting best hypertuning parameters
  best_params_enet <- getBestPars(opt_results_enet)
  
  ## saving meta-information on hypertuning
  niters <- opt_results_enet$iters
  
  stopStatus <- opt_results_enet$stopStatus
  
  totalTime <- opt_results_enet$elapsedTime
  
  ## train the final model with the best set of hypertuning parameters
  
  model_enet <- train(x = x_train_matrix,  # Use filtered predictors
                      y = y_train_vector,  # Response variable
                      method = "glmnet",
                      trControl = trainControl(method = "none"),
                      # No CV for the final model as parameters were already 
                      ## optimized
                      tuneGrid = data.frame(alpha = best_params_enet$alpha,
                                            lambda = best_params_enet$lambda))
  
  ## return optimized tuned model
  final_model_bayes <- model_enet$finalModel
  
  # cv tuned lambda
  best_lambda_bayes <- final_model_bayes$lambdaOpt
  
  # Extract the coefficients for tuned lambda
  coef_matrix_bayes <- coef(final_model_bayes, s = best_lambda_bayes)
  
  # Convert to df
  coef_df_bayes <- as.data.frame(as.matrix(coef_matrix_bayes))
  coef_df_bayes$Predictor <- rownames(coef_df_bayes)
  rownames(coef_df_bayes) <- NULL
  
  
  # Filter for non-zero coefficients (those are the variables
  # that should be kept for subsequent prediction models!)
  non_zero_coef_bayes <- coef_df_bayes[coef_df_bayes[, 1] != 0, ]
  
  non_zero_predictors_bayes <- unique(non_zero_coef_bayes$Predictor)
  
  ## Removing Intercept (not needed for tree-based models)
  non_zero_predictors_bayes <- grep("(Intercept)",
                                    non_zero_predictors_bayes,
                                    value = TRUE, invert = TRUE)
  
  ## returning output to be passed into next step in pipeline, coding of 
  ## other ML prediction models
  return(list(best_params_enet = best_params_enet,
              time_hypertuning = tWithPar,
              non_zero_predictors = non_zero_predictors_bayes,  
              niters = niters,
              stopStatus = stopStatus, 
              totalTime = totalTime,
              early_stopping_triggered  = early_stopping_triggered,
              model_enet = model_enet)) ## model object to also make predictions
  
  
}

###############################################################################

## function to carry out Bayesian hypertuning for random forest model, 
## works analogue to elastic net hypertuning function

bayes_hyper_rf <- function(df_train, df_test, folds, bounds_rf,
                           ncores = parallel::detectCores() - 2,
                           iters.n = 10,
                           iters.k = 10){

  ## making sure bounds is a list containing parameters 
  ## mtry, max.depth, min,node.size and num.trees
  if(!is.list(bounds_rf) | 
     length(
       setdiff(
         c("mtry", "max.depth", "min.node.size", "num.trees"),
         names(bounds_rf)
       )
     ) != 0){
    stop("bounds_rf must be a list with elements mtry, max.depth,
         min.node.size and num.trees")
  }
  
  ## name to export to cluster
  df_train_name <- deparse(substitute(df_train))
  
  ## excluding variables from being predictors
  ## creating x and y to avoid problems with formula object
  exclude_vars <-  c("FISNumber", "FamilyNumber", "QoL_simple")
  x_train_matrix <- model.matrix(~ . - 1, data = df_train)[
    , !(colnames(model.matrix(~ . - 1, data = df_train)) %in% exclude_vars)]
  y_train_vector <- df_train$QoL_simple

  ## making sure datasets are aligned
  cat("difference colnames between both datasets: ", "\n")
  setdiff(colnames(df_train %>% select(-all_of(exclude_vars))),
          colnames(x_train_matrix))

  cat("difference colnames between both datasets: ", "\n")
  setdiff(colnames(x_train_matrix),
          colnames(df_train %>% select(-all_of(exclude_vars))))

  # Initialize shared variable for best result
  ## these are set globally! So that they are available to the 
  ## rf_bayes function
  best_result_so_far <- NULL
  early_stopping_triggered <- FALSE
  
  # Define the objective function
  rf_bayes <- function(mtry, max.depth, min.node.size, num.trees) {
    
    # Prevent further iterations if early stopping
    # was triggered (stored globally)
    if (early_stopping_triggered) {
      return(list(Score = best_result_so_far$Score))
      # Return best result found so far
    }
    
    # Convert parameters to be hypertuned to integers
    mtry <- as.integer(mtry)
    max.depth <- as.integer(max.depth)
    min.node.size <- as.integer(min.node.size)
    num.trees <- as.integer(num.trees)
  
    
    ## initiating cross-validation by hand
    # Initialize an empty vector to store RMSE for each fold
    cv_rmse_rf <- numeric(length(folds))
    
    # Loop over each fold
    for (i in seq_along(folds)) {
      # Get the training and validation indices
      indices_x_train_ML <- unlist(folds[-i]) # All except the current fold
      indices_val <- folds[[i]]          # Current fold for validation
      
      # Split the data into training and validation sets
      x_train_rf <- x_train_matrix[indices_x_train_ML, ]
      y_train_rf <- y_train_vector[indices_x_train_ML]
      x_val_rf <- x_train_matrix[indices_val, ]
      y_val_rf <- y_train_vector[indices_val]
      
      # Train the ranger model
      model <- ranger(
        dependent.variable.name = "y",  # Target variable name in data frame
        data = data.frame(y = y_train_rf, x_train_rf),
        # Combine y and x into a data frame
        mtry = mtry,
        max.depth = max.depth,
        min.node.size = min.node.size,
        num.trees = num.trees
      )
      
      # Make predictions on the validation set
      predictions <- predict(model, data = data.frame(x_val_rf))$predictions
      ## ranger::predict only works with data = 
      
      # Calculate RMSE for the current fold
      fold_rmse <- sqrt(mean((predictions - y_val_rf)^2))
      
      # Store the RMSE
      cv_rmse_rf[i] <- fold_rmse
    }
    
    # Calculate overall cross-validated RMSE
    mean_cv_rmse <- mean(cv_rmse_rf)
    
    score <- -min(mean_cv_rmse)
    ## bayesOpt function maximizes, so taking negative score
    
    ## Cross-validation was built in by hand
    
    # Check for early stopping
    minUtility <- 0.001
    if (!is.null(best_result_so_far) &&
        (score - best_result_so_far$Score < minUtility)) {
      best_result_so_far$noImprovementCount <<- 
        best_result_so_far$noImprovementCount + 1
      ## global assignment, i.e. assinging this variable to be accessible outside
      ## the rf_bayes function (but not the outer function)
    } else {
      best_result_so_far <<- list(Score = score, noImprovementCount = 0)
    }
    
    # Trigger early stopping if no improvement for 5 iterations
    if (best_result_so_far$noImprovementCount >= 5) {
      early_stopping_triggered <<- TRUE
      # Set flag to prevent further evaluations
    } ## to global environment
    
    ## Note: This ensures that upon running again, no further model will 
    ## be trained if there was no improvement previously
    ## bayesOpt will still run all iterations, but it will only always return 
    ## the earlier score, saving a lot of time
    
    return(list(Score = best_result_so_far$Score))
    # Always return the best score found
  }
  
  
  ## Optimizing the hypertuning of random forest parameters
  
  ## To parallelize
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  clusterExport(cl,c(df_train_name, 'folds', 'rf_bayes',
                     'x_train_matrix', 'y_train_vector',
                     'best_result_so_far',
                     'early_stopping_triggered',
                     'bounds_rf'),
                envir = environment())
  invisible(clusterEvalQ(cl,expr= {
    library(ranger)
    library(caret)
    library(dplyr)
  }))
  invisible(clusterEvalQ(cl, ls()))
  
  ## running the hypertuning
  tWithPar_rf <- system.time(
    opt_results_rf <- bayesOpt(
      FUN = rf_bayes,
      bounds = bounds_rf,
      initPoints = 5,
      ## initPoints must be greater than the number of FUN inputs
      iters.n = iters.n,
      iters.k = iters.k,
      ## optional: different stopping trigger
      ## otherHalting = list(timeLimit = 6000),
      ## very low but this is only for testing
      parallel = TRUE,
      verbose = 1,
      acq = "ei"
      ## acquisition function type, "ei" signalizes expected improvement
    )
  )
  
  ## ending cluster
  stopCluster(cl)
  registerDoSEQ()
  
  # View the best parameters
  print(opt_results_rf)
  
  print(tWithPar_rf)
  
  
  # Extract the best parameters
  best_params_rf <- getBestPars(opt_results_rf)
  
  niters <- opt_results_rf$iters
  
  stopStatus <- opt_results_rf$stopStatus
  
  totalTime <- opt_results_rf$elapsedTime
  
  
  tune_grid_rf <- data.frame(
    ## note: tuneGrid only accepts mtry, min.node.size and splitrule for rf
    mtry = best_params_rf$mtry,
    # max.depth = best_params_rf$max.depth,
    min.node.size = best_params_rf$min.node.size,
    # num.trees = best_params_rf$num.trees,
    splitrule = "variance"
  )
  
  
  
  # Train the final model using the optimal parameters
  model_bayes_rf <- ranger(
    dependent.variable.name = "QoL_simple",
    # Target variable name in data frame
    data = data.frame(QoL_simple = y_train_vector, x_train_matrix),
    # Combine y and x into a data frame
    # No CV for the final model, hypertuning parameters
    # were already crossvalidated
    mtry = best_params_rf$mtry,
    min.node.size = best_params_rf$min.node.size, 
    max.depth = best_params_rf$max.depth, 
    num.trees = best_params_rf$num.trees,
    replace = TRUE
  )
  
  ## same for bayesian optimized tuned model
  
  ## ensuring dataframes are exactly the same
  x_train_matrix_rf <- model.matrix(~ . - 1, data = df_train)[
    , !(colnames(model.matrix(~ . - 1, data = df_train)) %in% exclude_vars)]

  
  ## predictions on training set with the final optimal model                          
  preds_rf_train <- predict(model_bayes_rf,
                            data = x_train_matrix_rf)$predictions

                            
 preds_df_train <- data.frame(predictions_rf = preds_rf_train,
                               FISNumber = df_train$FISNumber,
                               train = 1)                          
                              
  cat("predictions rf training set successful", "\n")

  cat("Best hypertuning parameters for random forest: ", "\n")
  print(best_params_rf)
  cat("\n")
  
  ## applying model to test set
  x_test_matrix_rf <- model.matrix(~ . - 1, data = df_test)[
    , !(colnames(model.matrix(~ . - 1, data = df_test)) %in% exclude_vars)]

  cat("creating test matrix successful", "\n")
  
  ## checking if columns align
  cat("columns training and test set match: ",
      all(colnames(x_train_matrix_rf) %in% colnames(x_test_matrix_rf)),
      all(colnames(x_test_matrix_rf) %in% colnames(x_train_matrix_rf)), "\n")

  cat("dimensions training set: ", dim(x_train_matrix_rf), "\n")
  cat("dimensions test set: ", dim(x_test_matrix_rf), "\n")


  preds_rf_test <- predict(model_bayes_rf,
                           data = x_test_matrix_rf)$predictions

  preds_df_test <- data.frame(predictions_rf = preds_rf_test,
                              FISNumber = df_test$FISNumber,
                              train = 0)
                            
  cat("predictions rf test set successful", "\n")
  
  cat("predictions training and test set have different length: ",
   length(preds_rf_train) == length(preds_rf_train), "\n", "\n")
  
  ## binding together obtaining full prediction df with
  ## FISNumber and train/test info column
  preds_df_rf <- rbind(preds_df_train, preds_df_test)

  cat("predictions dataframe created", "\n")

  ## return final output list to be used in analysis scripts
  return(list(best_params_rf = best_params_rf,
              time_hypertuning = tWithPar_rf,
              early_stopping_triggered  = early_stopping_triggered,
              niters = niters, 
              stopStatus = stopStatus,
              totalTime = totalTime,
              model_bayes_rf = model_bayes_rf,
              preds_rf_train = preds_rf_train,
              preds_rf_test = preds_rf_test,
              preds_df_rf = preds_df_rf))
  
}
  
#############################################################################

## function to carry out Bayesian hypertuning for random forest model, 
## works analogue to elastic net hypertuning function

bayes_hyper_svr <- function(df_train, df_test, folds, bounds_svr,
                           ncores = parallel::detectCores() - 2,
                           iters.n = 10,
                           iters.k = 10){
  
  ## making sure bounds is a list containing parameters 
  ## C, sigma, degree, scale and method
  if(!is.list(bounds_svr) | 
     length(
       setdiff(
         c("C", "sigma", "degree", "scale", "method"),
         names(bounds_svr)
       )
     ) != 0){
    stop("bounds_svr must be a list with elements C, sigma, degree, scale,
         method")
  }
  
  ## name to export to cluster
  df_train_name <- deparse(substitute(df_train))
  
  ## excluding variables from being predictors
  ## creating x and y to avoid problems with formula object
  exclude_vars <-  c("FISNumber", "FamilyNumber", "QoL_simple")
  x_train_matrix <- model.matrix(~ . - 1, data = df_train)[
    , !(colnames(model.matrix(~ . - 1, data = df_train)) %in% exclude_vars)]
  y_train_vector <- df_train$QoL_simple
  
  # Initialize shared variable for best result
  ## these are set globally! So that they are available to the 
  ## svr_bayes function
  best_result_so_far <- NULL
  early_stopping_triggered <- FALSE
  
  # Define the objective function
  svr_bayes <- function(C, sigma, degree, scale, method) {
    
    # Prevent further iterations
    ## if early stopping was triggered (stored globally)
    if (early_stopping_triggered) {
      return(list(Score = best_result_so_far$Score))
      # Return best result found so far
    }
    
    ## Rounding degree to integer (necessary for model training)
    degree <- as.integer(ifelse(degree < 2.5, 2, 3))
    
    # Dynamically select parameters based on the method
    method <- ifelse(method < 0.5, "svmRadial", "svmPoly")
    
    ## defining tuneGrid depending on kernel (different input)
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
    
    
    # Train the model (support vector regression with 10-fold CV)
    model <- train(
      x = x_train_matrix,
      y = y_train_vector,
      method = model_method,
      trControl = trainControl(
        method = "cv",
        number = 10,
        verboseIter = TRUE,
        allowParallel = FALSE,
        index = folds),
      ## making sure the family split is still applied 
      tuneGrid = tune_grid
    )
    
    ## negative score as bayesOpt function maximizes
    score <- -min(model$results$RMSE)
    
    # Check for early stopping
    minUtility <- 0.001
    if (!is.null(best_result_so_far) && 
        (score - best_result_so_far$Score < minUtility)) {
      best_result_so_far$noImprovementCount <<- 
        best_result_so_far$noImprovementCount + 1
      ## global assignment, 
      ## i.e. assinging this variable to be accessible outside
      ## the rf_bayes function (but not the outer function)
    } else {
      best_result_so_far <<- list(Score = score, noImprovementCount = 0)
    }
    
    # Trigger early stopping if no improvement for 5 iterations
    if (best_result_so_far$noImprovementCount >= 5) {
      early_stopping_triggered <<- TRUE
      # Set flag to prevent further evaluations
    } ## to global environment
    
    ## Note: This ensures that upon running again, no further model will 
    ## be trained if there was no improvement previously
    ## bayesOpt will still run all iterations, but it will only always return 
    ## the earlier score, saving a lot of time
    
    return(list(Score = best_result_so_far$Score)) 
    # Always return the best score found
  }
  
  
  ## Optimizing the hypertuning of support vector regression parameters
  
  ## To parallelize
  cl <- makeCluster(ncores)

  registerDoParallel(cl)
  clusterExport(cl,c(df_train_name, 'folds', 'svr_bayes',
                     'x_train_matrix', 'y_train_vector',
                     'best_result_so_far',
                     'early_stopping_triggered', 'bounds_svr'),
                envir = environment())
  invisible(clusterEvalQ(cl,expr= {
    library(caret)
    library(dplyr)
    library(kernlab)
  }))
  invisible(clusterEvalQ(cl, ls()))
  
  tWithPar_svr <- system.time(
    opt_results_svr <- bayesOpt(
      FUN = svr_bayes,
      bounds = bounds_svr,
      initPoints = 10,
      ## initPoints must be greater than the number of FUN inputs
      iters.n = iters.n,
      iters.k = iters.k,
      ## optional: other stopping trigger
      ## otherHalting = list(timeLimit = 6000),
      ## very low but this is only for testing
      parallel = TRUE,
      verbose = 1,
      acq = "ei"
    )
  )
  
  ## ending cluster
  stopCluster(cl)
  registerDoSEQ()
  
  # View the best parameters
  print(opt_results_svr)
  
  print(tWithPar_svr)
  
  
  # Extract the best parameters
  best_params_svr <- getBestPars(opt_results_svr)
  
  niters <- opt_results_svr$iters
  
  stopStatus <- opt_results_svr$stopStatus
  
  totalTime <- opt_results_svr$elapsedTime
  
  
  ## train final model, depending on outcome of hypertuning
  if(best_params_svr$method < 0.5) {
    method_svr <- "svmRadial"
    tune_grid_svr <- data.frame(
      C = best_params_svr$C, ## optimal C
      sigma = best_params_svr$sigma ## optimal sigma
    )

    
  } else {
    method_svr <- "svmPoly"
    tune_grid_svr <- data.frame(
      C = best_params_svr$C, ## optimal C
      degree = round(best_params_svr$degree), 
      ## optimal degree, round to integer
      scale = best_params_svr$scale # ## optimal scale,
    )
  }
  
  
  model_bayes_svr <- train(
    x = x_train_matrix,
    y = y_train_vector,
    method = method_svr,
    trControl = trainControl(method = "none"), ## no CV for final model
    tuneGrid = tune_grid_svr
  )

  ## ensuring dataframes are exactly the same
  x_train_matrix_svr <- model.matrix(~ . - 1, data = df_train)[
    , !(colnames(model.matrix(~ . - 1, data = df_train)) %in% exclude_vars)]

  preds_svr_train <- predict(model_bayes_svr,
                            newdata = x_train_matrix_svr)

  preds_df_train <- data.frame(predictions_svr = preds_svr_train,
                             FISNumber = df_train$FISNumber,
                             train = 1)   
                              
  cat("predictions svr training set successful", "\n")

  x_test_matrix_svr <- model.matrix(~ . - 1, data = df_test)[
    , !(colnames(model.matrix(~ . - 1, data = df_test)) %in% exclude_vars)]

  preds_svr_test <- predict(model_bayes_svr,
                           newdata = x_test_matrix_svr)
  
  preds_df_test <- data.frame(predictions_svr = preds_svr_test,
                            FISNumber = df_test$FISNumber,
                            train = 0)
                            
  cat("predictions svr test set successful", "\n")
  
  cat("predictions training and test set have different length: ",
   length(preds_svr_train) == length(preds_svr_train), "\n", "\n")

   ## binding together obtaining full prediction df with
   ## FISNumber and train/test info column
  preds_df_svr <- rbind(preds_df_train, preds_df_test)

cat("predictions dataframe created", "\n")
  
## return final output list to be used in analysis scripts
  return(list(best_params_svr = best_params_svr,
              time_hypertuning = tWithPar_svr,
              early_stopping_triggered  = early_stopping_triggered,
              niters = niters, 
              stopStatus = stopStatus,
              totalTime = totalTime,
              model_bayes_svr = model_bayes_svr, 
              ## model object to also make predictions
              preds_svr_train = preds_svr_train,
              preds_svr_test = preds_svr_test,
              preds_df_svr = preds_df_svr))
  
}

##############################################################################

## function to carry out Bayesian hypertuning for xgb model, 
## structure analogue to the previous functions, 
## model is trained with the xgboost library, requiring different parameters

## input analogue to previous hypertuning functions
bayes_hyper_xgb <- function(df_train, df_test, folds, bounds_xgb,
                            ncores = parallel::detectCores() - 2,
                            iters.n = 10,
                            iters.k = 10){
  
  ## making sure bounds is a list containing the required parameters 
  if(!is.list(bounds_xgb) | 
     length(
       setdiff(
         c("num_parallel_tree",
           "max_depth",
           "min_child_weight",
           "subsample",
           "colsample_bytree",
           "eta",
           "gamma",
           "lambda",
           "alpha", 
           "nrounds"),
         names(bounds_xgb)
       )
     ) != 0){
    stop("bounds_xgb must be a list with elements num_parallel_tree,
                      max_depth,
                      min_child_weight,
                      subsample,
                      colsample_bytree,
                      eta,
                      gamma,
                      lambda,
                      alpha,
                      nrounds")
  }
  
  ## name to export to cluster
  df_train_name <- deparse(substitute(df_train))
  
  ## excluding variables from being predictors
  ## creating x and y to avoid problems with formula object
  exclude_vars <- c("FISNumber", "FamilyNumber", "QoL_simple")
  
  
  train_matrix <- as.matrix(df_train %>% select(-all_of(exclude_vars)))
  # Predictor matrix, necessary for xgboost
  train_labels <- as.matrix(df_train$QoL_simple)  # Labels
  
  
  # Initialize shared variable for best result
  ## these are set globally! So that they are available to the 
  ## xgb_bayes function
  best_result_so_far <- NULL
  early_stopping_triggered <- FALSE
  
  ## hypertuning function
  xgb_bayes <- function(num_parallel_tree,
                        max_depth,
                        min_child_weight,
                        subsample,
                        colsample_bytree,
                        eta,
                        gamma,
                        lambda,
                        alpha,
                        nrounds) {
    
    # Re-create dtrain inside the function to make sure data is in 
    # correct format
    dtrain <- xgboost::xgb.DMatrix(train_matrix, label = train_labels)
    
    ## converting all integer inputs to integer type
    num_parallel_tree <- round(num_parallel_tree)
    max_depth <- round(max_depth)
    min_child_weight <- round(min_child_weight)
    
    ## list of parameters to hypertune
    Pars <- list(
      booster = "gbtree",
      ## using default option for booster, no variation of this parameter
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
      ## outcome metric to optimize, default option
      eval_metric = "rmse"
    )
    
    ## training xgb model
    xgbcv <- xgb.cv(
      params = Pars,
      data = dtrain,
      nrounds = nrounds,
      folds = folds,
      early_stopping_rounds = 10,
      ## trigger early stopping after 10 rounds of no further improvement
      maximize = TRUE,
      verbose = 1
    )
    
    ## negative as bayesOpt function maximizes
    score = -min(xgbcv$evaluation_log$test_rmse_mean)
    
    # Check for early stopping
    minUtility <- 0.001
    if (!is.null(best_result_so_far) &&
        (score - best_result_so_far$Score < minUtility)) {
      best_result_so_far$noImprovementCount <<- 
        best_result_so_far$noImprovementCount + 1
      ## global assignment, i.e. 
      ## assinging this variable to be accessible outside
      ## the rf_bayes function (but not the outer function)
    } else {
      best_result_so_far <<- list(Score = score, noImprovementCount = 0)
    }
    
    # Trigger early stopping if no improvement for 5 iterations of the bayesian
    # hypertuning
    if (best_result_so_far$noImprovementCount >= 5) {
      early_stopping_triggered <<- TRUE
      # Set flag to prevent further evaluations
    } ## to global environment
    
    ## Note: This ensures that upon running again, no further model will 
    ## be trained if there was no improvement previously
    ## bayesOpt will still run all iterations, but it will only always return 
    ## the earlier score, saving a lot of time
    
    return(list(Score = best_result_so_far$Score))
    # Always return the best score found
  }
  
  ## initiating parallelization over set number of cores
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  clusterExport(cl,c(df_train_name, 'folds', 'xgb_bayes',
                     'train_matrix', 'train_labels',
                     'best_result_so_far',
                     'early_stopping_triggered', 'bounds_xgb'),
                envir = environment())

  invisible(clusterEvalQ(cl,expr= {
    library(caret)
    library(dplyr)
    library(xgboost)
  }))
  invisible(clusterEvalQ(cl, ls()))
  
  ## running hypertuning
  tWithPar_xgb <- system.time(
    opt_results_xgb <- bayesOpt(
      FUN = xgb_bayes,
      bounds = bounds_xgb,
      initPoints = 11,
      ## initPoints must be greater than the number of FUN inputs
      iters.n = iters.n,
      iters.k = iters.k,
      ## keeping other stopping trigger to counter overfitting
      otherHalting = list(timeLimit = 6000),
      parallel = TRUE,
      verbose = 1,
      acq = "ei"
    )
  )
  
  
  stopCluster(cl)
  registerDoSEQ()
  
  # View the best parameters
  print(opt_results_xgb)
  
  print(tWithPar_xgb)
  
  # Extract the best parameters
  best_params_xgb <- getBestPars(opt_results_xgb)
  
  niters <- opt_results_xgb$iters
  
  stopStatus <- opt_results_xgb$stopStatus
  
  totalTime <- opt_results_xgb$elapsedTime
  
  ## train final model
  ## Converting training data to xgb Matrix
  ## making sure that only predictor columns are contained in training set!
  dtrain_xgb <- xgboost::xgb.DMatrix(as.matrix(df_train %>%
                                                 select(-all_of(exclude_vars))),
                                     label = as.matrix(df_train$QoL_simple))
  
  dtest_xgb <- xgboost::xgb.DMatrix(as.matrix(df_test %>%
                                                select(-all_of(exclude_vars))),
                                    label = as.matrix(df_test$QoL_simple))
  
  ## parameter list for training final xgb model
  par_xgb <- list(
    booster = "gbtree",
    objective = "reg:squarederror", 
    eval_metric = "rmse",
    num_parallel_tree = best_params_xgb$num_parallel_tree,
    max_depth = best_params_xgb$max_depth,
    min_child_weight = best_params_xgb$min_child_weight,
    subsample = best_params_xgb$subsample,
    colsample_bytree = best_params_xgb$colsample_bytree,
    eta = best_params_xgb$eta,
    gamma = best_params_xgb$gamma,
    lambda = best_params_xgb$lambda,
    alpha = best_params_xgb$alpha,
    nthread = 1
  )
  
  ## nrounds was hypertuned as well
  nrounds_opt = best_params_xgb$nrounds

  ## Debugging before model training:
  cat("Missings in train matrix: ", sum(is.na(dtrain_xgb)), "\n", "\n")

  ## ensuring model object is class xgb.DMatrix
  cat("Class dtrain: ", class(dtrain_xgb), "\n", "\n")

  
  ## final model trained with xgb with hypertuning parameters extracted
  model_bayes_xgb <- xgb.train(
    data = dtrain_xgb,
    params = par_xgb,
    nrounds = nrounds_opt,
    verbose = 1
  )
  
  
  ## getting predictions (On train and test set)
  preds_xgb_train <- predict(model_bayes_xgb, dtrain_xgb)

  preds_df_train <- data.frame(predictions_xgb = preds_xgb_train,
                             FISNumber = df_train$FISNumber,
                             train = 1)  
  
  preds_xgb_test <- predict(model_bayes_xgb, dtest_xgb)

  preds_df_test <- data.frame(predictions_xgb = preds_xgb_test,
                            FISNumber = df_test$FISNumber,
                            train = 0)
  
  ## binding together obtaining full prediction df with
  ## FISNumber and train/test info column
  preds_df_xgb <- rbind(preds_df_train, preds_df_test)

  

  cat("predictions dataframe created", "\n")
  
  ## return final output list to be used in analysis scripts
  return(list(best_params_xgb = best_params_xgb,
              time_hypertuning = tWithPar_xgb,
              early_stopping_triggered  = early_stopping_triggered,
              niters = niters, 
              stopStatus = stopStatus,
              totalTime = totalTime,
              model_bayes_xgb = model_bayes_xgb, ## model object to also make predictions
              preds_xgb_train = preds_xgb_train,
              preds_xgb_test = preds_xgb_test,
              preds_df_xgb = preds_df_xgb))
  
}

# eoS


