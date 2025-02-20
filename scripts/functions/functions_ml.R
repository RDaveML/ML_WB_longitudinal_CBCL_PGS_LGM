## function to automatize ML procedure with different datasets 
## and different models 


## negation operator
`%notin%` <- Negate(`%in%`)

options(scipen = 999, expressions = 50000)

## converting factor covariates to factors
f_conv <- function(df, covariates){
  data_converted <- df
  for(covariate in covariates){
    data_converted[, covariate] <- as.factor(data_converted[, covariate])
  }
  return(data_converted)
}
## function works 

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
      #cat("multiclass variable; variable ", colnames(df[var]),
      #    " is class: ", class(variable), "\n", "\n")
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
      cat("other variable detected; variable ", colnames(df[var]),
          " is class: ", class(variable), "\n", "\n")
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
## this function also works

## converting columns in dataset that had multiple column types to
## numeric
mult_to_numeric <- function(df){
  
  vars_mult_class <- vector()
  length_mult_class <- 0
  
  for(var in 1:ncol(df)){
    variable <- df[, var]
    if(length(class(variable)) > 1) {
      cat("multiclass variable; variable ", colnames(df[var]),
          " is class: ", class(variable), "\n", "\n")
      vars_mult_class <- c(vars_mult_class, colnames(df[var]))
      length_mult_class <- length_mult_class + 1
    } else {
      next
    }
  }
  
  if(length_mult_class > 0){
    cat("variables ", vars_mult_class, " will be converted to numeric")
    df_converted <- df %>%
      mutate_at(vars_mult_class, as.numeric)
  }
  
  
  return(df_converted)
}
## function also works but get rid of some of the print clutter


## preprocessing function taking current dataset and train and test split 
## varies thus per bootstrap
ml_preprocess <- function(df, train_ids, test_ids, covariates){
  
  df_FIS_fam <- df %>%
    select(FISNumber, FamilyNumber)
  
  data_train <- df %>% 
    filter(FISNumber %in% train_ids)
  
  data_test <- df %>%
    filter(FISNumber %in% test_ids)
  
  ## Rearranging Columns (ID, outcome and covariates in beginning)
  ## Should they be avoided in feature selection?
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
  
  ## append this back to the dataset after removing columns!
  
  
  ## For preprocessing: Remove all columns that are not supposed to be removed 
  ## by the preprocessing steps, based on the training data, later
  ## filter test data with remaining variables and append FISNumber again when 
  ## needed
  data_train <- data_train %>%
    select(-all_of(covariates), -FISNumber, -QoL_simple, -FamilyNumber)
  
  
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
  
  ##----------------------------------------------------------------------------
  
  
  ## B) Imputing data
  
  ## Note: Outlier removal by means of the Minimum covariance determinant (MCD)
  ## can only be done with imputed data and will only be done as sensitivity
  ## analysis after obtaining stable ML models
  
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
  k_pad <- round(sqrt(ncol(x_train)))
  train_pre_obj <- preProcess(x_train,
                              method = "knnImpute",
                              k = k_pad)
  
  t2 <- Sys.time()
  
  cat("duration KNN imputation object: ", difftime(t2, t1, unit = "mins"), "\n")
  
  x_train_imp <- predict(train_pre_obj, x_train)
  
  t3 <- Sys.time()
  
  cat("duration KNN imputation train data: ", difftime(t3, t2, unit = "mins"), "\n")
  
  x_test_imp <- predict(train_pre_obj, x_test)
  
  t4 <- Sys.time()
  
  cat("duration KNN imputation test data: ", difftime(t4, t3, unit = "mins"), "\n")
  
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

## Bayesian Hypertuning: First function that integrates hypertuning 
## for elastic net, then for rf, SVr and XGB
bayes_hyper_enet <- function(df, folds, formula, bounds_enet, ncores,
                             iters.n = 10,
                             iters.k = 10){
  
  ## making sure bounds is a list containing parameters "alpha" and "lambda",
  ## this can be used a lot more to stop functions from breaking
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
  x_train_matrix <- model.matrix(~ ., data = df)[, !(colnames(model.matrix(~ ., data = df)) %in% exclude_vars)]
  y_train_vector <- df$QoL_simple
  
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
    model <- train(x = x_train_matrix,  # Use filtered predictors
                   y = y_train_vector,  # Response variable
                   #data = df,
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
  
  
  ## integrating parallelization
  cl <- makeCluster(ncores)
  ## cluster of 64 on ntrcompute-2
  registerDoParallel(cl)
  clusterExport(cl, c("folds", df_name, "elastic_net_bayes", "x_train_matrix",
                      "y_train_vector"),
                envir = environment())
  ## Fix: Use `environment()` to get function scope
  ## Manually export the formula because `clusterExport` struggles with formulas
  # clusterCall(cl, function(f) assign("formula", f, envir = .GlobalEnv), formula)
  ## here: suppress printing content that is being exported
  invisible(clusterEvalQ(cl,expr= {
    library(glmnet)
    library(caret)
    library(dplyr)
  }))
  invisible(clusterEvalQ(cl, ls()))
  
  tWithPar <- system.time(
    opt_results_enet <- bayesOpt(
      FUN = elastic_net_bayes,
      bounds = bounds_enet,
      initPoints = 5,
      ## initPoints must be greater than the number of FUN inputs
      ## iters.n = (parallel::detectCores() - 1)*2,
      iters.n = iters.n,
      ## iters.n = 64*2,
      ## iters.k = (parallel::detectCores() - 1)*2,
      iters.k = iters.k,
      ## iters.k = 64*2,
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
  
  
  best_params_enet <- getBestPars(opt_results_enet)
  
  model_enet <- train(x = x_train_matrix,  # Use filtered predictors
                      y = y_train_vector,  # Response variable
                      method = "glmnet",
                      trControl = trainControl(method = "none"),  # No CV for the final model
                      tuneGrid = data.frame(alpha = best_params_enet$alpha,
                                            lambda = best_params_enet$lambda))
  
  ## same for bayesian optimized tuned model
  final_model_bayes <- model_enet$finalModel
  
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
  
  ## Removing Intercept
  non_zero_predictors_bayes <- grep("(Intercept)",
                                    non_zero_predictors_bayes,
                                    value = TRUE, invert = TRUE)
  
  
  return(list(best_params_enet = best_params_enet,
              time_hypertuning = tWithPar,
              non_zero_predictors = non_zero_predictors_bayes))
  
  
}

bayes_hyper_rf <- function(df, folds, bounds_rf, ncores,
                             iters.n = 10,
                             iters.k = 10){

  ## making sure bounds is a list containing parameters "alpha" and "lambda",
  ## this can be used a lot more to stop functions from breaking
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
  df_name <- deparse(substitute(df))
  
  ## excluding variables from being predictors
  ## creating x and y to avoid problems with formula object
  exclude_vars <-  c("FISNumber", "FamilyNumber", "QoL_simple")
  x_train_matrix <- model.matrix(~ ., data = df)[, !(colnames(model.matrix(~ ., data = df)) %in% exclude_vars)]
  y_train_vector <- df$QoL_simple
  
  # Define the objective function

  rf_bayes <- function(mtry, max.depth, min.node.size, num.trees) {
    ## Still adapt the parameters, CONTNINUE HERE!!!
    
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
      x_train <- x_train_matrix_ML[indices_x_train_ML, ]
      y_train <- y_train_vector_ML[indices_x_train_ML]
      x_val <- x_train_matrix_ML[indices_val, ]
      y_val <- y_train_vector_ML[indices_val]
      
      # Train the ranger model
      model <- ranger(
        #formula = formula,
        #data = train_data,
        dependent.variable.name = "y",  # Target variable name in data frame
        data = data.frame(y = y_train, x_train),  # Combine y and x into a data frame
        mtry = mtry,
        max.depth = max.depth,
        min.node.size = min.node.size,
        num.trees = num.trees
      )
      
      # Make predictions on the validation set
      predictions <- predict(model, data = data.frame(x_val))$predictions
      
      # Calculate RMSE for the current fold
      # true_values <- val_data[[all.vars(formula)[1]]] # Extract target variable
      fold_rmse <- sqrt(mean((predictions - y_val)^2))
      
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
  
  
}
  
  
  
#hypertuning_bayes <- function(df, algorithm, tuneGrid, bounds){}


## this function runs the entire pipeline constructed from the previous functions
## over various datasets
## Idea: Test this with another copy of the Model A dataset where simply
## the IDs are permuted, or with the model_0 only raw features
#ml_longitudinal <- function(datasets = list){}

#ml_stability <- function{}

#ml_compare <- function{}