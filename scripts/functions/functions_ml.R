## function to automatize ML procedure with different datasets 
## and different models 

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
    select(-all_of(covariates), -FISNumber, -QoL_simple)
  
  
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
  if(sum(colMeans(is.na(x_train_imp)) != 0)){
    cat("The following columns still contain NAs: ", "\n", 
        colnames(data_model_A)[which(colMeans(is.na(data_model_A)) != 0)])
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
              x_test_comb = x_train_comb,
              x_test_comb = x_test_comb,
              y_train = y_train,
              y_test = y_test,
              data_covariates_train = data_covariates_train,
              data_covariates_test = data_covariates_test))
}


#hypertuning_bayes <- function(df, algorithm, tuneGrid, bounds){}

#ml_longitudinal <- function(datasets = list){}

#ml_stability <- function{}

#ml_compare <- function{}