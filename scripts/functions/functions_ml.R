## function to automatize ML procedure with different datasets 
## and different models 


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

hypertuning_bayes <- function(df, algorithm, tuneGrid, bounds){
  
}

ml_longitudinal <- function(datasets = list){}

ml_stability <- function{}

ml_compare <- function{}