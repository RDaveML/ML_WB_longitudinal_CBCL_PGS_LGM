# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-05-13
#
# Script Name: 12_SHAP_analysis_model_A.R
#
# Script Description:
#
#
# Notes:
#
#

# Set options
t00 <- Sys.time()

cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "haven", "foreign", "here", "readr",
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "RANN",
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest",
               "fastshap", "shapviz")


## loading in ML custom ML + Hypertuning functions
source(here::here("scripts", "functions", "functions_ml.R"))

## Feature importance analysis (level 1 models)

## object of random forest model: model_rf
## object of xgboost model: model_xgb

## calculating Shapley Additive exPlanation (SHAP) values for random forest 
## and xgboost model

## accessing full ML prepared dataset of the model (preprocessed, elastic net 
## filtered training and test data per run)

## Note: Run SHAP value calculation on server!
## at ntr: ntrcompute1

## listing files with bootstrapped models
filepath <- here::here("data", "intermediate", "bootstrap")
list_files <- paste0(filepath, "/", grep("workspace_model_A", list.files(filepath),
                                         value = TRUE))

## listing files with the full prepared data
filepath_full_data <- here::here("data", "intermediate", "prep_data_A")
list_files_full_data <- grep(".rds", list.files(filepath_full_data),
                             value = TRUE)

## creating vector of all predictors that were in any of the bootstrapped models
system.time({all_predictors_model_A <- list_files %>%
  map(~ readRDS(.x)$predictors_level_1) %>%
  unlist() %>%
  unique()
})

## preparatory objects

ncores_ntr <- 48
nsim_shap <- 10
# Create a custom prediction function for ranger model
pfun_rf <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Create a custom prediction function for xgbmodel
pfun_xgb <- function(object, newdata) {
  predict(object, newdata = newdata)
}

## running over all workspaces: extracting predictor set and 
## run shap value calculation over all predictors that were in model
## if a predictor was not in the model, assign value 0
## in the end, create dataframe and visualize
## note: run for both models

system.time({SHAP_list <- lapply(1:length(list_files), function(run){
# system.time({SHAP_list <- lapply(1:2, function(run){
  
  predictors_run <- readRDS(list_files[run])[["predictors_level_1"]]
  
  ## creating dataframe with all predictors that are not contained in the 
  ## model
  predictors_not_in_model <- setdiff(all_predictors_model_A, predictors_run)
  
  ## creating dataframe with all predictors that are not contained in the 
  ## model
  shap_values_0 <- data.frame(matrix(0, nrow = 1, ncol = length(predictors_not_in_model)))
  colnames(shap_values_0) <- predictors_not_in_model
  
  ## coding the shap values for the random forest model
  filename <- list_files[run]
  ## only extracting the part of the filename after the last "/"
  filename_root <- sub(".*/", "", list_files[run])
  ## extract the numeric part of the filename_root
  run_id <- as.numeric(
    regmatches(filename_root, gregexpr("[0-9]+", filename_root)))
  
  filename_data <- paste0(filepath_full_data, "/", list_files_full_data[run])
  run_id_data <- as.numeric(
    regmatches(list_files_full_data, gregexpr("[0-9]+", list_files_full_data)))[run]
  
  print(run_id_data)
  print(filename_data)
  
  ## stopping if indices are not aligned correctly
  if(run_id != run_id_data){
    stop("Error: dataset and bootstrapped model predictions need to have same index")
  }
  ## loading in data model was trained with and evaluated on
  x_shap <- rbind(readRDS(filename_data)[["x_train"]], 
                  readRDS(filename_data)[["x_test"]]) %>%
    select(all_of(predictors_run))
  

  # Calculate SHAP values for the random forest model
  ## Note: The final model was a caret model object!
  ## has implications for the predict function
  model_rf <- readRDS(filename)$run_rf$model_bayes_rf$finalModel
  
  registerDoParallel(cores = ncores_ntr)
  # Compute fast (approximate) Shapley values using 10 Monte Carlo repetitions
  ## note that these are aggregate values for the entire dataset, no local
  ## importance values
  set.seed(run)
  system.time({  # estimate run time
    shap_values_rf <- shap_calc(x_shap = x_shap, model = model_rf,
                                pfun = pfun_rf, ncores = ncores_ntr, nsim = nsim_shap)
  })

  stopImplicitCluster()

  shap_df_rf <- cbind(shap_values_rf, shap_values_0)
  
  ## combining aggregated (mean) shap values with the zero dataframe
  shap_values_rf_run <- cbind(as.data.frame(t(colMeans(abs(shap_values_rf)))), 
                               shap_values_0)

  shap_values_rf_run$run <- run
  
  
  # Calculate SHAP values for the xgboost model
  model_xgb <- readRDS(filename)$run_xgb$model_bayes_xgb
  
  # Compute fast (approximate) Shapley values using 10 Monte Carlo repetitions
  ## note that these are aggregate values for the entire dataset, no local
  ## importance values
  
  ## note: for calculating the shap values for an xgboost model, 
  ## x_shap needs to be a matrix!
  registerDoParallel(cores = ncores_ntr)
  set.seed(run)
  system.time({  # estimate run time
    shap_values_xgb <- shap_calc(x_shap = x_shap, model = model_xgb,
                                 pfun = pfun_xgb, ncores = ncores_ntr,
                                 nsim = nsim_shap)
  })

  stopImplicitCluster()
  
  shap_df_xgb <- cbind(shap_values_xgb, shap_values_0)
  
  ## combining aggregated (mean) shap values with the zero dataframe
  shap_values_xgb_run <- cbind(as.data.frame(t(colMeans(abs(shap_values_xgb)))), 
                                shap_values_0)
  shap_values_xgb_run$run <- run
  
  
  
  return(list(shap_df_rf = shap_values_rf,
              shap_df_xgb = shap_values_xgb,
              shap_values_rf_run = shap_values_rf_run,
              shap_values_xgb_run = shap_values_xgb_run))
  
}
)
})

## creating dataframe out of all the single row elements in SHAP_list
save.image(here::here("data", "intermediate", "workspace_SHAP_analysis_model_A.RData"))

## combining all sub elements of the SHAP_list list into one data frame, separately 
## for rf and xgb. 
shap_values_bootstrapped_rf <- do.call(rbind, lapply(SHAP_list, function(x) x[["shap_values_rf_run"]]))
shap_values_bootstrapped_xgb <- do.call(rbind, lapply(SHAP_list, function(x) x[["shap_values_xgb_run"]]))

save.image(here::here("data", "intermediate", "workspace_SHAP_analysis_model_A.RData"))

t01 <- Sys.time()

cat("duration entire script (model A, Bootstrapping SHAP values): ",
    difftime(t01, t00, unit = "mins"), " minutes")

plot(FALSE)
if(plot == FALSE){
  stop("only calculating the SHAP values, no plotting")
}

load(here::here("data", "intermediate", "workspace_SHAP_analysis_model_A.RData"))


old <- FALSE
if(old){
## for(i in 1:length(list_files_full_data)){
for(run in 1:2) {
  filename <- paste0(filepath, "/", list_files[run])
  run_id <- as.numeric(
    regmatches(list_files, gregexpr("[0-9]+", list_files)))[run]
  print(run_id)
  print(filename)
  
  filename_data <- paste0(filepath_full_data, "/", list_files_full_data[run])
  run_id_data <- as.numeric(
    regmatches(list_files_full_data, gregexpr("[0-9]+", list_files_full_data)))[run]
  
  print(run_id_data)
  print(filename_data)
  
  ## stopping if indices are not aligned correctly
  if(run_id != run_id_data){
    stop("Error: dataset and bootstrapped model predictions need to have same index")
  }
  
  predictors_run <- readRDS(filename)[["predictors_level_1"]]
  x_shap <- rbind(readRDS(filename_data)[["x_train"]], 
                  readRDS(filename_data)[["x_test"]]) %>%
    select(all_of(predictors_run))
  
  y_shap <- data_full %>%
    # filter(FISNumber %in% train_ids) %>%
    select(QoL_simple) %>%
    pull()
  
  # Calculate SHAP values for the random forest model
  ## Note: The final model was a caret model object!
  ## has implications for the predict function
  model_rf <- readRDS(filename)$run_rf$model_bayes_rf$finalModel
  
  # Create a custom prediction function for iml
  pfun_rf <- function(object, newdata) {
    predict(object, data = newdata)$predictions
  }
  
  registerDoParallel(cores = 48)
  # Compute fast (approximate) Shapley values using 10 Monte Carlo repetitions
  ## note that these are aggregate values for the entire dataset, no local
  ## importance values
  system.time({  # estimate run time
    set.seed(5038)
    shap_rf <- fastshap::explain(model_rf, X = x_shap, pred_wrapper = pfun_rf,
                                 nsim = 10, parallel = TRUE, adjust = TRUE)
  })
  ## on ntr1 with 48 cores, this takes about 3.5 minutes
  
  baseline_rf <- attr(shap_rf, "baseline") 
  
  shv_rf <- shapviz(shap, X = x_shap, baseline = baseline_rf)
  
  sv_importance(shv_rf)
  
  ## sv_waterfall(shv_rf)  
  ## sv_waterfall is more relevant for individual predictions
  ## sv_dependence plots Scatterplot of the SHAP values of a feature 
  ## against its feature values
  ## sv_dependence(shv, v = "sd_self", alpha = 0.3)
  
  ## aggregated table
  shap_values_rf <- tibble::as_tibble(shap_rf)
  
  
}

## shap values for xgboost model
for(run in 1:2) {
  filename <- paste0(filepath, "/", list_files[run])
  run_id <- as.numeric(
    regmatches(list_files, gregexpr("[0-9]+", list_files)))[run]
  print(run_id)
  print(filename)
  
  filename_data <- paste0(filepath_full_data, "/", list_files_full_data[run])
  run_id_data <- as.numeric(
    regmatches(list_files_full_data, gregexpr("[0-9]+", list_files_full_data)))[run]
  
  print(run_id_data)
  print(filename_data)
  
  ## stopping if indices are not aligned correctly
  if(run_id != run_id_data){
    stop("Error: dataset and bootstrapped model predictions need to have same index")
  }
  
  predictors_run <- readRDS(filename)[["predictors_level_1"]]
  x_shap <- rbind(readRDS(filename_data)[["x_train"]], 
                  readRDS(filename_data)[["x_test"]]) %>%
    select(all_of(predictors_run))
  
  train_ids_run <- readRDS(filename)[["train_ids"]]
  
  test_ids_run <- readRDS(filename)[["test_ids"]]
  
  y_shap <- data_full %>%
    # filter(FISNumber %in% train_ids) %>%
    select(QoL_simple) %>%
    pull()
  
  # Calculate SHAP values for the xgboost model
  model_xgb <- readRDS(filename)$run_xgb$model_bayes_xgb
  
  # Create a custom prediction function for iml
  pfun_xgb <- function(object, newdata) {
    predict(object, newdata = newdata)
  }
  # Compute fast (approximate) Shapley values using 10 Monte Carlo repetitions
  ## note that these are aggregate values for the entire dataset, no local
  ## importance values
  
  ## note: for calculating the shap values for an xgboost model, 
  ## x_shap needs to be a matrix!
  registerDoParallel(cores = 48)
  system.time({  # estimate run time
    set.seed(5038)
    shap_xgb <- fastshap::explain(
      model_xgb, X = as.matrix(x_shap), pred_wrapper = pfun_xgb,
      nsim = 10, parallel = TRUE, adjust = TRUE)
  })
  
  
  baseline_xgb <- attr(shap_xgb, "baseline") 
  
  shv_xgb <- shapviz(model_xgb, X_pred = data.matrix(x_shap), X = x_shap)
  
  ## CONTINUE HERE!!!
  # shv_xgb <- shapviz(shap, X = x_shap, baseline = baseline_xgb)
  
  ## start printing device
  ## for plots to be plotted in VSC window
  
  sv_importance(shv_xgb)
  
  ## sv_waterfall(shv_rf)  
  ## sv_waterfall is more relevant for individual predictions
  ## sv_dependence plots Scatterplot of the SHAP values of a feature 
  ## against its feature values
  ## sv_dependence(shv, v = "sd_self", alpha = 0.3)
  
  ## aggregated table
  ## create a dataframe with all shap values of the xbg model
  shap_values_xgb <- tibble::as_tibble(shap_xgb)
  
  
  ## got it for rf and xgb model
  
  
  
}

## CONTINUE HERE!! 
load(here::here("data", "intermediate", "workspace_stacking_A_server.RData"))

## creating empty vector where all predictor variables that occured in any 
## of the bootstrapped models are stored
list_files <- paste0(
  here::here("data", "intermediate", "bootstrap"), "/", list_files
)
system.time({all_predictors_model_A <- list_files %>%
  map(~ readRDS(.x)$predictors_level_1) %>%
  unlist() %>%
  unique()
})


}



## loading in covariates names for model A to compare importance of covariates 
## against raw CBCL item scores

covariates_full <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_A_iteration_1.rds"))[["covariates_full"]]



list_SHAP_dfs <- list(
  "shap_values_bootstrapped_rf" = shap_values_bootstrapped_rf,
  "shap_values_bootstrapped_xgb" = shap_values_bootstrapped_xgb)



## plotting: visualizing the top x features with highest mean absolute
## SHAP feature importance over all bootstrapped runs, with CIs 
## plotted according to specified significance level
SHAP_analysis_plots <- lapply(names(list_SHAP_dfs), function(df_name) {
  df <- list_SHAP_dfs[[df_name]]
  SHAP_viz_top_x(
    shap_df = df,
    shap_df_name = df_name,
    show_features = 20,
    alpha = 0.05,
    covariates = covariates_full
  )
})
  

## plot for random forest
SHAP_analysis_plots[[1]]$plot

## plot for xgb
SHAP_analysis_plots[[2]]$plot

## saving plots
plot_path <- here::here("data", "plots")

ggsave(filename = "SHAP_20_A_rf.png",
       plot = SHAP_analysis_plots[[1]]$plot,
       device = "png",
       width = 8,
       path = plot_path,
       create.dir = TRUE)

ggsave(filename = "SHAP_20_A_xgb.png",
       plot = SHAP_analysis_plots[[2]]$plot,
       device = "png",
       width = 8,
       path = plot_path,
       create.dir = TRUE)
dev.off()

##############################################################################

## The analyses above concern the fluctuation of SHAP values across all 
## runs. Now, analyzing only the first run (the original run)

shap_run1 <- SHAP_list[[1]]

## shap_df_rf is SHAP values for each participant in the rf model,
## shap_values_rf_run is the aggregate (mean aboslute SHAP value over all
## participants), same goes for xgb accordingly
head(shap_run1$shap_df_rf)
dim(shap_run1$shap_df_rf)

head(shap_run1$shap_df_xgb)
dim(shap_run1$shap_df_xgb)

## visualizing individual SHAP values from the rf model only for run 1

## importing predictors
predictors_1 <- readRDS(list_files[1])[["predictors_level_1"]]

## importing models
model_rf <- readRDS(list_files[1])$run_rf$model_bayes_rf$finalModel
model_xgb <- readRDS(list_files[1])$run_xgb$model_bayes_xgb

## importing data
filename_r1 <- paste0(filepath_full_data, "/", list_files_full_data[1])

X_1_shap <- rbind(readRDS(filename_r1)$x_train, 
                  readRDS(filename_r1)$x_test) %>%
  select(all_of(readRDS(list_files[1])[["predictors_level_1"]]))

registerDoParallel(cores = 48)
set.seed(1)

## calculating aggregated SHAP values for rf model 
system.time({
    shp_1_rf <- fastshap::explain(
    object = model_rf, X = X_1_shap, pred_wrapper = pfun_rf,
    nsim = 50, parallel = TRUE, adjust = TRUE)
})

stopImplicitCluster()

baseline_rf <- attr(shp_1_rf, "baseline")
shv_rf <- shapviz(shp_1_rf, X = X_1_shap, baseline = baseline_rf)
sv_importance(shv_rf)
sv_importance(shv_rf, kind = "bee")





## calculating aggregated SHAP values for xgb model 
registerDoParallel(cores = 48)
set.seed(1)

system.time({  # estimate run time
  shp_1_xgb <- fastshap::explain(
    object = model_xgb, X = as.matrix(X_1_shap), pred_wrapper = pfun_xgb,
    nsim = 50, parallel = TRUE, adjust = TRUE)
})

stopImplicitCluster()

baseline_xgb <- attr(shp_1_xgb, "baseline")
shv_xgb <- shapviz(shp_1_xgb, X = X_1_shap, baseline = baseline_xgb)
sv_importance(shv_xgb)

## beeswarm plot for xgb model
shp_xgb_a <- shapviz(model_xgb, X_pred = data.matrix(X_1_shap), X = X_1_shap)
# Three types of variable importance plots
sv_importance(shp_xgb_a)
sv_importance(shp_xgb_a, kind = "beeswarm", alpha = 0.2, width = 0.2)
sv_importance(shp_xgb_a, kind = "both", alpha = 0.2, width = 0.2)
## sv_importance(shp_xgb_a, kin = "no")
## This one might be used however to sort the features according to importance



save.image(here::here("data", "intermediate", "workspace_SHAP_analysis_model_A.RData"))

load(here::here("data", "intermediate", "workspace_SHAP_analysis_model_A.RData"))

t02 <- Sys.time()

cat("duration model A, entire script SHAP analysis: ",
    difftime(t02, t00, unit = "mins"), " minutes")




## eoS