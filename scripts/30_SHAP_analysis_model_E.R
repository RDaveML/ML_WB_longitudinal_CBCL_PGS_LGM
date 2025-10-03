# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-09-10
#
# Script Name: 30_SHAP_analysis_model_E.R
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
               "fastshap", "shapviz", "kernelshap")

# devtools::install_github("mayer79/permshap")
# devtools::install_github("ModelOriented/kernelshap")


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
list_files <- paste0(filepath, "/", grep("workspace_model_E", list.files(filepath),
                                         value = TRUE))

## extracting the integers from all elements of list_files, only the ones after the 
## last "/" with the sub function
integers_workspaces <- as.numeric(
  regmatches(sub(".*/", "", list_files), gregexpr("[0-9]+", 
                                                  sub(".*/", "", list_files))))


## listing files with the full prepared data
filepath_full_data <- here::here("data", "intermediate", "prep_data_E")
list_files_full_data <- grep(".rds", list.files(filepath_full_data),
                             value = TRUE)

integers_full_data <- as.numeric(
  regmatches(sub(".*/", "", list_files_full_data), gregexpr("[0-9]+", 
                                                  sub(".*/", "", list_files_full_data))))

missing_workspace_integer <- setdiff(integers_full_data, integers_workspaces)

## removing the datafile with the missing integer from the list of full data files
## find the filenames in list_files_full_data, where the missing workspace integer occurs
list_files_full_data <- list_files_full_data[!integers_full_data %in% 
                                               missing_workspace_integer]

## creating vector of all predictors that were in any of the bootstrapped models
system.time({all_predictors_model_E <- list_files %>%
  map(~ readRDS(.x)$predictors_level_1) %>%
  unlist() %>%
  unique()
})

## preparatory objects

## for run of all bootstrapped workspaces, adjust this and run on cluster

# ncores_ntr <- 4
ncores_ntr <- 48
nsim_shap <- 10
# nsim_shap <- 50
# Create a custom prediction function for ranger model
pfun_rf <- function(object, newdata) {
  if (!requireNamespace("ranger", quietly = TRUE)) stop("ranger package not available")
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
  predictors_not_in_model <- setdiff(all_predictors_model_E, predictors_run)
  
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

## combining all sub elements of the SHAP_list list into one data frame, separately 
## for rf and xgb. 
shap_values_bootstrapped_rf <- do.call(rbind, lapply(SHAP_list, function(x) x[["shap_values_rf_run"]]))
shap_values_bootstrapped_xgb <- do.call(rbind, lapply(SHAP_list, function(x) x[["shap_values_xgb_run"]]))

save.image(here::here("data", "intermediate", "workspace_SHAP_analysis_model_E.RData"))

t01 <- Sys.time()

cat("duration model E, Bootstrapping SHAP values: ",
    difftime(t01, t00, unit = "mins"), " minutes")

plot <- TRUE
if(plot == FALSE){
  stop("only calculating the SHAP values, no plotting")
}

load(here::here("data", "intermediate", "workspace_SHAP_analysis_model_E.RData"))



## loading in covariates names for model A to compare importance of covariates 
## against raw CBCL item scores

covariates_full <- readRDS(
  here::here("data", "intermediate", "bootstrap",
             "workspace_model_E_iteration_1.rds"))[["covariates_full"]]



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

ggsave(filename = "SHAP_20_E_rf.png",
       plot = SHAP_analysis_plots[[1]]$plot,
       device = "png",
       width = 8,
       path = plot_path,
       create.dir = TRUE)

ggsave(filename = "SHAP_20_E_xgb.png",
       plot = SHAP_analysis_plots[[2]]$plot,
       device = "png",
       width = 8,
       path = plot_path,
       create.dir = TRUE)
dev.off()

##############################################################################


## The analyses above concern the fluctuation of SHAP values across all 
## runs. Now, analyzing only the first run (the original run)

workspace_objects <- readRDS(file = here::here(
  "data", "intermediate", "workspace_SHAP_analysis_model_E_run1.rds"))

list2env(workspace_objects, globalenv())
rm(workspace_objects)

shap_run1 <- SHAP_list[[1]]

## shap_df_rf is SHAP values for each participant in the rf model,
## shap_values_rf_run is the aggregate (mean aboslute SHAP value over all
## participants), same goes for xgb accordingly
#head(shap_run1$shap_df_rf)
cat("dimensions sh_df_rf:", "\n")
dim(shap_run1$shap_df_rf)

#head(shap_run1$shap_df_xgb)
cat("dimensions sh_df_xgb:", "\n")
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


bee_plot_rf_E <- 
  sv_importance(shv_rf, kind = "bee", max_display = 20L, show_number = TRUE) + 
  ggtitle("Top 20 predictors with the highest average feature importance") +
  labs(subtitle = expression(underline(Model:~CBCL+PGS+LGM))) +
  theme_classic() + 
  theme(plot.title = element_text(size=15, face="bold.italic"),
        plot.subtitle = element_text(size=12, face="italic"),
        axis.text.x = element_text(face="bold", size=10, angle=0),
        axis.title.x = element_text(face="bold", size=15, angle=0),
        axis.text.y = element_text(face="bold", size=10, angle=0),
        axis.title.y = element_text(face="bold", size=15))

ggsave(filename = "bee_plot_rf_E.png",
       plot = bee_plot_rf_E,
       width = 9.0,
       device = "png",
       path = here::here("data", "plots"),
       create.dir = TRUE)



## further inspection of the models 


imp_vec_rf <- sv_importance(shv_rf, kind = "no")   # named numeric vector, sorted by mean |SHAP|

importance_df_rf <- data.frame(
  Feature = names(imp_vec_rf),
  Importance = as.numeric(imp_vec_rf),
  stringsAsFactors = FALSE
) %>% rowid_to_column()

head(importance_df_rf, 30)

## covariates
covariates_full
all(covariates_full %in% importance_df_rf$Feature)

## checking ranks of the covariates in the feature importance df 
for(covariate in covariates_full){
  cat("Rank feature ", covariate, " in model E: ",
      importance_df_rf %>%
        filter(Feature == covariate) %>%
        select(rowid) %>%
        pull(),
      " / ", nrow(importance_df_rf), "\n")
}


## non-LGM and LGM features
## collect non LGM features in model
non_LGM_preds <- grep("non",
                      grep("LGM", importance_df_rf$Feature, value = TRUE),
                      invert = FALSE, value = TRUE)

LGM_preds <- grep("non",
                  grep("LGM", importance_df_rf$Feature, value = TRUE),
                  invert = TRUE, value = TRUE)

types_non_LGM <- c("mean", "sd", "RMSSD", "acf", "ar_order")
types_LGM <- c("_I_", "_S_", "CPROB")

types_longitudinal <- c(types_non_LGM, types_LGM)

for(t_long in types_longitudinal){
  cat("Features of type ", t_long, " in model: ",
      length(grep(t_long, importance_df_rf$Feature, value = TRUE)), "\n")
}



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

## further inspection of the models 


imp_vec_xgb <- sv_importance(shv_xgb, kind = "no")   # named numeric vector, sorted by mean |SHAP|

importance_df_xgb <- data.frame(
  Feature = names(imp_vec_xgb),
  Importance = as.numeric(imp_vec_xgb),
  stringsAsFactors = FALSE
) %>% rowid_to_column()

head(importance_df_xgb, 30)

## covariates
covariates_full
all(covariates_full %in% importance_df_xgb$Feature)

## checking ranks of the covariates in the feature importance df 
for(covariate in covariates_full){
  cat("Rank feature ", covariate, " in model E: ",
      importance_df_xgb %>%
        filter(Feature == covariate) %>%
        select(rowid) %>%
        pull(),
      " / ", nrow(importance_df_xgb), "\n")
}

## Comparison rf and xgb feature importances

# Merge dataframes by feature
merged_SHAP_df <- merge(importance_df_rf,
                        importance_df_xgb,
                        by = "Feature",
                        suffixes = c("_rf", "_xgb"))

# Correlation of importance values
importance_cor <- cor(merged_SHAP_df$Importance_rf,
                      merged_SHAP_df$Importance_xgb,
                      method = "pearson")

# Correlation of ranks
rank_cor <- cor(merged_SHAP_df$rowid_rf,
                merged_SHAP_df$rowid_xgb,
                method = "spearman")

importance_cor
## .59 correlation of average SHAP importance

rank_cor
## .63 correlation of rank of average SHAP importance

library(boot)

# Function to compute correlation (Pearson or Spearman)
correlation_fn <- function(data, indices, method = "pearson") {
  d <- data[indices, ]
  return(cor(d$x, d$y, method = method))
}

# importance values
data_importance <- data.frame(x = merged_SHAP_df$Importance_rf,
                              y = merged_SHAP_df$Importance_xgb)

# Bootstrap for Pearson correlation
set.seed(123)
boot_res <- boot(data_importance,
                 statistic = correlation_fn, R = 2000, method = "pearson")

# Bootstrap CI
boot.ci(boot_res, type = c("perc", "bca"))

# ranks
data_importance_rank <- data.frame(x = merged_SHAP_df$rowid_rf,
                                   y = merged_SHAP_df$rowid_xgb)

# Bootstrap for Pearson correlation
set.seed(123)
boot_res_rank <- boot(data_importance_rank,
                      statistic = correlation_fn, R = 2000, method = "spearman")

# Bootstrap CI
boot.ci(boot_res_rank, type = c("perc", "bca"))


## eoS

workspace_objects <- mget(c("shap_run1", "predictors_1", "model_rf", 
                            "model_xgb", "X_1_shap", "shp_1_rf", "baseline_rf",
                            "shv_rf", "shp_1_xgb", "baseline_xgb", "shv_xgb", 
                            "shp_xgb_a"))


# Save the list to an RDS file

saveRDS(workspace_objects, file = here::here(
  "data", "intermediate", "workspace_SHAP_analysis_model_E_run1.rds"))

save.image(here::here("data", "intermediate", "workspace_SHAP_analysis_model_E.RData"))

load(here::here("data", "intermediate", "workspace_SHAP_analysis_model_E.RData"))

t02 <- Sys.time()

cat("duration model E, entire script SHAP analysis: ",
    difftime(t02, t00, unit = "mins"), " minutes")