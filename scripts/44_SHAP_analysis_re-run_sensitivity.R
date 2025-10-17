# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-10-06
#
# Script Name: 44_SHAP_analysis_re-run_sensitivity.R
#
# Script Description:
#
#
# Notes:
#
# Loops the SHAP calculation over all datasets for the sensitivity
# analysis
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
               "fastshap", "shapviz", "kernelshap", "ggtext", "boot")

# devtools::install_github("mayer79/permshap")
# devtools::install_github("ModelOriented/kernelshap")


## loading in ML custom ML + Hypertuning functions
source(here::here("scripts", "functions", "functions_ml.R"))

# ncores_ntr <- 4
ncores_ntr <- 48
nsim_shap <- 10
# nsim_shap <- 50

## Note: Run SHAP value calculation on server!
## at ntr: ntrcompute1

# Create a custom prediction function for ranger model
pfun_rf <- function(object, newdata) {
  if (!requireNamespace("ranger", quietly = TRUE)) stop("ranger package not available")
  predict(object, data = newdata)$predictions
}

# Create a custom prediction function for xgbmodel
pfun_xgb <- function(object, newdata) {
  predict(object, newdata = newdata)
}

## listing files with workspaces
filepath <- here::here("data", "intermediate", 
                       "workspaces_sensitivity_analysis_MCD")

list_files <- list.files(filepath)

filepath_data <- here::here("data", "intermediate", "prep_data_MCD")

list_files_data <- list.files(filepath_data)

feature_sets <- LETTERS[1:5]

## here begin to loop over A - E

SHAP_analysis_re_run <- lapply(feature_sets, function(x){

  t1 <- Sys.time()
  ## creating vector of all predictors that were in any of the bootstrapped models
  workspace <- readRDS(paste0(filepath, "/",
                              grep(paste0("model_", x), list_files, value = TRUE)
                              ))
  
  predictors_model <- workspace$predictors_level_1
  
  filename_data <- paste0(filepath_data, "/",
                          grep(paste0("data_", x), list_files_data, value = TRUE))
  
  ## loading in data model was trained with and evaluated on
  x_shap <- rbind(readRDS(filename_data)[["x_train"]], 
                  readRDS(filename_data)[["x_test"]]) %>%
    select(all_of(predictors_model))
  
  ## loading in covariates
  covariates_full <- workspace$covariates_full
  
  # Calculate SHAP values for the random forest model
  model_rf <- workspace$run_rf$model_bayes_rf
  
  # Compute fast (approximate) Shapley values using 50 Monte Carlo repetitions
  ## note that these are aggregate values for the entire dataset, no local
  ## importance values
  
  registerDoParallel(cores = ncores_ntr)
  set.seed(which(LETTERS == x))
  
  ## calculating aggregated SHAP values for rf model 
  system.time({
    shp_1_rf <- fastshap::explain(
      object = model_rf, X = x_shap, pred_wrapper = pfun_rf,
      nsim = nsim_shap, parallel = TRUE, adjust = TRUE)
  })
    
  stopImplicitCluster()
  
  
  # Calculate SHAP values for the xgboost model
  model_xgb <- workspace$run_xgb$model_bayes_xgb

  ## note: for calculating the shap values for an xgboost model, 
  ## x_shap needs to be a matrix!
  registerDoParallel(cores = ncores_ntr)
  
  set.seed(which(LETTERS == x))
  
  system.time({  # estimate run time
    shp_1_xgb <- fastshap::explain(
      object = model_xgb, X = as.matrix(x_shap), pred_wrapper = pfun_xgb,
      nsim = nsim_shap, parallel = TRUE, adjust = TRUE)
  })
  
  stopImplicitCluster()
  
  t2 <- Sys.time()
  
  cat("time SHAP calculation feature set ", x, ": ",
      difftime(t2, t1, unit = "mins"), " minutes", "\n")
  
  return(list(shp_1_rf = shp_1_rf,
              shp_1_xgb = shp_1_xgb,
              x_shap = x_shap,
              covariates_full = covariates_full))

})
## creating dataframe out of all the single row elements in SHAP_list

## combining all sub elements of the SHAP_list list into one data frame, separately 
## for rf and xgb. 

## saving the list of shap value calculations  

names(SHAP_analysis_re_run) <- paste0("SHAP_analysis_sensitivity_",
                                      feature_sets)

saveRDS(SHAP_analysis_re_run, file = here::here(
  "data", "final", "workspace_SHAP_analysis_rerun_sensitivity.rds"))

t0a <- Sys.time()
cat("duration SHAP value calculation: ",
    difftime(t0a, t00, unit = "mins"), " minutes (n_sim: ", nsim_shap, ")", "\n")
  
plot <- FALSE

if(plot == FALSE){
  stop("Only calculating SHAP values, no plotting")
}
  
t01 <- Sys.time()

## loading in calculations object(not working on server anymore)
SHAP_analysis_re_run <- readRDS(here::here(
  "data", "final", "workspace_SHAP_analysis_rerun_sensitivity.rds"))

feature_sets <- LETTERS[1:5]

## next: write function for plotting and additional analyses
## (also including e.g. whether feature is a covariate)

## Next: Loop these calculations over all feature sets

output_full_SHAP_plotting <- capture.output({
shap_plots_full <<- lapply(feature_sets, function(x){ ## assign to global env!
  

  ## content of feature set
  set <- case_when(
    x == "A" ~ "CBCL only",
    x == "B" ~ "PGS only",
    x == "C" ~ "CBCL + PGS",
    x == "D" ~ "CBCL + longitudinal",
    x == "E" ~ "CBCL + PGS + longitudinal",
    .default = NA
  )
  
  feature_set_looped <- grep(paste0("_", x), names(SHAP_analysis_re_run))
  
  ## workspace object of the current feature set
  ws_looped <- SHAP_analysis_re_run[[feature_set_looped]]

  
  ## also looping over the algorithms within feature set
  alg_objects <- grep("shp_1", names(ws_looped))
  
  alg_compact <- lapply(alg_objects, function(ob){  ## initiating algorithm lapply statement
    alg_name <- ifelse(ob == 1, "rf", "xgb")
    
    ## SHAP value calculation of current algorithm
    alg_looped <- ws_looped[[ob]]
    
    
    
    ## Creating named vector whether feature is covariate or Feature
    var_type <- sapply(colnames(alg_looped), function(z){
      if(z %in% ws_looped$covariates_full){
        return("Covariate")
      } else {
        return("Feature")
      }
    })
    
    
    baseline <- attr(alg_looped, "baseline")
    shv <- shapviz(alg_looped, X = ws_looped$x_shap, baseline = baseline)
    shv$feature_type <- var_type[colnames(shv$S)]
    
    
    
    
    bee_plot <- 
      sv_importance(shv, kind = "bee", max_display = 20L, show_number = TRUE) + 
      ggtitle("Top 20 predictors with the highest average feature importance") +
      labs(subtitle = eval(
            parse(text = paste0(
              'expression(underline("Feature set: ', set, ' (Algorithm: ', alg_name, ')"))')
        ))) +
      # labs(subtitle = paste0("Feature set: ", set, " (algorithm: ", alg_name, ")"))
      #labs(subtitle = expression(underline(Model:~CBCL+PGS+LGM))) +
      theme_classic() + 
      theme(plot.title = element_text(size=15, face="bold.italic"),
            plot.subtitle = element_text(size=12, face="italic"),
            axis.text.x = element_text(face="bold", size=10, angle=0),
            axis.title.x = element_text(face="bold", size=15, angle=0),
            axis.text.y = element_text(face="bold", size=10, angle=0),
            axis.title.y = element_text(face="bold", size=15))
    
    
    filename_bee_plot <- paste0("bee_plot_", alg_name, "_", x, "_sensitivity.png")
      
    
    ## potentially only do this later!  
    #ggsave(filename = filename_bee_plot,
    #       plot = bee_plot,
    #       width = 9.0,
    #       device = "png",
    #       path = here::here("data", "plots", "sensitivity"),
    #       create.dir = TRUE)
    
    
    
    ## further inspection of the models 
    
    ## create additional dataframe that stores whether variable type is 
    ## Genetic feature, PGS, LGM, non-LGM or covariate
    ## further inspection of the models 
    imp_vec <- sv_importance(shv, kind = "no")   # named numeric vector, sorted by mean |SHAP|
    
    importance_df <- data.frame(
      Feature = names(imp_vec),
      Importance = as.numeric(imp_vec),
      stringsAsFactors = FALSE) %>% 
        rowid_to_column() %>%
        mutate(
          is_covariate = as.factor(
            ifelse(Feature %in% ws_looped$covariates_full, "Yes", "No")))
    
    head(importance_df, 30)
    
    ## plotting: With indicator whether variable is covariate
    ## Adapt titles in case, probably makes more sense to leave it 
    ## out and give in in figure caption on overleaf
    
    ## (Add potential title)
    plot_SHAP_global <- ggplot(
      data = importance_df, aes(x = reorder(Feature, Importance),
                                y = Importance,
                                fill = is_covariate)) + 
        geom_col() + 
        coord_flip() + 
        labs(x = "Variable", fill = "Covariate")
    
    filename_global_plot <- paste0("global_SHAP_", alg_name, "_", x, "_sensitivity.png")
    
    
    ## check if all covariates are still in
    all(ws_looped$covariates_full %in% importance_df$Feature)
    
    ## checking ranks of the covariates in the feature importance df 
    model_name <- paste0(set, " (", alg_name, ")") ## How can I here get the actual name from the 
    ## name of the looped object
    for(covariate in ws_looped$covariates_full){
      cat("Rank feature ", covariate, " in model ", model_name, ": ",
          importance_df %>%
            filter(Feature == covariate) %>%
            select(rowid) %>%
            pull(),
          " / ", nrow(importance_df), "\n")
    }
    
    
    ## non-LGM and LGM features
    ## collect non LGM features in model
    non_LGM_preds <- grep("non",
                          grep("LGM", importance_df$Feature, value = TRUE),
                          invert = FALSE, value = TRUE)
    
    LGM_preds <- grep("non",
                      grep("LGM", importance_df$Feature, value = TRUE),
                      invert = TRUE, value = TRUE)
    
    long_vars <- c(non_LGM_preds, LGM_preds)
    
    
    ## printing on LGM variables in case they are present
    if(length(long_vars) > 0){
      types_non_LGM <- c("mean", "sd", "RMSSD", "acf", "ar_order")
      types_LGM <- c("_I_", "_S_", "CPROB")
      
      types_longitudinal <- c(types_non_LGM, types_LGM)
      
      for(t_long in types_longitudinal){
        cat("Features of type ", t_long, " in model: ",
            length(grep(t_long, importance_df$Feature, value = TRUE)), "\n")
      }
    }
    
    return(list(bee_plot = bee_plot,
                filename_bee_plot = filename_bee_plot,
                importance_df = importance_df,
                plot_SHAP_global = plot_SHAP_global,
                filename_global_plot = filename_global_plot))
  
  }) ## closing the algorithm lapply statement
  
  ## Comparison rf and xgb feature importances
  
  # Merge dataframes by feature
  merged_SHAP_df <- merge(alg_compact[[1]]$importance_df,
                          alg_compact[[2]]$importance_df,
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
  
  cat("Pearson correlation feature importances feature set ",
      set, ": ", importance_cor, "\n")
  ## .59 correlation of average SHAP importance
  
  cat("Spearman rank correlation feature importances feature set ",
      set, ": ", rank_cor, "\n")
  ## .63 correlation of rank of average SHAP importance
  
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
  CI_SHAP_pearson <- boot.ci(boot_res, type = c("perc", "bca"))
  
  # ranks
  data_importance_rank <- data.frame(x = merged_SHAP_df$rowid_rf,
                                     y = merged_SHAP_df$rowid_xgb)
  
  # Bootstrap for Pearson correlation
  set.seed(123)
  boot_res_rank <- boot(data_importance_rank,
                        statistic = correlation_fn, R = 2000, method = "spearman")
  
  # Bootstrap CI
  CI_SHAP_rank <- boot.ci(boot_res_rank, type = c("perc", "bca"))

  return(list(feature_set = set,
              alg_compact = alg_compact,
              cor_SHAP_rf_xgb_pearson = importance_cor,
              CI_SHAP_pearson = CI_SHAP_pearson,
              cor_SHAP_rf_xgb_spearman = rank_cor,
              CI_SHAP_rank = CI_SHAP_rank))
 })
})  

## writing console output to file
writeLines(output_full_SHAP_plotting,
           here::here("data", "plots", "sensitivity",
                      "output_SHAP_plotting.txt"))

names(shap_plots_full[[1]]) 



 
## Here insert plotting the plots I need and combining them to multipanel
## plots





## Deciding on what I intend to save

# Save the list to an RDS file
saveRDS(shap_plots_full, file = here::here(
  "data", "final", "shap_plots_full_snsitivity.rds"))

t02 <- Sys.time()

cat("duration plotting complete SHAP analysis (sensitivity analysis): ",
    difftime(t02, t01, unit = "mins"), " minutes")

## Plotting only here!!