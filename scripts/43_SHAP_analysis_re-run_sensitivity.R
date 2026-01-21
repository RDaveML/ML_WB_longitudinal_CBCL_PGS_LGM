# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-10-10
#
# Script Name: 43_SHAP_analysis_re-run_sensitivity.R
#
# Script Description:
# This script calculates
# Shapley Additive exPlanation (SHAP) values for the 
# all xgboost and random forest models estimated on all 
# MCD outlier cleaned variables sets (A-E)
# (same as SHAP calculation script of SHAP values on original analysis, but with
## MCD-outliers removed from datasets)
#
#
# Notes:
# Global variable importance scores are estimated, not local ones
# Entire sample is used for calculation of variable importance
#
# Note: Script was run on computational server (ntr-compute1) parallelizing
# SHAP calculaito nover 48 cores
# 

# Set options
t00 <- Sys.time()

cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "haven", "foreign", "here", "readr",
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ranger", "e1071", "randomForestSRC", "parallel", "doParallel",
               "RANN", "kernlab", "ggplot2", "purrr", "tidyr", "rvest",
               "fastshap", "kernelshap", "ggtext", "boot", "tibble")


## there was an issue with the xgboost package, if this is still the case
## remove recent installation of xgboost and install old version

old_xgboost <- TRUE

if(old_xgboost){
  remove.packages("xgboost")
  
  pkgbuild::check_build_tools(debug = TRUE)
  ## checking if Rtools is installed, if not, install from 
  ## https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45-6691-6492.exe
  
  
  ## installing old version of xgboost with which models were 
  ## estimated (valid from beginning 2025)
  xgburl <- 
    "https://cran.r-project.org/src/contrib/Archive/xgboost/xgboost_1.7.8.1.tar.gz"
  install.packages(xgburl, repos=NULL, type="source")
  ## with this version, it works!
  
  install.packages("shapviz")

}

## load xgboost separately
library(xgboost)
library(shapviz) ## dependent on xgboost

## loading in ML custom ML + Hypertuning functions
source(here::here("scripts", "functions", "functions_ml.R"))

# ncores_ntr <- 4
ncores_ntr <- 48
nsim_shap <- 10
# nsim_shap <- 50

## Note: SHAP value calculation on server (at ntr: ntrcompute1)

# Create a custom prediction function for ranger model
pfun_rf <- function(object, newdata) {
  if (!requireNamespace("ranger", quietly = TRUE)){
    stop("ranger package not available")
  }
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

filepath_data <- here::here("data", "final", "OG_prep_data")

list_files_data <- list.files(filepath_data)

feature_sets <- LETTERS[1:5]

## here begin to loop over A - E


SHAP_analysis_re_run <- lapply(feature_sets, function(x){
  
  t1 <- Sys.time()
  
  ## creating vector of all predictors
  workspace <- readRDS(paste0(filepath, "/",
                              grep(paste0("model_", x),
                                   list_files, value = TRUE)
  ))
  
  predictors_model <- workspace$predictors_level_1
  
  filename_data <- paste0(filepath_data, "/",
                          grep(paste0("data_", x),
                               list_files_data, value = TRUE))
  
  ## loading in data model was trained with and evaluated on
  x_shap <- rbind(readRDS(filename_data)[["x_train"]], 
                  readRDS(filename_data)[["x_test"]]) %>%
    select(all_of(predictors_model))
  
  ## loading in covariates of the model (already preprocessed)
  covariates_full <- workspace$covariates_full
  
  
  
  # Calculate SHAP values for the random forest model
  model_rf <- workspace$run_rf$model_bayes_rf$finalModel
  
  # Compute fast (approximate) Shapley values using 10 Monte Carlo repetitions
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
  
  #############################################################################
  
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

###############################################################################

## creating dataframe out of all the single row elements in SHAP_list
## combining all sub elements of the SHAP_list list into one data frame,
## separately for rf and xgb. 

## saving the list of shap value calculations  

names(SHAP_analysis_re_run) <- paste0("SHAP_analysis_re_run_",
                                      feature_sets)

saveRDS(SHAP_analysis_re_run, file = here::here(
  "data", "final", "workspace_SHAP_analysis_re_run.rds"))

t0a <- Sys.time()
cat("duration SHAP value calculation: ",
    difftime(t0a, t00, unit = "mins"),
    " minutes (n_sim: ", nsim_shap, ")", "\n")

###############################################################################

## plotting can be done separately, if objects already calculated, script can
## start here

plot <- FALSE

if(plot == FALSE){
  stop("Only calculating SHAP values, no plotting")
}

t01 <- Sys.time()

## loading in calculations object 
SHAP_analysis_re_run <- readRDS(here::here(
  "data", "final", "workspace_SHAP_analysis_re_run.rds"))

feature_sets <- LETTERS[1:5]

## Loop over all variable sets
set.seed(12)
output_full_SHAP_plotting <- capture.output({
  shap_plots_full <<- lapply(feature_sets, function(x){ 
    ## assign to global env
    
    
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
    
    
    ## also looping over the algorithms (rf + xgb) within feature set
    alg_objects <- grep("shp_1", names(ws_looped))
    
    alg_compact <- lapply(alg_objects, function(ob){ 
    ## initiating algorithm lapply statement
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
      
      ## shapviz part
      baseline <- attr(alg_looped, "baseline")
      shv <- shapviz(alg_looped, X = ws_looped$x_shap, baseline = baseline)
      shv$feature_type <- var_type[colnames(shv$S)]
      
      ## creating bee plot of top 20 features with highest global importance
      bee_plot <- 
        sv_importance(shv, kind = "bee", max_display = 20L, show_number = TRUE) + 
        ggtitle("Top 20 predictors with the highest average feature importance") +
        labs(subtitle = eval(
          parse(text = paste0(
            'expression(underline("Feature set: ',
            set, ' (Algorithm: ', alg_name, ')"))')
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
      
      
      filename_bee_plot <- paste0("bee_plot_", alg_name, "_", x, "_re_run.png")
      
      
      ########################################################################
      
      ## further inspection of the models 
      
      ## create additional dataframe that stores whether variable type is 
      ## Genetic feature, PGS, LGM, non-LGM or covariate
      ## further inspection of the models 
      imp_vec <- sv_importance(shv, kind = "no")
      # named numeric vector, sorted by mean |SHAP|
      
      ## creating dataframe of global importance values
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
      plot_SHAP_global <- ggplot(
        data = importance_df[1:20,],
        aes(x = reorder(Feature, Importance),
                        y = Importance,
                        fill = is_covariate)) + 
        geom_col() + 
        coord_flip() + 
        labs(x = "Variable", fill = "Covariate")
      
      filename_global_plot <- paste0("global_SHAP_", alg_name,
                                     "_", x, "_re_run.png")
      
      
      ## check if all covariates are still in
      all(ws_looped$covariates_full %in% importance_df$Feature)
      
      ## checking ranks of the covariates in the feature importance df 
      model_name <- paste0(set, " (", alg_name, ")")
      for(covariate in ws_looped$covariates_full){
        cat("Rank feature ", covariate, " in model ", model_name, ": ",
            importance_df %>%
              filter(Feature == covariate) %>%
              select(rowid) %>%
              pull(),
            " / ", nrow(importance_df), "\n")
      }
      
      
      ## non-LGM vs LGM features
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
                  shv = shv,
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
                          statistic = correlation_fn, R = 2000,
                          method = "spearman")
    
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
if(!dir.exists(here::here("data", "plots", "sensitivity"))){
  dir.create(here::here("data", "plots", "sensitivity"))
}

writeLines(output_full_SHAP_plotting,
           here::here("data", "plots", "sensitivity",
                      "output_SHAP_plotting_re_run.txt"))

## loading in table with item codes
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

## saving SHAP_plots object (now with shv so that plots can still be 
## customized)
saveRDS(shap_plots_full,
        here::here("data", "final", "shap_plots_full_re_run.rds"))


##############################################################################

## plots for variable importance analysis in paper


## re-load if script was ended after calculation
# shap_plots_full <- 
## readRDS(here::here("data", "final", "shap_plots_full_re_run.rds"))



## Viewing importance plots

## extracting all global plots, bee plots and importance dfs each in one list

plots_global <- shap_plots_full %>%
  map(~ .x[["alg_compact"]]) %>%   # list of alg_compact lists
  map(~ map(.x, "plot_SHAP_global")) %>%
  # for each alg_compact, extract plot from each unnamed subelement
  flatten()   

bee_plots <- shap_plots_full %>%
  map(~ .x[["alg_compact"]]) %>%   # list of alg_compact lists
  map(~ map(.x, "bee_plot")) %>%
  # for each alg_compact, extract plot from each unnamed subelement
  flatten()  

importance_dfs <- shap_plots_full %>%
  map(~ .x[["alg_compact"]]) %>%   # list of alg_compact lists
  map(~ map(.x, "importance_df")) %>%
  # for each alg_compact, extract plot from each unnamed subelement
  flatten()    

## saving the full importance dfs
for(i_df in 1:length(importance_dfs)){
  set <- case_when(i_df %in% c(1:2) ~"A",
                   i_df %in% c(3:4) ~"B",
                   i_df %in% c(5:6) ~"C",
                   i_df %in% c(7:8) ~"D",
                   i_df %in% c(9:10) ~"E",
                   .default = NA)
  
  alg <- ifelse(i_df %% 2 == 0, "xgb", "rf")
  
  filename <- paste0("SHAP_importance_df_", alg, "_", set, "_re_run.rds")
  
  saveRDS(importance_dfs[[i_df]], file = here::here("data", "final", filename))
  
}

## variable set A (CBCL items + covariates)

## Analyze these to fill in content of variables in documents
View(importance_dfs[[1]])
View(importance_dfs[[2]])

## plotting in grid
combined_plot_A <- cowplot::plot_grid(plots_global[[1]], plots_global[[2]],
                                      labels = c("RF", "XGB"))

beeswarm_plot_A <- cowplot::plot_grid(bee_plots[[1]], bee_plots[[2]],
                                      labels = c("RF", "XGB"))

beeswarm_plot_A

ggsave(filename = "beeswarm_plot_A_re_run.png",
       plot = beeswarm_plot_A,
       width = 16.0,
       height = 12.8,
       # device = "pdf",
       dpi = 300,
       path = here::here("data", "plots", "SHAP"),
       create.dir = TRUE)


## variable set B (PGS + genetic covariates)
View(importance_dfs[[3]])
View(importance_dfs[[4]])

combined_plot_B <- cowplot::plot_grid(plots_global[[3]], plots_global[[4]],
                                      labels = c("RF", "XGB"))

combined_plot_AB <- 
  cowplot::ggdraw() +
  cowplot::draw_plot(
    cowplot::plot_grid(
      combined_plot_A,
      NULL,
      combined_plot_B,
      nrow = 3,
      rel_heights = c(1, 0.1, 1),
      labels = paste0("Variable Set ", LETTERS[1:2]),
      label_y = 1.05
    ),
    x = 0, y = 0, width = 1, height = 1
  ) +
  theme(plot.margin = margin(10, 10, 10, 10))

beeswarm_plot_B <- cowplot::plot_grid(bee_plots[[3]], bee_plots[[4]],
                                      labels = c("RF", "XGB"))

beeswarm_plot_B

ggsave(filename = "beeswarm_plot_B_re_run.png",
       plot = beeswarm_plot_B,
       width = 20,
       height = 16,
       # device = "pdf",
       dpi = 300,
       path = here::here("data", "plots", "SHAP"),
       create.dir = TRUE)

## variable set C (CBCL + PGS + covariates)
View(importance_dfs[[5]])
View(importance_dfs[[6]])

beeswarm_plot_C <- cowplot::plot_grid(bee_plots[[5]], bee_plots[[6]],
                                      labels = c("RF", "XGB"))

beeswarm_plot_C

ggsave(filename = "beeswarm_plot_C_re_run.png",
       plot = beeswarm_plot_C,
       width = 18,
       height = 14.4,
       #device = "pdf",
       dpi = 300,
       path = here::here("data", "plots", "SHAP"),
       create.dir = TRUE)

## variable set D (CBCL + longitudinal variables + covariates)
View(importance_dfs[[7]])
importance_dfs[[7]][1:20,]
View(importance_dfs[[8]])
importance_dfs[[8]][1:20,]

beeswarm_plot_D <- cowplot::plot_grid(bee_plots[[7]], bee_plots[[8]],
                                      labels = c("RF", "XGB"))

beeswarm_plot_D

ggsave(filename = "beeswarm_plot_D_re_run.png",
       plot = beeswarm_plot_D,
       width = 18,
       height = 14.4,
       #device = "pdf",
       dpi = 300,
       path = here::here("data", "plots", "SHAP"),
       create.dir = TRUE)

## variable set E (all data modalities)
View(importance_dfs[[9]])
importance_dfs[[9]][1:20,]
View(importance_dfs[[10]])
importance_dfs[[10]][1:20,]

beeswarm_plot_E <- cowplot::plot_grid(bee_plots[[9]], bee_plots[[10]],
                                      labels = c("RF", "XGB"))

beeswarm_plot_E

ggsave(filename = "beeswarm_plot_E_re_run.png",
       plot = beeswarm_plot_E,
       width = 18,
       height = 14.4,
       #device = "pdf",
       dpi = 300,
       path = here::here("data", "plots", "SHAP"),
       create.dir = TRUE)


t02 <- Sys.time()

cat("duration plotting complete SHAP analysis (sensitivity analysis): ",
    difftime(t02, t01, unit = "mins"), " minutes")

## eoS