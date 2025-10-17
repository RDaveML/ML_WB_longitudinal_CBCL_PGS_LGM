# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-08-21
#
# Script Name: 46_ML_model_comparison_significance_testing_sensitivity.R
#
# Script Description: This script compares the performances of all models 
# to find significant differences and see which model and which feature set
# performs best. (sensitivity analysis!)
#
#
# Notes: Model performance results were calculated in the scripts
# 39-43
#
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "haven", "foreign", "here", "readr",
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "fastDummies", "RANN",
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest", "boot", "iml",
               "fastshap", "shapviz", "PMCMRplus", "tsutils", "lme4",
               "lmerTest", "emmeans", "clusrank")


## loading in bootstrapped model performance measures: points estimates and CIs

## model A
boot_performance_A <- readRDS(here::here(
  "data", "intermediate", "bootstrap", "boot_results_performace_A_sensitivity.rds"))
## save the sub_elements error_df in new list
## and remove them from the sub_elements of the original lists
error_dfs_A <- lapply(boot_performance_A, function(x) x[[length(x)]])
boot_performance_A <- lapply(boot_performance_A, function(x) x[-length(x)])



## model B
boot_performance_B <- readRDS(here::here(
  "data", "intermediate", "bootstrap", "boot_results_performace_B_sensitivity.rds"))
error_dfs_B <- lapply(boot_performance_B, function(x) x[[length(x)]])
boot_performance_B <- lapply(boot_performance_B, function(x) x[-length(x)])

## model C
boot_performance_C <- readRDS(here::here(
  "data", "intermediate", "bootstrap", "boot_results_performace_C_sensitivity.rds"))
error_dfs_C <- lapply(boot_performance_C, function(x) x[[length(x)]])
boot_performance_C <- lapply(boot_performance_C, function(x) x[-length(x)])

## model D
boot_performance_D <- readRDS(here::here(
  "data", "intermediate", "bootstrap", "boot_results_performace_D_sensitivity.rds"))
error_dfs_D <- lapply(boot_performance_D, function(x) x[[length(x)]])
boot_performance_D <- lapply(boot_performance_D, function(x) x[-length(x)])


## model E
boot_performance_E <- readRDS(here::here(
  "data", "intermediate", "bootstrap", "boot_results_performace_E_sensitivity.rds"))
error_dfs_E <- lapply(boot_performance_E, function(x) x[[length(x)]])
boot_performance_E <- lapply(boot_performance_E, function(x) x[-length(x)])

list_performances <- list(A = boot_performance_A,
                          B = boot_performance_B,
                          C = boot_performance_C,
                          D = boot_performance_D, 
                          E = boot_performance_E
)

## list of error dfs
list_error_dfs <- list(A = error_dfs_A,
                       B = error_dfs_B,
                       C = error_dfs_C,
                       D = error_dfs_D,
                       E = error_dfs_E
)

## next step: Create dataframe with the variables feature_set, model_name,
## rmse_est, r2_est, mae_est,
## rmse_ci_lower, rmse_ci_upper,
## r2_ci_lower, r2_ci_upper,
## mae_ci_lower, mae_ci_upper

## this df will be used for summary tables of the ML performances

list_model_metric_dfs <- lapply(1:length(list_performances), function(x){
  df_metrics <- data.frame()
  feature_set <- names(list_performances)[x]
  ## create a new list from the list currently being iterated where the 
  ## last list element is not included
  
  for(mod in 1:length(list_performances[[x]])){
    metrics <- c(feature_set = feature_set, unlist(list_performances[[x]][[mod]]))
    df_metrics <- rbind(df_metrics, metrics)
  }
  names(df_metrics) <- c("feature_set", "algorithm",
                         "rmse_est", "r2_est", "mae_est",
                         "rmse_ci_lower", "rmse_ci_upper",
                         "r2_ci_lower", "r2_ci_upper",
                         "mae_ci_lower", "mae_ci_upper")
  return(df_metrics)
})

model_performance_df <- do.call(rbind, list_model_metric_dfs) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(contains("rmse") | contains("r2") | contains("mae"), as.character)) %>%
  mutate(across(contains("rmse") | contains("r2") | contains("mae"), as.numeric)) %>%
  filter(algorithm != "xgb_stack") %>%
  droplevels() ## dropping unused levels

saveRDS(model_performance_df, file = here::here("data", "final", "model_performance_df_sensitivity.rds"))

levels(model_performance_df$algorithm)

## purely descriptively, we see that the point estimats consistently indicate
## higher performance of the random forest models over the 
## xgb models

## plotting model performance
pd <- position_dodge(width = 0.9)

model_descriptions <- c(
  "Only CBCL",
  "Only PGS",
  "CBCL + PGS",
  "CBCL + LGM",
  "CBCL + LGM + PGS"
)

levels(model_performance_df$feature_set) <- model_descriptions

algs <- c("Stacked LM", "Random Forest", "XGBoost")

levels(model_performance_df$algorithm) <- algs

plot_RMSE <- 
  model_performance_df %>%
  ggplot(aes(x = algorithm, y = rmse_est, fill = algorithm)) +
  ggtitle("Comparative model performance") +
  geom_col() + 
  scale_y_continuous(name = "RMSE") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(subtitle = "Metric: RMSE, error bars symbolize bootstrapped 95% Confidence Intervals",
       x = "Feature set") + 
  geom_errorbar(aes(ymin = rmse_ci_lower, ymax = rmse_ci_upper),
                position = pd, width = 0.4,
                linewidth = 0.4) +
  coord_cartesian(ylim = c(0.8, NA)) +
  facet_wrap(~feature_set,
             labeller = labeller(feature_set = label_wrap_gen(width = 10)),
             nrow = 1) + 
  theme_classic() + 
  theme(plot.title = element_text(size=15, face="bold.italic"),
        plot.subtitle = element_text(size=12, face="italic"),
        axis.text.x=element_blank(),
        axis.title.x = element_text(face="bold", size=15, angle=0, vjust = -1.2),
        axis.text.y = element_text(face="bold", size=15, angle=0),
        axis.title.y = element_text(face="bold", size=15),
        #legend.position = "none",
        panel.spacing = unit(1, "cm"))

ggsave(filename = "model_comparison_RMSE_sensitivity.png",
       plot = plot_RMSE,
       #width = 9.0,
       device = "png",
       path = here::here("data", "plots"),
       create.dir = TRUE)


plot_r2 <- 
  model_performance_df %>%
  ggplot(aes(x = algorithm, y = r2_est, fill = algorithm)) +
  geom_col() + 
  scale_y_continuous(name = "R²", limits = c(0, 0.3)) +
  geom_errorbar(aes(ymin = r2_ci_lower, ymax = r2_ci_upper),
                position = pd, width = 0.4,
                linewidth = 0.4) +
  facet_wrap(~feature_set, nrow = 3)



##----------------------------------------------------------------------------

## significance testing: using the error dfs

## create one df out of all error dfs. 
full_error_df <- do.call(rbind,
                         lapply(list_error_dfs, function(x) {
                           do.call(rbind, x)
                         })) %>%
  mutate(across(where(is.character), as.factor)) #%>%
# filter(algorithm == "rf" | algorithm == "lm_stack")

rownames(full_error_df) <- NULL

saveRDS(full_error_df, file = here::here("data", "final", "full_error_df_sensitivity.rds"))