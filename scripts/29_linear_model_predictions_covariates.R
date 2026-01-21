# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-09-30
#
# Script Name: 29_linear_model_predictions_covariates.R
#
# Script Description: This script contains a confounder analysis
# the predictions from all models on the training set are regressed on 
# all covariates of the model in a linear regression to see if the
# confounders explain variance in the predictions of the outcome which is not
# explained by any of the other features, as according to Dinga et al., 2023
# Code partly taken from:
# https://github.com/pchabets/chronicity-prediction-depression/blob/main/code/additional_analyses/confounder_analysis/confounders_xgboost.R
#
#
# Notes:
#
#

# Set options
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "modEvA", "caret", "splines",
               "cowplot")

## loading in custom functions
source(here::here("scripts", "functions", "functions_ml.R"))

## function to calculate and compare partial and decomposed r2 
## (source: https://github.com/pchabets/chronicity-prediction-depression/blob/main/code/additional_analyses/confounder_analysis/confounders_xgboost.R)
## Note: If categorical outcome, use d2 (Dsquared function from modEvA package)
## for deviation instead of R2

decompose_r2 <- function(m_conf, m_pred, m_conf_pred){
  r2_conf <- summary(m_conf)$adj.r.squared
  r2_pred <- summary(m_pred)$adj.r.squared
  r2_conf_pred <- summary(m_conf_pred)$adj.r.squared
  
  conf_unexplained <- 1 - r2_conf
  pred_unexplained <- 1 - r2_pred
  
  delta_pred <- r2_conf_pred - r2_conf
  delta_conf <- r2_conf_pred - r2_pred
  
  partial_pred <- delta_pred / conf_unexplained
  partial_conf <- delta_conf / pred_unexplained
  
  shared = r2_conf_pred - delta_conf - delta_pred
  
  res <- c('confounds' = r2_conf,
           'predictions' = r2_pred,
           'confounds+predictions' = r2_conf_pred,
           'delta confounds' = delta_conf,
           'delta predictions' = delta_pred,
           'partial confounds' = partial_conf,
           'partial predictions' = partial_pred,
           'shared' = shared)
  res <- as.data.frame(res)
  res$r2_type <- rownames(res)
  return(res)
}

###############################################################################

filepath_bootstrap <- here::here("data", "intermediate", "bootstrap")

## preparing function to load in data and covariates, preprocess, 
## and load in workspaces for all feature sets (A-E) and algorithm types
## (random forest & xgb)
feature_sets <- LETTERS[1:5]
algorithm_type <- c("rf", "xgb")

full_models_list <- lapply(feature_sets, function(set){
  
  print(set)
  ## determining names of files and loading in data and ids, depending on 
  ## which set
  
  ## covariates names and train / test ids 
  if(set %in% c("A", "D")){
    
    covariates_names <- readRDS(
      here::here("data", "intermediate", "names_covariates.rds")
    )
    
    rater_covariates <- readRDS(
      here::here("data", "intermediate", "names_rater_covariates.rds")
    )
    
    ## one vector with all covariates names
    covariates_names <- c(covariates_names, rater_covariates)
    
    
    ## further distinction: numeric and factor covariates
    num_covariates <- c(grep("time_lag", covariates_names, value = TRUE),
                        grep("age_qol", covariates_names, value = TRUE),
                        rater_covariates)
    
    factor_covariates <- setdiff(covariates_names, num_covariates)
    
    train_ids <- readRDS(
      here::here("data", "intermediate", "indices_train.rds"))
    
    test_ids <- readRDS(here::here("data", "intermediate", "indices_test.rds"))
    
  } else if (set %in% c("C", "E")){
    
    covariates_names <- readRDS(
      here::here("data", "intermediate", "names_covariates.rds"))
    
    ## loading in rater covariates names
    rater_covariates <- readRDS(
      here::here("data", "intermediate", "names_rater_covariates.rds")
    )
    
    ## loading in vector with names genetic covariates
    gen_covariates <- readRDS(
      here::here("data", "intermediate", "names_genetic_covariates.rds"))
    
    ## one vector with all covariates names
    covariates_names <- c(covariates_names, gen_covariates)
    
    num_covariates <- c(
      grep("time_lag", covariates_names, value = TRUE),
      grep("age_qol", covariates_names, value = TRUE),
      grep("[0-9]+_1KG", gen_covariates, value = TRUE),
      rater_covariates
    )
    
    factor_covariates <- c(setdiff(covariates_names, num_covariates),
                           "EUR_1KG_Outlier",
                           "NL_Strict_Outlier")
    
    covariates_names <- c(covariates_names,
                          "EUR_1KG_Outlier",
                          "NL_Strict_Outlier", 
                          rater_covariates)
    
    train_ids <- readRDS(
      here::here("data", "intermediate", "indices_train_PGS.rds"))
    
    test_ids <- readRDS(
      here::here("data", "intermediate", "indices_test_PGS.rds"))
    
  } else { ## set == B, no rater covariates
    ## loading in covariate names
    covariates_names <- readRDS(
      here::here("data", "intermediate", "names_covariates.rds"))
    
    ## loading in vector with names genetic covariates
    gen_covariates <- readRDS(
      here::here("data", "intermediate", "names_genetic_covariates.rds"))
    
    ## one vector with all covariates names
    covariates_names <- c(covariates_names, gen_covariates)
    
    num_covariates <- c(
      grep("time_lag", covariates_names, value = TRUE),
      grep("age_qol", covariates_names, value = TRUE),
      grep("[0-9]+_1KG", gen_covariates, value = TRUE)
    )
    
    factor_covariates <- c(setdiff(covariates_names, num_covariates),
                           "EUR_1KG_Outlier",
                           "NL_Strict_Outlier")
    
    covariates_names <- c(covariates_names,
                          "EUR_1KG_Outlier",
                          "NL_Strict_Outlier")
    
    train_ids <- readRDS(
      here::here("data", "intermediate", "indices_train_PGS.rds"))
    
    test_ids <- readRDS(
      here::here("data", "intermediate", "indices_test_PGS.rds"))
  }
  
  ## loading in data (still need to be preprocessed)
  data <- readRDS(
    here::here("data", "intermediate", paste0("data_model_", set, ".rds"))) %>%
    select(FISNumber, FamilyNumber, QoL_simple, all_of(covariates_names))
  
  ## preprocessing
  ## converting covariates to factors
  ## (this is done in the function f_conv)
  data <- f_conv(df = data, covariates = factor_covariates)
  
  
  ## converting columns with multiple
  ## class types to numeric
  data <- mult_to_numeric(df = data)
  

  ## workspace with predictions dataframe
  filename_workspace <- grep(paste0("workspace_model_", set),
                             list.files(filepath_bootstrap), value = TRUE)[1]
  
  workspace <- readRDS(paste0(filepath_bootstrap,
                              "/", 
                              filename_workspace))
  
  ## running over both algorithms: rf and xgb
  list_set <- lapply(algorithm_type, function(alg){
    
    ## selecting the prediction dataframe associated with the 
    ## algorithm iterated
    pred_df <- 
      workspace[[grep(alg, names(workspace))]][[
        grep(paste0("preds_df_", alg),
             names(workspace[[grep(alg, names(workspace))]]))]]
    
    set <- case_when(
      set == "A" ~ "Symptoms only",
      set == "B" ~ "PGS only",
      set == "C" ~ "Symptoms + PGS",
      ## adding linebreak before the string "Longitudinal variables"
      set == "D" ~ "Symptoms + \nLongitudinal variables",
      #set == "D" ~ "Symptoms + Longitudinal variables",
      # set == "E" ~ "PGS + Symptoms + Longitudinal variables",
      set == "E" ~ "PGS + Symptoms + \nLongitudinal variables",
      #.default == as.character(set)
    )
    
    alg <- case_when(
      alg == "rf" ~ "Random Forest",
      alg == "xgb" ~ "XGBoost",
      #.default == as.character(alg)
    )
    
    ## saving name of column that contains predicted value
    prediction_col <- grep("predictions", colnames(pred_df), value = TRUE)
    print(prediction_col)
    
    ## model being calculated on test set
    data <- data %>%
      left_join(pred_df, by = "FISNumber") %>%
      filter(train == 0)
    
    ###########################################################################
    
    ## models to compare:
    
    ## 1) linear model with only confounders as predictors
    formula_1 <- formula(paste0("QoL_simple ~ ",
                                paste(covariates_names, collapse = "+ ")))
    
    # if non-continuous outcome: use glm, family e.g. binomial
    m_conf <- lm(formula = formula_1, data = data)
    
    ## 2) model with only prediction as predictor of the outcome
    formula_2 <- formula(paste0("QoL_simple ~ ", prediction_col))
    
    m_pred <- lm(formula = formula_2, data = data)
    
    ## 3) model with both ML predictions and confounds as covariate
    formula_3 <- formula(paste0("QoL_simple ~ ",
                                paste(covariates_names, collapse = "+ "),
                                " + ", prediction_col))
    
    m_conf_pred <- lm(formula = formula_3, data = data)
    
    # get partial and delta r2 for all confounds
    decomposed_r2 <- decompose_r2(m_conf, m_pred, m_conf_pred)
    
    # testing whether the model that also includes predictions 
    # has significantly higher R² than the confounders only model
    # F-test between nested models
    test <- anova(m_conf, m_conf_pred, test = "F")
    p_val <- test$`Pr(>F)`[2]   # second row gives test of added predictors
    
    ## Bonferroni-corrected significance
    ## in total 10 tests, 2(rf + xgb) for each feature set (A-E)
    pred_sig_bonf <- p_val < 0.05 / 10
    
    # table and plotting
    t1 <- decomposed_r2[c(1:5,8),]
    t1$conf <- 'all'
    #t1 <- rbind(t1, t2)
    # t1
    
    t_small <- t1[t1$r2_type %in% 
                    c('delta predictions', 'shared', 'delta confounds'),]
    t_small$r2_type <- factor(t_small$r2_type,
                              levels=rev(c('delta predictions',
                                           'shared',
                                           'delta confounds')),
                              labels=c('covariates only',
                                       'covariates + predictions', 
                                       'predictions only'
                              ))
    p <- ggplot(t_small, aes(y=res, x=conf, fill=r2_type)) +
      geom_bar(stat='identity') +
      coord_flip() +
      labs(y=expression( R^{2}),
           x='Confounders') +
      theme_minimal_vgrid() +
      theme(aspect.ratio = 0.25) +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank()) + 
      scale_fill_manual(values = c("#F0E442", "#9CCA9D", "#56B4E9"),
                        name='Variance explained by') +
      labs(title = paste0("Variable set: ", set, "\n", "Algorithm: ", alg))
    
    return(list(decomposed_r2 = decomposed_r2,
                pred_sig_bonf = pred_sig_bonf,
                pval = p_val,
                plot_decomposition = p))
  })
  
  ## assigning the correct name to the list elements
  names(list_set) <- algorithm_type
    
  return(list_set = list_set)  
  
})

names(full_models_list) <- paste0("confounder_analysis_set_", feature_sets)


## analysis: In how many of the cases is adding the predictions to the 
## confounders significant regarding R²?


## A) Bonferroni-corrected p-value

## Indexing lists to the deepest level
sig_values_bonf <- unlist(lapply(full_models_list, function(sublist) {
  lapply(sublist, function(x) x$pred_sig_bonf)
}))

# Print them out
print(sig_values_bonf)


## B) Benjamini-Hochberg method (less conservative)
sig_values_bh <- unlist(lapply(full_models_list, function(sublist) {
  lapply(sublist, function(x) x$pval)
}))

## applying the correction
sig_values_bh_adj <- p.adjust(unname(sig_values_bh), method = "BH")

sig_bh <- sig_values_bh_adj < 0.05

names(sig_bh) <- names(sig_values_bh)

print(sig_bh)


###############################################################################

## plots
decomposition_plots <- flatten(map(full_models_list,
                                   ~ map(.x, "plot_decomposition")))
## plotting in grid

## removing legend from all plots but first one
for(p in 1:length(decomposition_plots)){
  if(p == 5){
    next
    } else{ decomposition_plots[[p]] <- decomposition_plots[[p]] +
    theme(legend.position="none")
    }
}


## reducing margins for better plotting
for (i in seq_along(decomposition_plots)) {
  if (i %% 2 == 1) {  # left column
    decomposition_plots[[i]] <- decomposition_plots[[i]] +
      theme(plot.margin = margin(5, -5, 5, 5))
  } else {            # right column
    decomposition_plots[[i]] <- decomposition_plots[[i]] +
      theme(plot.margin = margin(5, 5, 5, -5))
  }
}

## align all plots to ensure proper alignment
aligned_plots <- cowplot::align_plots(plotlist = decomposition_plots,
                                      align = "hv")

## Creating combined plot with 5 rows and two columns with less space between 
## the columns than it would be default
combined_plot <- cowplot::plot_grid( 
  plotlist = aligned_plots, ncol = 2, nrow = 5)#,
  # panel_spacing = unit(0.4, "lines"))

combined_plot <- cowplot::plot_grid(
  plotlist = aligned_plots,
  ncol = 2,
  labels = NULL,
  label_size = 12,
  hjust = -0.5,
  vjust = 1,
  rel_heights = rep(1, 5),
  rel_widths = c(1, 1)
)
#combined_plot

## saving workspace image to continue working on later
save.image(
  here::here("data", "intermediate", "workspace_confounder_analysis.RData"))

# eos



