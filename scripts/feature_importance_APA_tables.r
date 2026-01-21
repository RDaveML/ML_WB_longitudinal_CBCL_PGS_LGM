## Script to make APH style and latex tables from 
# variable importance dataframes

pacman::p_load("dplyr", "haven", "foreign", "here", "readr",
               "stringr", "readxl", "data.table", "caret", "car",
               "parallel", "doParallel", "RANN",
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest",
               "fastshap", "shapviz", "kernelshap", "ggtext", "boot", "tibble",
               "flextable", "rempsyc", "kableExtra", "officer", "ggplot2bdc")


## loading in SHAP plots and objects
sensitivity <- FALSE
if(sensitivity){
  shap_plots_full <- readRDS(
    here::here("data", "final", "shap_plots_full_sensitivity.rds"))
  
  importance_dfs <- shap_plots_full %>%
    map(~ .x[["alg_compact"]]) %>%   # list of alg_compact lists
    map(~ map(.x, "importance_df")) %>%
    # for each alg_compact, extract plot from each unnamed subelement
    flatten()   
  
  CBCL_items_table <- read_excel(here::here("doc",
                                            "CBCL_table_t_per_item.xlsx"))
  raw_CBCL_items <- c(
    readRDS(
      here::here("data", "intermediate", "CBCL_items_drop.rds")),
    readRDS(
      here::here("data", "intermediate", "CBCL_items_keep.rds"))
  )
  
  importance_tables_APA <- lapply(importance_dfs, function(df){
    # create flextable
    ft <- df[1:20,] %>%
      mutate(Importance = round(Importance, 3),
             `Type variable` = case_when(
               Feature %in% raw_CBCL_items ~ "Symptom",
               ## if the string "LGM" is contained in the value of Feature, 
               ## then assign "Longitudinal variable"
               grepl("LGM", Feature) ~ "Longitudinal variable",
               grepl("PGS", Feature) ~ "Polygenic score",
               is_covariate == "Yes" ~ "Covariate",
               .default = NA
             )) %>%
      select(-rowid, -is_covariate) %>%
      flextable() %>%
      set_header_labels(
        Feature = "Variable",
        Importance = "Global importance") %>%
      fontsize(size = 8, part = "all") %>%  # Smaller font to fit more columns
      bold(part = "header") %>%
      set_table_properties(layout = "autofit", width = 1) %>%
      # Use full page width
      autofit()
    
    # create latex code
    latex_code <- df[1:20,] %>%
      mutate(Importance = round(Importance, 3),
             `Type variable` = case_when(
               Feature %in% raw_CBCL_items ~ "Symptom",
               grepl("LGM", Feature) ~ "Longitudinal variable",
               grepl("PGS", Feature) ~ "Polygenic Score",
               is_covariate == "Yes" ~ "Covariate",
               .default = NA
             )) %>%
      select(-rowid, -is_covariate) %>%
      rename("Variable" = Feature,
             "Global importance"  = Importance) %>%
      kable(format = "latex", booktabs = TRUE, longtable = TRUE) %>%
      kable_styling(latex_options = c("hold_position", "repeat_header"))
    
    return(list(flextable = ft, latex = latex_code))
  })
  
  importance_tables_APA[[9]]$latex
}



shap_plots_full <- readRDS(
  here::here("data", "final", "shap_plots_full_OG.rds"))



## loading in items table
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))
raw_CBCL_items <- c(
  readRDS(
    here::here("data", "intermediate", "CBCL_items_drop.rds")),
  readRDS(
    here::here("data", "intermediate", "CBCL_items_keep.rds"))
)


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

## saved shapviz elements to customize plots
shvs <- shap_plots_full %>%
  map(~ .x[["alg_compact"]]) %>%   # list of alg_compact lists
  map(~ map(.x, "shv")) %>%
  # for each alg_compact, extract plot from each unnamed subelement
  flatten()

## prepare CBCL_items table for APA style table
CBCL_items_table_APA <- CBCL_items_table %>%
   ## replace NA with "-"
    mutate_all(~ ifelse(is.na(.), "-", .)) %>%
    ## rename columns that being with "age": 
    ## Add "Variable name at" before the name
    rename_with(~ paste("Variable name at", .), starts_with("age")) %>%
    rename("Variable prefix" = question_number, 
           "Times question was asked" = t_per_question)

CBCL_items_flextable <- CBCL_items_table_APA %>%
    flextable() %>%
    fontsize(size = 8, part = "all") %>%  # Smaller font to fit more columns
    bold(part = "header") %>%
    set_table_properties(layout = "autofit", width = 1) %>%  
    # Use full page width
    autofit()


## create latex code from CBCL_table
latex_CBCL_items_table <- CBCL_items_table_APA %>%
    kable(format = "latex", booktabs = TRUE, longtable = TRUE,
          caption = "CBCL Items Included in Analysis") %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))


## for all data frames saved in importance_dfs, create APA
## style flextables and latex code
## always only use top 20 features,
## full tables can be requested from first author or adapting the code below 
## to show full importance dfs
## 

importance_tables_APA <- lapply(importance_dfs, function(df){
  # create flextable
  ft <- df[1:20,] %>%
    mutate(Importance = round(Importance, 3),
           `Type variable` = case_when(
             Feature %in% raw_CBCL_items ~ "Symptom",
             ## if the string "LGM" is contained in the value of Feature, 
             ## then assign "Longitudinal variable"
             grepl("LGM", Feature) ~ "Longitudinal variable",
             grepl("PGS", Feature) ~ "Polygenic score",
             is_covariate == "Yes" ~ "Covariate",
             .default = NA
           )) %>%
    select(-rowid, -is_covariate) %>%
    flextable() %>%
    set_header_labels(
      Feature = "Variable",
      Importance = "Global importance") %>%
    fontsize(size = 8, part = "all") %>%  # Smaller font to fit more columns
    bold(part = "header") %>%
    set_table_properties(layout = "autofit", width = 1) %>% 
    # Use full page width
    autofit()
  
  # create latex code
  latex_code <- df[1:20,] %>%
    mutate(Importance = round(Importance, 3),
           `Type variable` = case_when(
             Feature %in% raw_CBCL_items ~ "Symptom",
             grepl("LGM", Feature) ~ "Longitudinal variable",
             grepl("PGS", Feature) ~ "Polygenic Score",
             is_covariate == "Yes" ~ "Covariate",
             .default = NA
           )) %>%
    select(-rowid, -is_covariate) %>%
    rename("Variable" = Feature,
           "Global importance"  = Importance) %>%
    kable(format = "latex", booktabs = TRUE, longtable = TRUE) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))
  
  return(list(flextable = ft, latex = latex_code))
})

# give out to print code to console and write to overleaf document
# importance_tables_APA[[10]]$flextable
# importance_tables_APA[[3]]$latex
importance_tables_APA[[10]]$latex


## ----------------------------------------------------------------------

## bee plots of SHAP values

## Use this renaming scheme if other bee plots are requested

## saving beeswarm_plot_E for random forest
shvs_E_rf <- shvs[[9]]

names_top_20_E_rf <- names(sv_importance(shvs_E_rf, kind = "no"))[1:20]

## more readable variable names for this plot specifically,
## variable codes added maunally
names_plot <- c("'Secretive' \n(age 16)",
                "SD self-report \nitems",
                "Longitudinal SD \n'Loneliness'",
                "Mean self-report \nitems",
                "Longitudinal Mean \n'Loneliness'",
                "Autocorrelation lag-4 \n'Loneliness'",
                "'Loneliness' \n(age 16)",
                "'Feels worthless' \n(age 16)",
                "'Unhappy, sad' \n(age 16)",
                "ADHD (PGS)",
                "Genetic \nPC 16",
                "'Enjoys little' \n(age 16)",
                "Longitudinal Mean \n'Enjoys little'",
                "Latent slope \n'Self-conscious'",
                "Prob latent group 1\n 'Feels worthless'",
                "'Feels no love' \n(age 16)",
                "Prob latent group 1 \n'Feels no love'",
                "Longitudinal SD \n'Inattentive, distracted'",
                "Autoregression lag-9 \n'Disobedient at home'",
                "Latent slope \n'Worries")

names_df_custom <- data.frame(name_OG = names_top_20_E_rf,
                              name_new = names_plot)


names(shvs_E_rf$X) <- sapply(names(shvs_E_rf$X), function(y){
  
  if(y %in% names_top_20_E_rf){
    
    new_name <- names_df_custom %>%
      filter(name_OG == y) %>%
      select(name_new) %>%
      pull()
    
    cat("name variable ", y, " changed to: ", new_name, "\n")
  } else {
    new_name <- y
  }
  return(new_name)
})

names(shvs_E_rf$X) <- sapply(names(shvs_E_rf$X), function(y){
  
  if(y %in% names_top_20_E_rf){
    
    new_name <- names_df_custom %>%
      filter(name_OG == y) %>%
      select(name_new) %>%
      pull()
    
    cat("name variable ", y, " changed to: ", new_name, "\n")
  } else {
    new_name <- y
  }
  return(new_name)
})

colnames(shvs_E_rf$S) <- sapply(colnames(shvs_E_rf$S), function(y){
  
  if(y %in% names_top_20_E_rf){
    
    new_name <- names_df_custom %>%
      filter(name_OG == y) %>%
      select(name_new) %>%
      pull()
    
    cat("name variable ", y, " changed to: ", new_name, "\n")
  } else {
    new_name <- y
  }
  return(new_name)
})

names(shvs_E_rf$feature_type) <- sapply(names(shvs_E_rf$feature_type), function(y){
  
  if(y %in% names_top_20_E_rf){
    
    new_name <- names_df_custom %>%
      filter(name_OG == y) %>%
      select(name_new) %>%
      pull()
    
    cat("name variable ", y, " changed to: ", new_name, "\n")
  } else {
    new_name <- y
  }
  return(new_name)
})


## saving alternative bee plot with more clear labels
bee_plot_E_rf <- 
  sv_importance(
    shvs_E_rf, kind = "bee", max_display = 20L, show_number = TRUE) + 
  #ggtitle("Top 20 predictors with the highest average feature importance") +
  #labs(subtitle = eval(
  #  parse(text = paste0(
  #    'expression(underline("Feature set: ',
  #             "E (all modalities)", ' (Algorithm: ', "random forest", ')"))')
  #  ))) +
  # labs(subtitle = paste0("Feature set: ", set, " (algorithm: ", alg_name, ")"))
  #labs(subtitle = expression(underline(Model:~CBCL+PGS+LGM))) +
  theme_classic() + 
  theme(#plot.title = element_text(size=15, face="bold.italic"),
        #plot.subtitle = element_text(size=12, face="italic"),
        axis.text.x = element_text(face="bold", size=22, angle=0),
        axis.title.x = element_text(face="bold", size=24, angle=0,
                                    margin = margin(t = 16)),
        axis.text.y = element_text(face="bold", size=22, angle=0),
        axis.title.y = element_blank(),
        legend.title    = element_text(face = "bold", size = 26),
        legend.text     = element_text(size = 24),
        legend.key.size = grid::unit(3, "cm"),
        legend.spacing  = grid::unit(1.8, "cm"))

for (i in seq_along(bee_plot_E_rf$layers)) {
  if (inherits(bee_plot_E_rf$layers[[i]]$geom, "GeomText")) {
    bee_plot_E_rf$layers[[i]]$aes_params$size <- 8   # <-- set larger value here
  }
}


path_paper_A <- here::here("data", "plots")

## saving the plot bee_plot_E_rf so that it fits within the margins of an
## overleaf document and without quality being compromised
ggsave(
  filename = "bee_plot_E_rf_updated.png",
  plot = bee_plot_E_rf,
  width = 12,
  height = 18,    
  dpi = 300,
  path = path_paper_A,
  create.dir = TRUE)

## Combining for each variable set the beeswarm plots

## 1) Removing titles
for(p in 1:length(bee_plots)){
  bee_plots[[p]] <- bee_plots[[p]] +
    ggtitle("") +
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 7))
}

## combining always 2 into a cowplot grid
for(p in seq(2, 10, 2)){
  beeswarm_plot_combined <- 
    cowplot::plot_grid(bee_plots[[p-1]], bee_plots[[p]],
                       labels = c("RF", "XGB"), nrow = 2, ncol = 1,
                       label_size = 12)
  
  set <- LETTERS[(p/2)]
  
  
  ggsave(
    filename = paste0("bee_plot_", set, "_combined_full.pdf"),
    plot = beeswarm_plot_combined,
    width=14,
    height=24.1,
    units="cm",   
    dpi = 300,
    path = paste0(path_paper_A, "/SHAP"),
    create.dir = TRUE)
  
}

poster <- FALSE


if(poster){
  ## Importance df 9 for poster
  importance_df_poster <- importance_dfs[[9]][c(1:10), ] %>%
    mutate(Importance = round(Importance, 3))
  importance_df_poster$group_variable <- 
    c("Childhood mental health item",
      "Covariate (rater)", 
      "Longitudinal variable",
      "Covariate (rater)", 
      "Longitudinal variable",
      "Longitudinal variable",
      "Childhood mental health item",
      "Childhood mental health item",
      "Childhood mental health item",
      "Polygenic Score")#,
  #    "Covariate",
  #    "CBCL item",
  #    "Longitudinal variable",
  #    "Longitudinal variable",
  #    "Longitudinal variable",
  #    "CBCL item",
  #    "Longitudinal variable",
  #    "Longitudinal variable",
  #    "Longitudinal variable",
  #    "Longitudinal variable")
  
  importance_df_poster$Feature <- 
    c("Being secretive (age 16)",
      "SD self-report items", 
      "longitudinal SD complaints of loneliness",
      "Mean self-report items", 
      "longitudinal mean complaints of loneliness",
      "Lag-4 autocorrelation complaints of loneliness",
      "Complaints of loneliness (age 16)",
      "Feels worthless (age 16)",
      "Feels unhappy / sad (age 16)",
      "Polygenic Score ADHD")
  
  ft_poster <- importance_df_poster %>%
    select(-rowid, -is_covariate) %>%
    flextable() %>%
    set_header_labels(
      Feature = "Variable",
      Importance = "Global importance",
      group_variable = "Data modality") %>%
    fontsize(size = 8, part = "all") %>%  # Smaller font to fit more columns
    bold(part = "header") %>%
    set_table_properties(layout = "autofit", width = 1) %>%
    # Use full page width
    #autofit() %>%
    align(align = "left", part = "all") %>%
    set_table_properties(layout = "autofit") %>% 
    width(width = 1)
  
  ft_poster
}


## tables and bee plots for other variable sets can be requested from 
## the first author




