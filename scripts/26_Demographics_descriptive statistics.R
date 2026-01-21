# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2025
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2025-09-08
#
# Script Name: 26_Demographics_descriptive statistics.R
#
# Script Description: Calculating and formatting descriptive statistics 
# of demographic variables for the report
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
               "stringr", "readxl", "data.table", "psych", "flextable",
               "rempsyc", "fasano.franceschini.test", "kableExtra", "mde")

#devtools::install_github("https://github.com/rempsyc/rempsyc/blob/main/R/nice_table.R")

path_paper_A <- here::here("data", "plots")
if(!dir.exists(path_paper_A)){
  dir.create(path_paper_A)
}

###############################################################################

## 1) full dataset 


## loading in raw dataset A (no genetic data) and covariates A to calculate 
## descriptive statistics
data_full_raw <- readRDS(here::here("data", "intermediate", "data_model_A.rds"))

covariates_names <- readRDS(
  here::here("data", "intermediate", "names_covariates.rds")
)


## number of families 
length(unique(data_full_raw$FamilyNumber))
## 3,250 families

## extract descriptives about the covariates
table(data_full_raw$sex)
table(data_full_raw$twzyg)
table(data_full_raw$ea4fa_agg)
table(data_full_raw$ea4mo_agg)
table(data_full_raw$QoL_indicator)
mean(data_full_raw$age_qol)
sd(data_full_raw$age_qol)
range(data_full_raw$age_qol)
table(data_full_raw$QoL_simple)

## descriptives for continuous variables
describe(data_full_raw %>% select(age_qol, time_lag))

## histogram of age
data_full_raw %>%
  ggplot(aes(x = age_qol)) + 
  geom_histogram(bins = round(max(data_full_raw$age_qol, na.rm = TRUE), 1) - 
                        round(min(data_full_raw$age_qol, na.rm = TRUE), 1) + 1)

## histogram of outcome QoL
hist_QoL <- 
data_full_raw %>%
  ggplot(aes(x = QoL_simple)) + 
  geom_histogram(bins = 10, fill = "#3182bd") +
  #ggtitle("Histogram of Quality of Life in sample") +
  scale_x_continuous(name="Quality of life", breaks=c(1:10)) + 
  #labs(subtitle = "N = 5,087") +
  theme_classic() + 
  theme(plot.title = element_text(size=15, face="bold.italic"),
        axis.text.x = element_text(face="bold", size=8, angle=0),
        axis.title.x = element_text(face="bold", size=9, angle=0),
        axis.text.y = element_text(face="bold", size=8, angle=0),
        axis.title.y = element_text(face="bold", size=9, angle=0))
## caption and title not needed, will be added in markdown
  
ggsave(filename = "histogram_QoL.pdf",
       plot = hist_QoL,
       #device = "png",
       width=12.1,
       height=8.07,
       units="cm",
       path = path_paper_A,
       create.dir = TRUE)

describe(data_full_raw %>% select(all_of(covariates_names)))


##############################################################################

## descriptive demographics summary table

## For full sample (N = 5,087)

## Summary for categorical variables
PGS <- FALSE
if(PGS == FALSE){
  categorical_vars <- c("sex", "twzyg", "ea4fa_agg",
                        "ea4mo_agg", "QoL_indicator")
  categorical_vars_names <- c("ANTR wave", 
                              "Educational attainment (father)",
                              "Educational attainment (mother)",
                              "Zygosity")
  
  
  categorical_summary <- data_full_raw %>%
    mutate(across(all_of(categorical_vars), as.character)) %>%
    rename("ANTR wave" = QoL_indicator, 
           "Educational attainment (father)" = ea4fa_agg,
           "Educational attainment (mother)" = ea4mo_agg,
           "Zygosity" = twzyg) %>%
    pivot_longer(all_of(categorical_vars_names),
                 names_to = "Variable", values_to = "Category") %>%
    group_by(Variable, Category) %>%
    summarise(N = n(), .groups = "drop") %>%
    group_by(Variable) %>%
    mutate(Percent = round(100 * N / sum(N), 1),
           `N (%)` = paste0(N, " (", Percent, "%)")) %>%
           ## concatenating variables
    ungroup() %>%
    select(Variable, Category, `N (%)`)
  
  # summary for Continuous variables 
  continuous_vars <- c("time_lag", "age_qol")
  
  continuous_vars_names <- c("Time between YSR and QoL assessment", 
                              "Age at QoL assessment")
  
  continuous_summary <- data_full_raw %>%
    select(all_of(continuous_vars)) %>%
    rename("Time between YSR and QoL assessment" = time_lag,
           "Age at QoL assessment" = age_qol) %>%
    summarise(across(everything(), ~ paste0(
      round(mean(. , na.rm=TRUE),1),
      " (", round(sd(., na.rm=TRUE),1), ")"))) %>%
    pivot_longer(cols = everything(), names_to = "Variable",
                 values_to = "M_SD") %>%
    mutate(Category = "") %>%   # continuous variables don't have categories
    rename(`N (%)` = M_SD)      # reuse the column for display
  
  # Combine categorical and continuous
  demographics_table <- bind_rows(
    categorical_summary %>% rename(Statistic = `N (%)`),
    continuous_summary %>% rename(Statistic = `N (%)`)
  ) %>%
    select(Variable, Category, Statistic)
  
  # APA-style flextable 
  ft <- flextable(demographics_table) %>%
    set_header_labels(Variable = "Variable",
                      Category = "Category",
                      Statistic = "N (%) or M (SD) in years") %>%
    autofit()
  
  ft
  
  # Optional: for APA style, make Variable only appear once per group
  demographics_table_apa <- demographics_table %>%
    group_by(Variable) %>%
    mutate(Variable = ifelse(row_number() == 1, Variable, "")) %>%
    ungroup() %>%
    rename("N (%) or M (SD) in years" = Statistic)
  
  # Export as LaTeX table
  latex_table <- kable(demographics_table_apa, 
                       format = "latex", 
                       booktabs = TRUE,
                       caption = "Demographic Descriptives",
                       align = c("l", "l", "c")) %>%
    kable_styling(latex_options = c("hold_position"))
  
  # Print LaTeX code to console
  latex_table

}

##############################################################################

## for dataset with PGS (N = 2,656)

if(PGS){
  PGS_indices <- c(
    readRDS(here::here("data", "intermediate", "indices_train_PGS.rds")),
    readRDS(here::here("data", "intermediate", "indices_test_PGS.rds")))
  
  categorical_vars <- c("sex", "twzyg", "ea4fa_agg",
                        "ea4mo_agg", "QoL_indicator")
  categorical_vars_names <- c("ANTR wave", 
                              "Educational attainment (father)",
                              "Educational attainment (mother)",
                              "Zygosity")
  
  
  categorical_summary <- data_full_raw %>%
    filter(FISNumber %in% PGS_indices) %>%
    mutate(across(all_of(categorical_vars), as.character)) %>%
    rename("ANTR wave" = QoL_indicator, 
           "Educational attainment (father)" = ea4fa_agg,
           "Educational attainment (mother)" = ea4mo_agg,
           "Zygosity" = twzyg) %>%
    pivot_longer(all_of(categorical_vars_names),
                 names_to = "Variable", values_to = "Category") %>%
    group_by(Variable, Category) %>%
    summarise(N = n(), .groups = "drop") %>%
    group_by(Variable) %>%
    mutate(Percent = round(100 * N / sum(N), 1),
           `N (%)` = paste0(N, " (", Percent, "%)")) %>%
    ## concatenating variables
    ungroup() %>%
    select(Variable, Category, `N (%)`)
  
  # summary for Continuous variables 
  continuous_vars <- c("time_lag", "age_qol")
  
  continuous_vars_names <- c("Time between YSR and QoL assessment", 
                             "Age at QoL assessment")
  
  continuous_summary <- data_full_raw %>%
    filter(FISNumber %in% PGS_indices) %>%
    select(all_of(continuous_vars)) %>%
    rename("Time between YSR and QoL assessment" = time_lag,
           "Age at QoL assessment" = age_qol) %>%
    summarise(across(everything(), ~ paste0(
      round(mean(. , na.rm=TRUE),1),
      " (", round(sd(., na.rm=TRUE),1), ")"))) %>%
    pivot_longer(cols = everything(), names_to = "Variable",
                 values_to = "M_SD") %>%
    mutate(Category = "") %>%   # continuous variables don't have categories
    rename(`N (%)` = M_SD)      # reuse the column for display
  
  # Combine categorical and continuous
  demographics_table <- bind_rows(
    categorical_summary %>% rename(Statistic = `N (%)`),
    continuous_summary %>% rename(Statistic = `N (%)`)
  ) %>%
    select(Variable, Category, Statistic)
  
  # APA-style flextable 
  ft <- flextable(demographics_table) %>%
    set_header_labels(Variable = "Variable",
                      Category = "Category",
                      Statistic = "N (%) or M (SD) in years") %>%
    autofit()
  
  ft
  
  # Optional: for APA style, make Variable only appear once per group
  demographics_table_apa <- demographics_table %>%
    group_by(Variable) %>%
    mutate(Variable = ifelse(row_number() == 1, Variable, "")) %>%
    ungroup() %>%
    rename("N (%) or M (SD) in years" = Statistic)
  
  # Export as LaTeX table
  latex_table <- kable(demographics_table_apa, 
                       format = "latex", 
                       booktabs = TRUE,
                       caption = "Demographic Descriptives",
                       align = c("l", "l", "c")) %>%
    kable_styling(latex_options = c("hold_position"))
  
  # Print LaTeX code to console
  latex_table
}

##----------------------------------------------------------------------------

## Demographic analysis: are the training and test set comparable in
## demographics

train_ids <- readRDS(here::here("data", "intermediate", "indices_train.rds"))
test_ids <- readRDS(here::here("data", "intermediate", "indices_test.rds"))

## train set
data_AD_train <- data_full_raw %>% 
  filter(FISNumber %in% train_ids)

## number of families 
length(unique(data_AD_train$FamilyNumber))
## 2,592 families

## extract descriptives about the covariates
table(data_AD_train$sex)
table(data_AD_train$sex) / nrow(data_AD_train)
table(data_AD_train$twzyg)
table(data_AD_train$twzyg) / nrow(data_AD_train)
table(data_AD_train$ea4fa_agg)
table(data_AD_train$ea4fa_agg) / nrow(data_AD_train)
table(data_AD_train$ea4mo_agg)
table(data_AD_train$ea4mo_agg) / nrow(data_AD_train)
table(data_AD_train$QoL_indicator)
table(data_AD_train$QoL_indicator) / nrow(data_AD_train)
table(data_AD_train$QoL_simple)
table(data_AD_train$QoL_simple) / nrow(data_AD_train)

mean(data_AD_train$age_qol)
sd(data_AD_train$age_qol)
range(data_AD_train$age_qol)

## descriptives for continuous variables
describe(data_AD_train %>% select(age_qol, time_lag))

## test set
data_AD_test <- data_full_raw %>% 
  filter(FISNumber %in% test_ids)

## number of families 
length(unique(data_AD_test$FamilyNumber))
## 658 families

## extract descriptives about the covariates
table(data_AD_test$sex)
table(data_AD_test$sex) / nrow(data_AD_test)
table(data_AD_test$twzyg)
table(data_AD_test$twzyg) / nrow(data_AD_test)
table(data_AD_test$ea4fa_agg)
table(data_AD_test$ea4fa_agg) / nrow(data_AD_test)
table(data_AD_test$ea4mo_agg)
table(data_AD_test$ea4mo_agg) / nrow(data_AD_test)
table(data_AD_test$QoL_indicator)
table(data_AD_test$QoL_indicator) / nrow(data_AD_test)
table(data_AD_test$QoL_simple)
table(data_AD_test$QoL_simple) / nrow(data_AD_test)

mean(data_AD_test$age_qol)
sd(data_AD_test$age_qol)
range(data_AD_test$age_qol)

## descriptives for continuous variables
describe(data_AD_test %>% select(age_qol, time_lag))


## Kolmogorov-Smirnov test: 
## Distribution of zygosity not significantly different?
ks.test(data_AD_train_fasano$twzyg,
        data_AD_test_fasano$twzyg)

## Not significant


## maybe run this on server?

###############################################################################

## 2) participants with genetic data available (models B, C, E)

## loading in covariate names
covariates_names <- readRDS(
  here::here("data", "intermediate", "names_covariates.rds"))
covariates_names

## loading in vector with names genetic covariates
gen_covariates <- readRDS(
  here::here("data", "intermediate", "names_genetic_covariates.rds"))

gen_covariates <- c(gen_covariates, "EUR_1KG_Outlier", "NL_Strict_Outlier")

data_model_B_base <- readRDS(
  here::here("data", "intermediate", "PGS", "data_PGS_model_B.rds"))

data_full_raw_B <- data_full_raw %>%
  filter(FISNumber %in% unique(data_model_B_base$FISNumber))

table(data_full_raw_B$sex)
table(data_full_raw_B$sex) / nrow(data_full_raw_B)
table(data_full_raw_B$twzyg)
table(data_full_raw_B$twzyg) / nrow(data_full_raw_B)
table(data_full_raw_B$ea4fa_agg)
table(data_full_raw_B$ea4fa_agg) / nrow(data_full_raw_B)
table(data_full_raw_B$ea4mo_agg)
table(data_full_raw_B$ea4mo_agg) / nrow(data_full_raw_B)
table(data_full_raw_B$QoL_indicator)
table(data_full_raw_B$QoL_indicator) / nrow(data_full_raw_B)
table(data_full_raw_B$QoL_simple)
table(data_full_raw_B$QoL_simple) / nrow(data_full_raw_B)
table(data_model_B_base$NL_Strict_Outlier)
table(data_model_B_base$NL_Strict_Outlier) / nrow(data_full_raw_B)
table(data_model_B_base$EUR_1KG_Outlier)
table(data_model_B_base$EUR_1KG_Outlier) / nrow(data_full_raw_B)

mean(data_full_raw_B$age_qol)
sd(data_full_raw_B$age_qol)
range(data_full_raw_B$age_qol)

## descriptives for continuous variables
describe(data_full_raw_B %>% select(age_qol, time_lag))


## train / test split

train_ids_PGS <- readRDS(
  here::here("data", "intermediate", "indices_train_PGS.rds"))
test_ids_PGS <- readRDS(
  here::here("data", "intermediate", "indices_test_PGS.rds"))

## train set
data_BCE_train <- data_full_raw %>% 
  filter(FISNumber %in% train_ids_PGS)

## number of families 
length(unique(data_BCE_train$FamilyNumber))
## 2,592 families

## extract descriptives about the covariates
table(data_BCE_train$sex)
table(data_BCE_train$sex) / nrow(data_BCE_train)
table(data_BCE_train$twzyg)
table(data_BCE_train$twzyg) / nrow(data_BCE_train)
table(data_BCE_train$ea4fa_agg)
table(data_BCE_train$ea4fa_agg) / nrow(data_BCE_train)
table(data_BCE_train$ea4mo_agg)
table(data_BCE_train$ea4mo_agg) / nrow(data_BCE_train)
table(data_BCE_train$QoL_indicator)
table(data_BCE_train$QoL_indicator) / nrow(data_BCE_train)
table(data_BCE_train$QoL_simple)
table(data_BCE_train$QoL_simple) / nrow(data_BCE_train)

mean(data_BCE_train$age_qol)
sd(data_BCE_train$age_qol)
range(data_BCE_train$age_qol)

## descriptives for continuous variables
describe(data_BCE_train %>% select(age_qol, time_lag))

## test set
data_BCE_test <- data_full_raw %>% 
  filter(FISNumber %in% test_ids)

## number of families 
length(unique(data_BCE_test$FamilyNumber))
## 658 families

## extract descriptives about the covariates
table(data_BCE_test$sex)
table(data_BCE_test$sex) / nrow(data_BCE_test)
table(data_BCE_test$twzyg)
table(data_BCE_test$twzyg) / nrow(data_BCE_test)
table(data_BCE_test$ea4fa_agg)
table(data_BCE_test$ea4fa_agg) / nrow(data_BCE_test)
table(data_BCE_test$ea4mo_agg)
table(data_BCE_test$ea4mo_agg) / nrow(data_BCE_test)
table(data_BCE_test$QoL_indicator)
table(data_BCE_test$QoL_indicator) / nrow(data_BCE_test)
table(data_BCE_test$QoL_simple)
table(data_BCE_test$QoL_simple) / nrow(data_BCE_test)

mean(data_BCE_test$age_qol)
sd(data_BCE_test$age_qol)
range(data_BCE_test$age_qol)

## descriptives for continuous variables
describe(data_BCE_test %>% select(age_qol, time_lag))


## Kolmogorov-Smirnov test: 
## Distribution of zygosity not significantly different?
ks.test(data_BCE_train_fasano$twzyg,
        data_BCE_test_fasano$twzyg)

## significant! There are 10% more female MZ twins in the training than 
## in test sample

###############################################################################


## manually creating nice table with demographics

var_dem <- c("Feature Set", "N total", "N(male)", "Proportion male",
             "N(female)", "Proportion female", "Mean age QoL",
             "SD age QoL", "Range age QoL",
             "N (non-NL genome)", "N (non-EUR genome)")


table_dem <- as.data.frame(t(rbind(
  c("Full CBCL data", "5,087", "1,857", "36.5%", "3,229", "63.5%",
    "19.77", "2.26", "18.0 - 32.0", "-", "-"),
  c("Full CBCL data + PGS", "2,656", "926", "34.9%", "1,730", "65.1%",
     "19.74", "2.26", "18.0 - 32.0", "133", "94")
)))

names(table_dem) <- var_dem

print(nice_table(table_dem), preview = "docx")


## CBCL table for Latex
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx"))

CBCL_items_table_apa <- CBCL_items_table %>%
  recode_na_as(value = "(-)")



ft_CBCL <- flextable(CBCL_items_table_apa) %>%
  set_header_labels(question_number = "Label (Analysis)",
                    t_per_question = "Times asked in CBCL",
                    Label = "Content item") %>%
  autofit()

ft_CBCL

# Export as LaTeX table (Needs to be done in two tables)
latex_table_CBCL_1 <- kable(CBCL_items_table_apa %>%
                          select(-Label, -t_per_question) %>%
                            rename("Label (Analysis)" = question_number), 
                          format = "latex", 
                          booktabs = TRUE,
                          caption = "CBCL items and analysis labels",
                          align = rep(
                            "l", (ncol(CBCL_items_table_apa) - 2))) %>%
  kable_styling(latex_options = c("hold_position"))

latex_table_CBCL_1

CBCL_items_table_apa <- cbind(CBCL_items_table_apa[,(8:10)],
                              CBCL_items_table_apa[,(1:7)])

latex_table_CBCL_2 <- kable(CBCL_items_table_apa %>%
                              select(Label, question_number ,t_per_question) %>%
                              rename("Label (Analysis)" = question_number,
                                     "Times asked in CBCL" = t_per_question,
                                     "Content item" = Label), 
                            format = "latex", 
                            booktabs = TRUE,
                            caption = "Content of CBCL items",
                            align = rep("c", 3)) %>%
  kable_styling(latex_options = c("hold_position"))

latex_table_CBCL_2

# eoS
