# HEADER --------------------------------------------
#
# Author: Dave Leitritz (RDaveML)
# Year, 2024
# Email:  d.m.leitritz@vu.nl
#   
# Date: 2024-08-15
#
# Script Name: 07_LGM.R
#
# Script Description: In this script, the filtered CBCL variables 
# (see script 04_data_cleaning_filtering1.R) from the project:
# Combining longitudinal change features of childhood psychopathology 
# with Polygenic scores in machine learning models of adult wellbeing
# will be used for latent growth modeling (LGM) to create 
# features carrying longitudinal information about childhood psychopathology
# these features will later be used in the ML predictor space for the 
# prediction of adult wellbeing (Qualitý of life)
#
#
# Notes: This steps needs to happen after the splitting of the dataset
# in training and test data
#
#

# Set options
cat("SETTING OPTIONS... /n/n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "MplusAutomation")


## setting working directory
setwd(here::here())

## loading in necessary dataset and vectors / tables of variables

## loading in training set
load(here::here("data", "intermediate", "train_data.RData"))

## loading in refined variable table (with labels and description of CBCL items)
CBCL_items_table <- read_excel(here::here("doc", "CBCL_table_t_per_item.xlsx")) %>%
  as.data.frame()

## loading in vectors of variable names for filtering and selecting
## those were created in the script 02_data_exploration.R
load(here::here("scripts", "variable_vectors.RData"))

## loading in list of CBCL items per question
load(here::here("scripts", "CBCL_questions_list.RData"))

## loading in CBCL items to be retained and to be dropped
load(here::here("scripts", "CBCL_items_keep"))
load(here::here("scripts", "CBCL_items_drop"))

#------------------------------------------------------------------------------

## Important step before actual analysis: For trial calculations, permute 
## IDs so one remains blind for data
permute <- TRUE
if(permute){
  train_data <- transform(train_data, FISNumber = sample(FISNumber))
}

#------------------------------------------------------------------------------



## First task: Adjust the variable vectors, some of the variables are not 
## in the dataset anymore! 
CBCL_items_LGM <- intersect(CBCL_items_keep, CBCL_items_vec)
YSR_items_LGM <- intersect(CBCL_items_keep, YSR_items_vec)



## Second task: Which of the CBCL longitudinal questions are still eligible for
## longitudinal modeling? Some of the items were eliminated during data cleaning
## In order to do longitudinal modeling, there need to be at least 4 measuring
## points per CBCL question

## counting how many items per question are still available, exclude any 
## CBCL questions with less than 4 measures still available
CBCL_items_table <- CBCL_items_table %>%
  rowwise() %>%
  mutate(n_items_available = sum(
    sapply(across(everything()), function(x) as.character(x) %in% CBCL_items_keep)
  )) %>%
  ungroup() %>%
  filter(n_items_available >= 4)
## 80 questions might still be used for latent growth modeling
## (If IQR = 0 columns are not removed)

## shrinking down: table to contain only the variable names and the question
## names
CBCL_items_table_reduced <- CBCL_items_table %>%
  select(starts_with("Age"), question_number)


## Checking structure of missing:
# colMeans(is.na(data_LGM)) %>% as.data.frame() %>% View()

## calculate per question: How many measures does a participant have 
## for this specific question?
n_m_df <- data.frame()
step <- 1
for(question in unique(CBCL_items_table_reduced$question_number)){
  items_question <- CBCL_items_table_reduced %>%
    filter(question_number == question) %>%
    select(-question_number) %>%
    as.character() %>%
    na.omit()
  
  data_question <- train_data %>%
    select(FISNumber, all_of(items_question)) %>%
    # rowwise() %>%
    mutate(n_measures = rowSums(!is.na(.)) - 1) %>%
    select(FISNumber, n_measures)
  
  q_col <- paste0("n_measures_", question)
  
  colnames(data_question) <- c("FISNumber", q_col)
  
  #if(step == 1){
  #n_m_df <- data_question
  #} else {
  #  n_m_df <- n_m_df %>%
  #    left_join(data_question, by = "FISNumber")
  #}
  #step <- step + 1
  train_data <- train_data %>%
    left_join(data_question, by = "FISNumber")
  
}

## Next: Binding this to data


## Now: With one item test it out: Pivot data, calculate all you 
## need for LGM, make the entire Mplus stuff for one item and save output
## write down what the function that does the job needs for arguments
## then lapply or sapply it over the items and then bind together all
## the resulting dfs


## Important: before any analyses, permute Family IDs and FISNumbers so
## you remain blind to the actual content of the data! 



## sampling test Question
test_q <- na.omit(as.character(sample_n(CBCL_items_table_reduced, 1)))

# question name
test_q_name <- test_q[length(test_q)]

test_df <- train_data %>%
  select(FISNumber, FamilyNumber, twzyg, any_of(test_q), contains(test_q))


## Add number of family members for each participant
count_fam <- test_df[,c("FamilyNumber","FISNumber")] %>%
  count(FamilyNumber) %>%
  rename(fam_count = "n")

test_df <- merge(test_df, count_fam, by = "FamilyNumber")

##----------------------------------------------------------------------------

## LGM part: Bottom-up from simpler to more complex models



## Leave out this part for now, long pivoting and rater coding comes later,
## family mean can be omitted from analysis


rater <- FALSE

if(rater) { ## initiating parenthesis rater coding

## Now: In order to take into account Time AND Rater change, 
## data needs to be pivoted to long format
# make data long
# note: order of variable names in "varying" is important
long_df <- reshape(test_df, direction = "long", 
                   varying = test_q[1:length(test_q) - 1], 
                   timevar = "time",
                   times = c(1:(length(test_q) - 1)),
                   v.names = c(test_q[length(test_q)]),
                   idvar=c("FISNumber")) %>%
  mutate(item_name = NA) ## initialize item_name column

## Code rater in there as well (other-rating vs. self-rating)
## All ysr items need to be coded as rater - self 
long_df$item_name <- sapply(long_df$time, function(x) {
  test_q[x]  # Directly use the value of 'time' to index into 'test_q'
})

long_df <- long_df %>%
  mutate(rater = ifelse(grepl("ysr", item_name), 1, 0))



## next: calculate family means and deviation (if single family member, 
## take deviation from grand mean)

long_df <- long_df[order(long_df$FISNumber),]      # order on person id
long_df <- long_df[order(long_df$FamilyNumber),]   # order on fam id

## calculate person mean
long_df <- long_df %>%
  group_by(FISNumber) %>%
  mutate(mean_CBCL_question_ind = mean(!!sym(test_q_name), na.rm = TRUE)) %>%
  ungroup()

long_df <- long_df %>%
  group_by(FamilyNumber) %>%
  mutate(m_fam = mean(mean_CBCL_question_ind, na.rm = TRUE)) %>%
  ungroup

## calculate mean of family means (for participants who don't have
## family members in the sample)
fam_means <- long_df %>%
  group_by(FamilyNumber) %>%
  summarise(mean_fam_mean = mean(m_fam, na.rm = TRUE), n = n())


## calculation of dependent variable! Individual deviation from family mean
## Or grand family mean median (If no siblings)

long_df <- long_df %>%
  mutate(DV_LGM = case_when(
    fam_count == 1 ~ !!sym(test_q_name) - median(fam_means$mean_fam_mean),
    TRUE ~ !!sym(test_q_name) - m_fam),
         FISNumber = as.character(FISNumber)) %>%
  rename(FIS_NR = "FISNumber")
## renaming because variables in Mplus are only allowed to have max 8 characters


## This is the long_df that is needed for the Longitudinal modeling! 

## saving long_df for separate inspection
save(long_df, file = here::here("data", "intermediate", "long_df_test.RData"))


} ## closing parenthesis rater coding

## Next step: Give this to the MplusAutomation syntax

## base model: latent growth model (1 group) with latent slope and intercept

## reminder: after fully working model has been programmed for one question
## this needs to be looped over all questions, aliasing the columns names 
## and question names

working_full <- FALSE
if(working_full){
  print("Looping over question names with creating of vectors for var names")
}

## renaming the variables that are the longitudinal measures
## getting variable names (starting with q)
var_t <- grep("^q", colnames(test_df), value = TRUE, invert = TRUE)

## renaming variable simply with t1 - tmax (note that later still needs to 
## be adjusted to the presumed ages!)
colnames(test_df)[colnames(test_df) != var_t] <- paste0("t", 1:length(var_t))
colnames(test_df)
## again saving vector of longitudinal var names
var_t <- grep("^t[0-9]", colnames(test_df), value = TRUE)


model_base <- mplusObject(
  VARIABLE =
  "usevar = t1-t5;
   categorical = t1-t5;",
  ANALYSIS = 
  "estimator = ML;",
  MODEL = 
  "i s | t1@0 t2@3 t3* t4* t5*;",
  OUTPUT = "sampstat standardized;",
  usevariables = colnames(test_df), # alternative tech1 tech8;
  rdata = test_df
)

fit_base <- mplusModeler(model_base,
                         dataout = here("mplus_files", "model_base.dat"),
                         # note: data needs to be given to model! 
                         # only solution seems to be to directly delete it 
                         # afterwards!
                         modelout = here("mplus_files", "model_base.inp"),
                         check = TRUE, run = TRUE, hashfilename = FALSE,
                         Mplus_command = "C:/Program Files/Mplus/Mplus.exe")


## This created a working model! The data are also correct!

## removing the .dat file to save memory and not confuse Mplus for the next
## model! 
unlink(list.files(here("mplus_files"), pattern = "\\.dat$", full.names = TRUE))

old_model <- FALSE
if(old_model){
  model_old <- mplusObject(
    
    VARIABLE = 	"usevar = FIS_NR time DV_LGM rater m_fam twzyg;
               CLASSES = c(2);
               cluster = FIS_NR twzyg;",
    ANALYSIS = "type = twolevel mixture complex;
                 starts = 100 20;",
    ## note: the starts argument here specifies that 100 initial stage random
    ## sets of starting values are used and 20 final stage optimizations are
    ## carried out
    MODEL = "%WITHIN% 
  %OVERALL%
  iw sw | DV_LGM; ! intercept and slope are defined by the dependent variable
  iw sw ON time; ! instead of wide data, in long data, time is covariate
  iw sw ON rater; ! rater has an effect on the intercept and slope because we assume differences other vs. self-rating
  c ON m_fam; ! on within-level, only family mean has influence on group-membership
  ! DV_LGM ON time rater; (Does this need to be specified explicitly? Or is it enough to mention that the intercept and slope are influenced?)
  %BETWEEN%
  %OVERALL%
  DV_LGM ON m_fam; ! is m_fam effective on within or between level or both?
  ib sb | DV_LGM; ! definition of ib and sb, what exactly is this here?
  ib sb ON time; ! also dependent on time?
  c#1 ON m_fam; ! what influences the latent class variable on between level?
  !ib sb ON twzyg; ! unclear where twin status is effective at all, only influencing standard errors?
  ! This causes error because twzyg is not found, unclear why, it was given to 
  ! the variable names at all previous steps
  sb@0; ! residual variance of slope growth factor fixed at 0? 
  c#1*1; ! unclear what this is exactly
  %c#1%
  [ib sb]; ! starting values of mean of intercept and slope = 0 in class 1?
  %c#2%
  [ib*3 sb*1]; ! starting values of mean of intercept and slope = [3;1] in class 2?",
    rdata = long_df,
    OUTPUT = "standardized tech1 tech8;"
  )
}


## next step: updating model making it a mixture with 2 latent classes!

model_mixture0 <- update(
  model_base, 
  VARIABLE = 	~ "usevar = t1-t5;
               CLASSES = c(2);
               ! categorical = t1-t5;",
  ANALYSIS = ~"type = mixture;
                 starts = 100 20;",
  MODEL = 
  ~"%overall% 
  ! this is still very unclear! change once read more about GMM
  b0 by t1@1 t2@1 t3@1 t4@1 t5@1;
  b1 by t1@0 t2@1 t3@2 t4@3 t5@4;
  [t1@0 t2@0 t3@0 t4@0 t5@0]; 
  t1* t2* t3* t4* t5*
  b0*1;
  b1*.2;
  b0 with b1@0;
  %c#1%
  [b0*1 b1*.1];
  %c#2%
  [b0*5 b1*.1];"
)
## note that the starting values are totally arbitrary, more material will be 
## helpful, now the only priority is that the model will run, 
## regardless how bad it fits, it will be adjusted in the next step

fit_mixture0 <- mplusModeler(model_mixture0,
                             dataout = here("mplus_files", "model_mixture0.dat"),
                             modelout = here("mplus_files", "model_mixture0.inp"),
                             check = TRUE, run = TRUE, hashfilename = FALSE,
                             Mplus_command = "C:/Program Files/Mplus/Mplus.exe")

## It worked! 
## The model fit even got better! 
## so definitely, a simple 2 class model is already better than a model that
## assumes one homogenous population for this question

## CONTINUE HERE!!!


## example code for the update of models! always needs the tilde to 
## update what R thinks is a formula! 
example1 <- mplusObject(MODEL = "mpg ON wt;",
                        usevariables = c("mpg", "hp"), rdata = mtcars)
x <- ~ "ESTIMATOR = ML;"
example1 <- update(example1, ANALYSIS = ~ "ESTIMATOR = ML;")
str(update(example1, ANALYSIS = x))




