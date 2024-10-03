## Test file Latent growth modeling 


# Set options
cat("SETTING OPTIONS... /n/n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "MplusAutomation")


#setwd("C:/Users/qrq337/OneDrive - Vrije Universiteit Amsterdam/Documents/VU/PhD_Machine_Learning_x_Well-being/01_PROJECTS/Project01_longitudinal_machine_learning/analysis/ML_WB_longitudinal_CBCL_PGS_LGM")

## loading in test data frame
load(here::here("data", "intermediate", "long_df_test.RData"))

## This dataframe contains the data of a time series in long format 

## Because the variable "rater" which is dependent on the specific
## timepoint needed to be coded in there as well

## the variable "DV_LGM" is the dependent variable for the Latent 
## growth modeling (in the wide data, it would have been y1-y5)

## the variable m_fam is the family mean of the original longitudinal variable
## which is a necessary covariate

## twzyg gives the twin status (all participants are twins but no the same type
## of twins)

## rater codes if variable was other-report (0) or self-report (1)

## time gives the timepoint of the measurement

## FISNumber is the personal ID variable

## family number is the ID of the family an individual belongs to

#------------------------------------------------------------------------------

## Coding up Mplus model
## estimate a simple LGM model with 2 groups taking into account the 
## relevant covariates! 

## creating input and output files
file.create("result_test01.inp")

file.create("result_test01.out")

model_test1 <- mplusObject(
  TITLE = "Mixture model one CBCL sample question and several covariates",
  DATA = NULL,
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

## Issue now: always own output data file created instead of prespecified
## Unclear how this behaviour can be turned off
## solution: instantly deleting created .dat file to save storage

result_test1 <- mplusModeler(model_test1,
                             modelout = "result_test01.inp",
                             run = 1L,
                             writeData = "ifmissing",
                             hashfilename = FALSE,
                             Mplus_command = "C:/Program Files/Mplus/Mplus.exe")


## detecting created datafile and deleting it
#file.remove(grep("*.dat", list.files(), value = TRUE))
unlink(grep("*.dat", list.files(), value = TRUE))

## Warning message thrown: running command had status 1


## Following error still in model output: 
## *** ERROR in MODEL command
## The number of fixed time scores is not sufficient for model identification
## in the following growth process:   IW SW

