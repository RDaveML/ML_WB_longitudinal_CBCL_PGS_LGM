## Report script for R packages used

library(dplyr)
library(MASS)
library(lcmm)
library(lme4)
library(MplusAutomation)
library(caret)
library(glmnet)
library(shapviz)

report::report_packages()

packages <- c("dplyr", "MASS", "lcmm", "lme4", "MplusAutomation", 
              "caret", "glmnet", "shapviz")

cita1 <- citation(package = "dplyr")
toBibtex(cita1)

for (pack in packages) {
  print(pack)
  print(toBibtex(citation(package = pack)))
}