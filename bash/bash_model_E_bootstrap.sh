#!/bin/bash
#SBATCH --partition=genoa
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=d.m.leitritz@vu.nl


## This script runs the bash script that runs the 
## R script multiple times distributed over multiple nodes
## (adapt directory name if different name in own working environment!)
## script should be run from home directory with 
## bash bash_model_E_bootstrap.sh

## Note: Depending on operation system, script might need to be converted to unix using dos2unix scripts/bash_model_E_bootstrap.sh
## and dos2unix scripts/bash_model_E_run.sh

## changing to bash directory to execute scripts in there
cd "$HOME"/ML_WB_longitudinal_CBCL_PGS_LGM/bash

## 101 runs: 1 original, B = 100 bootstrapped for stability assessment
sbatch -a 1-101 bash_model_E_run.sh
