#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --partition=genoa
#SBATCH --time=5:00:00 ## node time 

## this is a bash script to run the
## R file where B = 100 bootstrapped ML models are run on dataset
## E (raw CBCL items + longitudinal variables + PGS + covariates),
## R file: 23_model_E_run_bootstrap_stability.R
## SBATCH options;
## one task, 96 CPUs per task, node time of 5:00:00 
## (all runs exceeding this will be aborted!)

## note: This script will be exectued 100 times in parallel on multiple
## nodes by executing the script bash_model_E_bootstrap.sh via the 
## console interface of the cluster

## changing to home directory
cd $HOME

## printing resource allocation details
echo "Job ID: $SLURM_JOB_ID"
echo "Node list: $SLURM_NODELIST"
echo "Number of tasks: $SLURM_NTASKS"
echo "CPUs per task: $SLURM_CPUS_PER_TASK"
echo "Number of nodes: $SLURM_JOB_NUM_NODES"
echo "Allocated partition: $SLURM_JOB_PARTITION"
echo "Submit host: $SLURM_SUBMIT_HOST"

## loading R module
module purge && module load 2024 R/4.4.2-gfbf-2024a


##  copy all the files and subdirectories that are needed for the job to run
## -r does that given all files are located 
## Only the objects that are called in the script 23_model_E_run_bootstrap_stability.R
## need to be copied relevant files will be copied into directory on SNELLIUS
TMPDIR=${TMPDIR:-/tmp}
echo "Using TMPDIR: $TMPDIR"

# Define the working directory name
WORKDIR_NAME="ML_WB_longitudinal_CBCL_PGS_LGM"
SOURCE_DIR="$HOME/$WORKDIR_NAME"
TMP_WORKDIR="$TMPDIR/$WORKDIR_NAME"
DEST_DIR="$SOURCE_DIR/data/intermediate/bootstrap"

## creating DEST_DIR only if it does not exist
if [ ! -d "$DEST_DIR" ]; then
    mkdir -p "$DEST_DIR"
    echo "Created directory: $DEST_DIR"
fi

# Copy the working directory into TMPDIR
cp -r "$SOURCE_DIR" "$TMPDIR/"

# Change to that copied directory
cd "$TMP_WORKDIR" || { echo "Failed to cd to $TMP_WORKDIR"; exit 1; }
echo "Now in working directory: $(pwd)"

echo "Slurm array ID: $SLURM_ARRAY_TASK_ID"

Rscript scripts/23_model_E_run_bootstrap_stability.R $SLURM_ARRAY_TASK_ID 


cd "$SOURCE_DIR"

shopt -s nullglob
files=("$TMP_WORKDIR/data/intermediate/bootstrap/"*.rds)
if [ ${#files[@]} -gt 0 ]; then
    echo "Copying .rds files back to $DEST_DIR"
    cp "${files[@]}" "$DEST_DIR"
else
    echo "No .rds files found to copy."
fi

echo "Run ended"