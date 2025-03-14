#!/bin/bash
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --partition=genoa
#SBATCH --time=12:30:00 ## node time 

module purge && module load 2024 R/4.4.2-fgbg-2024a



cp -r "$HOME"/PhD_VU/p01_CBCL_PGS_LGM "$TMPDIR" 
## Here, I need to copy all the files and subdirectories that are needed for the job to run -r does that given all files are located 
## Be careful: Only copy to tmp what you need, the useless nonsense from the Mplus folder and other bulk 
## is not needed, you should make a list of files that you need and write in the bash script that you need to dismiss the rest
## to use a lot less memory! After all, it also needs to be copied to and deleted from the tmp directories
cd "$TMPDIR"/PhD_VU/p01_CBCL_PGS_LGM

echo $SLURM_ARRAY_TASK_ID

Rscript scripts/08a_ML_model_A.R $SLURM_ARRAY_TASK_ID

cd "$HOME/PhD_VU/p01_CBCL_PGS_LGM"

cp -r "$TMPDIR"/PhD_VU/p01_CBCL_PGS_LGM/*.RDS "$HOME"/PhD_VU/p01_CBCL_PGS_LGM