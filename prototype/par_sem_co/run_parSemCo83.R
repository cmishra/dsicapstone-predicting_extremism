#!/bin/bash



#SBATCH --nodes=1

#SBATCH --time=01:00:00

#SBATCH --partition=serial

#SBATCH --mem=5000

#SBATCH --account=dsOne



module load R/openmpi/3.1.1

R --no-save --quiet < './prototype/script_parSemCo83.R'

