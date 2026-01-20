#!/bin/bash
#SBATCH --job-name="globi-${MODEL_NAME}"
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=3
#SBATCH --cpus-per-task=1
#SBATCH --hint=nomultithread
#SBATCH --time=48:00:00
#SBATCH --mem=132gb
#SBATCH --partition=cpu
#SBATCH --account=coop
#SBATCH --output=/home/gdirenzo/globi/log/Model_%x_${MODEL_NAME}.output.txt
#SBATCH --error=/home/gdirenzo/globi/log/Model_%x_${MODEL_NAME}.error.txt

# Load R module
module load cray-R/4.2.1.2

# Print job details
echo "Job started on $(hostname) at $(date)"
echo "Running model: $MODEL_NAME with run type: $MODEL_RUN_TYPE and priors $PRIORS"

# Run the R script with the model name and run type
Rscript --vanilla /home/gdirenzo/globi/Code/Multi-sp-occ-mod-vector-NIMBLE-2025-07-02.R "$MODEL_NAME" "$MODEL_RUN_TYPE" "$PRIORS"

echo "Job ended at $(date)"