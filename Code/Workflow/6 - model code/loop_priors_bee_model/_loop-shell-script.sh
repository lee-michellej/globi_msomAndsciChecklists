#!/bin/bash

# Define an array of model names
models=("bee_species")

# Define model run type: either test or full
model_run_type="full"

# Define the set of priors
priors=("1" "2" "3" "4")

# Loop through each model name
for priors in "${priors[@]}"; do
    echo "Submitting job for model: $models with run type: $model_run_type with priors $priors" 

    # Submit the job with a unique job name and pass environment variables
    sbatch --job-name="globi-${model}-p${priors}" \
         --export=MODEL_NAME="$models",MODEL_RUN_TYPE="$model_run_type",PRIORS="$priors" /home/gdirenzo/globi/Code/globi-job.sh

    echo "Job submitted for model: $models with priors $priors"
done

echo "All jobs have been submitted."