#!/bin/bash

# Define an array of model names
models=("no_bee_plant" "bee_species" "plant_species" "bee_family" "plant_family" "bee_plant_family")

# Define model run type: either test or full
model_run_type="full"

# Loop through each model name
for model in "${models[@]}"; do
    echo "Submitting job for model: $model with run type: $model_run_type"

    # Submit the job with a unique job name and pass environment variables
    sbatch --job-name="globi-${model}" --export=MODEL_NAME="$model",MODEL_RUN_TYPE="$model_run_type" ./Code/globi-job.sh

    echo "Job submitted for model: $model"
done

echo "All jobs have been submitted."