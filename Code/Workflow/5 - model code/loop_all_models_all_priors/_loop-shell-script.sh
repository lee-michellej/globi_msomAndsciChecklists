#!/bin/bash

# Define an array of model names
models=("no_bee_plant" "bee_species" "plant_species" "bee_family" "plant_family" "bee_plant_family")

# Define model run type: either test or full
model_run_type="full"

# Define the set of priors
priors=("1" "2" "3" "4" "5" "6")

# Loop through each model name
for model in "${models[@]}"; do

    for prior in "${priors[@]}"; do

    echo "Submitting job for model: $model with run type: $model_run_type with priors $prior" 

    # Submit the job with a unique job name and pass environment variables
        sbatch --job-name="globi-${model}-p${prior}" \
            --export=MODEL_NAME="$model",MODEL_RUN_TYPE="$model_run_type",PRIORS="$prior" \
            "./Code/loop_all_models_all_priors/globi-job.sh"

    echo "Job submitted for model: $model with priors $prior"

    done

done

echo "All jobs have been submitted."