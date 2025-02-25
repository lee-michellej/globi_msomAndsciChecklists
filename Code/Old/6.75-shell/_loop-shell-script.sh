#!/bin/bash

# Define an array of model names
models=("no_bee_plant" "bee_species" "plant_species" "bee_family" "plant_family" "bee_plant_family")

# Define model run type: either test or full
model_run_type="test"

# Loop through each model name
for model in "${models[@]}"; do
    echo "Running model: $model"

    # Run the R script and pass the model name as an argument
    Rscript ./Multi-sp-occ-mod-vector-NIMBLE-2025-01-07.R "$model" "$model_run_type"

    echo "Finished running model: $model"
done

echo "All models have been processed."