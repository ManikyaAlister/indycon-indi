#!/bin/bash
# to run, type into the terminal: sh analyses/01_models/run-indi-2.sh

# Loop through all 78 participants and fit the alternative model to each participant. 
for i in {1..1}; do
    echo "Processing alternative model for participant $i"
    Rscript analyses/01_models/indi_2.R $i  # run model for each participant. 
done

echo "All participants processed."