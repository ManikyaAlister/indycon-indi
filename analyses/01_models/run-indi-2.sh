#!/bin/bash

# to run, type into the terminal: sh analyses/01_models/run-indi-1.sh

# Loop through all 78 participants and fit the null model to each participant. 
for i in {1..78}; do
    echo "Processing null model for participant $i"
    Rscript analyses/01_models/indi_2.R $i # run model for each participant, defining the number of cores
done

echo "All participants processed."