#!/bin/bash
# Thiss script loops through each data set and participant, calling the simulation script for each one. 
cores=4
for dataset in 6; do #9 12 16 20; do
  for participant in {1..1}; do
    Rscript power-simulation/03_modelling-sim-data-adjusted.R $dataset $participant $cores
  done
done