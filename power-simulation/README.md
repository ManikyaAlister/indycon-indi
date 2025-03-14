** Open 05_power-simulation.pdf for the full write-up of the power simulation. **

- `run-sims-local.sh`: Shell script that will run all of the simulations serially and locally. Note that this may take a long time. 
- `01_get-generating-params`: Get generating parameters for the simulation by getting effect sizes from a previous data set with a similar design. 
- `02_simulate-data`: Simulate data from generating parameters with varying trial numbers and different participants with different effect sizes. 
- `03_modelling-sim-data-adjusted`: Fit the models to the simulated data to see whether independence effects could be recovered.
- `03.1_job-array.txt`: Job array for running multiple simulations (03_modelling-sim-data-adjusted) in parallel on a cluster (HPC). 
- `03.2_run-sims-local.sh`: Shell script for running 03_modelling-sim-data-adjusted serially for every participant and data set **on a local computer**.
- `04_combine-output`: Combine the output from multiple data sets and models. 
- `05_power-simulation`: Fully written up analysis of the power simulation. Jump to here if you are just interested in reading the results. 