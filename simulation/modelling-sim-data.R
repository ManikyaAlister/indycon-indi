library(here)
library(tidyverse)
library(brms)
library(parallel)  
library(doParallel)

load(here("simulation/generating_data.Rdata"))
participants <- unique(generating_data$uid)
n <- length(participants)

# Baseline (actual participants, 12 trials)

# Determine number of cores
num_cores <- detectCores() - 2

# Create cluster
cl <- makeCluster(num_cores) 

# Register cluster
registerDoParallel(cl)

# Specify the different data sets to loop over
all_trials_per_cell <- c(3, 6, 9, 12, 16, 20)
n_cells <- 4

for (j in 1:length(all_trials_per_cell)){
  d_trials <- all_trials_per_cell[j]
  n_trials <- d_trials*n_cells
  

start <- 1

  
  
foreach (i = start:n, .packages=c('brms', 'tidyverse', 'here')) %dopar% {
  p <- participants[i]
  
  print(paste0("Starting participant ", p, " ", j, " trials"))
  
  load(here(paste0("simulation/data/p",i,"-",n_trials,"-trials.Rdata")))
  
  d <- sim_data
  
  n_trials <- length(d$update)
  
  p_type <-  unique(d$type)
  
  
  null <- brm(update~sideA, data = d)
  sum_null <- summary(null)
  loo_null <- loo(null)
  
  alt <- brm(update~sideA+nSources_A, data = d)
  sum_alt <- summary(alt)
  loo_alt <- loo(alt)
  
  save(p_type, p, d, null, sum_null, loo_null, alt, sum_alt, loo_alt, file = here(paste0("simulation/output/P",i,"-output-",n_trials,"-trials-simdata.Rdata")))
  print(paste0("P ", p, " ", j, " trials done"))
}
print(paste0("dataset ",j, " out of ", length(all_trials_per_cell)))
}