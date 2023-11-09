library(here)
library(tidyverse)
# simulate data
load(here("simulation/generating_data.Rdata"))
participants <- unique(generating_data$uid)
n <- length(participants)

all_trials_per_cell <- c(3, 6, 9, 12, 16, 20)
max_trials_per_cell <- 20
n_cells <- 4

# generate the maximum number of trials per cell
for (i in 1:n){
  p <- participants[i]
  p_gen_data <- generating_data %>%
    filter(uid == p)
  all_sim_data <- NULL
  for (j in 1:n_cells){
    m = as.numeric(p_gen_data[j, "mean"])
    sd = as.numeric(p_gen_data[j, "sd"])
    if (is.na(sd)) {
      sd = 10
    }
    sideA = rep(as.character(p_gen_data[j, "sideA"]), max_trials_per_cell)
    nSources_A = rep(as.character(p_gen_data[j, "nSources_A"]), max_trials_per_cell)
    type <- rep(as.character(p_gen_data[j, "p_type"]), max_trials_per_cell)
    update <- rnorm(max_trials_per_cell, m, sd)
    d_cell <- cbind(p, sideA, nSources_A, update, type)
    all_sim_data <- rbind(all_sim_data, d_cell)
  }
  # take just the trials in that n_trials condition for each cell
  for (n_trials_per_cell in all_trials_per_cell) {
    sim_data <- as.data.frame(all_sim_data) %>%
      group_by(sideA, nSources_A) %>%
      slice_head(n = n_trials_per_cell)
    n_trials <- n_trials_per_cell * n_cells
    sim_data <- as.data.frame(sim_data)
    sim_data$update = as.numeric(sim_data$update)
    save(sim_data, file = here(paste0("simulation/data/p",i,"-",n_trials,"-trials.Rdata")))
  }

}