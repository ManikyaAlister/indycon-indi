
library(here)
library(tidyverse)
# simulate data
load(here("power-simulation/data/derived/generating_params_adjusted.Rdata"))
load(here("power-simulation/data/simulated/participant_types.Rdata"))
load(here("power-simulation/data/experiment-3-2022/data.rdata"))


participants <- participant_types$full_uid
n <- length(participants)

all_trials_per_cell <- c(6, 9, 12, 16, 20) #c(24, 28, 32)

trials_per_cell_real <- 6 # how many trials per cell in real data
max_trials_per_cell <- 20-trials_per_cell_real # so that first n trials are the real data
n_cells <- 2

# generate the maximum number of trials per cell
for (i in 1:n){
  id <- participants[i]
  p <- i
  p_gen_data <- generating_params %>%
    filter(uid == id)
  
  # get actual empirical data
  real_data <- data %>%
    filter(uid == id)
  
  all_sim_data <- NULL
  for (j in 1:n_cells){
    m_update = as.numeric(p_gen_data[j, "mean_update"])
    sd_update = as.numeric(p_gen_data[j, "sd_update"])
    m_prior = as.numeric(p_gen_data[j, "mean_prior"])
    sd_prior = as.numeric(p_gen_data[j, "sd_prior"])
    m_post = as.numeric(p_gen_data[j, "mean_post"])
    sd_post = as.numeric(p_gen_data[j, "sd_post"])
    if (is.na(sd_update)) {
      sd_update = 10
    }
    if (is.na(sd_prior)) {
      sd_prior = 10
    }
    if (is.na(sd_post)) {
      sd_post = 10
    }
    nSources <- as.character(p_gen_data[j, "nSources_A"])
    type <- p_gen_data[j, "p_type"]
    
    nSources_A = rep(as.character(nSources), max_trials_per_cell)
    type <- rep(as.character(type), max_trials_per_cell)
    
    cell_real <- real_data %>%
      filter(nSources_A == nSources) %>%
      select(update, prior_adjusted, post_adjusted)
    
    update_sim <- rnorm(max_trials_per_cell, m_update, sd_update)
    prior_sim <- rnorm(max_trials_per_cell, m_prior, sd_prior)
    post_sim <- rnorm(max_trials_per_cell, m_post, sd_post)
    
    update <- c(cell_real$update, update_sim)
    prior <- c(cell_real$prior_adjusted, prior_sim)
    post <- c(cell_real$post_adjusted, post_sim)
    
    # Ensure nSources_A and type have the same length as update/prior/post
    n_total <- length(update)  # Get the actual total number of trials
    nSources_A = rep(as.character(nSources), n_total)
    type <- rep(as.character(type), n_total)
    
    d_cell <- cbind(p, nSources_A, update, prior, post, type, id)
    all_sim_data <- rbind(all_sim_data, d_cell)
  }
  # take just the trials in that n_trials condition for each cell
  for (n_trials_per_cell in all_trials_per_cell) {
    sim_data <- as.data.frame(all_sim_data) %>%
      group_by(nSources_A) %>%
      slice_head(n = n_trials_per_cell)
    n_trials <- n_trials_per_cell * n_cells
    sim_data <- as.data.frame(sim_data)
    sim_data$update = as.numeric(sim_data$update)
    save(sim_data, file = here(paste0("power-simulation/data/simulated/p",i,"-",n_trials,"-trials-adjusted.Rdata")))
  }
}