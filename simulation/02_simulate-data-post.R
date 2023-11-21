library(here)
library(tidyverse)
# simulate data
load(here("simulation/data/generating_params_post.Rdata"))
participants <- unique(generating_params$uid)
n <- length(participants)

all_trials_per_cell <- c(3, 6, 9, 12, 16, 20)
max_trials_per_cell <- 20
n_cells <- 4

# generate the maximum number of trials per cell
for (i in 1:n){
  p <- participants[i]
  p_gen_data <- generating_params %>%
    filter(uid == p)
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
    sideA = rep(as.character(p_gen_data[j, "sideA"]), max_trials_per_cell)
    nSources_A = rep(as.character(p_gen_data[j, "nSources_A"]), max_trials_per_cell)
    type <- rep(as.character(p_gen_data[j, "p_type"]), max_trials_per_cell)
    update <- rnorm(max_trials_per_cell, m_update, sd_update)
    prior <- rnorm(max_trials_per_cell, m_prior, sd_prior)
    post <- rnorm(max_trials_per_cell, m_post, sd_post)
    d_cell <- cbind(p, sideA, nSources_A, update, prior, post, type)
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
    save(sim_data, file = here(paste0("simulation/data/p",i,"-",n_trials,"-trials-post.Rdata")))
  }

}