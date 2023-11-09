library(here)
library(tidyverse)
library(brms)
library(parallel)  
library(doParallel)

load(here("simulation/generating_data_post.Rdata"))
participants <- unique(generating_data$uid)
n <- length(participants)

n_cells <- 4


# load in arguments defined by job array (participant and trials per cell)
args <- commandArgs(trailingOnly = TRUE)

# participant (defined by job array)
p <- args[2]

# trials per cell in the data set (defined by job array)
trials_per_cell <- args[1]

# total trials in the cell, therefore determining the data set to load
n_trials <- trials_per_cell*n_cells

# print progress
print(paste0("Starting participant ", p, " ", trials_per_cell, " trials"))

# load data set
load(here(paste0(
  "simulation/data/p", p, "-", n_trials, "-trials-post.Rdata"
)))

d <- sim_data

n_trials <- length(d$update)

p_type <-  unique(d$type)

# set up models 
null <- brm(post ~ prior + sideA, data = d)
sum_null <- summary(null)
loo_null <- loo(null)

alt <- brm(post ~ prior + sideA + nSources_A, data = d)
sum_alt <- summary(alt)
loo_alt <- loo(alt)

save(p_type,
     p,
     d,
     null,
     sum_null,
     loo_null,
     alt,
     sum_alt,
     loo_alt,
     file = here(
       paste0(
         "simulation/output/P",
         p,
         "-output-",
         n_trials,
         "-trials-simdata-post.Rdata"
       )
     ))
print(paste0("P ", p, " ", j, " trials done"))
