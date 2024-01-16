rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(brms, lib.loc = lib)

load(here("simulation/data/generating_params_adjusted.Rdata"))
participants <- unique(generating_params$uid_num)
n <- length(participants)

n_cells <- 2


# load in arguments defined by job array (participant and trials per cell)
args <- commandArgs(trailingOnly = TRUE)

# participant (defined by job array)
p <- as.numeric(args[2])

# trials per cell in the data set (defined by job array)
trials_per_cell <- as.numeric(args[1])

# total trials in the cell, therefore determining the data set to load
n_trials <- trials_per_cell*n_cells

uid <- participants[p]

# print progress
print(paste0("Starting participant ", p, ", ", n_trials, " trials"))

# load data set
load(here(paste0(
  "simulation/data/p", p, "-", n_trials, "-trials-adjusted.Rdata"
)))

d <- sim_data

d$post <- as.numeric(d$post)
d$prior <- as.numeric(d$prior)


n_trials <- length(d$update)

p_type <-  unique(d$type)

# set up models 
null <- brm(post ~ prior, data = d)
sum_null <- summary(null)
loo_null <- loo(null)

print(paste0("Null model done"))

alt <- brm(post ~ prior + nSources_A, data = d)
sum_alt <- summary(alt)
loo_alt <- loo(alt)

print(paste0("Alternative model done"))


save(p_type,
     p,
     d,
     uid,
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
         "-trials-simdata-adjusted.Rdata"
       )
     ))

print(paste0("P ", p, ", ", n_trials, " trials done"))
