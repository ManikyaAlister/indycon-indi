library(here)
library(tidyverse)
library(brms)
library(parallel)  
library(doParallel)

load(here("simulation/generating_data.Rdata"))
participants <- unique(generating_data$uid)
n <- length(participants)

# Baseline (actual participants, 12 trials)

load(here("data/data.2.rdata"))
data = data.2
data <- data %>%
  mutate(update = post-prior)

UIDs = unique(data$uid)[participants]

# Determine number of cores
num_cores <- detectCores() - 2

# Create cluster
cl <- makeCluster(num_cores) 

# Register cluster
registerDoParallel(cl)

foreach (i = 1:n, .packages=c('brms', 'tidyverse', 'here')) %dopar% {
  p <- participants[i]
  subj <- UIDs[i]
  
  d = data %>%
    filter(uid == subj)
  
  # double the number of trials
  d = bind_rows(d, d)
  
  d <- d %>% 
    mutate(nSources_A = as.character(nSources_A))
 
  n_trials <- length(d$uid)
  
  gd <- generating_data %>% filter(uid == p)

  p_type <-  unique(gd$p_type)

  
  null <- brm(update~sideA, data = d)
  sum_null <- summary(null)
  loo_null <- loo(null)
  
  alt <- brm(update~sideA+nSources_A, data = d)
  sum_alt <- summary(alt)
  loo_alt <- loo(alt)
  
  save(p_type, p, subj, d, null, sum_null, loo_null, alt, sum_alt, loo_alt, file = here(paste0("simulation/output/P",i,"-output-",n_trials,"-trials-update.Rdata")))
}