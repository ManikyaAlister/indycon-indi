rm(list = ls())
library(here)
library(tidyverse)
library(brms)

load(here("simulation/data/generating_params_adjusted.Rdata"))
participants <- unique(generating_params$uid_num)
n <- length(participants)

participant_types <- generating_params %>% 
  group_by(uid_num) %>%
  summarise(p_type)

models = c("null", "alt")

null_looic <- c()
alt_looic <- c()
ind_estimate <- c()
best_model <- c()
ps <- c()
subjs <- c()
type <- c()
lower95 <- c()
upper95 <- c()
upper89 <- c()
lower89 <- c()
upper89 <- c()


all_trials_per_cell <- c(6, 9, 12, 16, 20)
n_cells <- 2

all_output <- NULL
for (j in all_trials_per_cell){
  start <- 1
  # if (j == 12){
  #   start <- 49
  # }
  n_trials <- n_cells * j
  print(paste0("Trials: ", n_trials))
  for (i in start:n){
    # if (i %in% c(36,58, 74, 93,99)){
    #   next
    # }
    print(paste0("Participant ", i))
    load(here(paste0("simulation/output/P",i,"-output-",n_trials,"-trials-simdata-adjusted.Rdata")))
    null_looic[i] <- loo_null$estimates["looic",1]
    alt_looic[i] <- loo_alt$estimates["looic",1]
    ind_estimate[i] <- sum_alt$fixed["nSources_A4","Estimate"]
    lower95[i] <- sum_alt$fixed["nSources_A4","l-95% CI"]
    upper95[i] <- sum_alt$fixed["nSources_A4","u-95% CI"]
    best_model[i] <- models[which.min(c(loo_null$estimates["looic",1], loo_alt$estimates["looic",1]))]
    ps[i] <- p
    subjs[i]<-i
    gd <- generating_params %>% filter(uid_num == uid)
    type[i] <- unique(gd$p_type)
    posterior_sample <- as_draws_df(alt)
    CI_89 <- quantile(posterior_sample$b_nSources_A4, probs = c(0.055, 0.945))
    lower89[i] <- CI_89[1]  
    upper89[i] <- CI_89[2]  

  }
  
  output <- cbind(null_looic, alt_looic, best_model, ind_estimate, lower95,upper95,lower89,upper89,ps, type, n_trials)
  all_output <- rbind(all_output, output)
}

all_output <- as.data.frame(all_output)
save(all_output, file = here("simulation/output/all-output-adjusted.Rdata"))
