# 

rm(list = ls())
library(here)
library(tidyverse)
load(here("data/experiment-3-2022/proportion_above_median.Rdata"))
load(here("data/experiment-3-2022/data.2.Rdata"))

data <- data.2 %>%
  mutate(
         #uid_num = factor(as.integer(uid), levels = unique(as.integer(uid))),
         post_adjusted = ifelse(sideA == "pro", post, (100-post)),
         prior_adjusted = ifelse(sideA == "pro", prior, (100-prior)),
         update = post_adjusted-prior_adjusted
  )

save(data, file = here("data/experiment-3-2022/data.rdata"))

# Filter data for topic level analysis for Ee Von
data_topics <- data %>%
  select(uid, trialType, prior_adjusted, post_adjusted, nSources_A)%>% # "adjusted" refers to the fact that we're adjusting for pro/con trials.
  mutate(update = post_adjusted-prior_adjusted)%>% 
  group_by(trialType, nSources_A) %>%
  summarise(update = median(update)) %>%
  pivot_wider(names_from = nSources_A, values_from = update)
write.csv(data_topics, file = here("power-simulation/topic-analysis/data/topic_data.csv"))

proportion_above_median$uid_num = proportion_above_median$uid

participant_types <- proportion_above_median %>% 
  filter(n_above_median >= 3) %>% # avoid duplicates 
  mutate(p_type = case_when(
    n_above_median == 5 & n_sources == 1 ~ "dep_strong",
    n_above_median == 4 & n_sources == 1 ~ "dep_med",
    n_above_median == 3 ~ "none",
    n_above_median == 5 & n_sources == 4 ~ "ind_strong",
    n_above_median == 4 & n_sources == 4 ~ "ind_med",
  )) %>%
  # when n)above median = 3 it appears for both 4 sources and 1, so need to filter out one of those so participants aren't repeated. 
  filter(!duplicated(uid))
# More convinced by independence

save(participant_types, file = "power-simulation/data/participant_types.Rdata")

sim_participants <- participant_types$full_uid

generating_params <- NULL
for (i in 1:length(sim_participants)) {
  # reset gen_data
  gen_data <- NULL
  
  # get uid number
  id <- sim_participants[i]
  
  p_type <- as.character(participant_types[participant_types$full_uid == id, "p_type"])
  
  if(is.na(p_type)) stop()
  # get generating params
  gen_data <- data %>%
    filter(uid == id) %>%
    #select(uid, uid_num, nSources_A, sideA, update, prior, prior_adjusted, post, post_adjusted)
  group_by(uid, nSources_A) %>%
    summarise(
      mean_update = mean(update),
      sd_update = sd(update),
      mean_prior = mean(prior_adjusted),
      sd_prior = sd(prior_adjusted),
      mean_post = mean(post_adjusted),
      sd_post = sd(post_adjusted)
    ) %>%
    mutate(p_type = p_type)
  
  generating_params <- rbind(generating_params, gen_data)
}

save(generating_params, file = here("power-simulation/data/generating_params_adjusted.Rdata"))
