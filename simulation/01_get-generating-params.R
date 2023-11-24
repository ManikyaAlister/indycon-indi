rm(list = ls())
library(here)
library(tidyverse)
load(here("data/experiment-3-2022/proportion_above_median.Rdata"))
load(here("data/experiment-3-2022/data.2.Rdata"))

data <- data.2 %>%
  mutate(
         uid_num = factor(as.integer(uid), levels = unique(as.integer(uid))),
         post_adjusted = ifelse(sideA == "con", post, (100-post)),
         prior_adjusted = ifelse(sideA == "con", prior, (100-prior)),
         update = post_adjusted-prior_adjusted
  )

save(data, file = here("data/experiment-3-2022/data.rdata"))

# Filter data for topic level analysis for Ee Von
data_topics <- data %>%
  select(uid, trialType, prior_adjusted, post_adjusted, nSources_A)%>%
  mutate(update = post_adjusted-prior_adjusted)%>% 
  group_by(trialType, nSources_A) %>%
  summarise(update = median(update)) %>%
  pivot_wider(names_from = nSources_A, values_from = update)
write.csv(data_topics, file = here("simulation/topic-analysis/data/topic_data.csv"))

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

sim_participants <- participant_types$uid_num

generating_params <- NULL
for (i in 1:length(sim_participants)) {
  # reset gen_data
  gen_data <- NULL
  
  # get uid number
  id <- sim_participants[i]
  
  p_type <- as.character(participant_types[participant_types$uid == as.character(id), "p_type"])
  
  if(is.na(p_type)) stop()
  # get generating params
  gen_data <- data %>%
    filter(uid_num == id) %>%
    group_by(uid_num, nSources_A) %>%
    summarise(
      mean_update = mean(update),
      sd_update = sd(update),
      mean_prior = mean(prior),
      sd_prior = sd(prior),
      mean_post = mean(post),
      sd_post = sd(post)
    ) %>%
    mutate(p_type = p_type)
  
  generating_params <- rbind(generating_params, gen_data)
}

save(generating_params, file = here("simulation/data/generating_params_adjusted.Rdata"))
