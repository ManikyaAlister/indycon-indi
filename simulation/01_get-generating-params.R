rm(list = ls())
library(here)
library(dplyr)
library(tidyverse)
load(here("data/experiment-3-2022/proportion_above_median.Rdata"))
load(here("data/experiment-3-2022/data.2.Rdata"))

data <- data.2 %>%
  mutate(
         uid_num = factor(as.integer(uid), levels = unique(as.integer(uid))),
         post_adjusted = ifelse(sideA == "pro", post, (100-post)),
         #prior_adjusted = ifelse(sideA == "pro", prior, (100-prior)),
         update = post_adjusted-prior
  )

save(data, file = here("data/experiment-3-2022/data.rdata"))

# Filter data for topic level analysis for Ee Von
data_topics <- data %>%
  select(uid, trialType, prior, post_adjusted, nSources_A)%>%
  mutate(update = post_adjusted-prior)%>% 
  group_by(trialType, nSources_A) %>%
  summarise(update = median(update)) %>%
  pivot_wider(names_from = nSources_A, values_from = update)
write.csv(data_topics, file = here("simulation/topic-analysis/data/topic_data.csv"))

proportion_above_median$uid_num = proportion_above_median$uid

# More convinced by independence
ps_ind_strong <- proportion_above_median %>%
  filter(n_above_median == 5, n_sources == 4) %>%
  select(uid_num)

d_ind_strong <- data %>%
  filter(uid_num %in% ps_ind_strong$uid_num) %>%
  group_by(uid_num, nSources_A) %>%
  summarise(mean_update = mean(update), sd_update = sd(update), mean_prior = mean(prior), sd_prior = sd(prior), mean_post = mean(post), sd_post = sd(post))%>%
  mutate(p_type = "ind_strong")


ps_ind_med <- proportion_above_median %>%
  filter(n_above_median == 4, n_sources == 4) %>%
  select(uid_num)

d_ind_med <- data %>%
  filter(uid_num %in% ps_ind_med$uid_num) %>%
  group_by(uid_num, nSources_A) %>%
  summarise(mean_update = mean(update), sd_update = sd(update), mean_prior = mean(prior), sd_prior = sd(prior), mean_post = mean(post), sd_post = sd(post)) %>%
  mutate(p_type = "ind_med")

# More convinced by dependence 
ps_dep_strong <- proportion_above_median %>%
  filter(n_above_median == 5, n_sources == 1) %>%
  select(uid_num, n_sources)

d_dep_strong <- data %>%
  filter(uid_num %in% ps_dep_strong$uid_num) %>%
  group_by(uid_num, nSources_A) %>%
  summarise(mean_update = mean(update), sd_update = sd(update), mean_prior = mean(prior), sd_prior = sd(prior), mean_post = mean(post), sd_post = sd(post)) %>%
  mutate(p_type = "dep_strong")

ps_dep_med <- proportion_above_median %>%
  filter(n_above_median == 4, n_sources == 1) %>%
  select(uid_num,n_sources)

d_dep_med <- data %>%
  filter(uid_num %in% ps_dep_med$uid_num) %>%
  group_by(uid_num, nSources_A) %>%
  summarise(mean_update = mean(update), sd_update = sd(update), mean_prior = mean(prior), sd_prior = sd(prior), mean_post = mean(post), sd_post = sd(post)) %>%
  mutate(p_type = "dep_med")

# none
ps_none_dep <- proportion_above_median %>%
  filter(n_above_median == 3, n_sources == 1) %>%
  select(uid_num, n_sources)

d_dep_none <- data %>%
  filter(uid_num %in% ps_none_dep$uid_num) %>%
  group_by(uid_num, nSources_A) %>%
  summarise(mean_update = mean(update), sd_update = sd(update), mean_prior = mean(prior), sd_prior = sd(prior), mean_post = mean(post), sd_post = sd(post)) %>%
  mutate(p_type = "dep_none")

ps_none_ind <- proportion_above_median %>%
  filter(n_above_median == 3, n_sources == 4) %>%
  select(uid_num, n_sources)

d_ind_none <- data %>%
  filter(uid_num %in% ps_none_ind$uid_num) %>%
  group_by(uid_num, nSources_A) %>%
  summarise(mean_update = mean(update), sd_update = sd(update), mean_prior = mean(prior), sd_prior = sd(prior), mean_post = mean(post), sd_post = sd(post)) %>%
  mutate(p_type = "ind_none")

# combine all 

generating_params <- rbind(d_ind_strong, d_ind_med, d_ind_none, d_dep_strong, d_dep_med, d_dep_none)
save(generating_params, file = here("simulation/data/generating_params_adjusted.Rdata"))
