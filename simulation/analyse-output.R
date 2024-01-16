library(here)
library(tidyverse)
library(brms)

# load combined output
load(here("simulation/output/all-output.Rdata"))
n <- length(unique(all_output$ps))

# Figure out which data sets still contained NAs despite correcting for instances where sd = 0 in real data. 
all_NA <- NULL
has_NA <- c()
p <- c()
for (i in 1:n) {
  load(here(paste0("simulation/data/p",i,"-12-trials.Rdata")))
  has_na <- any(is.na(sim_data$sideA))
  p <- i
  d_iteration <- cbind(p,has_na)
  all_NA <-rbind(all_NA, d_iteration)
}

na_subjects <- all_NA[all_NA[,"has_na"]==1,]
subjects_not_analysed <- c(na_subjects[,"p"], 58) # models didn't run on p58 (index 44) for some reason

# filter subjects from all_output
all_output <- all_output %>%
  filter(!ps %in% subjects_not_analysed)


# see how many people were best fit by the alternative model as a function of n trials. 
alt <- all_output %>%
  filter(best_model == "alt")

alt %>%
  group_by(n_trials) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = n_trials, y = n, group = 1)) +
  ylim(0,adjusted_n)+
  geom_col()+
  geom_line()
  

  
# see how many people had credible estimates as a function of n trials. 

all_output$estimate_contains_0_95 <- with(all_output, lower95 < 0 & upper95 > 0)
all_output$estimate_contains_0_89 <- with(all_output, lower89 < 0 & upper89 > 0)

credible_estimates_95 <- all_output %>%
  filter(estimate_contains_0_95 == FALSE) 

credible_estimates_85 <- all_output %>%
  filter(estimate_contains_0_89 == FALSE) 

credible_estimates_85 %>%
  group_by(n_trials) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = n_trials, y = n, group = 1)) +
  ylim(0,adjusted_n)+
  geom_col()+
  geom_line()

credible_estimates$positive <- credible_estimates$ind_estimate > 0
  

