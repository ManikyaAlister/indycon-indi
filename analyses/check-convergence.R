library(here)
library(dplyr)
library(rstan)

# get R hat details
load(here("analyses/02_output/rhat-details.Rdata")) 

# check to see if there's any signs of poor convergence (Rhat > 1.05)
bad_rhat <- rhat_details[rhat_details$Rhat > 1.05,]
bad_rhat

# it lookd likes all of the bad convergent was with one person (subject 58) and one model (prior-consemsus) in the independent vs. contested condition. 
# Load output of bad convergence model
load(here("analyses/02_output/P58-indi-prior-consensus-rm-contested.Rdata"))

# inspect trace plots and histograms
plot(output) # # histograms resemble normal distributions and traceplots suggest good mixing/convergence


