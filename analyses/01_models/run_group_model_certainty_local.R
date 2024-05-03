rm(list = ls())

# for HPC, need to define library path. Comment out and just use "here" if running locally
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(dplyr)
library(brms, lib.loc = lib)
library(parallel)  
library(doParallel)


# Add an extra (non-preregistered) model that considers the prior certainty


# define model name 
model <- c("group-prior-consensusXclaim-certainty")

# load participant data
load(here(paste0("data/clean/all_data_clean.Rdata")))
data <- all_data[[1]]

# add transformed prior that captures prior certainty
data <- data %>% 
  mutate(pre_certainty = abs(pre_adjusted - 50))

conditions_to_remove <- c("contested", "dependent")

# Determine number of cores
num_cores <- detectCores() - 2

# Create cluster
cl <- makeCluster(num_cores) 

# Register cluster
registerDoParallel(cl)

# run models in parallel
#foreach (remove = conditions_to_remove, .packages=c('brms', 'tidyverse', 'here')) %dopar% {
for (remove in conditions_to_remove){
  filtered_data <- data[data[,"consensus"] != remove,]
  
  output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + pre_certainty + consensus * claim_type, data = filtered_data)
  
  save(output, model, remove, file = here(paste0("analyses/02_output/",model,"-rm-",remove,".Rdata")))
}
    

  
#}