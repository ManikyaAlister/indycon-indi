rm(list = ls())

# for HPC, need to define library path. Comment out and just use "here" if running locally
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(dplyr)
library(brms, lib.loc = lib)


# define model name 
models <- c("group-prior-consensusXclaim-certainty", "group-prior-consensusXclaimXsource")

# how to run wirh commandArgs: type in terminal 
# Rscript analyses/01_models/run_follow_up_group_models_local.R  <model e.g., "group-prior-consensusXclaimXsource">
model <- commandArgs(trailingOnly = TRUE)

# load participant data
load(here(paste0("data/clean/all_data_clean.Rdata")))
data <- all_data[[1]]

# add transformed prior that captures prior certainty
data <- data %>% 
  mutate(pre_certainty = abs(pre_adjusted - 50))

conditions_to_remove <- c("contested", "dependent")


for (remove in conditions_to_remove){
  filtered_data <- data[data[,"consensus"] != remove,]
   if (model == "group-prior-consensusXclaim-certainty"){
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + pre_certainty + consensus * claim_type, data = filtered_data)
   } else if (model == "group-prior-consensusXclaimXsource"){
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus * claim_type * source, data = filtered_data)
   }
  
  save(output, model, remove, file = here(paste0("analyses/02_output/",model,"-rm-",remove,".Rdata")))
}
    