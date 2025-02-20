rm(list = ls())

# for HPC, need to define library path. Comment out and just use "here" if running locally
# lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
# library(here, lib.loc = lib)
# library(brms, lib.loc = lib)
library(here)
library(brms)
library(parallel)  
library(doParallel)

# define model name 
models <- c("group-prior", "group-prior-consensus", "group-prior-consensus-claim", "group-prior-consensusXclaim")

# load participant data
load(here(paste0("data/clean/all_data_clean.Rdata")))
data <- all_data[[1]]

conditions_to_remove <- c("contested", "dependent")

# Determine number of cores
num_cores <- detectCores() - 2

# Create cluster
cl <- makeCluster(num_cores) 

# Register cluster
registerDoParallel(cl)

# run models in parallel
foreach (model = models, .packages=c('brms', 'tidyverse', 'here')) %dopar% {

# run the model for each filtered data set with the appropriate condition removed
# running in a loop makes it slower, but don't want to overload cores
for (remove in conditions_to_remove) {
  
filtered_data <- data[data[,"consensus"] != remove,]

if (model == "group-prior"){
  output <- brm(post_adjusted ~ (1|participant) + pre_adjusted,data = filtered_data)
} else if (model == "group-prior-consensus"){
  output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus,data = filtered_data)
} else if (model == "group-prior-consensus-claim"){
  output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus + broad_claim_type, data = filtered_data)
} else if (model == "group-prior-consensusXclaim") {
  output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus * broad_claim_type, data = filtered_data)
}
save(output, model, remove, file = here(paste0("analyses/02_output/broad-",model,"-rm-",remove,".Rdata")))
}
}