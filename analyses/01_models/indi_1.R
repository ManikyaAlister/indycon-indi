

# for HPC, need to define library path. Comment out and just use "here" if running locally
# lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
# library(here, lib.loc = lib)
# library(brms, lib.loc = lib)
library(here)
library(brms)

# define model name 
model <- "indi-prior"

# get participant number from environment (defined when running job_array script)
subject <- commandArgs(trailingOnly = TRUE)

# load participant data
load(here(paste0("data/clean/P", subject, ".Rdata")))

# Define which condition to remove from data (comparison is either diverse v
# repeated or diverse v contested, so need to remove the condition that is not
# in the comparison)
conditions_to_remove <- c("contested", "dependent")

# run the model for each filtered data set with the appropriate condition removed
# running in a loop makes it slower, but don't want to create too many jobs on the HPC.
for (remove in conditions_to_remove) {
  # filter data to remove condition
  filtered_data <- participant_data[participant_data[,"consensus"] != remove,]
  
  # run model
  output <- brm(post_adjusted ~ pre_adjusted, data = filtered_data) # can adjust this "cores" argument to run the chains separately on different cores and therefore be faster. 
  
  # save output and other participant info
  save(subject, output, model, remove, file = here(paste0("analyses/02_output/P",subject,"-",model,"-rm-",remove,".Rdata")))
}
