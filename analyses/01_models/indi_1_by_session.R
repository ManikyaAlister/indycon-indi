rm(list = ls())

# for HPC, need to define library path. Comment out and just use "here" if running locally
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(brms, lib.loc = lib)

# define model name 
model <- "indi-prior"

# get participant number from environment (defined when running job_array script)
subject <- commandArgs(trailingOnly = TRUE)

# load participant data
load(here(paste0("data/clean/P", subject, ".Rdata")))

# Remove contested condition
remove <- "contested"
participant_data <- participant_data[participant_data[,"consensus"] != remove,]

# define sessions
sessions <- c(1,2)

# run the model for each filtered data set with the appropriate condition removed
# running in a loop makes it slower, but don't want to create too many jobs on the HPC.
for (session in sessions) {
  # filter data to remove condition
  filtered_data <- participant_data[participant_data[,"session_number"] != session,]
  
  # run model
  output <- brm(post_adjusted ~ pre_adjusted, data = filtered_data)
  
  # save output and other participant info
  save(subject, output, model, remove, session, file = here(paste0("analyses/02_output/P",subject,"-",model,"-rm-",remove,"-session-",session,".Rdata")))
}
