rm(list = ls())

# for HPC, need to define library path. Comment out and just use "here" if running locally
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(dplyr)
library(brms, lib.loc = lib)


# define model name 
models <- c("group-prior-consensus*age")
  #c("group-prior-consensusXclaim-certainty", "group-prior-consensusXclaimXsource")

# how to run wirh commandArgs: type in terminal 
# Rscript analyses/01_models/run_follow_up_group_models_local.R  <model e.g., "group-prior-consensusXclaimXsource">
model <- commandArgs(trailingOnly = TRUE)

# load participant data
load(here(paste0("data/clean/all_data_clean.Rdata")))
data_reponses <- all_data$data
demographics <- all_data$demographics

# join demographic data with response data
data <- full_join(data_reponses, demographics, by = "ANON_PID") %>%
  select(-participant.y) %>%
# add transformed prior that captures prior certainty
  mutate(participant = participant.x,
    pre_certainty = abs(pre_adjusted - 50),
    demographics_age = as.numeric(demographics_age)
  )


conditions_to_remove <- c("dependent", "contested")


for (remove in conditions_to_remove){
  filtered_data <- data[data[,"consensus"] != remove,]
   if (model == "group-prior-consensusXclaim-certainty"){
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + pre_certainty + consensus * claim_type, data = filtered_data)
   } else if (model == "group-prior-consensusXclaimXsource"){
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus * claim_type * source, data = filtered_data)
   } else if (model == "group-prior-consensus*age"){
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus * demographics_age, data = filtered_data)
   } else if (model == "group-prior-consensus*politcalstrength") {
     filtered_data_pol <- filtered_data %>%
       filter(! demographics_politics %in% c("soc", "anarch")) # these don't fall as nicely in the storng-lib to stron-conservative scale (even though one could argue thay are strong lib)
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus * political_strength, data = filtered_data_pol)
   }else if (model == "group-prior-consensus*politcalscale") {
     filtered_data_pol <- filtered_data %>%
       filter(! demographics_politics %in% c("soc", "anarch")) 
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus * political_scale, data = filtered_data_pol)
   } else if (model == "group-prior-consensus*politcalgroup") {
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus * political_group, data = filtered_data)
   } else if (model == "group-prior-consensus*unieducation") {
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus * university_education, data = filtered_data)
   } else if (model == "group-prior-consensus*socmedprop") {
     output <- brm(post_adjusted ~ (1|participant) + pre_adjusted + consensus * socmed_proportionate, data = filtered_data)
   } 
  save(output, model, remove, file = here(paste0("analyses/02_output/",model,"-rm-",remove,".Rdata")))
}
    