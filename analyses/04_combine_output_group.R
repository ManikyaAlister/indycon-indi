rm(list = ls())
library(here)
library(brms)


original_models <-
  c(
    "group-prior",
    "group-prior-consensus",
    "group-prior-consensus-claim",
    "group-prior-consensusXclaim"
  )

follow_up_models <-
  c(
    "group-prior-consensusXclaim-certainty",
    "group-prior-consensusXclaimXsource")

demographic_models <- c(
  "group-prior-consensus*age",
  "group-prior-consensus*politcalscale",
  "group-prior-consensus*politcalstrength",
  "group-prior-consensus*politcalgroup",
  "group-prior-consensus*unieducation",
  "group-prior-consensus*socmedprop"
)


models <- c(original_models, follow_up_models, demographic_models)
is_follow_up <- models %in% follow_up_models # label whether it's an original or follow up model
is_demographic <-  models %in% demographic_models# label whether it's modelling demographics
excluded_conditions <- c("contested", "dependent")

model_details <- expand.grid(model = models, excluded_condition = excluded_conditions)

all_output <- NULL
all_looic <- NULL
all_se <- NULL
for ( i in 1:length(model_details[,1])){
  print(i)
  output <- NULL
  looic <- NULL
  model <- model_details[i,"model"]
  excluded_condition <- model_details[i,"excluded_condition"]
  load(here(paste0("analyses/02_output/",model,"-rm-",excluded_condition,".Rdata")))
  all_output[[i]] = list(model, excluded_condition, output)
  looic <- loo(output)$estimates["looic","Estimate"]
  se <- loo(output)$estimates["looic","SE"]
  all_se[i] <- se
  all_looic[i] <- looic
}

model_LOOICs <- cbind(model_details, all_looic, all_se, is_follow_up, is_demographic)

save(all_looic,all_output, model_LOOICs, file = here("data/derived/group_output_combined.Rdata"))


# for broad claim types

model_details_broad <- model_details[model_details$model %in% original_models,] # only interested in original modles


all_looic <- NULL
for ( i in 1:length(model_details_broad[,1])){
  print(i)
  output <- NULL
  looic <- NULL
  model <- model_details_broad[i,"model"]
  excluded_condition <- model_details_broad[i,"excluded_condition"]
  load(here(paste0("analyses/02_output/broad-",model,"-rm-",excluded_condition,".Rdata")))
  looic <- loo(output)$estimates["looic","Estimate"]
  all_looic[i] <- looic
}

model_LOOICs <- cbind(model_details_broad, all_looic)

save(model_LOOICs, file = here("data/derived/broad_group_output_combined.Rdata"))

