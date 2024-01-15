rm(list = ls())
library(here)
library(brms)

models <- c("group-prior", "group-prior-consensus", "group-prior-consensus-claim", "group-prior-consensusXclaim")
excluded_conditions <- c("contested", "dependent")

model_details <- expand.grid(model = models, excluded_condition = excluded_conditions)
<<<<<<< HEAD

all_output <- NULL
all_looic <- NULL
all_se <- NULL
=======
all_output <- NULL
all_looic <- NULL
>>>>>>> de0e661 (by session models)
for ( i in 1:length(model_details[,1])){
  print(i)
  output <- NULL
  looic <- NULL
  model <- model_details[i,"model"]
  excluded_condition <- model_details[i,"excluded_condition"]
  load(here(paste0("analyses/02_output/",model,"-rm-",excluded_condition,".Rdata")))
  all_output[[i]] = list(model, excluded_condition, output)
  looic <- loo(output)$estimates["looic","Estimate"]
<<<<<<< HEAD
  se <- loo(output)$estimates["looic","SE"]
  all_se[i] <- se
  all_looic[i] <- looic
}

model_LOOICs <- cbind(model_details, all_looic, all_se)

save(all_looic,all_output, model_LOOICs, file = here("data/derived/group_output_combined.Rdata"))

# for broad claim types
all_looic <- NULL
for ( i in 1:length(model_details[,1])){
  print(i)
  output <- NULL
  looic <- NULL
  model <- model_details[i,"model"]
  excluded_condition <- model_details[i,"excluded_condition"]
  load(here(paste0("analyses/02_output/broad-",model,"-rm-",excluded_condition,".Rdata")))
  looic <- loo(output)$estimates["looic","Estimate"]
=======
>>>>>>> de0e661 (by session models)
  all_looic[i] <- looic
}

model_LOOICs <- cbind(model_details, all_looic)

<<<<<<< HEAD
save(model_LOOICs, file = here("data/derived/broad_group_output_combined.Rdata"))
=======
save(all_looic,all_output, model_LOOICs, file = here("data/derived/group_output_combined.Rdata"))
>>>>>>> de0e661 (by session models)
