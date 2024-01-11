rm(list = ls())
library(here)
library(brms)

models <- c("indi-prior", "indi-prior-consensus")
n_sub <- 2
model_details <- NULL
excluded_conditions <- c("contested", "dependent")

model_details <- expand.grid(subject = 1:n_sub, model = models, excluded_condition = excluded_conditions)

getCredibleInt <- function(output, probs = c(0.055, 0.945)){ # default is 89% credible interval 
  posterior_sample <- as_draws_df(output)
  interval <- quantile(posterior_sample$b_consensusindependent, probs = probs)
  names(interval) <- c("lower-CI", "upper-CI")
  interval
}

getModelOutput <- function(model_details){
  
  if (is.data.frame(model_details)){
    vec_names <- colnames(model_details)
    model_details <- as.vector(as.matrix(model_details))
    names(model_details) <- vec_names
  }
  
  participant <- model_details["subject"]
  model <- model_details["model"]
  remove <- model_details["excluded_condition"]
  
  # empty vector for processed output 
  #processed_output <- c(subject = participant, model = model, excluded_cond = remove, looic = NA, consensus_estimate = NA, lower_CI = NA, upper_CI = NA)
  processed_output <- c(looic = NA, consensus_estimate = NA, lower_CI = NA, upper_CI = NA)
  
  
  # load output file
  load(here(
    paste0(
      "analyses/02_output/P",
      participant,
      "-",
      model,
      "-rm-",
      remove,
      ".Rdata"
    )
  ))
  
  summ_output <- summary(output)
  processed_output["looic"] <- loo(output)$estimates["looic",1]
  
  if (model == "indi-prior-consensus") {
    processed_output["consensus_estimate"] <- summ_output$fixed["consensusindependent","Estimate"]
    credible_interval <- getCredibleInt(output)
    processed_output["lower_CI"] <- credible_interval[1]
    processed_output["upper_CI"] <-  credible_interval[2]
  }
  
    processed_output
  
}

# get model output for all participants and conditions
procesed_output <- t(apply(model_details, 1, getModelOutput))

# combine with model details 
model_output <- cbind(model_details, procesed_output)

# less typing (so I don't have to specify argument each time)
maxNA = function(x){
  max(x, na.rm = TRUE)
}

model_comparison <- model_output %>%
  pivot_wider(names_from = model, values_from = looic) %>%
  # simplify column names
  rename("looic_null" = `indi-prior`, "looic_alt" = `indi-prior-consensus`) %>%
  # collapse across models
  group_by(subject, excluded_condition) %>%
  summarise(
    consensus_estimate = maxNA(consensus_estimate),
    lower_CI = maxNA(lower_CI),
    upper_CI = maxNA(upper_CI),
    looic_null = maxNA(looic_null),
    looic_alt = maxNA(looic_alt)
  ) %>%
  mutate(looic_diff = looic_null - looic_alt)


