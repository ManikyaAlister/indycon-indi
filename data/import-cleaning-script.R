# Import experiment data from google cloud
# Python3 export_results.py live rectangleworld learn c23 v2 results.json

rm(list = ls())
library(here)
library(jsonlite)
library(tidyverse)

# Creating the data frame
claim_info <- data.frame(
  id = c(
    "aiBan", "avocado", "cleanCoal", "gmoCrops", "goNuclear", 
    "greenSpaces", "narcissism", "onlineLearning", "readingFiction", 
    "regulateCrypto", "security", "tourism", "transportation", 
    "workFromHome", "writeOrType", "beautyPageants", "books", 
    "dogsOrCats", "fishing", "hardLabour", "militaryService", 
    "noSmoking", "artsAndCulture", "plastic", "returnTheLoot", 
    "schoolUniforms", "socialPublishers", "spaceEx", "superPower", 
    "votingAge", "bank", "chameleons", "cleopatra", "cloud", 
    "dozen", "duckQuack", "eiffelTower", "honey", "jellyfish", 
    "longestWithoutSleep", "loudestShout", "nerd", "shortestWar", 
    "venus", "youtubeVideo", "burningBuilding", "chessGrandmaster", 
    "dinosaurFossil", "ducks", "elephantsEscape", "enormousPearl", 
    "rescueMission", "mathematicsSolve", "minorEarthquake", 
    "oldestTree", "paintingFound", "pumpkin", "sinkhole", 
    "warArtifact", "winningCookies"
  ),
  type = c(
    rep("Unknowable Expert", 15), rep("Unknowable Preference", 15), 
    rep("Knowable Facts", 15), rep("Knowable Eye witness", 15)
  )
)

# Displaying the created data frame
print(data)


RAW_DATA_FILE <- "data/raw/results-t1.json";

# load data
d_json <- read_json(here(RAW_DATA_FILE))

#t <- as.character(unname(as.matrix(sapply(test, function(x) x["P"]))))

# function to clean learning phase data
cleaning_fun = function(raw_data, all_complete = TRUE) { # all complete referst to whether all of the sessions have completed, so there should be 60 trials
  raw_data <- raw_data[[1]]
  all_variables <- names(raw_data)
  all_data <- NULL
  # subject variable to fill with subject numbers, starting at 0
  running_subject <- 0
  # empty df to fill with participants who failed accuracy requirements. 
  rm_accuracy <- NULL
  # keep track of participants who didn't complete both sessions
  rm_drop_out <- NULL
  # data frame to store follow up questions
  follow_ups <- NULL 
  for (i in 1:length(raw_data)) {
    print(i)
    #if (i == 44) next
    d_participant <- raw_data[[i]]
    # skip if participant didn't finish
    if (is.null(d_participant$experimentEndStatus))
      next
    # skip if participant isn't an mturker
    if (d_participant$src != "prolific")
      next
    
    # skip if not in "live" environment
    if (d_participant$env != "live")
      next
    # assign participant number
    
    
    # convert data from list to single column matrix
    d_df <- as.matrix(unlist(d_participant))
    
    responses <- d_df[,1]
    names(responses) <- rownames(d_df)
    
    is_trial_response <- str_detect(rownames(d_df), "Test_T")
  
    
    # Questions that had multiple responses per experiment (e.g., belief update)
    trial_response <- responses[is_trial_response]
    
    raw_trial_variables <- names(trial_response)
    
    # remove "Test_T-" from the start of the the response names
    trimmed_trial_variables <- str_extract(raw_trial_variables, "(?<=Test_T-)[^_]+_(.+)$")
    
    # get all of the trial id's (different claims in each experiment)
    claim <- str_extract(trimmed_trial_variables, "^[^_]+")
    
    # get the actual variable that has been recorded on each trial. 
    trial_variable <- str_extract(raw_trial_variables, "(?<=_)[^_]+$")
    
    # combine the claim, variable, and responses
    trial_data_long <- as.data.frame(cbind(claim, trial_variable, trial_response))
    
    # convert to wide format where each row is a trial and each variable is colu,n
    trial_data_wide <- pivot_wider(data = trial_data_long, id_cols = claim, names_from = trial_variable, values_from = trial_response)
    
    matchStances = function(tweetOrder, stances){
    
      # Splitting the tweetOrder string by ":" and excluding the first element (TW-0)
      actual_stances <- strsplit(tweetOrder, ":", fixed = TRUE)[[1]]
      is_pro_actual <- str_detect(actual_stances, "pro")
      
      # Splitting the stances string by ":"
      participant_stances <- strsplit(stances, ":", fixed = TRUE)[[1]]
      is_pro_participant <- participant_stances == "agrees"
      
      
      # Matching the actual stances with participant stances
      matches <- is_pro_actual == is_pro_participant
      matches
    }
    
    stanceAccuracy <- function(tweetOrder, stances, n_trials = 4){
      stances_match <- matchStances(tweetOrder, stances)
      accuracy <- sum(stances_match)/n_trials
      accuracy
    }
    
    trial_accuracy <- mapply(stanceAccuracy, trial_data_wide$stanceOrder, trial_data_wide$stances)
    mean_accuracy <- mean(trial_accuracy)
    
    # somehow, people can still have an experiment end status as completed even though they 
    # don't appear to have completed all of the questions. 
    if (is.na(mean_accuracy)) {
      print("Subject removed due to incomplete data.")
      next
    }
    
    
    prolific_id <- d_participant[["PROLIFIC_PID"]]
    
    session_number <- d_participant[["session_number"]]
    
    if (session_number == 2){
      self_report_strategy <- d_participant[["followUp_strategy"]]
      self_report_free <- d_participant[["followUp_freeText"]]
      # store the follow up strategies 
      follow_ups <- rbind(follow_ups, c(self_report_strategy, self_report_free))
    }
    
    accuracy_info <- c(mean_accuracy,prolific_id)
    
    print(paste0("Accuracy: ", mean_accuracy, " Participant: ", prolific_id))

    if (mean_accuracy < .9){
      rm_accuracy <- rbind(rm_accuracy, accuracy_info)
      next
    }
    
  
    getTrialReadTimes <- function(read_time){
      times <- as.numeric(strsplit(read_time, ":", fixed = TRUE)[[1]])
      mean_time <- mean(times)/60000
    }
    
    mean_read_times <- apply(trial_data_wide[,"tweetReadTime"], 1, getTrialReadTimes) # doesn't look right
    
    trial_data_wide <- trial_data_wide %>%
      mutate(trial_accuracy = trial_accuracy)
    
   trial_data_wide$claim_type <- claim_info$type[match(trial_data_wide$claim, claim_info$id)]
    
             
    
    getConfigData = function(tweetConfig) {
      if (str_detect( tweetConfig,"4-4-1-4")){
        config_data <- c(
        nSources_A = "4",
        nSources_B = "0",
        side_A = str_extract(tweetConfig, "^(.*?)(?=-4)"),
        side_B = NA,
        consensus = "independent"
        )
      } else if (str_detect(tweetConfig,"4-4-1-1")){
        config_data <- c(
        nSources_A = "1",
        nSources_B = "0",
        # figure out if it's a pro or con trial
        side_A = str_extract(tweetConfig, "^(.*?)(?=-4)"),
        side_B = NA,
        consensus = "dependent"
        )
      } else if (str_detect(tweetConfig,"2-2-2-2")){
        config_data <- c(
        nSources_A = "2",
        nSources_B = "2",
        side_A = "pro",
        side_B = "con", # order shouldn't matter 
        consensus = "contested"
        )
      }
      config_data
    }
    
    config_data <-
      as.data.frame(do.call(rbind, lapply(
        trial_data_wide$tweetConfig, getConfigData
      )))
    no_contested <- config_data %>%
      filter(consensus != "contested")
    
    # check to make sure that the number of pro and con tweets is even
    prop_pro <-
      sum(no_contested$side_A == "pro") / length(no_contested$side_A)
    
    config_data <- config_data %>%
      mutate(prop_pro = prop_pro)
    
    
    # variables that only had one response per experiment (e.g., prolific ID)
    responses_one_off <- data.frame(t(responses[!is_trial_response]))
    
    # match the number of rows to config data so they can be combined
    responses_one_off <- responses_one_off[rep(seq_len(nrow(responses_one_off)), length.out = nrow(config_data)), ]
  
    
    data_all <- cbind(responses_one_off, trial_data_wide, config_data)
    data_all <- data_all %>%
      mutate(total_duration = as.numeric(experimentEndTime)/60000,
             post = as.numeric(`slider-post`),
             pre = as.numeric(`slider-pre`),
             post_adjusted = ifelse(side_A == "con"  & consensus != "contested", (100-post), post),
             pre_adjusted = ifelse(side_A == "con"  & consensus != "contested", (100-pre), pre))
    # remove unnecessary columns
    data <- data_all %>%
      select(PROLIFIC_PID, session_number, claim_set, claim, claim_type, pre, post, pre_adjusted, post_adjusted, consensus, side_A, side_B,
             nSources_A, nSources_B, prop_pro, trial_accuracy, total_duration,  stances, tweetOrder)
  all_data <- rbind(all_data, data)
  }
  
  # get all of the unique prolific id's
  all_prolific_ids <- unique(all_data$PROLIFIC_PID)
  # for each unique participant, filter data into their own data set 
  for (i in 1:length(all_prolific_ids)){
    print(i)
    # get the prolific id
    id <- all_prolific_ids[i]
    # filter data for that participant
    participant_data <- all_data[all_data$PROLIFIC_PID == id,]
    # get the number of trials for that participant
    n_trials <- nrow(participant_data)
    # get the mean accuracy for that participant
    mean_accuracy <- mean(participant_data$trial_accuracy)
    
    # if mean accuracy less than .9, add to rm_accuracy df
    if (mean_accuracy < .9){
      rm_accuracy <- rbind(rm_accuracy, c(mean_accuracy, id))
    }

    if (all_complete & n_trials != 60){
      rm_drop_out <- c(rm_drop_out, id)
    }

    if (all_complete){
      if (n_trials != 60 | mean_accuracy < .9){
        # remove participants who didn't complete the experiment or whose accuracy was below 90%
        all_data <- all_data[all_data$PROLIFIC_PID != id,]

        # do not output data for participants who didn't complete the experiment
        next
      }
    
    # get participant number
    running_subject <- running_subject +1
    participant <- as.character(running_subject)
    
    # add to data
    participant_data$participant <- participant

    # move participant column to front
    participant_data <- participant_data[c("participant", names(participant_data)[-which(names(participant_data) == "participant")])]

    # save participant data
    save(participant_data, file = here(paste0("data/clean/P",participant, ".Rdata")))

    }
  }

  if (all_complete){
    colnames(follow_ups) <- c("self_report_strategy", "free_text")
  }
  # add numeric id column for each participant
  all_data$participant <- as.numeric(factor(all_data$PROLIFIC_PID, levels = unique(all_data$PROLIFIC_PID)))
  
  # Reordering the dataframe by 'id' column
  all_data <- all_data[order(all_data$participant), ]
  
  # Moving the 'id' column to the front
  all_data <- all_data[c("participant", names(all_data)[-which(names(all_data) == "participant")])]
  
  list(all_data, rm_accuracy, rm_drop_out, follow_ups)
}

# indicate whether 
all_complete <- FALSE

all_data <- cleaning_fun(d_json, all_complete = all_complete)
#save(all_data, file = here("data/clean/all_data_clean.Rdata"))

just_data <- all_data[[1]]
accuracy_data <- all_data[[2]]
rm_accuracy <- length(unique(accuracy_data[,2]))
rm_accuracy_ids <- unique(accuracy_data[,2])
rm_accuracy_string <- paste(rm_accuracy_ids, collapse = ",")

paste0(rm_accuracy, " participants removed for accuracy < .9")
rm_drop_out <- length(all_data[[3]])
paste0(rm_drop_out, " participants removed for failing to complete session 2")
follow_ups <- all_data[[4]]
paste0(length(unique(just_data$PROLIFIC_PID)), " participants remain after cleaning.")

time <- just_data %>%
  group_by(PROLIFIC_PID) %>%
  summarise(time = median(total_duration))

median_time <- median(time$time)

paste0("It took participants a median of ", median_time, " minutes to complete the study")

s <- just_data %>%
  filter(PROLIFIC_PID == "656f042cc31f84842ad587e4")


