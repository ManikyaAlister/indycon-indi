# Import experiment data from google cloud
# Python3 export_results.py live rectangleworld learn c23 v2 results.json


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
    rep("Knowable Facts", 15), rep("Knowable Eyewitness", 15)
  )
)

# load source type information
source_info <- read.csv(here("analyses/derived-data/source_data.csv"))

# Displaying the created data frame
print(data)


RAW_DATA_FILE <- "data/raw/anon-results.json";#data/raw/anon-results.json";

# load data
d_json <- read_json(here(RAW_DATA_FILE))

# check source =prolific
count_prolific <- function(x) {
  sum(sapply(x, function(y) is.list(y) && "src" %in% names(y) && y$src == "prolific"))
}
count_prolific(t)

extract_anon_pids <- function(x) {
  anon_pids <- unlist(sapply(x, function(y) {
    if (is.list(y) && "src" %in% names(y) && y$src == "prolific" && "ANON_PID" %in% names(y)) {
      return(y$ANON_PID)
    } else {
      return(NULL)
    }
  }))
  return(anon_pids)
}


#t <- as.character(unname(as.matrix(sapply(test, function(x) x["P"]))))

# function to clean learning phase data
cleaning_fun = function(raw_data, all_complete = TRUE) { # all complete refers to whether all of the sessions have completed, so there should be 60 trials
  raw_data <- raw_data[[1]]
  all_variables <- names(raw_data)
  all_data <- NULL
  all_demographics <- NULL 
  # subject variable to fill with subject numbers, starting at 0
  running_subject <- 0
  # empty df to fill with participants who failed accuracy requirements. 
  rm_accuracy <- NULL
  # keep track of participants who didn't complete both sessions
  rm_drop_out <- NULL
  # data frame to store follow up questions
  follow_ups <- NULL 
  # keep track of people who removed for accuracy in session 1
  rm_accuracy_s1 <- NULL 

  # all prolific participants
  all_prolific_unfiltered <- NULL 
  
  # keep track of participant ids and session numbers
  id_sesh <- NULL
  
  for (i in 1:length(raw_data)) {
    print(i)
    #if (i == 44) next
    d_participant <- raw_data[[i]]
    
    
    # skip if participant isn't an mturker
    if (d_participant$src != "prolific")
      next
    
    # skip if not in "live" environment
    if (d_participant$env != "live")
      next
  
    
    session_number <- d_participant[["session_number"]]
    prolific_id <- d_participant[["ANON_PID"]]
    
    # store to know how many prolific participants we had total, even those who dropped out in s1
    all_prolific_unfiltered <- c(all_prolific_unfiltered, prolific_id)
    
    # skip if participant didn't finish and record them
    if (is.null(d_participant$experimentEndStatus)){
      next
    }
  
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
    
    
    accuracy_info <- c(mean_accuracy,prolific_id, session_number)
    
    #print(paste0("Accuracy: ", mean_accuracy, " Participant: ", prolific_id))

    if (mean_accuracy < .9){
      rm_accuracy <- rbind(rm_accuracy, accuracy_info)
     # next
      if (session_number == 1){
        rm_accuracy_s1 <- rbind(rm_accuracy_s1, accuracy_info)
      }
    }
    
    
    if (session_number == 2){
      self_report_strategy <- d_participant[["followUp_strategy"]]
      if(is.null(self_report_strategy)) self_report_strategy <- NA
     self_report_free <- d_participant[["followUp_freeText"]]
      # store the follow up strategies 
      follow_ups <- rbind(follow_ups, c(prolific_id, self_report_strategy, self_report_free))
    }
    
    # get pid and session 
   id_sesh <-  rbind(id_sesh, c(id = prolific_id, session = session_number))
    
    getTrialReadTimes <- function(read_time){
      times <- as.numeric(strsplit(read_time, ":", fixed = TRUE)[[1]])
      mean_time <- mean(times)/60000
    }
    
    mean_read_times <- apply(trial_data_wide[,"tweetReadTime"], 1, getTrialReadTimes) 
    
    trial_data_wide <- trial_data_wide %>%
      mutate(trial_accuracy = trial_accuracy)
    
   trial_data_wide$claim_type <- claim_info$type[match(trial_data_wide$claim, claim_info$id)]
   trial_data_wide$source <- source_info$source[match(trial_data_wide$claim, source_info$claimId)]
   
   trial_data_wide <- trial_data_wide %>%
     mutate(
       broad_claim_type = case_when(
         claim_type == "Unknowable Preference" | claim_type == "Unknowable Expert" ~ "Unknowable",
         claim_type == "Knowable Facts" | claim_type == "Knowable Eyewitness" ~ "Knowable "
       )
     )
   
             
    
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
    
    # get demographic questions
    if (session_number == 1){
      demographics <- select(responses_one_off, starts_with("demographics"))
      # one participant's demographics aren't saving properly
      if (!is.null(all_demographics)){
        dem_names <- colnames(all_demographics)
      if(ncol(demographics) != length(dem_names) - 2){
        #browser(expr = {i == 328})
        print("Error recording demographic data")
        demographics <- t(as.data.frame(rep(NA, length(dem_names)-2)))
        colnames(demographics) <- dem_names[1:(length(dem_names)-2)]
      }
      }
      demographics = as.data.frame(demographics)
      demographics$ANON_PID <- prolific_id
      # empty subject number column
      demographics$participant <- NA
      rownames(demographics) <- NULL
      all_demographics <- rbind(all_demographics, demographics) 
      
    }
    
    # match the number of rows to config data so they can be combined
    responses_one_off <- responses_one_off[rep(seq_len(nrow(responses_one_off)), length.out = nrow(config_data)), ]
    

    
    data_all <- cbind(responses_one_off, trial_data_wide, config_data)
    data_all <- data_all %>%
      mutate(total_duration = as.numeric(experimentEndTime)/60000,
             post = as.numeric(`slider-post`),
             pre = as.numeric(`slider-pre`),
             post_adjusted = ifelse(side_A == "con"  & consensus != "contested", (100-post), post),
             pre_adjusted = ifelse(side_A == "con"  & consensus != "contested", (100-pre), pre),
             # calculate whether people 'were originally for/against a given claim's prior originally matched or went against the consensus 
             original_stance_against_consensus = case_when(pre < 50 & side_A == "pro" & consensus != "contested" ~ T,
                                                 pre > 50 & side_A == "con" & consensus != "contested" ~ T,
                                                 pre < 50 & consensus == "contested" ~ T,
                                                 TRUE ~ F
                                                   ),
             original_stance_against_claim = ifelse(pre < 50, T,F),
             original_stance_claim = case_when(
               pre < 50 ~ "against",
               pre == 50 ~ "equal",
               pre > 50 ~ "for"
             ),
             changed_mind = case_when(
               pre < 50 & post > 50 ~ T,
               pre > 50 & post < 50 ~ T,
               pre == 50 ~ NA,
               TRUE ~ F
             ),
             update = post_adjusted - pre_adjusted
      )
    # remove unnecessary columns
    data <- data_all %>%
      select(
        ANON_PID,
        session_number,
        claim_set,
        claim,
        claim_type,
        broad_claim_type,
        source,
        pre,
        post,
        original_stance_against_consensus,
        original_stance_against_claim,
        original_stance_claim,
        changed_mind,
        pre_adjusted,
        post_adjusted,
        update,
        consensus,
        side_A,
        side_B,
        nSources_A,
        nSources_B,
        prop_pro,
        trial_accuracy,
        total_duration,
        stances,
        tweetOrder,
        index
      )
  all_data <- rbind(all_data, data)
  # keep a version of this without any removals
  all_data_nr <- all_data
  }
  
  # get all of the unique prolific id's
  all_prolific_ids <- unique(all_data$ANON_PID)
  print(paste0("all subjects: ", length(all_prolific_ids)))
  # for each unique participant, filter data into their own data set 
  for (i in 1:length(all_prolific_ids)){
    print(i)
    # get the prolific id
    id <- all_prolific_ids[i]
    # filter data for that participant
    participant_data <- all_data[all_data$ANON_PID == id,]
    # get the number of trials for that participant
    n_trials <- nrow(participant_data)
    # get the mean accuracy for that participant
    mean_accuracy <- mean(participant_data$trial_accuracy)
    
    # if mean accuracy less than .9, add to rm_accuracy df
    if (mean_accuracy < .9){
     # rm_accuracy <- rbind(rm_accuracy, c(mean_accuracy, id))
    }

    if (all_complete & n_trials == 30){
      rm_drop_out <- c(rm_drop_out, id)
    }

    colnames(follow_ups) <- c("prolific_id", "self_report_strategy", "self_report_free")
    
    if (all_complete){
      if (n_trials == 30 | mean_accuracy < .9){
        # remove participants who didn't complete the experiment or whose accuracy was below 90%
        all_data <- all_data[all_data$ANON_PID != id,]
        all_demographics <- all_demographics[all_demographics$ANON_PID != id,]
        follow_ups <- follow_ups[follow_ups[,"prolific_id"] != id,]

        # do not output data for participants who didn't complete the experiment
        next
      }
    
    # get participant number
    running_subject <- running_subject +1
    participant <- as.character(running_subject)
    
    # add to data
    participant_data$participant <- participant
    all_demographics[all_demographics[,"ANON_PID"] == id, "participant"] <- participant

    # move participant column to front
    participant_data <- participant_data[c("participant", names(participant_data)[-which(names(participant_data) == "participant")])]

    # save participant data
    save(participant_data, file = here(paste0("data/clean/P",participant, ".Rdata")))
    
    # save csv version as per reviewer request
    write.csv(participant_data, file = here(paste0("data/clean/csv_version/P",participant, ".csv")))
    

    }
  }

  if (all_complete){
    colnames(follow_ups) <- c("ANON_PID", "self_report_strategy", "free_text")
    follow_ups <- as.data.frame(follow_ups)
  }
  # add numeric id column for each participant
  all_data$participant <- as.numeric(factor(all_data$ANON_PID, levels = unique(all_data$ANON_PID)))
  
  # do the same for  follow up questions, making sure that it matches the behavioural data. 
  follow_ups$participant <-  as.numeric(factor(follow_ups$ANON_PID, levels = unique(all_data$ANON_PID)))
  
  # Reordering the dataframe by 'id' column
  all_data <- all_data[order(all_data$participant), ]
  follow_ups <- follow_ups[order(follow_ups$participant),]
  
  # categorize political identities
  pol_left <- c("modLib", "slightLib", "strongLib", "soc", "anarch")
  pol_right <- c("modCons", "slightCons", "strongCons")
  
  
  # transform demographic variables
  all_demographics <- all_demographics %>%
    mutate(
      political_group = case_when(demographics_politics %in% pol_left ~ "Left wing",
                                  demographics_politics %in% pol_right ~ "Right wing",
                                  demographics_politics == "middle" ~ "Centre"),
      # convert politics scale from far left to far right
      political_scale = case_when(
        demographics_politics %in% c("soc", "anarch") ~ -4,
        demographics_politics == "strongLib" ~ -3,
        demographics_politics == "modLib" ~ -2,
        demographics_politics == "slightLib" ~ -1,
        demographics_politics == "middle" ~ 0,
        demographics_politics == "slightCons" ~ 1,
        demographics_politics == "modCons" ~ 2,
        demographics_politics == "strongCons" ~ 3,
      ),
      # convert into absolute strength of politcal belief 
      political_strength = abs(political_scale),
      # convert social media response into *roughly* how much each person uses it each year based on their response
      twitter_proportionate = case_when(
        demographics_twitter == "daily" ~ 365,
        demographics_twitter == "weekly" ~ 52,
        demographics_twitter == "monthly" ~ 12,
        demographics_twitter == "rarely" ~ 6,
        demographics_twitter == "never" ~ 0,
      ),
      facebook_proportionate = case_when(
        demographics_facebook == "daily" ~ 365,
        demographics_facebook == "weekly" ~ 52,
        demographics_facebook == "monthly" ~ 12,
        demographics_facebook == "rarely" ~ 6,
        demographics_facebook == "never" ~ 0,
      ),
      socmed_proportionate = twitter_proportionate + facebook_proportionate,
      university_education = ifelse(demographics_education %in% c("masters", "doctorate", "bachelors"), T,F)
      )
  
  # Moving the 'id' column to the front
  all_data <- all_data[c("participant", names(all_data)[-which(names(all_data) == "participant")])]
  follow_ups <- follow_ups[c("participant", names(follow_ups)[-which(names(follow_ups) %in% c("participant", "ANON_PID"))])]
  #print(id_sesh)
  list(
    data = all_data,
    rm_accuracy = rm_accuracy,
    rm_drop_out = rm_drop_out,
    follow_up = follow_ups,
    demographics = all_demographics,
    no_removals = all_data_nr,
    rm_accuracy_s1 = rm_accuracy_s1,
    all_prolific_unfiltered = all_prolific_unfiltered )
  
}

# indicate whether 
all_complete <- TRUE

all_data <- cleaning_fun(d_json, all_complete = all_complete)
save(all_data, file = here("data/clean/all_data_clean.Rdata"))

# save the just the data for all participants as a csv as requested by reviewer. 
write.csv(all_data$data, file = here("data/clean/csv_version/all_data_clean.csv"))


just_data <- all_data[[1]]
accuracy_data <- all_data[[2]]
# total participants removed for accuracy
rm_accuracy <- length(unique(accuracy_data[,2]))
rm_accuracy_ids <- unique(accuracy_data[,2])
rm_accuracy_string <- paste(rm_accuracy_ids, collapse = ",")

total_prolific_unfiltered <- length(unique(all_data$all_prolific_unfiltered))
paste0("Total unique participants recruited from prolific before filtering: ", total_prolific_unfiltered)



paste0(rm_accuracy, " total participants removed for accuracy < .9")

# participants removed for accuracy in s1 ad not invited back
rm_acc_s1 <- unique(all_data$rm_accuracy_s1[,2])
paste0(length(rm_acc_s1), " participants removed for accuracy < .9 in session 1 and not invited back")

rm_accuracy_s2 <- all_data$rm_accuracy[all_data$rm_accuracy[,3] == "2",2]
rm_accuracy_2[rm_accuracy_2 %in% rm_acc_s1]

# find out who did not complete s2 of the participants who were invited to return from s1

rm_drop_out_ids <- unique(all_data$rm_drop_out)
rm_drop_out_s2 <- rm_drop_out_ids[!rm_drop_out_ids %in% rm_acc_s1]
rm_drop_out <- length(rm_drop_out_s2)


paste0(rm_drop_out, " participants removed for failing to complete session 2")
follow_ups <- all_data[[4]]
paste0(length(unique(just_data$ANON_PID)), " participants remain after cleaning.")

time <- just_data %>%
  group_by(ANON_PID) %>%
  summarise(time = median(total_duration))

table(all_data$demographics$demographics_gender)

median_time <- median(time$time)

paste0("It took participants a median of ", median_time, " minutes to complete the study")

## Ee Von Put Extra Demographics Below
demographics <- all_data$demographics

checkParticipant = function(id){
  d <- just_data %>%
    filter(ANON_PID == id)
  d
}
#check <- checkParticipant("656f042cc31f84842ad587e4")



