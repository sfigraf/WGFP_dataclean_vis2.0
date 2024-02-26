### states function
states_function <- function(combined_events_stations, GhostTags, AvianPredation) {
  start_time = Sys.time()
  print("Running States Function: Assigns letters A, B, C, or G based on position relative to dam, or Ghost/predated tag.")
  
  #checking if ghost tags have 1 tag entry for each
  problemGhostTags <- GhostTags %>%
    count(TagID) %>%
    filter(n > 1)
  if(nrow(problemGhostTags) > 0){
    print(paste0("The following tags in the Ghost Tag dataframe have multiple entries in the Ghost Tag dataframe: ", unique(problemTags$TagID),
                 ". Please adjust this in the original df, otherwise there will be a left_join() warning."))
  }
  
  # these dates are cleaned before they go into this function
  
  GhostTagsForJoining <- GhostTags %>%
    rename(TAG = TagID) %>%
    select(TAG, GhostDate) 
  
  AvianPredationForJoining <- AvianPredation %>%
    rename(TAG = TagID) %>%
    select(TAG, PredationDate)
    
  #joining with ghost tag df
  eventsWithGhostDates <- left_join(combined_events_stations, GhostTagsForJoining, by = c("TAG"))
  # joining with avian predation
  #Shouldn't matter really, but this just cuts down unnecesary rows. left_joining creates more rows than df1 if there are duplicate values in the key_col of df1 (in this case TAG)
  #some new rows are created then when joining to the ghost tag df because many of the ghost tags have multiple detections after the ghost date in the combined_df
  # I'm just cutting out these excess rows but they would get cut down anyway later in this function. As long as each ghost tag gains a row with ghost date or predation date, that's all that matters
  eventsWithGhostDatesAndAvianPredation <- left_join(eventsWithGhostDates, AvianPredationForJoining, by = c("TAG")) %>%
    distinct(TAG, Event, Datetime, UTM_X, UTM_Y, first_last, .keep_all = TRUE)
  
  
  #daily_unique_events = length(unique(Event))
  states <- eventsWithGhostDatesAndAvianPredation %>%
    filter(!TAG %in% c('230000999999')) %>%
    mutate(
      #the case_whens also are a priority list, so important not to rearange these 
      
      state = case_when(Date >= GhostDate ~ "G",
                         Date >= PredationDate ~ "P",
                        Event %in% ConnectivityChannelCodes ~ "C",
                         ET_STATION <= DamLocation ~ "A",
                         ET_STATION > DamLocation ~ "B")
    )
  
  weeklyStates <- states %>%
    group_by(weeks_since, TAG) %>%
    arrange(Datetime) %>%
    mutate(
      allWeeklyStates = paste(state, collapse = ""),
      condensedWeeklyStates = gsub('([[:alpha:]])\\1+', '\\1', allWeeklyStates), #removes consecutive letters
      weekly_unique_events = length(unique(Event))
    )
 
  
  #this is now a weekly chart
  cleanedWeeklyStates <- weeklyStates %>%
    distinct(weeks_since, TAG, condensedWeeklyStates, .keep_all = TRUE) %>%
    select(Date, weeks_since, TAG, condensedWeeklyStates, det_type, ReleaseSite, Species, Release_Length, Release_Weight, c_number_of_detections, weekly_unique_events, days_since, UTM_X, UTM_Y) %>%
    rename(State = condensedWeeklyStates)
  
  # this makes some columns from all states of fish, detects certain letters/patterns, and tells if a fish EVER had certain patterns
  summarizedStates <- states %>%
    group_by(TAG) %>%
    arrange(Datetime) %>%
    mutate(allStates = paste(state, collapse = ""),
           condensedAllStates = gsub('([[:alpha:]])\\1+', '\\1', allStates), #removes consecutive letters
           #new columns to say if fish stayed above or below?
           went_above_dam_noChannel = str_detect(condensedAllStates, "AB"),
           went_below_dam_noChannel = str_detect(condensedAllStates, "BA"),
           went_below_dam_throughChannel = str_detect(condensedAllStates, "BCA"),
           went_above_dam_throughChannel = str_detect(condensedAllStates, "ACB"),
           entered_channel_from_DS = str_detect(condensedAllStates, "AC"),
           entered_channel_from_US = str_detect(condensedAllStates, "BC"),
           
    ) %>%
    select(TAG, went_above_dam_noChannel, went_below_dam_noChannel,went_below_dam_throughChannel,went_above_dam_throughChannel,entered_channel_from_DS,entered_channel_from_US) %>%
    distinct(TAG, .keep_all = TRUE)
  
# Flagged Tags ------------------------------------------------------------

  #should we put states in this that have multiple letters?
  checking <- cleanedWeeklyStates %>%
    group_by(TAG) %>%
    arrange(Date) %>%
    mutate(through_dam1 = case_when(det_type == "Release" ~ "Initial Release",
                                    str_sub(State,-1,-1) == "A" & lag(str_sub(State,-1,-1) %in% c("B", "C"), order_by = Date) ~ "Went Below Dam",
                                    str_sub(State,-1,-1) == "B" & lag(str_sub(State,-1,-1) %in% c("A", "C"), order_by = Date) ~ "Went Above Dam",
                                    State %in% c("BA", "CA", "BCA") ~ "Went Below Dam",
                                    State %in% c("AB", "CB", "ACB") ~ "Went Above Dam",
                                    State == lag(str_sub(State,-1,-1), order_by = Date) ~ "No state change",
                                    # TRUE ~ NA
                                    
    )
    )
  
  unknown_states <- checking %>%
    filter(is.na(through_dam1) & !det_type %in% c("Release", "Recapture and Release", "Recapture"))  
  
  states_df_list <- list("All_States" = cleanedWeeklyStates, "Flagged_movements" = unknown_states, "States_summarized" = summarizedStates)
  end_time <- Sys.time()
  print(paste("States Function took", round(end_time-start_time,2)))
  
  return(states_df_list)
  
}

