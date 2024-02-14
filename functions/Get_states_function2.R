### new states function
# AvianPredation <- AvianPredation %>%
#   mutate(PredationDate = mdy(PredationDate))
# 
# GhostTags <- GhostTags %>%
#   mutate(GhostDate = mdy(GhostDate)) 
#x <- states_function(combined_events_stations)
states_function <- function(combined_events_stations, GhostTags, AvianPredation) {
  start_time = Sys.time()
  print("Running States Function: Assigns letters A, B, C, or G based on position relative to dam, or Ghost/predated tag.")
  # these dates are cleaned before they go into this function
  ghost_tag_df <- GhostTags %>%
    rename(TAG = TagID) %>%
    select(TAG, GhostDate) 
  
  av_pred_df <- AvianPredation %>%
    rename(TAG = TagID) %>%
    select(TAG, PredationDate)
    
  #joining with ghost tag df
  wghost_date <- left_join(combined_events_stations, ghost_tag_df, by = c("TAG"))
  # joining with avian predation
  #Shouldn't matter really, but this just cuts down unnecesary rows. left_joining creates more rows than df1 if there are duplicate values in the key_col of df1 (in this case TAG)
  #some new rows are created then when joining to the ghost tag df because many of the ghost tags have multiple detections after the ghost date in the combined_df
  # Im'm just cutting out these excess rows but they would get cut down anyway later in this function. As long as each ghost tag gains a row with ghost date or predation date, that's all that matters
  wghost_av <- left_join(wghost_date, av_pred_df, by = c("TAG")) %>%
    distinct(TAG, Event, Datetime, UTM_X, UTM_Y, first_last, .keep_all = TRUE)
  
  
  #daily_unique_events = length(unique(Event))
  states1 <- wghost_av %>%
    filter(!TAG %in% c('230000999999')) %>%
    mutate(
      #the case_whens also are a priority list, so important not to rearange these 
      #might have to readjust 8330 stationing
      state1 = case_when(Date >= GhostDate ~ "G",
                         Date >= PredationDate ~ "P",
                         str_detect(Event, "CD1|CD2|CS1|CS2|CU1|CU2") ~ "C",
                         ET_STATION <= 8330 ~ "A",
                         ET_STATION > 8330 ~ "B")
    )
  
  states2 <- states1 %>%
    group_by(weeks_since, TAG) %>%
    arrange(Datetime) %>%
    mutate(
      teststate_2 = paste(state1, collapse = ""),
      teststate_3 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2), #removes consecutive letters
      weekly_unique_events = length(unique(Event))
    )
 
  
  #this is now a weekly chart
  states_final <- states2 %>%
    distinct(weeks_since, TAG, teststate_3, .keep_all = TRUE) %>%
    select(Date, weeks_since, TAG, teststate_3, det_type, ReleaseSite, Species, Release_Length, Release_Weight, c_number_of_detections, weekly_unique_events, days_since, UTM_X, UTM_Y) %>%
    rename(State = teststate_3)
  
  # this makes some columns from all states of fish 
  states_summarized <- states1 %>%
    group_by(TAG) %>%
    arrange(Datetime) %>%
    mutate(teststate_2 = paste(state1, collapse = ""),
           teststate_3 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2), #removes consecutive letters
           #new columns to sa yif fish stayed above or below?
           went_above_dam_noChannel = str_detect(teststate_3, "AB"),
           went_below_dam_noChannel = str_detect(teststate_3, "BA"),
           went_below_dam_throughChannel = str_detect(teststate_3, "BCA"),
           went_above_dam_throughChannel = str_detect(teststate_3, "ACB"),
           entered_channel_from_DS = str_detect(teststate_3, "AC"),
           entered_channel_from_US = str_detect(teststate_3, "BC"),
           
    ) %>%
    select(TAG, went_above_dam_noChannel, went_below_dam_noChannel,went_below_dam_throughChannel,went_above_dam_throughChannel,entered_channel_from_DS,entered_channel_from_US) %>%
    distinct(TAG, .keep_all = TRUE)
  
# Flagged Tags ------------------------------------------------------------

  #should we put states in this that have multiple letters?
  checking <- states_final %>%
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
  
  states_df_list <- list("All_States" = states_final, "Flagged_movements" = unknown_states, "States_summarized" = states_summarized)
  end_time <- Sys.time()
  print(paste("States Function took", round(end_time-start_time,2)))
  
  return(states_df_list)
  
}

