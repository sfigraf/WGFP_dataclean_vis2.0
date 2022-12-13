### new states function
#x <- states_function(combined_events_stations)
states_function <- function(combined_events_stations, ghost_tag_df) {
  start_time = Sys.time()
  print("Running States Function: Assigns letters A, B, C, or G based on position relative to dam, or Ghost/predated tag.")
  
  wghost_date <- left_join(combined_events_stations, ghost_tag_df, by = c("TAG"))
  
  #daily_unique_events = length(unique(Event))
  states1 <- wghost_date %>%
    #group_by(weeks_since) %>%
    mutate(
      #the case_whens also are a priority list, so important not to rearange these 
      state1 = case_when(Date >= Ghost_date ~ "G",
                         str_detect(Event, "CD7|CD8|CD9|CD10|CU11|CU12") ~ "C",
                         ET_STATION <= 8330 ~ "A",
                         ET_STATION > 8330 ~ "B")
    )
  
  states2 <- states1 %>%
    group_by(weeks_since, TAG) %>%
    #arranging my datetime ensures that all states will be recorded in the correct order
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
  ## pivot wider
  #days <- data.frame(days_since = 1:max(states_final$days_since))
  weeks <- data.frame(weeks_since = 1:max(states_final$weeks_since))
  
# Pivot Wide --------------------------------------------------------------

  
  # days_and_states <- full_join(days, states_final, by = "days_since")
  # 
  # 
  # days_and_states_wide <- pivot_wider(days_and_states, id_cols = TAG, names_from = days_since, values_from = State)
  # 
  # days_and_states_wide <- days_and_states_wide %>%
  #   select(TAG, `0`, 2:ncol(days_and_states_wide))
  
  weeks_and_states <- full_join(weeks, states_final, by = "weeks_since")
  
  
  weeks_and_states_wide <- pivot_wider(weeks_and_states, id_cols = TAG, names_from = weeks_since, values_from = State)
  
  weeks_and_states_wide <- weeks_and_states_wide %>%
    select(TAG, `0`, 2:ncol(weeks_and_states_wide))
  

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
  
  states_df_list <- list("All_States" = states_final, "Weeks_and_states_wide" = weeks_and_states_wide, "Flagged_movements" = unknown_states)
  end_time <- Sys.time()
  print(paste("States Function took", round(end_time-start_time,2)))
  
  return(states_df_list)
  
}

