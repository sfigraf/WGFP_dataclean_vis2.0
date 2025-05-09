###station data is originally made in GIS from the all_events dataset that has been filtered on distinct TAG, Event, UTM_X, UTM_Y, and Date

### this function is up to date with the new antennas
# # #all_events is needed in this dataset because the returning df has station data from all pertinent events
#All_events <- df_list$All_Events #comes from WFGFP encounter histories function.

# station data comes from spatial join function
#you can get station info for all antennas from this file, since it's the result of the spatial join
# stations are manually assigned later just in case so that's why this is important
#station_data <- Stationdata1

# station_data1 <- spatial_join_stations_detections(df_list$All_Events_most_relevant, simple_stations2)
# station_data <- as.data.frame(station_data1)
#stations
# dam is listed at 8330
#b3 is 8190

combine_events_and_stations <- function(All_events, station_data){
  
  # the file takes a condensed verson of all_events filtered on distinct()
  
  start_time <- Sys.time()
  print("Running Combine_events_and_stations function: Combining all events with station data.")
  # Combining stations into all_events dataset ------------------------------
  # takes awhile to join data... what if we gave it hte distinct()) smaller file then added it 
  # this part exists to
  #just getting distinct rows makes joining easier; all we need from this df is stations
  stations <- station_data %>%
    distinct(UTM_X, UTM_Y, .keep_all = TRUE)  #can't do it by ET_station because Sherriff ranch upper field and fraser river ranch the same station initially
    # select(-Date_)
  
  
  #massive datafrmae occurs when there are multiple rows in B for which the key columns (same-name columns by default) match the same, single row in A
  #usually this means you have to make sure you join by the fields which will not have any differenitation: iun this case, "TAG", UTM_X", "UTM_Y", and "Event". The other fields are just to help keep the dataframe more concise
  # date doesn't matter toi join; no matter what day the detection/event happens, the station is the same depending on UTM
  #because the stations are added by joining these columns instead of site, 
  
  
  all_events_stations_2 <- left_join( All_events,stations, by = c("UTM_X", "UTM_Y")) # "Species", "Release_Length", "Release_Weight", "Event", "Date", "Time", "ReleaseSite", "Release_Date", "RecaptureSite", "Recap_Length", "Recap_Weight"
  
  All_events_stations_3 <- all_events_stations_2 %>%
    
    rename(
      TAG = TAG.x,
      Date = Date.x,
      Time = Time.x,
      Datetime = Datetime.x,
      Event = Event.x,
      Species = Species.x,
      Release_Length = Release_Length.x,
      Release_Weight = Release_Weight.x, 
      ReleaseSite = ReleaseSite.x,
      Release_Date = Release_Date.x,
      RecaptureSite = RecaptureSite.x,
      Recap_Length = Recap_Length.x,
      Recap_Weight = Recap_Weight.x) %>%
    
    mutate(
      #River also needs to be assigned for new detections 
      River = case_when(
        (Event %in% c("RB1", "RB2")) ~ "Colorado River", # there is no is.na here because RB UTM
        (Event %in% c("HP3", "HP4")) ~ "Colorado River",
        (Event %in% c("CF5", "CF6")) ~ "Colorado River",
        (Event %in% c("CD7", "CD8", "CD9", "CD10", "CU11", "CU12")) ~ "Connectivity Channel",
        (Event %in% c("B3", "B5")) ~ "Colorado River",
        (Event %in% c("B4", "B6")) ~ "Fraser River",
        TRUE ~ River
      ),
      # this part is needed because stations are assigned from 0 up the fraser river starting at the confluence
      #new antennas weren't showing up because I didn't include connectivity channel to to river
      # this assigns a station, then in the get_movements function the distance moved is calculated
      ET_STATION = case_when(River %in% "Fraser River" ~ ET_STATION + 10120, #10120 is above Fraser River Confluence; pre-construciton was 9566
                             River %in% c("Colorado River", "Connectivity Channel") ~ ET_STATION,
                             TRUE ~ ET_STATION)
    ) %>%
    # this line just makes the df smaller if htere are duplicates; usually doesn't change anything since All_events has a line that does this also in the WGFP ENC hist_function
    distinct(Datetime, Event, TAG, .keep_all = TRUE) %>%
    select(Date, Time, Datetime, TAG, Event, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, Recap_Length, Recap_Weight, UTM_X, UTM_Y, ET_STATION)
  
  
  # Days_since and Prev_event -----------------------------------------------
  
  # making these columns prepares the data for making states and pivoting wider to days/weeks
  All_events_days <- All_events_stations_3 %>%
    mutate(
      days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
      #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
      # if you want to start at week 1 instead of week 0, add +1 to the end of expression
      # when you change this too, it changes the number of entries in the states dataframe
           weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks")))
    )
  
  #getting all_events down to most essential info: how a unique fish/Tag began the day, how it ended the day, and if there were events different than that in between
  # this gets put into get_movements_function, ind_tag_enc_hist_summary_wide, and get_states
  All_events_days1 <- All_events_days %>%
    
    group_by(Date, TAG) %>%
    mutate(first_last = case_when(Datetime == min(Datetime) & Event != "Release" ~ "First_of_day",
                                  Datetime == max(Datetime) ~ "Last_of_day",
                                  Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0",
                                  #Event == "Release" ~ "Last_of_day"
    ),
    c_number_of_detections = n(),
    daily_unique_events = length(unique(Event))
    ) %>%
    ungroup() %>%
    #getting all_events down to most essential info: how a unique fish/Tag began the day, how it ended the day, and if there were events different than that in between
    
    distinct(TAG, Event, Date, first_last, UTM_X, UTM_Y, .keep_all = TRUE) %>%
    
    group_by(TAG) %>%
    mutate(
      #this is used in getting states; not sure if needed anymore
      previous_event = lag(Event, order_by = Datetime),
      #this part is used in movemnets map
      det_type = case_when(str_detect(Event, "RB1|RB2") ~ "Red Barn Stationary Antenna",
                           str_detect(Event, "HP3|HP4") ~ "Hitching Post Stationary Antenna",
                           str_detect(Event, "CF5|CF6") ~ "Confluence Stationary Antenna",
                           str_detect(Event, "CD7|CD8|CD9|CD10") ~ "Connectivity Channel Downstream Stationary Antenna", #Caused by error in `"CD7|CD8" | "CD9"`: solved because quotation marks in the worng places
                           str_detect(Event, "CU11|CU12") ~ "Connectivity Channel Upstream Stationary Antenna",
                           str_detect(Event, "B3") ~ "Windy Gap Dam Biomark Antenna",
                           str_detect(Event, "B4") ~ "Kaibab Park Biomark Antenna",
                           str_detect(Event, "B5") ~ "River Run Biomark Antenna",
                           str_detect(Event, "B6") ~ "Fraser River Canyon Biomark Antenna",
                           str_detect(Event, "M1|M2") ~ "Mobile Run",
                           Event == "Recapture" ~ "Recapture",
                           TRUE ~ Event),
      #need to check this function out given new stationing and connectivity channel
      above_below = case_when(
        ET_STATION >= DamLocation ~ "Above the Dam",
        ET_STATION < DamLocation ~ "Below the Dam"
      )
      
    ) %>%
    
    select(Date, Datetime, TAG, Event, det_type, ReleaseSite,Species, Release_Length, Release_Weight, Release_Date, RecaptureSite, River, days_since, weeks_since, first_last, previous_event,  c_number_of_detections, daily_unique_events, ET_STATION, above_below, UTM_X, UTM_Y) #next_event, next_event_2, same_day_next_events,
  
  
  end_time <- Sys.time()
  print(paste("Combine Events and Stations Function took", round(end_time-start_time,2), "Seconds"))
  
  return(All_events_days1)
}

#combined_events_stations <- combine_events_and_stations(All_events, station_data)
