
### this function is up to date with the new antennas

# station data comes from spatial join function

# station_data1 <- spatial_join_stations_detections(df_list$All_Events_most_relevant, simple_stations2)
# station_data <- as.data.frame(station_data1)
#stations
# dam is listed at DamLocation
#b3 is 8190

PrepareforStatesMovementsandSummary <- function(DailyMovements_withStations){
  DailyMovements_withStations <- DailyMovements_withStations %>%
    mutate(
      #River also needs to be assigned for new detections 
      River = case_when(
        (Event %in% c("RB1", "RB2")) ~ "Colorado River", # there is no is.na here because RB UTM
        (Event %in% c("HP3", "HP4")) ~ "Colorado River",
        (Event %in% c("CF5", "CF6")) ~ "Colorado River",
        (Event %in% c("CD1", "CD2", "CS1", "CS2", "CU1", "CU2")) ~ "Connectivity Channel",
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
    ) 
  # making these columns prepares the data for making states and pivoting wider to days/weeks
  DailyMovements_withStations <- DailyMovements_withStations %>%
    mutate(
      days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
      #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
      # if you want to start at week 1 instead of week 0, add +1 to the end of expression
      # when you change this too, it changes the number of entries in the states dataframe
      weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks")))
    )
  
  #getting number of daily events
  DailyMovements_withStations <- DailyMovements_withStations %>%
    group_by(Date, TAG) %>%
    mutate(c_number_of_detections = n(),
           daily_unique_events = length(unique(Event))
    ) %>%
    ungroup()
  #generating generic event title for movements map
  DailyMovements_withStations <- DailyMovements_withStations %>%
    mutate(
      #this part is used in movemnets map
      det_type = case_when(str_detect(Event, "RB1|RB2") ~ "Red Barn Stationary Antenna",
                           str_detect(Event, "HP3|HP4") ~ "Hitching Post Stationary Antenna",
                           str_detect(Event, "CF5|CF6") ~ "Confluence Stationary Antenna",
                           str_detect(Event, "CD1|CD2") ~ "Connectivity Channel Downstream Stationary Antenna",
                           str_detect(Event, "CS1|CS2") ~ "Connectivity Channel Side Channel Stationary Antenna", #Caused by error in `"CD7|CD8" | "CD9"`: solved because quotation marks in the worng places
                           str_detect(Event, "CU1|CU2") ~ "Connectivity Channel Upstream Stationary Antenna",
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
      
    ) 
  
  # Transform the coordinates back to UTM
  sf_object_utm <- st_transform(DailyMovements_withStations, crs = 32613)  # Assuming UTM zone 13 with GRS80
  
  
  coordinates <- st_coordinates(sf_object_utm)
  
  # Convert the coordinates to a data frame
  coordinatesDf <- as.data.frame(coordinates)
  
  # Rename the columns
  colnames(coordinatesDf) <- c("UTM_X", "UTM_Y")
  
  DailyMovements_withStations$UTM_X <- coordinatesDf$UTM_X
  DailyMovements_withStations$UTM_Y <- coordinatesDf$UTM_Y
  # # Extract the UTM_X and UTM_Y coordinates
  # DailyMovements_withStations$UTM_X <- x <- as.data.frame(as.numeric(st_coordinates(DailyMovements_withStations)))["X"]
  # DailyMovements_withStations$UTM_Y <- as.data.frame(as.numeric(st_coordinates(DailyMovements_withStations)))["Y"]
  # 
  # x <- DailyMovements_withStations %>%
  #   dplyr::rename( UTM_X =`UTM_X$X`)
  
  # # Convert the coordinates to a data frame
  # df <- as.data.frame(coordinates)
  # 
  # # Rename the columns
  # colnames(df) <- c("UTM_X", "UTM_Y")
  #need to convert class sf object back to dataframe so that it processes faster in combine_events_stations_function
  DailyMovements_withStations <- as.data.frame(DailyMovements_withStations)
  DailyMovements_withStations <- DailyMovements_withStations %>%
    select(Date, Datetime, TAG, Event, det_type, ReleaseSite,Species, Release_Length, Release_Weight, Release_Date, RecaptureSite, River, days_since, weeks_since, first_last, c_number_of_detections, daily_unique_events, ET_STATION, above_below, UTM_X, UTM_Y) #next_event, next_event_2, same_day_next_events,
  
  return(DailyMovements_withStations)
}

