
### this function is up to date with the new antennas

# station data comes from spatial join function

#stations
# dam is listed at DamLocation
# DailyMovements_withStations <- DailyDetectionsStationsStates$spatialList$stationData
# x <- PrepareforStatesMovementsandSummary(DailyMovements_withStations)
PrepareforStatesMovementsandSummary <- function(DailyMovements_withStations){
  
  DailyMovements_withStationsFraserColoradoCorrected <- DailyMovements_withStations %>%
    left_join(wgfpMetadata$AntennaMetadata[,c("FrontendSiteCode", "River")], by = c("Event" = "FrontendSiteCode")) %>%
    #selects first non-NA value from set of columns; by having River.Y first it prioritizes that column
    mutate(River = coalesce(River.y, River.x), 
           # this part is needed because stations are assigned from 0 up the fraser river starting at the confluence
           #new antennas weren't showing up because I didn't include connectivity channel to to river
           # this assigns a station, then in the get_movements function the distance moved is calculated
           ET_STATION = case_when(River %in% "Fraser River" ~ ET_STATION + fraserColoradoRiverConfluence, #10120 is above Fraser River Confluence; pre-construciton was 9566
                                  River %in% c("Colorado River", "Connectivity Channel") ~ ET_STATION,
                                  TRUE ~ ET_STATION)
           ) %>%
    select(-River.x, -River.y)
  
      
  # making these columns prepares the data for making states and pivoting wider to days/weeks
  # DailyMovements_withStationsDaysSince <- DailyMovements_withStationsFraserColoradoCorrected %>%
  #   mutate(
  #     days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
  #     #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
  #     # if you want to start at week 1 instead of week 0, add +1 to the end of expression
  #     # when you change this too, it changes the number of entries in the states dataframe
  #     weeks_since = as.numeric(floor(difftime(Datetime, min(Datetime), units = "weeks")))
  #   )
  
  #getting number of daily events
  DailyMovements_withStationsNumberofDailyEvents <- DailyMovements_withStationsFraserColoradoCorrected %>%
    group_by(Date, TAG) %>%
    mutate(c_number_of_detections = n(),
           daily_unique_events = length(unique(Event))
    ) %>%
    ungroup()
  #generating generic event title for movements map
  
  ####can join this
  DailyMovements_withStationsAndDetectionType <- DailyMovements_withStationsNumberofDailyEvents %>%
    left_join(wgfpMetadata$AntennaMetadata[,c("FrontendSiteCode", "SiteName")], by = c("Event" = "FrontendSiteCode")) %>%
    mutate(det_type = coalesce(SiteName, Event), 
           #in this instance, "above the dam" would be mean the CRCC too; any detection in it
           above_below = case_when(
             ET_STATION >= DamLocation ~ "Above the Dam",
             ET_STATION < DamLocation ~ "Below the Dam"
           )
    )
  
  # Transform the coordinates back to UTM
  sf_object_utm <- st_transform(DailyMovements_withStationsAndDetectionType, crs = 32613)  # Assuming UTM zone 13 with GRS80
  
  
  coordinates <- st_coordinates(sf_object_utm)
  
  # Convert the coordinates to a data frame
  coordinatesDf <- as.data.frame(coordinates)
  
  # Rename the columns
  colnames(coordinatesDf) <- c("UTM_X", "UTM_Y")
  # # Extract the UTM_X and UTM_Y coordinates
  DailyMovements_withStationsAndDetectionType$UTM_X <- round(coordinatesDf$UTM_X, 0)
  DailyMovements_withStationsAndDetectionType$UTM_Y <- round(coordinatesDf$UTM_Y, 0)

  #need to convert class sf object back to dataframe so that it processes faster in combine_events_stations_function
  DailyMovements_withStationsAndDetectionType <- as.data.frame(DailyMovements_withStationsAndDetectionType)
  DailyMovements_withStationsAndSummaryInfo <- DailyMovements_withStationsAndDetectionType %>%
    select(Date, Datetime, TAG, Event, det_type, ReleaseSite,Species, Release_Length, Release_Weight, Release_Date, RecaptureSite, River, first_last, c_number_of_detections, daily_unique_events, ET_STATION, above_below, UTM_X, UTM_Y) #next_event, next_event_2, same_day_next_events,
  
  return(DailyMovements_withStationsAndSummaryInfo)
}

