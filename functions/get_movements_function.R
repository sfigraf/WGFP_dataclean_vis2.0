#TimePeriods <- wgfpMetadata$TimePeriods 
get_movements_function <- function(combined_events_stations, dailyUSGSData, eventsWithPeriodsSelect, TimePeriods) {
  start_time <- Sys.time()
  startMessage <- "Running get_movements_function: Calculates movements of fish based off a change in station."
  print(startMessage)
  
  dailyMovementsTableAll <- combined_events_stations %>%
    #### removing dummy tag
    filter(!TAG %in% c("230000999999")) %>%
    select(Date, Datetime, TAG, det_type, Event, ET_STATION, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, UTM_X, UTM_Y, first_last) %>%
    #grouping by TAG and arranging by datetime makes sure that total distance moved is totalled and summed in order
    group_by(TAG) %>%
    arrange(Datetime) %>%
    #dist_moved would be the place to fenagle new movements based on previous event...ie hitting wg biomark followed by connectivity channel = add 300 m
    #trickier but doable for mobile runs
    ### accounting for FRASER/UPPER COLROADO MOVEMENTS
    #if previous station is above the confluence and current station is above the confluence and you changed rivers, 
    #then take the previous station and subtract the confluence station to get distance travelled to the confluence (A), then subtract the new station minus confluence station to get distance travelled up the new river (B). Then add A + B to get total distance
    # otherwise, just subtract current station from previous
    mutate(dist_moved = case_when(lag(ET_STATION, order_by = Datetime) > fraserColoradoRiverConfluence & ET_STATION > fraserColoradoRiverConfluence & River != lag(River, order_by = Datetime) ~ (lag(ET_STATION, order_by = Datetime) - fraserColoradoRiverConfluence) + (ET_STATION - fraserColoradoRiverConfluence),
                                  TRUE ~ ET_STATION - lag(ET_STATION, order_by = Datetime)
    ),
    sum_dist = (sum(abs(dist_moved), na.rm = TRUE)),
    #learned that you need to specify units = "" not just provide the arguments
    MPerSecondBetweenDetections = ifelse(dist_moved == 0, 0, 
                             dist_moved/(as.numeric(difftime(Datetime, lag(Datetime), units = "secs")))
    ),
    
    movement_only = case_when(lag(ET_STATION, order_by = Datetime) > fraserColoradoRiverConfluence & ET_STATION > fraserColoradoRiverConfluence & River != lag(River, order_by = Datetime) ~ "Changed Rivers",
                              Event %in% c("Release", "Recapture and Release")  ~ "Initial Release",
                              dist_moved == 0 ~ "No Movement",
                              dist_moved > 0 ~ "Upstream Movement",
                              dist_moved < 0 ~ "Downstream Movement"),
    #this is for mapping later on
    #MARKERCOLOR options: limited because markers rely on static image
    #red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "white", "gray", "lightgray", "black"
    # these also correspond to the movements maps, so if you add or change a color you should change it on the movements map as well.
    marker_color = case_when(movement_only == "No Movement" ~ "black",
                             movement_only == "Upstream Movement" ~ "green",
                             movement_only == "Downstream Movement" ~ "red",
                             movement_only == "Initial Release" ~ "orange",
                             movement_only == "Changed Rivers" ~ "purple",
                             #str_detect(movement_only, "Initial Release (or recapture and release)") ~ "yellow"
    ),
    
    icon_color = case_when(str_detect(det_type, "Stationary Antenna") ~ "orange",
                           str_detect(det_type, "Biomark Antenna") ~ "yellow",
                           str_detect(det_type, "Mobile Run") ~ "purple",
                           det_type %in% c("Release", "Recapture and Release") ~ "blue",
                           det_type == "Recapture" ~ "brown",
    )
    ) 
  
  dailyMovementsTableMost <- dailyMovementsTableAll %>%
    #takes “movement” into account instead of “first_last”, this is why this df winds up with less rows than combined_events_stations
    distinct(Date, TAG, det_type, movement_only, UTM_X, UTM_Y, .keep_all = TRUE)
  ###upstream and downstream movements that are not captured in the above code: 
  #typically predated fish that have multipl US/DS movements in the same day to an antenna, but we have them just to be safe. 
  #FOr example: 230000144498 went to CU to CF to CU on the same day and this code captures that last movement to end at CU
  dailyMovementsEdgeCases <- dailyMovementsTableAll %>%
    distinct(Date, TAG, det_type, first_last, movement_only, UTM_X, UTM_Y, .keep_all = TRUE) %>%
    anti_join(dailyMovementsTableMost) %>%
    filter(movement_only %in% c("Upstream Movement", "Downstream Movement", "Changed Rivers"))
  
  dailyMovementsTable <- bind_rows(dailyMovementsTableMost, dailyMovementsEdgeCases)
  
  
  ##add on environmental Data
  dailyMovementsTable <- dailyMovementsTable %>%
    left_join(dailyUSGSData[,c("Date", "WtempF", "Flow")], by = "Date") %>%
    rename(USGSDischargeDaily = Flow)

  #avian Predation
  #red batr to confluence: 5950/60
  #rb to HP: 2190
  #hp to CF: 3760
  longMovements <- dailyMovementsTable %>%
    filter(abs(dist_moved) > 3700) %>%
    arrange(desc(abs(dist_moved))) %>%
    relocate(dist_moved, .after = TAG)
  #gets top 5% of speedy movements, US or DS
  fastMovements <- head(dailyMovementsTable[order(abs(dailyMovementsTable$MPerSecondBetweenDetections),decreasing=T),], .05*nrow(dailyMovementsTable))
  fastMovements <- fastMovements %>%
    relocate(MPerSecondBetweenDetections, .after = TAG)
  
  avianPredationDFs <- list(
    "longMovements" = longMovements,
    "fastMovements" = fastMovements
  )
  
  #######get lat/longs for plotting with leaflet
  #convert events to sf object
  #the utms are grs80 and utm zone 13, which corresponds to crs  32613
  dailyMovementsTableSF <- sf::st_as_sf(dailyMovementsTable, coords = c("UTM_X", "UTM_Y"), crs = 32613, remove = FALSE)
  #convert to lat/long
  dailyMovementsTableSFLatLong <- sf::st_transform(dailyMovementsTableSF, latLongCRS)
  
  coordinates <- st_coordinates(dailyMovementsTableSFLatLong)
  
  # Convert the coordinates to a data frame
  coordinatesDf <- as.data.frame(coordinates)
  # # Extract the lat/long coordinates
  dailyMovementsTableSFLatLong$X <- coordinatesDf$X
  dailyMovementsTableSFLatLong$Y <- coordinatesDf$Y
  
  ###get Time periods with movements: used to group together in animation
  #time periods comees from WGFP metadata;
  timePeriodsClean <- TimePeriods %>%
    mutate(`start date` = janitor::excel_numeric_to_date(as.numeric(`start date`)), 
           `end date` = janitor::excel_numeric_to_date(as.numeric(`end date`))
    )
  tagsEventsLongwithTpDates <- eventsWithPeriodsSelect %>%
    left_join(timePeriodsClean[,c("periods", "start date", "end date")], by = c("TimePeriod" = "periods")) %>%
    mutate(TimePeriodDates = paste(`start date`, "to", `end date`)) 
  
  #as of now, movement table still has rows reminiscent from first_last etc which are helpful when you want to know where it ended the day and stuff.
  #but if you want to know concise movments, then this will eliminate uneeded rows
  #example: 230000142723
  dailyMovementsTable1 <- as.data.frame(dailyMovementsTableSFLatLong) %>%
    left_join(tagsEventsLongwithTpDates, by = c("TAG", "Datetime", "Event")) %>%
    select(Date, Datetime, TAG, movement_only, det_type, dist_moved, MPerSecondBetweenDetections, sum_dist, 
           ET_STATION, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, 
           USGSDischargeDaily, WtempF, UTM_X, UTM_Y, X, Y, marker_color, icon_color, TimePeriod, TimePeriodDates, State)
  
  end_time <- Sys.time()
  endMessage <- paste("Movements Function took", round(difftime(end_time, start_time, units = "mins"),2), "minutes")
  print(endMessage)
  
  return(list("dailyMovementsTable1" = dailyMovementsTable1, 
              "message" = paste(c(startMessage, endMessage), collapse = "<br>"), 
              "avianPredationDFS" = avianPredationDFs)
         )
}

