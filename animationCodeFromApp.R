#list of inputs
aggregator <- "First Movement"
TimeframeButtons <- "hours_since" #daySequence hours_since
anim_Caption <- "one tag"
endPauseValue = 5
anim_Title <- "230000294213"
facetWrapOption <- ""
fps_Select <- 10 #default is 10 as well
latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326
fraserColoradoRiverConfluence <- as.numeric(wgfpMetadata$ImportantStationingVariables[wgfpMetadata$ImportantStationingVariables$Variable == "Fraser/Colorado River Confluence", "StationingLocation"])

if(TimeframeButtons == "hours_since"){
  movementsOneTag <-  combinedData_df_list$All_Events %>% #movements_list$Movements_df
    filter(TAG == "230000294213", 
           Date >= as.Date("2025-10-05"))    
} else{
  movementsOneTag <- movements_list$Movements_df %>%
    filter(TAG == "230000294213", 
           Date >= as.Date("2025-10-05"))   
}



movementsWithTimeForFrames <- movementsOneTag %>%
  mutate(
    #currently days_since isn't being used since daySequence will show date when used in transition time than days_since
    #however it might be a lto slower to render so still keeping it in just in case
    days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
    #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
    # if you want to start at week 1 instead of week 0, add +1 to the end of expression
    weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))), 
    #hours not being used at all currently but could potentially be used in future. 
    #Doesn't make sense to use with movement data since it's subsetted by day but hours are used in bigAnimation on all data
    hours_since = as.numeric(floor(difftime(Datetime, min(Datetime), units = "hours"))),
    daySequence = as.Date(as.character(round(Datetime, units = "days"))),
  ) %>%
  # need to ungroup to get this code to work
  ungroup()


if(aggregator == "First Movement"){
  
  movementsWithTimeForFrames1 <- movementsWithTimeForFrames %>%
    group_by(TAG, !!sym(TimeframeButtons)) %>%
    filter(Datetime == first(Datetime)) %>%
    ungroup()
  
} else if(aggregator == "Last Movement"){
  movementsWithTimeForFrames1 <- movementsWithTimeForFrames %>%
    group_by(TAG, !!sym(TimeframeButtons)) %>%
    filter(Datetime == last(Datetime)) %>%
    ungroup()
} else{
  movementsWithTimeForFrames1 <- movementsWithTimeForFrames
}

if(TimeframeButtons == "hours_since"){
  #get movements
  LastEvent1 <- sf::st_as_sf(movementsWithTimeForFrames1, coords = c("UTM_X", "UTM_Y"), crs = 32613, remove = FALSE)
  #condensedEventsSF <- sf::st_as_sf(condensedEventsFiltered, coords = c("UTM_X", "UTM_Y"), crs = 32613)
  #convert to lat/long
  EventsSFLatLong <- sf::st_transform(LastEvent1, latLongCRS)
  #stattions needed to calculate movements and distance moved
  stationsAndEvents <- sf::st_join(EventsSFLatLong, simpleStations, st_nearest_feature)
  dailyMovementsTableAll <- stationsAndEvents %>%
    #### removing dummy tag
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
                              dist_moved < 0 ~ "Downstream Movement")
    )
  
} else {
  dailyMovementsTableAll <- movementsGrouped
}

#time frame options
#aggregating and completing helps to create continuous animation to avoid erros, especially when gacet wrapping by tag
if (TimeframeButtons == "weeks_since"){
  movementsGrouped <- dailyMovementsTableAll %>%
    complete(TAG, weeks_since = full_seq(min(weeks_since):max(weeks_since),1)) %>%
    group_by(TAG) %>%
    arrange(weeks_since) %>%
    fill(Species, ReleaseSite, X, Y, .direction = "downup") %>%
    ungroup()
  
} else if (TimeframeButtons == "daySequence"){
  
  dateRange <- range(dailyMovementsTableAll$daySequence, na.rm = TRUE)
  
  movementsGrouped <- dailyMovementsTableAll %>%
    complete(TAG, daySequence = seq.Date(
      from = dateRange[1],
      to   = dateRange[2],
      by   = "day"
    )) %>%
    group_by(TAG) %>%
    arrange(daySequence) %>%
    fill(Species, ReleaseSite, X, Y, .direction = "downup") %>%
    ungroup()
  
} else if(TimeframeButtons == "hours_since") {
  #dateRange <- range(dailyMovementsTableAll$daySequence, na.rm = TRUE)
  ###for some reason if it's a grouped df but only one tag, it's not interpolating how it should. that's the ungroup() call
  # movementsGrouped <- dailyMovementsTableAll %>%
  #   as.data.frame() %>%
  #   complete(hours_since = full_seq(min(hours_since):max(hours_since),1)) %>%
  #   group_by(TAG) %>%
  #   arrange(hours_since) %>%
  #   ungroup() %>%
  #   rename(X = UTM_X,
  #          Y = UTM_Y) %>%
  #   fill(Species, ReleaseSite, X, Y, .direction = "downup") %>%
  #   ungroup() 
}

if(TimeframeButtons == "hours_since") {
  boundingBox <- st_as_sfc(st_bbox(c(xmin = -106.0771, xmax = -105.8938, ymax = 40.14896, ymin = 40.05358), crs = st_crs(4326)))
  
  #turn movements df into a sf object
  movementsWithTimeForFramesSF <- sf::st_as_sf(movementsGrouped, coords = c("X", "Y"), crs = 4326, remove = FALSE)
  #need to tranform to web mercator for ggplotting
  mercatorSFMovements <- st_transform(movementsWithTimeForFramesSF, crs = 3857)
  animationDatalist <- list(
    "num_hours" = max(movementsWithTimeForFramesSF$hours_since, na.rm = TRUE) - min(movementsWithTimeForFramesSF$hours_since, na.rm = TRUE) + 1,
    "mercatorSFMovements" = mercatorSFMovements, 
    "boundingBox" = boundingBox
  )
  
} else{
  animationDatalist <- Animation_function(movementsGrouped)
  
}


basemaps::set_defaults(map_service = "esri", map_type = "world_imagery")

basemapGGplot <- ggplot() +
  basemap_gglayer(animationDatalist$boundingBox) +
  scale_fill_identity() +
  coord_sf() +
  theme_classic() +
  guides(size = 'none', color = guide_legend(title = "Movement")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

mapWithData <- basemapGGplot + 
  #to get the data to show up, it needs to be a layer over the basemap
  #to associate the right type of movements wth the same tag, need to group by Tag for aesthetics
  geom_sf(data = animationDatalist$mercatorSFMovements, aes(size = 10,
                                                              color = animationDatalist$mercatorSFMovements$movement_only, 
                                                              group = animationDatalist$mercatorSFMovements$TAG)) +
  #assigns based on manually set names in app.R
  scale_color_manual(values = allColors) +
  #https://www.r-bloggers.com/2021/01/ease_aes-demo/ for dif options; not a big deal
  ease_aes('cubic-in-out') +
  labs(caption = anim_Caption)

#time frame options
if (TimeframeButtons == "weeks_since"){
  
  nframesUnit <- "num_weeks"
  
  mapWithData <- mapWithData +  
    transition_time(weeks_since) +
    ggtitle(
      paste(anim_Title, '{frame_time}'),
      subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist$mercatorSFMovements$Date, na.rm = TRUE)))
  
  
} else if (TimeframeButtons == "daySequence"){
  
  nframesUnit <- "num_days"
  
  mapWithData <- mapWithData +
    transition_time(daySequence) +
    ggtitle(
      paste(anim_Title, '{frame_time}'),
      subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(animationDatalist$mercatorSFMovements$Date, na.rm = TRUE)))
  
  
} else if (TimeframeButtons == "TimePeriodDates"){
  
  nframesUnit <- "num_periods"
  
  mapWithData <- mapWithData + 
    
    transition_states(TimePeriodDates, 
                      transition_length = 4,
                      state_length = 2) +
    labs(title = "MARK Periods") +
    ggtitle(
      
      paste(anim_Title, '{closest_state}')
    )
} else if(TimeframeButtons == "hours_since") {
  nframesUnit <- "num_hours"
  mapWithData <- mapWithData + 
    transition_time(hours_since) +
    enter_fade() +
    exit_shrink() +
    ggtitle(
      paste(anim_Title, '{frame_time}'),
      subtitle = paste("Hour {frame} of {nframes} past Initial Date of", min(animationDatalist$mercatorSFMovements$Date, na.rm = TRUE)))
}
###Facet wrap options

if(facetWrapOption == "Species"){
  
  mapWithData <- mapWithData +
    facet_wrap(~Species)
  
} else if(facetWrapOption == "ReleaseSite") {
  
  mapWithData <- mapWithData +
    facet_wrap(~ReleaseSite)
} else if(facetWrapOption == "TAG") {
  
  mapWithData <- mapWithData +
    facet_wrap(~TAG)
} 
#display data in app
#mapWithData
#save it automatically
#anim_save("WindyGapFishMovements.gif", 
#default nframes unit is 100
gganimate::animate(mapWithData, 
                   nframes = animationDatalist[[nframesUnit]] + endPauseValue, 
                   end_pause = endPauseValue,
                   fps = as.numeric(fps_Select), height = 1200, width = 1200)
