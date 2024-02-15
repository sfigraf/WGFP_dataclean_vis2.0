### connect station data to detection data
###station data is originally made in GIS from the all_events dataset that has been filtered on distinct TAG, Event, UTM_X, UTM_Y, and Date

#condensedEvents is the detection data filtered on distinct stuff from all_events. comes from all_combined_events function
# condesned_events has UTMs for coordinates 
# condensedEvents = df_list$All_Events_most_relevant
# simpleStations = simpleStations2
#simplestations is a sptial lines dataframe brought in with map_polygon_readins 

library(PBSmapping)
spatial_join_stations_detections <- function(condensedEvents, simpleStations) {
  
  start_time <- Sys.time()
  print("Running spatial_join_stations_detections function: Joining detections and events to stations shapefile.")
  ### converting to lat/longs instead of UTM's
  #convert events to sf object
  condensedEventsSF <- sf::st_as_sf(condensedEvents, coords = c("UTM_X", "UTM_Y"), crs = 32613)
  #convert to lat/long
  condensedEventsSFLatLong <- st_transform(condensedEventsSF, latLongCRS)
  # condensedEvents <- condensedEvents %>%
  #   
  #   mutate(
  #     X = as.numeric(UTM_X),
  #     Y = as.numeric(UTM_Y)
  #   ) #end of mutate
  # 
  # # assigning projection to ready df lat/longs for plotting
  # attr(condensedEvents, "zone") = "13"
  # attr(condensedEvents, "projection") = "UTM"
  # attr(condensedEvents, "datum") = "GRS80"
  # 
  # # need a column that has x and Y for this 
  # # converts lutms to lat/long
  # condensedEvents <- convUL(condensedEvents, km=FALSE, southern=NULL)
  # 
  # #converting lat/long entries to spatial points dataframe
  # # needs to have a df of just coordinates (xy)
  # xy <- condensedEvents %>%
  #   select(X, Y)
  # 
  # spdf <- SpatialPointsDataFrame(coords = xy, data = condensedEvents,
  #                                proj4string = CRS("+init=epsg:4326"))
  
  ## making sf objects
  # detections_sf <- st_as_sf(spdf)
  # stations_sf <- st_as_sf(simpleStations)
  
  
  
  joined <- st_join(condensedEventsSFLatLong, simpleStations, st_nearest_feature)
  #need to convert class sf object back to dataframe so that it processes faster in combine_events_stations_function
  stationData <- as.data.frame(joined)
  
  
  
  end_time <- Sys.time()
  print(paste("Spatial_join_stations_detections took", round(end_time-start_time,2), "Seconds"))
  
  return(stationData)
  
}


#test1 <- spatial_join_stations_detections(df_list$All_Events_most_relevant, simpleStations2)

