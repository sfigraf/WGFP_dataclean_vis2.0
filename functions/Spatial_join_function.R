### connect station data to detection data
###station data is originally made in GIS from the all_events dataset that has been filtered on distinct TAG, Event, UTM_X, UTM_Y, and Date

#condensedEvents is the detection data filtered on distinct stuff from all_events. comes from all_combined_events function
# condesned_events has UTMs for coordinates 
#condensedEvents = df_list$All_Events_most_relevant
# simpleStations = simpleStations2
#simplestations is a sptial lines dataframe brought in with map_polygon_readins 

library(sf)
spatial_join_stations_detections <- function(condensedEvents, simpleStations) {
  
  start_time <- Sys.time()
  print("Running spatial_join_stations_detections function: Joining detections and events to stations shapefile.")
  ### converting to lat/longs instead of UTM's
  #convert events to sf object
  #the utms are grs80 and utm zone 13, which corresponds to crs  32613
  #can't do it if there's any NA values in the utm fields
  problemRows <- condensedEvents %>%
    filter(is.na(UTM_X))
  condensedEventsFiltered <- condensedEvents %>%
    filter(!is.na(UTM_X))
  condensedEventsSF <- sf::st_as_sf(condensedEventsFiltered, coords = c("UTM_X", "UTM_Y"), crs = 32613)
  #convert to lat/long
  condensedEventsSFLatLong <- sf::st_transform(condensedEventsSF, latLongCRS)
  
  stationData <- sf::st_join(condensedEventsSFLatLong, simpleStations, st_nearest_feature)
  spatialList <- list("stationData" = stationData, "noUTMS" = problemRows)
  end_time <- Sys.time()
  print(paste("Spatial_join_stations_detections took", round(difftime(end_time, start_time, units = "mins"),2), "minutes"))
  
  return(spatialList)
  
}
