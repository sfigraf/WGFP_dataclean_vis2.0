### connect station data to detection data
###station data is originally made in GIS from the all_events dataset that has been filtered on distinct TAG, Event, UTM_X, UTM_Y, and Date

#condensed_events is the detection data filtered on distinct stuff from all_events. comes from all_combined_events function
# condesned_events has UTMs for coordinates 
# condensed_events = df_list$All_Events_most_relevant
# simple_stations = simple_stations2
#simplestations is a sptial lines dataframe brought in with map_polygon_readins 

library(PBSmapping)
spatial_join_stations_detections <- function(condensed_events, simple_stations) {
  
  start_time <- Sys.time()
  print("Running spatial_join_stations_detections function: Joining detections and events to stations shapefile.")
  ### converting to lat/longs instead of UTM's
  condensed_events <- condensed_events %>%
    
    mutate(
      X = as.numeric(UTM_X),
      Y = as.numeric(UTM_Y)
    ) #end of mutate
  
  # assigning projection to ready df lat/longs for plotting
  attr(condensed_events, "zone") = "13"
  attr(condensed_events, "projection") = "UTM"
  attr(condensed_events, "datum") = "GRS80"
  
  # need a column that has x and Y for this 
  # converts lutms to lat/long
  condensed_events <- convUL(condensed_events, km=FALSE, southern=NULL)
  
  #converting lat/long entries to spatial points dataframe
  # needs to have a df of just coordinates (xy)
  xy <- condensed_events %>%
    select(X, Y)
  
  spdf <- SpatialPointsDataFrame(coords = xy, data = condensed_events,
                                 proj4string = CRS("+init=epsg:4326"))
  
  ## making sf objects
  detections_sf <- st_as_sf(spdf)
  stations_sf <- st_as_sf(simple_stations)
  
  joined <- st_join(detections_sf, stations_sf, st_nearest_feature)
  #need to convert class sf object back to dataframe so that it goes faster in combine_events_stations_function
  station_data <- as.data.frame(joined)
  
  
  
  end_time <- Sys.time()
  print(paste("Spatial_join_stations_detections took", round(end_time-start_time,2), "Seconds"))
  
  return(station_data)
  
}


#test1 <- spatial_join_stations_detections(df_list$All_Events_most_relevant, simple_stations2)

