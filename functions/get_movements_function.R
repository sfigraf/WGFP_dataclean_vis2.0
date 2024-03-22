
get_movements_function <- function(combined_events_stations) {
  start_time <- Sys.time()
  print("Running get_movements_function: Calculates movements of fish based off a change in station.")
  
  movement_table_notrans <- combined_events_stations %>%
    #### removing dummy tag
    filter(!TAG %in% c("230000999999")) %>%
    select(Date, Datetime, TAG, det_type, Event, ET_STATION, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, UTM_X, UTM_Y) %>%
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
    ) %>%
  distinct(Date, TAG, det_type, movement_only, UTM_X, UTM_Y, .keep_all = TRUE)  #end of mutate
    
  #######get lat/longs for plotting with leaflet
  #convert events to sf object
  #the utms are grs80 and utm zone 13, which corresponds to crs  32613
  movement_table_notransSF <- sf::st_as_sf(movement_table_notrans, coords = c("UTM_X", "UTM_Y"), crs = 32613, remove = FALSE)
  #convert to lat/long
  movement_table_notransSFLatLong <- sf::st_transform(movement_table_notransSF, latLongCRS)
  
  coordinates <- st_coordinates(movement_table_notransSFLatLong)
  
  # Convert the coordinates to a data frame
  coordinatesDf <- as.data.frame(coordinates)
  # # Extract the lat/long coordinates
  movement_table_notransSFLatLong$X <- coordinatesDf$X
  movement_table_notransSFLatLong$Y <- coordinatesDf$Y
  
  #as of now, movement table still has rows reminiscent from first_last etc which are helpful when you want to know where it ended the day and stuff.
  #but if you want to know concise movments, then this will eliminate uneeded rows
  #example: 230000142723
  movement_table_notrans1 <- as.data.frame(movement_table_notransSFLatLong) %>%
    select(Date, Datetime, TAG, movement_only, det_type, dist_moved, sum_dist, ET_STATION, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, UTM_X, UTM_Y, X, Y, marker_color, icon_color)
  
  end_time <- Sys.time()
  print(paste("Movements Function took", round(end_time-start_time,2), "Seconds"))
  
  return(movement_table_notrans1)
}

