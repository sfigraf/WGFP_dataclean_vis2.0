
# the EVENT field is not super helpful here; for most it doesn't matter if it hit RB1 or RB2; can be misleading; maybe omit it? same idea for states DF

get_movements_function <- function(combined_events_stations) {
  start_time <- Sys.time()
  
  
  movement_table_notrans <- combined_events_stations %>%
    select(Date, Datetime, TAG, det_type, Event, ET_STATION, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, UTM_X, UTM_Y) %>%
    #grouping by TAG and arranging by datetime makes sure that total distance moved is totalled and summed in order
    group_by(TAG) %>%
    arrange(Datetime) %>%
    mutate(dist_moved = ET_STATION - lag(ET_STATION, order_by = Datetime),
           sum_dist = (sum(abs(diff(ET_STATION, na.rm = TRUE)))),
           
           
           movement_only = case_when(Event %in% c("Release", "Recapture and Release")  ~ "Initial Release",
                                     dist_moved == 0 ~ "No Movement",
                                     dist_moved > 0 ~ "Upstream Movement",
                                     dist_moved < 0 ~ "Downstream Movement"),
           #this is for mapping later on
           #MARKERCOLOR options: limited because markers rely on static image
           #red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "white", "gray", "lightgray", "black"
           
           marker_color = case_when(movement_only == "No Movement" ~ "black",
                                    movement_only == "Upstream Movement" ~ "green",
                                    movement_only == "Downstream Movement" ~ "red",
                                    movement_only == "Initial Release" ~ "orange"
                                    #str_detect(movement_only, "Initial Release (or recapture and release)") ~ "yellow"
                                    ),
           
           icon_color = case_when(str_detect(det_type, "Stationary Antenna") ~ "orange",
                                  str_detect(det_type, "Biomark Antenna") ~ "yellow",
                                  str_detect(det_type, "Mobile Run") ~ "purple",
                                  det_type %in% c("Release", "Recapture and Release") ~ "blue",
                                  det_type == "Recapture" ~ "brown",
           ),
           X = as.numeric(UTM_X),
           Y = as.numeric(UTM_Y)
    ) #end of mutate
    
  
  attr(movement_table_notrans, "zone") = "13"
  attr(movement_table_notrans, "projection") = "UTM"
  attr(movement_table_notrans, "datum") = "GRS80"
  
  # need a column that has x and Y for this 
  movement_table_notrans <- convUL(movement_table_notrans, km=FALSE, southern=NULL)
  #as of now, movement table still has rows reminiscent from first_last etc which are helpful when you want to know where it ended the day and stuff.
  #but if you want to know concise movments, then this will eliminate uneeded rows
  #example: 230000142723
  movement_table_notrans1 <- movement_table_notrans %>%
    distinct(Date, TAG, det_type, movement_only, UTM_X, UTM_Y, .keep_all = TRUE) %>%
    
    select(Date, Datetime, TAG, movement_only, det_type, dist_moved, sum_dist, ET_STATION, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, UTM_X, UTM_Y, X, Y, marker_color, icon_color)
  
  #giving id column to make map proxy easier
  #movement_table_notrans1$id <- seq.int(nrow(movement_table_notrans1))
  
  end_time <- Sys.time()
  print(paste("Movements Function took", round(end_time-start_time,2)))
  
  return(movement_table_notrans1)
}

#mvts <- get_movements_function(combined_events_stations)
