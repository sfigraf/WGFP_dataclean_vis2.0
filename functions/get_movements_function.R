
# the EVENT field is not super helpful here; for most it doesn't matter if it hit RB1 or RB2; can be misleading; maybe omit it? same idea for states DF
###get rid of pbs mapping here
library(PBSmapping)
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
    mutate(dist_moved = case_when(lag(ET_STATION, order_by = Datetime) > FraserColoradoRiverConfluence & ET_STATION > FraserColoradoRiverConfluence & River != lag(River, order_by = Datetime) ~ (lag(ET_STATION, order_by = Datetime) - FraserColoradoRiverConfluence) + (ET_STATION - FraserColoradoRiverConfluence),
                                  TRUE ~ ET_STATION - lag(ET_STATION, order_by = Datetime)
                                  ),
           sum_dist = (sum(abs(dist_moved), na.rm = TRUE)),
           
           
           movement_only = case_when(lag(ET_STATION, order_by = Datetime) > FraserColoradoRiverConfluence & ET_STATION > FraserColoradoRiverConfluence & River != lag(River, order_by = Datetime) ~ "Changed Rivers",
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
           ),
           X = as.numeric(UTM_X),
           Y = as.numeric(UTM_Y)
    ) #end of mutate
    
  # assigning projection to ready df lat/longs for plotting
  attr(movement_table_notrans, "zone") = "13"
  attr(movement_table_notrans, "projection") = "UTM"
  attr(movement_table_notrans, "datum") = "GRS80"
  
  # need a column that has x and Y for this 
  # converts lutms to lat/long
  movement_table_notrans <- convUL(movement_table_notrans, km=FALSE, southern=NULL)
  #as of now, movement table still has rows reminiscent from first_last etc which are helpful when you want to know where it ended the day and stuff.
  #but if you want to know concise movments, then this will eliminate uneeded rows
  #example: 230000142723
  movement_table_notrans1 <- movement_table_notrans %>%
    distinct(Date, TAG, det_type, movement_only, UTM_X, UTM_Y, .keep_all = TRUE) %>%
    select(Date, Datetime, TAG, movement_only, det_type, dist_moved, sum_dist, ET_STATION, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, UTM_X, UTM_Y, X, Y, marker_color, icon_color)
  
  #giving id column to make map proxy easier
  # actually the id column needs to be remade every time a filter is applied. See the movements_df_reactives 
  #movement_table_notrans1$id <- seq.int(nrow(movement_table_notrans1))
  
  end_time <- Sys.time()
  print(paste("Movements Function took", round(end_time-start_time,2), "Seconds"))
  
  return(movement_table_notrans1)
}

#mvts <- get_movements_function(combined_events_stations)
