# Movements_df <- movements %>%
#   filter(TAG == 230000228275)
# changes coords to put on webMercator projection to be ready for animation
#currently this function doesn't really do that much
#Movements_df <- movementsGrouped
Animation_function <- function(Movements_df){
  
  boundingBox <- st_as_sfc(st_bbox(c(xmin = -106.0771, xmax = -105.8938, ymax = 40.14896, ymin = 40.05358), crs = st_crs(4326)))

  #turn movements df into a sf object
  movementsWithTimeForFramesSF <- sf::st_as_sf(Movements_df, coords = c("X", "Y"), crs = 4326, remove = FALSE)
  #need to tranform to web mercator for ggplotting
  mercatorSFMovements <- st_transform(movementsWithTimeForFramesSF, crs = 3857)
  
  #these are the values used to get frame time
  #na.rm fixed a negtive legnth vector error
  num_weeks <- max(movementsWithTimeForFramesSF$weeks_since, na.rm = TRUE) - min(movementsWithTimeForFramesSF$weeks_since, na.rm = TRUE) + 1
  num_hours <- max(movementsWithTimeForFramesSF$hours_since, na.rm = TRUE) - min(movementsWithTimeForFramesSF$hours_since, na.rm = TRUE) + 1
  num_days <- max(movementsWithTimeForFramesSF$days_since, na.rm = TRUE) - min(movementsWithTimeForFramesSF$days_since, na.rm = TRUE) + 1
  num_periods <- max(as.numeric(movementsWithTimeForFramesSF$TimePeriod), na.rm = TRUE) - min(as.numeric(movementsWithTimeForFramesSF$TimePeriod), na.rm = TRUE) + 1
  
  animationList <- list("num_weeks" = num_weeks, "num_days" = num_days, 
                        "num_periods" = num_periods,
                        "mercatorSFMovements" = mercatorSFMovements, 
                        "boundingBox" = boundingBox)
  return(animationList)
}

