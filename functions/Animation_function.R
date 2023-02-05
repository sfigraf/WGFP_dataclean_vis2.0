library(sf)
# changes coords to put on webMercator projection to be ready for animation
Animation_function <- function(Movements_df){
  
  m1 <- Movements_df %>%
    mutate(
      days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
      #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
      # if you want to start at week 1 instead of week 0, add +1 to the end of expression
      # when you change this too, it changes the number of entries in the states dataframe
      weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks")))
      #months_since = as.numeric(floor(difftime(Date, min(Date), units = "months")))
    ) %>%
    ungroup()
  coords1 <- st_as_sfc(st_bbox(c(xmin = -106.0771, xmax = -105.8938, ymax = 40.14896, ymin = 40.05358), crs = st_crs(4326)))
  
  
  
  xy <- m1 %>%
    select(X, Y)
  # need to ungroup to get this code to work
  spdf <- SpatialPointsDataFrame(coords = xy, data = m1,
                                 proj4string = CRS("+proj=longlat")) #"+init=epsg:3857" #+proj=aeqd +lat_0=53.6 +lon_0=12.7
  
  webMercator <- spTransform(spdf, CRS("+init=epsg:3857"))
  m3 <- as.data.frame(webMercator)
  
  num_weeks <- max(m1$weeks_since) - min(m1$weeks_since) + 1
  list1 <- list("num_weeks" = num_weeks, "data" = m3, "coords1" = coords1)
  return(list1)
}