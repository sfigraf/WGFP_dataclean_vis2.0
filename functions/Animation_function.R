library(sf)
library(tidyverse)
library(gganimate)
library(basemaps)
`MovementsData_2025-01-07` <- readRDS("~/WGFP_dataclean_vis2.0/MovementsData_2025-01-07.rds")
Movements_df <- `MovementsData_2025-01-07`[1:500,]
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
  #dailyMovementsTableSF <- sf::st_as_sf(dailyMovementsTable, coords = c("UTM_X", "UTM_Y"), crs = 32613, remove = FALSE)
  
  spdf <- sf::st_as_sf(m1, coords = c("X", "Y"), crs = 4326, remove = FALSE)
  # spdf <- SpatialPointsDataFrame(coords = xy, data = m1,
  #                                proj4string = CRS("+proj=longlat")) #"+init=epsg:3857" #+proj=aeqd +lat_0=53.6 +lon_0=12.7
  
  webMercator <- st_transform(spdf, crs = 3857)
  
  w1 <- webMercator%>%
    mutate(X.1 = st_coordinates(webMercator)[,1], 
           Y.1 = st_coordinates(webMercator)[,2] )
  
  #m3 <- as.data.frame(webMercator) %>%
    
  
  num_weeks <- max(m1$weeks_since) - min(m1$weeks_since) + 1
  num_days <- max(m1$days_since) - min(m1$days_since) + 1
  list1 <- list("num_weeks" = num_weeks, "num_days" = num_days, "data" = w1, "coords1" = coords1)
  return(list1)
}
# crs(spdf)
# 
# bounds <- st_as_sfc(st_bbox(spdf))
# backgroundImage <- annotation_map(spdf,
#   type = "os", 
#   zoom = 10
# )
# library(rosm)
# base <- osm.raster(bounds)
# ggplot(base)
# basemaps::set_defaults(map_service = "esri", map_type = "world_imagery")
# base <- basemap_gglayer(coords1)
# ggplot() +
#   base
# 
# spdf1 <-st_transform(st_as_sfc(spdf), crs = 3857)
# 
# ggplot(spdf1) +
#   #basemap_gglayer(spdf) +
#   geom_sf() #+
#   #coord_sf()
#   #transition_time(weeks_since)  +
#   ggtitle(
#     #paste("Date", m3$Date),
#     paste("test ", '{frame_time}'),
#     subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(spdf$Date) ))
#   # basemap_gglayer(list1$coords1) +
#   # coord_sf(default_crs = sf::st_crs(4326))
# animate(x, nframes = num_days)
# x
# map_with_data <- ggplot(spdf) +
#   basemap_gglayer(coords1, map_service = "esri", map_type = "world_imagery") +
#   scale_fill_identity() +
#   #coord_sf(crs = st_crs(4326)) +
#   theme_classic() +
#   guides(size = "none", color = guide_legend(title = "Movement"))
# map_with_data
# map_with_data + 
#   coord_sf(crs = 4326, datum = sf::st_crs(3857)) +
#   scale_fill_identity() +
#   geom_sf(data =spdf1) #, aes(x = webMercator$Y, y = webMercator$X, size = 10, color =webMercator$movement_only, group =webMercator$weeks_since)
#   # + 
#    #+
#   #transition_time(weeks_since)
# 
# 
# 
# map_with_data <- map_with_data + 
#   geom_point(data =w1, aes(x =w1$X.1, y = w1$Y.1,
#                                                   size = 10,
#                                                   color =w1$movement_only, group =w1$weeks_since)) +
#   transition_time(weeks_since) +
#   ggtitle(
#     #paste("Date", m3$Date),
#     paste("test ", '{frame_time}'),
#     subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(w1$Date) ))
# 
# #st_crs(map_with_data)
# map_with_data
# anim_save("WindyGapFishMovements.gif", animate(map_with_data, nframes =list1$num_weeks, fps = input$fps_Slider, height = 1200, width =1200)) # New
# 

# map_with_data <- map_with_data + 
#   geom_point(data = animationDatalist()$data, aes(x = animationDatalist()$data$X.1, y = animationDatalist()$data$Y.1,
#                                                   size = 10,
#                                                   color = animationDatalist()$data$movement_only, group = animationDatalist()$data$weeks_since))+
#   transition_time(weeks_since) +
#   ggtitle(
#     #paste("Date", m3$Date),
#     paste(input$anim_Title, '{frame_time}'),
#     subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist()$data$Date) ))
# map_with_data
# anim_save("WindyGapFishMovements.gif", animate(map_with_data, nframes = animationDatalist()$num_weeks, fps = input$fps_Slider, height = 1200, width =1200)) # New
