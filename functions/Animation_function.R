library(sf)
library(tidyverse)
library(gganimate)
library(basemaps)
# `MovementsData_2025-01-07` <- readRDS("~/WGFP_dataclean_vis2.0/MovementsData_2025-01-07.rds")
#movements <- movements_list$Movements_df
# Movements_df <- movements %>%
#   filter(TAG == 230000228275)
# changes coords to put on webMercator projection to be ready for animation
Animation_function <- function(Movements_df){
  
  # movementsWithTimeForFrames <- Movements_df %>%
  #   mutate(
  #     days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
  #     #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
  #     # if you want to start at week 1 instead of week 0, add +1 to the end of expression
  #     # when you change this too, it changes the number of entries in the states dataframe
  #     weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))), 
  #     hours_since = as.numeric(floor(difftime(Date, min(Date), units = "hours"))), 
  #   ) %>%
  #   # need to ungroup to get this code to work
  #   ungroup()
  # 
  # m1 %>%
  #   group_by(TAG, days_since) %>%
  #   filter(Datetime == last(Datetime)) %>%
  #   ungroup()
  
  boundingBox <- st_as_sfc(st_bbox(c(xmin = -106.0771, xmax = -105.8938, ymax = 40.14896, ymin = 40.05358), crs = st_crs(4326)))

  #turn movements df into a sf object
  movementsWithTimeForFramesSF <- sf::st_as_sf(Movements_df, coords = c("X", "Y"), crs = 4326, remove = FALSE)
  #need to tranform to web mercator for ggplotting
  mercatorSFMovements <- st_transform(movementsWithTimeForFramesSF, crs = 3857)

  num_weeks <- max(movementsWithTimeForFramesSF$weeks_since) - min(movementsWithTimeForFramesSF$weeks_since) + 1
  num_hours <- max(movementsWithTimeForFramesSF$hours_since) - min(movementsWithTimeForFramesSF$hours_since) + 1
  num_days <- max(movementsWithTimeForFramesSF$days_since) - min(movementsWithTimeForFramesSF$days_since) + 1
  num_periods <- max(as.numeric(movementsWithTimeForFramesSF$TimePeriod)) - min(as.numeric(movementsWithTimeForFramesSF$TimePeriod)) + 1
  
  animationList <- list("num_weeks" = num_weeks, "num_days" = num_days, 
                        "num_periods" = num_periods,
                        "mercatorSFMovements" = mercatorSFMovements, 
                        "boundingBox" = boundingBox)
  return(animationList)
}

# basemaps::set_defaults(map_service = "esri", map_type = "world_imagery")
# 

# w2 <- w1 %>%
#   filter(Date > as.Date("2021-04-02"))
# map_with_data <- ggplot() +
#   basemap_gglayer(boundingBox) +
#   scale_fill_identity() +
#   coord_sf() +
#   theme_classic() +
#   guides(size = 'none', color = guide_legend(title = "Movement"))
# 
# map_with_data <- map_with_data +
#   geom_point(data = w1, aes(x =X.1, y = Y.1,
#                                                   size = 10,
#                                                   color = movement_only, group = weeks_since))+
#   transition_time(weeks_since) +
#   labs(title = "Days") +
#   ggtitle(
# 
#     paste('{frame_time}'),
#     subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(w1$Date) ))
# animate(map_with_data, nframes = num_weeks,   height = 1200, width =1200)
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
# base <- basemap_gglayer(boundingBox)
# ggplot() +
#   base
# 
#spdf1 <-st_transform(st_as_sfc(spdf), crs = 3857)
# 
#############WORKS FOR slow moving tranisitons 
# x <- ggplot() +
#   basemap_gglayer(boundingBox) +
#   scale_fill_identity() +
#   coord_sf() +
#   theme_classic() +
#   #####CHANGING COLOR changes the transition "movement" of the fish
#   geom_sf(data = w1, aes(
#                          size = 10, color = movement_only, group = TAG)) +
#   transition_time(weeks_since) +
#   #transition_states(TAG) +
#   coord_sf() +
#   #transition_time(weeks_since)  +
#   ggtitle(
#     #paste("Date", m3$Date),
#     paste("test ", '{frame_time}'),
#     subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(spdf$Date) ))
#   # enter_fade() +
#   # exit_fade()
#   # basemap_gglayer(list1$boundingBox) +
#   # coord_sf(default_crs = sf::st_crs(4326))
# gganimate::animate(x, nframes = num_weeks, end_pause = 5)
# 
# ###################
# iris$group <- seq_len(nrow(iris))
# anim1 <- ggplot(iris, aes(Sepal.Width, Petal.Width, group = group)) +
#   geom_point() +
#   labs(title = "{closest_state}") +
#   transition_states(Species, transition_length = 3, state_length = 1) +
#   enter_fade() +
#   exit_fade()

#animate(x, nframes = num_days,  renderer = gifski_renderer(), height = 1200, width =1200)
# x
# map_with_data <- ggplot(spdf) +
#   basemap_gglayer(boundingBox, map_service = "esri", map_type = "world_imagery") +
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
