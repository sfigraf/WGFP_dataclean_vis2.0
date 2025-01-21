
movements <- movements_list$Movements_df %>%
  filter(Species != "TGM")
Movements_df <- movements %>%
  # group_by(TAG, TimePeriodDates) %>%
  # filter(Datetime == last(Datetime)) %>%
  # ungroup() %>%
  filter(TAG == 230000228275,
         Datetime > as.Date("2021-04-20")
    #!as.numeric(TimePeriod) %in% c(1:20)
         )
#,
#Datetime > as.Date("2021-01-01"

#fromAPPNew <- fromAPP[1:100,]
Movements_df <- x
m1 <- Movements_df %>%
  mutate(
    days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
    #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
    # if you want to start at week 1 instead of week 0, add +1 to the end of expression
    # when you change this too, it changes the number of entries in the states dataframe
    weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))), 
    hours_since = as.numeric(floor(difftime(Datetime, min(Datetime), units = "hours"))), 
    hourSequence = as_datetime(as.character(round(Datetime, units = "hours"))),
    #standardizes timePeriods so it cna be used for fram number, gonna try at least. maybe not necessary?
    TimeperiodsSince = as.numeric(TimePeriod) - min(as.numeric(TimePeriod)),
    daySequence = as.Date(as.character(round(Datetime, units = "days"))),
    
    
    #months_since = as.numeric(floor(difftime(Date, min(Date), units = "months")))
  ) %>%
  group_by(TAG, daySequence) %>%
  filter(Datetime == last(Datetime)) %>%
  ungroup()
  #ungroup() 

#change weeks_since as specified and either first or last as specified
# test <- m1 %>%
#   group_by(TAG, TimePeriodDates) %>%
#   filter(Datetime == last(Datetime)) %>%
#   ungroup() #%>%
  # complete(weeks_since = seq(min(weeks_since), max(weeks_since))
  # ) %>%
  # fill(movement_only, TAG, X, Y, .direction = "down")
  #complete(nesting(weeks_since))
  # mutate(first_last = case_when(Datetime == min(Datetime) ~ "First",
  #                               Datetime == max(Datetime) ~ "Last",
  #                               Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
  # )
  
coords1 <- st_as_sfc(st_bbox(c(xmin = -106.0771, xmax = -105.8938, ymax = 40.14896, ymin = 40.05358), crs = st_crs(4326)))

# need to ungroup to get this code to work

spdf <- sf::st_as_sf(m1, coords = c("X", "Y"), crs = 4326, remove = FALSE)
# spdf <- SpatialPointsDataFrame(coords = xy, data = m1,
#                                proj4string = CRS("+proj=longlat")) #"+init=epsg:3857" #+proj=aeqd +lat_0=53.6 +lon_0=12.7

webMercator <- st_transform(spdf, crs = 3857)

#w1 <- webMercator #%>%
  # mutate(X.1 = st_coordinates(webMercator)[,1], 
  #        Y.1 = st_coordinates(webMercator)[,2] )

#m3 <- as.data.frame(webMercator) %>%


num_weeks <- max(m1$weeks_since) - min(m1$weeks_since) + 1
num_hours <- max(m1$hours_since) - min(m1$hours_since) + 1
num_days <- max(m1$days_since) - min(m1$days_since) + 1
num_periods <- max(as.numeric(m1$TimePeriod)) - min(as.numeric(m1$TimePeriod)) + 1

basemaps::set_defaults(map_service = "esri", map_type = "world_imagery")
map_with_data <- ggplot() +
  basemap_gglayer(coords1) +
  scale_fill_identity() +
  coord_sf() +
  theme_classic() +
  guides(size = 'none', color = guide_legend(title = "Movement"))

map_with_data1 <- map_with_data + 
  #to get the data to show up, it needs to be a layer over the basemap
  #to associate the right type of movements wth the same tag, need to group by Tag for aesthetics
  geom_sf(data = webMercator, aes(#x = animationDatalist()$data$X.1, y = animationDatalist()$fromAPP$Y.1,
    size = 10, 
    color = movement_only, group = TAG)) +
  # transition_states(TimePeriodDates, 
  #                   transition_length = 4,
  #                   state_length = 2) +
  scale_color_manual(values = allColors) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  #facet_wrap(~Species) +
  transition_time(daySequence) +
  ease_aes('sine-in-out') +
  enter_fade() + 
  exit_shrink() +
  ggtitle(
    
    paste("Hour", '{frame_time}'), #{frame_time}
    subtitle = "Ghost/predated tags included, TGM excluded") +
  facet_wrap(~ReleaseSite)

#map_with_data1
endPauseValue = 5
animate(map_with_data1, nframes = num_days + endPauseValue, end_pause = endPauseValue, fps = 10) #, height = 1200, width =1200

#anim_save("MovementsBySpeciesTimePeriod.gif", 
##############
num_days <- max(x$days_since) - min(x$days_since) + 1
basemapGGplot <- ggplot() +
  basemap_gglayer(coords1) +
  scale_fill_identity() +
  coord_sf() +
  theme_classic() +
  guides(size = 'none', color = guide_legend(title = "Movement")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

mapWithData111 <- basemapGGplot + 
  #to get the data to show up, it needs to be a layer over the basemap
  #to associate the right type of movements wth the same tag, need to group by Tag for aesthetics
  geom_sf(data = x, aes(size = 10,
                                                              color = x$movement_only, 
                                                              group = x$TAG)) +
  scale_color_manual(values = allColors) +
  ease_aes('cubic-in-out') +
  labs(caption = "test")

mapWithData111 <- mapWithData111 +
  transition_time(days_since) +
  #labs(title = "Days") +
  ggtitle(
    paste("tets", '{frame_time}'),
    subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(x$Date)))

mapWithData111 <- mapWithData111 +
  facet_wrap(~Species)
animate(mapWithData111, nframes = num_days + endPauseValue, end_pause = endPauseValue, fps = 10) #, height = 1200, width =1200

df <- tibble(
  group = c(1:2, 1, 2),
  item_id = c(1:2, 2, 3),
  item_name = c("a", "a", "b", "b"),
  value1 = c(1, NA, 3, 4),
  value2 = 4:7
)
df
df %>%
  complete(
    group,
    nesting(item_id, item_name),
    fill = list(value1 = 0, value2 = 99)
  )
