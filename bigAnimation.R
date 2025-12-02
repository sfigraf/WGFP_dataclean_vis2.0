#####Big Gif making
library(tidyverse) 
library(lubridate)
library(leaflet)
library(sf)
library(gganimate)
library(basemaps)


TimeFrame <- "Hours"
#TimeFrame <- "Days"
latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326
fraserColoradoRiverConfluence <- as.numeric(wgfpMetadata$ImportantStationingVariables[wgfpMetadata$ImportantStationingVariables$Variable == "Fraser/Colorado River Confluence", "StationingLocation"])

AllEvents <- combinedData_df_list$All_Events %>%
  filter(TAG == "230000294213", 
         Date >= as.Date("2025-10-05") & Date <= as.Date("2025-10-08")
    ) %>%
  mutate(hours_since = as.numeric(floor(difftime(Datetime, min(Datetime), units = "hours"))), 
         hourSequence = as_datetime(as.character(round(Datetime, units = "hours"))), 
         days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
         #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
         # if you want to start at week 1 instead of week 0, add +1 to the end of expression
         # when you change this too, it changes the number of entries in the states dataframe
         weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))), 
         daySequence = as.Date(as.character(round(Datetime, units = "days")))
         )

if(TimeFrame == "Hours") {
  LastEvent <- AllEvents %>%
    group_by(TAG, hourSequence) %>%
    filter(Datetime == last(Datetime)) %>%
    ungroup()
} else if(TimeFrame == "Days") {
  LastEvent <- AllEvents %>%
    group_by(TAG, daySequence) %>%
    filter(Datetime == last(Datetime)) %>%
    ungroup()
}

#get movements
LastEvent1 <- sf::st_as_sf(LastEvent, coords = c("UTM_X", "UTM_Y"), crs = 32613, remove = FALSE)
#condensedEventsSF <- sf::st_as_sf(condensedEventsFiltered, coords = c("UTM_X", "UTM_Y"), crs = 32613)
#convert to lat/long
EventsSFLatLong <- sf::st_transform(LastEvent1, latLongCRS)
#stattions needed to calculate movements and distance moved
stationsAndEvents <- sf::st_join(EventsSFLatLong, simpleStations, st_nearest_feature)


MovementsStationsFraserColoradoCorrected <- stationsAndEvents %>%
  left_join(wgfpMetadata$AntennaMetadata[,c("FrontendSiteCode", "River")], by = c("Event" = "FrontendSiteCode")) %>%
  #selects first non-NA value from set of columns; by having River.Y first it prioritizes that column
  mutate(River = coalesce(River.y, River.x), 
         # this part is needed because stations are assigned from 0 up the fraser river starting at the confluence
         #new antennas weren't showing up because I didn't include connectivity channel to to river
         # this assigns a station, then in the get_movements function the distance moved is calculated
         ET_STATION = case_when(River %in% "Fraser River" ~ ET_STATION + fraserColoradoRiverConfluence, #10120 is above Fraser River Confluence; pre-construciton was 9566
                                River %in% c("Colorado River", "Connectivity Channel") ~ ET_STATION,
                                TRUE ~ ET_STATION)
  ) %>%
  select(-River.x, -River.y)

allLastEventsWithMovements <- MovementsStationsFraserColoradoCorrected %>%
  group_by(TAG) %>%
  arrange(Datetime) %>%
  mutate(dist_moved = case_when(lag(ET_STATION, order_by = Datetime) > fraserColoradoRiverConfluence & ET_STATION > fraserColoradoRiverConfluence & River != lag(River, order_by = Datetime) ~ (lag(ET_STATION, order_by = Datetime) - fraserColoradoRiverConfluence) + (ET_STATION - fraserColoradoRiverConfluence),
                                TRUE ~ ET_STATION - lag(ET_STATION, order_by = Datetime)
                                ), 
                                
    movement_only = case_when(lag(ET_STATION, order_by = Datetime) > fraserColoradoRiverConfluence & ET_STATION > fraserColoradoRiverConfluence & River != lag(River, order_by = Datetime) ~ "Changed Rivers",
                            Event %in% c("Release", "Recapture and Release")  ~ "Initial Release",
                            dist_moved == 0 ~ "No Movement",
                            dist_moved > 0 ~ "Upstream Movement",
                            dist_moved < 0 ~ "Downstream Movement")
  )

coords1 <- st_as_sfc(st_bbox(c(xmin = -106.0771, xmax = -105.8938, ymax = 40.14896, ymin = 40.05358), crs = st_crs(4326)))

webMercatorAllEvents <- st_transform(allLastEventsWithMovements, crs = 3857)

webMercatorAllEvents <- webMercatorAllEvents %>%
  ungroup() %>%
  #rowid_to_column("rowNum") %>%
  mutate(#xx = row_number(),
    X.1 = st_coordinates(webMercatorAllEvents)[row_number(),1], 
         Y.1 = st_coordinates(webMercatorAllEvents)[row_number(),2] )

####optoinal filtering
# webMercatorAllEvents <- webMercatorAllEvents[10000:16000,] %>%
#   ungroup()
#Error in colour_state_interpolator(data, states) : 
#the dims contain missing values

##Error in numeric_state_interpolator(lapply(data, as.numeric), states) : 
#negative length vectors are not allowed
#means it ran out of memory
#works <- webMercatorAllEvents
#doesntWork <- webMercatorAllEvents
num_hours <- max(webMercatorAllEvents$hours_since) - min(webMercatorAllEvents$hours_since) + 1
num_days <- max(webMercatorAllEvents$days_since) - min(webMercatorAllEvents$days_since) + 1


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
  # geom_point(data = webMercatorAllEvents, aes(x = webMercatorAllEvents$X.1, y = webMercatorAllEvents$Y.1, size = 10, 
  #                                             group = TAG)) +
  geom_sf(data = webMercatorAllEvents, aes(
    size = 10,
    color = movement_only,
    group = TAG)) +
  scale_color_manual(values = allColors) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


if(TimeFrame == "Hours") {
  frameUnit <- num_hours
  map_with_data1 <- map_with_data1 +
    transition_time(hourSequence) +
    enter_fade() +
    exit_shrink() +
    ggtitle(
      
      paste("All Tags (No TGM):", '{frame_time}'), #{frame_time}
      subtitle = paste("Starting Date: ", min(webMercatorAllEvents$Date)))
    
} else if(TimeFrame == "Days") {
  frameUnit <- num_days
  map_with_data1 <- map_with_data1 +
    transition_time(daySequence) +
    enter_fade() +
    exit_shrink() +
    ggtitle(
      paste("All Tags (No TGM):", '{frame_time}'), #{frame_time}
      subtitle = paste("Starting Date: ", min(webMercatorAllEvents$Date)))
}

# map_with_data1 <- map_with_data1 +
#   #ease_aes('cubic-in-out') +
#   facet_wrap(~ReleaseSite)

#animate(map_with_data1, nframes = num_days + endPauseValue, end_pause = endPauseValue, fps = 10)
#map_with_data1
endPauseValue = 30
anim_save("OneTagHourlyMOves.gif", animate(map_with_data1, nframes = frameUnit + endPauseValue, end_pause = endPauseValue, fps = 5, height = 1200, width =1200)) #, height = 1200, width =1200
