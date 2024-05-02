
start_time <- Sys.time()
print("Reading in Map Files.")

library(leaflet)
library(sf)
library(readr)

layerLocation <- file.path("./gis/")
latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326

antenna_sites <- st_transform(read_sf(file.path(layerLocation, "antenna_sites1.shp")), latLongCRS)

antenna_sites$SiteName <- gsub("Kaibab Park", "Granby Diversion", antenna_sites$SiteName)
antenna_sites$SiteName <- gsub("Below Windy Gap Dam", "Windy Gap Bypass Channel", antenna_sites$SiteName)
antenna_sites$SiteLabel <- gsub("B3", "WG1", antenna_sites$SiteLabel)
antenna_sites$SiteLabel <- gsub("B4", "GD1", antenna_sites$SiteLabel)
antenna_sites$SiteLabel <- gsub("B5", "RR1", antenna_sites$SiteLabel)
antenna_sites$SiteLabel <- gsub("B6", "FC1", antenna_sites$SiteLabel)

st_write(antenna_sites, "antenna_sites2.shp")
leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers(data = antenna_sites,
                    icon = Station_icons,
                    clusterOptions = markerClusterOptions(),
                    label = paste(antenna_sites$SiteLabel),
                    popup = paste(antenna_sites$SiteName, "<br>",
                                  "Channel Width:", antenna_sites$ChannelWid, "feet"),
                    group = "Antennas")


stream_centerline <- st_transform(read_sf(file.path(layerLocation, "Stream_Centerline_Post.shp")), latLongCRS)

releasesites <- st_transform(read_sf(file.path(layerLocation, "ReleaseSites2021.shp")), latLongCRS)

mobile_reaches <- st_transform(read_sf(file.path(layerLocation, "mobile_reaches.shp")), latLongCRS)

###no need to change files other than stations to .rds because the others aren't slow to bring in and convert to correct coordinate system

simpleStations <- readr::read_rds(file.path("./gis/simpleStations.rds"))

Station_icons <- leaflet::awesomeIcons(
  icon = 'add',
  iconColor = 'black',
  library = 'ion',
  #iconHeight = 20,
  markerColor = "purple"
)

release_icons <- awesomeIcons(
  icon = 'add',
  iconColor = 'black',
  library = 'ion',
  #iconHeight = 20,
  markerColor = "white"
)

label_style <- list(
  "color" = "white",
  "font-size" = "12px",
  "border-color" = "black"
)

#### this part if how to simplify a complex polygon layer that takes forever to read
# do this when you need to make new stations

# stations <- st_transform(read_sf(file.path(layerLocation, "Stations_10m_Post.shp")), latLongCRS)
# 
# library(rmapshaper)
# simpleStations <- rmapshaper::ms_simplify(stations, keep = .1)
# write_rds(simpleStations, file = file.path(paste0(layerLocation,"/simpleStations.rds")))


end_time <- Sys.time()
print(paste("Reading in Map files took", round(end_time-start_time,2), "Seconds"))
