
start_time <- Sys.time()
print("Reading in Map Files.")
#library(rgdal)
library(leaflet)

library(sf)

layerLocation <- file.path("./gis/")
latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326

# stationary_antennas <- readOGR(dsn = layerLocation, layer = "stationary_points")
# stationary_antennas <- sp::spTransform(stationary_antennas, CRS("+init=epsg:4326"))
# antenna_sites <- readOGR(dsn = layerLocation, layer = "antenna_sites1")
# antenna_sites <- sp::spTransform(antenna_sites, CRS("+init=epsg:4326"))
antenna_sites <- st_transform(read_sf(file.path(layerLocation, "antenna_sites1.shp")), latLongCRS)

# stream_centerline <- readOGR(dsn = layerLocation, layer = "Stream_Centerline_Post")
# stream_centerline <- sp::spTransform(stream_centerline, CRS("+init=epsg:4326"))
stream_centerline <- st_transform(read_sf(file.path(layerLocation, "Stream_Centerline_Post.shp")), latLongCRS)

# releasesites <- readOGR(dsn = layerLocation, layer = "ReleaseSites2021")
# releasesites <- sp::spTransform(releasesites, CRS("+init=epsg:4326"))
releasesites <- st_transform(read_sf(file.path(layerLocation, "ReleaseSites2021.shp")), latLongCRS)

# 
# mobile_reaches <- readOGR(dsn = layerLocation, layer = "mobile_reaches")
# mobile_reaches <- sp::spTransform(mobile_reaches, CRS("+init=epsg:4326"))
mobile_reaches <- st_transform(read_sf(file.path(layerLocation, "mobile_reaches.shp")), latLongCRS)

###no need to change files other than stations to .rds because the others aren't slow to bring in and convert to correct coordinate system

simple_stations2 <- readr::read_rds(file.path("./gis/simple_stations2.rds"))

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
# simple_stations2 <- readOGR(dsn = layerLocation, layer = "Stations_10m_Post")
# simple_stations2 <- sp::spTransform(simple_stations2, CRS("+init=epsg:4326"))

stations <- st_transform(read_sf(file.path(layerLocation, "Stations_10m_Post.shp")), latLongCRS)
# 
# library(rmapshaper)
simpleStations <- rmapshaper::ms_simplify(stations, keep = .1)
write_rds(simpleStations, file = file.path(paste0(layerLocation,"/simple_stations2.rds")))


end_time <- Sys.time()
print(paste("Reading in Map files took", round(end_time-start_time,2), "Seconds"))
