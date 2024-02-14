
start_time <- Sys.time()
print("Reading in Map Files.")
library(rgdal)
library(leaflet)

layer_location <- file.path("./gis/")


# stationary_antennas <- readOGR(dsn = layer_location, layer = "stationary_points")
# stationary_antennas <- sp::spTransform(stationary_antennas, CRS("+init=epsg:4326"))
antenna_sites <- readOGR(dsn = layer_location, layer = "antenna_sites1")
antenna_sites <- sp::spTransform(antenna_sites, CRS("+init=epsg:4326"))

stream_centerline <- readOGR(dsn = layer_location, layer = "Stream_Centerline_Post")
stream_centerline <- sp::spTransform(stream_centerline, CRS("+init=epsg:4326"))

releasesites <- readOGR(dsn = layer_location, layer = "ReleaseSites2021")
releasesites <- sp::spTransform(releasesites, CRS("+init=epsg:4326"))


mobile_reaches <- readOGR(dsn = layer_location, layer = "mobile_reaches")
mobile_reaches <- sp::spTransform(mobile_reaches, CRS("+init=epsg:4326"))

###no need to change files other than stations to .rds becuase the others aren't slow to bring in and convert to correct coordinate system

simple_stations2 <- read_rds(file.path("./gis/simple_stations2.rds"))

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
# simple_stations2 <- readOGR(dsn = layer_location, layer = "Stations_10m_Post")
# simple_stations2 <- sp::spTransform(simple_stations2, CRS("+init=epsg:4326"))
# library(rmapshaper)
# simple_stations3 <- ms_simplify(simple_stations2, keep = .1)
# write_rds(simple_stations3, file = file.path(paste0(layer_location,"/simple_stations2.rds")))


end_time <- Sys.time()
print(paste("Reading in Map files took", round(end_time-start_time,2), "Seconds"))
