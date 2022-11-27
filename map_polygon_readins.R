
start_time <- Sys.time()

layer_location <- file.path("./gis/")


stationary_antennas <- readOGR(dsn = layer_location, layer = "stationary_points")
stationary_antennas <- sp::spTransform(stationary_antennas, CRS("+init=epsg:4326"))

stream_centerline <- readOGR(dsn = layer_location, layer = "stream_centerline")
stream_centerline <- sp::spTransform(stream_centerline, CRS("+init=epsg:4326"))

releasesites <- readOGR(dsn = layer_location, layer = "ReleaseSites2021")
releasesites <- sp::spTransform(releasesites, CRS("+init=epsg:4326"))


mobile_reaches <- readOGR(dsn = layer_location, layer = "mobile_reaches")
mobile_reaches <- sp::spTransform(mobile_reaches, CRS("+init=epsg:4326"))

###no need to change files other than stations to .rds becuase the others aren't slow to bring in and convert to correct coordinate system

simple_stations2 <- read_rds(file.path("./gis/simple_stations.rds"))

Station_icons <- awesomeIcons(
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


end_time <- Sys.time()
print(paste("Reading in Map files took", round(end_time-start_time,2), "Seconds"))
