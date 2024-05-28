
start_time <- Sys.time()
print("Reading in Map Files.")

library(leaflet)
library(sf)
library(readr)

layerLocation <- file.path("./gis/")
latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326

antenna_sites <- st_transform(read_sf(file.path(layerLocation, "antenna_sites1.shp")), latLongCRS)



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

#####adding new antenna to data

# utm_x <- 420727
# utm_y <- 4437229
# new_point <- st_sfc(st_point(c(utm_x, utm_y)), crs = 32613) # EPSG:32613 is UTM zone 13N
# 
# # Transform the new point to the desired CRS (latLongCRS)
# new_point_transformed <- st_transform(new_point, st_crs(latLongCRS))
# 
# new_row <- tibble(
#   OBJECTID = NA,
#   SiteName = NA,
#   Objective = NA,
#   StudyPerio = NA,
#   SiteLabel = NA,
#   AntennaNam = NA,
#   WaterName = NA,
#   ChannelWid = NA,
#   UTM_X = utm_x,
#   UTM_Y = utm_y,
#   Notes = NA,
#   geometry = new_point_transformed
# )
# 
# antenna_sites1 <- antenna_sites %>%
#   add_row(new_row)
# 
# output_file <- file.path(layerLocation, "updated_antenna_sites.shp")
# 
# # Write the updated sf object to a new shapefile
# st_write(antenna_sites, output_file)

end_time <- Sys.time()
print(paste("Reading in Map files took", round(end_time-start_time,2), "Seconds"))
