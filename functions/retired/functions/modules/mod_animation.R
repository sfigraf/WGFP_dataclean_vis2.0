library(mapedit)
library(basemaps)
library(gganimate)
library(sf)

mod_animationUI <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(plotOutput("plot12"))
  
  )
}

mod_animationServer <- function(id, filtered_movements_df) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot12 <- renderPlot({
        m1 <- filtered_movements_df %>%
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
      
      x1 <- ggplot() + 
        basemap_gglayer(coords1) +
        scale_fill_identity() + 
        coord_sf() +
        theme_classic()
      

      xy <- m1 %>%
        select(X, Y)
      # need to ungroup to get this code to work
      spdf <- SpatialPointsDataFrame(coords = xy, data = m1,
                                     proj4string = CRS("+proj=longlat")) #"+init=epsg:3857" #+proj=aeqd +lat_0=53.6 +lon_0=12.7
      
      webMercator <- spTransform(spdf, CRS("+init=epsg:3857"))
      m3 <- as.data.frame(webMercator)
      
      map_with_data <- x1 +
        geom_point(data = m3, aes(x = X.1, y = Y.1, 
                                  size = 4,
                                  color=movement_only, group=weeks_since))
      #map_with_data
      num_weeks <- max(m1$weeks_since) - min(m1$weeks_since) + 1
      
      
      
        
        map_with_animation <- map_with_data +
          transition_time(weeks_since) +
          #### this is the title
          ggtitle(
            #paste("Date", m3$Date),
            'Week after Project: {frame_time}',
            subtitle = 'Frame {frame} of {nframes}')
        animate(map_with_animation, nframes = num_weeks, fps = 2)
        
      })
    }
  )
}