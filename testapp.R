# 
# library(shiny)
# library(shinycssloaders)
# library(tidyverse) #error has occ
# library(lubridate)
# library(leaflet)
# library(PBSmapping)
# library(plotly) #for turning maps to plotly
# library(rgdal)
# library(DT)
# library(shinyWidgets) # for pickerinput
# library(shinythemes)
# library(bslib)
# 
# #Stationary <- read.csv(paste0("WGFP_Raw_20221227.csv")) #WGFP_Raw_20211130.csv WGFP_Raw_20220110_cf6.csv
# Mobile <- read.csv("WGFP_Mobile_Detect_AllData.csv" , colClasses= c(rep("character",14), rep("numeric", 4), rep("character", 3)))
# Biomark <- read.csv("Biomark_Raw_20221102.csv", dec = ",")
# # need to have tagID as a numeric field in the .csv file in order to be read in correctly as opposed to 2.3E+11
# Release <- read.csv("WGFP_ReleaseData_Master.csv", na.strings = c(""," ","NA"), colClasses=c(rep("character",8), "numeric", "numeric",rep("character",8) ))
# Recaptures <- read.csv("WGFP_RecaptureData_Master.csv", na.strings = c(""," ","NA"), colClasses = c(rep("character", 9), rep("numeric", 2), rep("character", 8)))
# 
# 
# 
# # Date Wrangling ----------------------------------------------------------
# 
# # this readies raw files to be put into functions as well as displayed on Indivudal Datasets Page
# #needs to put dates in yyyymmdd format to be filtered with drangeinput1
# Mobile <- Mobile %>%
#   mutate(Date = as.character(mdy(Date)))
# 
# 
# Biomark <- Biomark %>%
#   #make a column for Scan>Date if parentheses are detected in the string, that means the format is in mdy
#   # and we want to convert it to YYYYMMDD format. elsewise, leave it as is
#   mutate(Scan.Date = ifelse(str_detect(Scan.Date, "/"),
#                             as.character(mdy(Scan.Date)),
#                             Scan.Date))
# 
# 
# Release_05 <- Release %>%
#   mutate(Date = as.character(mdy(Date)))
# 
# Recaptures_05 <- Recaptures %>%
#   mutate(Date = as.character(mdy(Date)))
# 
# 
# source("functions/UI Functions/uifunctions.r")
# source("functions/modules/mod_animation.R")
# most_recent_date <- "2022-03-22"
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
#     tabPanel("Individual Datasets",
#              sidebarLayout(
#                sidebarPanel(
#                  dateRangeInput("drangeinput1", "Select a Date Range:",
#                                 #was accidnetly omitting events from the allevents tab bc the earliest release date is 2020-09-01
#                                 #earliest detection was 2020-09-03, which was what it was set at before
#                                 start = "2020-08-01",
#                                 end = most_recent_date), #end of date range input
#                  actionButton("button1", label = "Render Table")
#                ),
# 
#                mainPanel(tabsetPanel(
#                  tabPanel("graph1",
#                           imageOutput("plot13")),
#                  tabPanel("animation",
#                           withSpinner(imageOutput("plot12"))
#                  ),
#                  tabPanel("Stationary Clean",
#                           hr(),
#                           downloadButton(outputId = "download3", label = "Save this data as CSV"),
#                           hr(),
#                           withSpinner(DT::dataTableOutput("stationary1"))),
#                  tabPanel("Biomark",
#                           withSpinner(DT::dataTableOutput("biomark1"))),
#                  tabPanel("Mobile",
#                           withSpinner(DT::dataTableOutput("mobile1"))),
#                  tabPanel("Recaptures",
#                           withSpinner(DT::dataTableOutput("recaps1"))),
#                  tabPanel("Release",
#                           withSpinner(DT::dataTableOutput("release1"))),
# 
# 
#                           #withSpinner(DT::dataTableOutput("graph1")))
# 
#                ) #end of sidebarlayout: incldes sidebar panel and mainpanel
#                ) #end of individual datasets tabset panel
#              )#end of individual datasets Mainpanel)
# 
#     ),#end of Individual data tab panel
# 
# 
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output, session) {
# 
#   #biomark <- Ind_data_reactive(dateid = "drangeinput1_2", buttonid = "button1_2", dataset = Biomark)
# 
#   biomark <- eventReactive(input$button1_2,{
#     data_filtered <- Biomark %>%
#       filter(Scan.Date >= input$drangeinput1_2[1] & Scan.Date <= input$drangeinput1_2[2])
#   })
#   # biomark_filtered <- Biomark %>%
#   #   filter(Scan.Date >= input$drangeinput1_2[1] & Scan.Date <= input$drangeinput1_2[2])
#   # output$biomark1 <- renderDataTable(
#   #
#   #   biomark(),
#   #   rownames = FALSE,
#   #   #extensions = c('Buttons'),
#   #   #for slider filter instead of text input
#   #   filter = 'top',
#   #   options = list(
#   #     pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
#   #     dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
#   #     language = list(emptyTable = "Enter inputs and press Render Table")
#   #   )
#   # )
#   output$recaps1 <- renderDataTable(
# 
#     #indiv_datasets_list()$recapdata,
#     Recaptures_05,
#     rownames = FALSE,
#     #extensions = c('Buttons'),
#     #for slider filter instead of text input
#     filter = 'top',
#     options = list(
#       pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
#       dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
#       language = list(emptyTable = "Enter inputs and press Render Table")
# 
#       #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
#     )
#   )
# 
#   #output$biomark1 <- Ind_data_table_render(biomark())
# 
#     # output$distPlot <- renderPlot({
#     #     # generate bins based on input$bins from ui.R
#     #     x    <- faithful[, 2]
#     #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     #
#     #     # draw the histogram with the specified number of bins
#     #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
#     #          xlab = 'Waiting time to next eruption (in mins)',
#     #          main = 'Histogram of waiting times')
#     # })
#   #mod_animationServer("graph1", m1)
#   output$plot13 <- renderImage({
#     m1 <- m1 %>%
#       mutate(
#         days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
#         #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
#         # if you want to start at week 1 instead of week 0, add +1 to the end of expression
#         # when you change this too, it changes the number of entries in the states dataframe
#         weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks")))
#         #months_since = as.numeric(floor(difftime(Date, min(Date), units = "months")))
#       ) %>%
#       ungroup()
#     coords1 <- st_as_sfc(st_bbox(c(xmin = -106.0771, xmax = -105.8938, ymax = 40.14896, ymin = 40.05358), crs = st_crs(4326)))
# 
# 
# 
#     xy <- m1 %>%
#       select(X, Y)
#     # need to ungroup to get this code to work
#     spdf <- SpatialPointsDataFrame(coords = xy, data = m1,
#                                    proj4string = CRS("+proj=longlat")) #"+init=epsg:3857" #+proj=aeqd +lat_0=53.6 +lon_0=12.7
# 
#     webMercator <- spTransform(spdf, CRS("+init=epsg:3857"))
#     m3 <- as.data.frame(webMercator)
# 
#     num_weeks <- max(m1$weeks_since) - min(m1$weeks_since) + 1
# 
#     map_with_data <- ggplot() +
#       basemap_gglayer(coords1) +
#       scale_fill_identity() +
#       coord_sf() +
#       theme_classic() +
#       geom_point(data = m3, aes(x = X.1, y = Y.1,
#                                 size = 4,
#                                 color=movement_only, group=weeks_since))+
#       transition_time(weeks_since) +
#       ggtitle(
#         #paste("Date", m3$Date),
#         'Week after Project: {frame_time}',
#         subtitle = 'Frame {frame} of {nframes}')
# 
#     # map_with_data <- x1 +
#     #   geom_point(data = m3, aes(x = X.1, y = Y.1,
#     #                             size = 4,
#     #                             color=movement_only, group=weeks_since))
#     #map_with_data
# 
#     anim_save("outfile.gif", animate(map_with_data))
#     list(src = "outfile.gif", contentType = "image/gif")
# 
# 
#     animate(map_with_animation, nframes = num_weeks, fps = 2)
# 
#   }, deleteFile = TRUE)
# 
# 
#   output$plot12 <- renderImage({
#     #create days_since and weeks_since columns to use as frames for animation
#     m1 <- m1 %>%
#       mutate(
#         days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
#         #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
#         # if you want to start at week 1 instead of week 0, add +1 to the end of expression
#         # when you change this too, it changes the number of entries in the states dataframe
#         weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks")))
#         #months_since = as.numeric(floor(difftime(Date, min(Date), units = "months")))
#       ) %>%
#       ungroup()
#     #
#     coords1 <- st_as_sfc(st_bbox(c(xmin = -106.0771, xmax = -105.8938, ymax = 40.14896, ymin = 40.05358), crs = st_crs(4326)))
# 
#     x1 <- ggplot() +
#       basemap_gglayer(coords1) +
#       scale_fill_identity() +
#       coord_sf() +
#       theme_classic()
# 
# 
#     xy <- m1 %>%
#       select(X, Y)
#     # need to ungroup to get this code to work
#     spdf <- SpatialPointsDataFrame(coords = xy, data = m1,
#                                    proj4string = CRS("+proj=longlat")) #"+init=epsg:3857" #+proj=aeqd +lat_0=53.6 +lon_0=12.7
# 
#     webMercator <- spTransform(spdf, CRS("+init=epsg:3857"))
#     m3 <- as.data.frame(webMercator)
# 
#     map_with_data <- x1 +
#       geom_point(data = m3, aes(x = X.1, y = Y.1,
#                                 size = 4,
#                                 color=movement_only, group=weeks_since))
#     map_with_data
#     num_weeks <- max(m1$weeks_since) - min(m1$weeks_since) + 1
# 
# 
# 
# 
#     map_with_animation <- map_with_data +
#       transition_time(weeks_since) +
#       #### this is the title
#       ggtitle(
#         #paste("Date", m3$Date),
#         'Week after Project: {frame_time}',
#         subtitle = 'Frame {frame} of {nframes}')
#     #map_with_animation
# 
#     #animate(map_with_animation, nframes = num_weeks, fps = 2)
# 
#   },deleteFile = TRUE)    # end of render image
# }

# # Run the application 
# shinyApp(ui = ui, server = server)
######### NEW APP
library(shiny)
library(gganimate)
library(tidyverse)
library(av)
library(lubridate)
library(rgdal)
library(shinythemes)
library(plotly)
library(leaflet.minicharts)
library(lubridate)


## Example data
# undergradDATA <- mpg %>%
#   count(class, sort = TRUE) %>%
#   mutate(class = reorder(class, n)) %>%
#   rename(Undergrad = class, HEI = n)
Release <- read.csv("./data/WGFP_ReleaseData_Master.csv", na.strings = c(""," ","NA"), colClasses=c(rep("character",8), "numeric", "numeric",rep("character",8) ))

Release_05 <- Release %>%
  mutate(Date = as.character(mdy(Date)))

Movements_df <- read_csv("notes/movements.csv", col_types = cols(Datetime = col_character(), 
                                                                 TAG = col_character()))
Movements_df <- Movements_df %>%
  filter(!TAG %in% c("230000999999"))
# UTM_movements_summed <- Movements_df %>%
#   ungroup() %>%
#   mutate(weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks")))
#   ) %>%
#   group_by(UTM_X, UTM_Y, weeks_since) %>%
#   mutate(total = n(),
#          date_week = as.Date("2020-09-01")+weeks(weeks_since))  %>%
#   distinct(UTM_X, UTM_Y, date_week, .keep_all = TRUE)

WeeklyMovementsbyType <- Movements_df %>%


  ungroup() %>%
  ### mobile filter
  filter(!det_type %in% c("Mobile Run")) %>%
  mutate(weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))),
         date_week = as.Date("2020-09-01")+weeks(weeks_since)
  ) %>%
  group_by(UTM_X, UTM_Y, date_week, movement_only) %>%
  mutate(total = n()) %>%
  distinct(UTM_X, UTM_Y, date_week, movement_only, .keep_all = TRUE) %>%
  pivot_wider(id_cols = c("X", "Y", "det_type", "date_week"), names_from = movement_only, values_from = total) %>%
  select(-9)

WeeklyMovementsbyType[is.na(WeeklyMovementsbyType)] <- 0

## this part is making new row with UTM's to get the initla release back down to 0 to not show as a big bar graph the whole time on minicharts
#just decided I'm going to put a disclaimer that it doesn't work as well with mobile detections
WeeklyMovementsbyType2 <- WeeklyMovementsbyType %>%
  group_by(X, Y, det_type) %>%
  arrange(date_week) %>%
  mutate(nextinitialRelease = lead(`Initial Release`, order_by = date_week)
  )
x <- WeeklyMovementsbyType2
df_skeleton <- WeeklyMovementsbyType2[1,]
df_skeleton[1,] <- list(-106, 55, NA, NA, 0, 0, 0, 0, 0, 0)

for (row in 1:nrow(WeeklyMovementsbyType2)) {
  #print(x$nextinitialRelease[row])
  if(is.na(WeeklyMovementsbyType2$nextinitialRelease[row]) & WeeklyMovementsbyType2$`Initial Release`[row] > 0) {
    #gettig values to make a new row with
    print("na val")
    X1 <- WeeklyMovementsbyType2$X[row]

    Y1 <- WeeklyMovementsbyType2$Y[row]
    next_week <- WeeklyMovementsbyType2$date_week[row] + weeks(1)

    events <- 0
    det_type <- WeeklyMovementsbyType2$det_type[row]

    new_row <- df_skeleton
    new_row[1,] <- list(X1, Y1, det_type, next_week, events, NA, NA, NA, NA, NA)
    WeeklyMovementsbyType2 <- rbind(WeeklyMovementsbyType2, new_row)
  }
}



# WeeklyMovementsbyType2 <- WeeklyMovementsbyType2 %>%
#   filter(date_week > "2020-10-01")
#   #mutate(weeks_since = as.numeric(floor(difftime(date_week, min(date_week), units = "weeks")))
#   #)

## need to make it so the week after a release event happens, and there's no data, a row is added with thos eexact same utm's that will have a 0.
  
#Movements_df <- Movements_df[1:300, ] #sample(nrow(Movements_df), 20)
source("functions/Animation_function.R")
animationDatalist <- Animation_function(Movements_df)

for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}

ui <- fluidPage(
  navbarPage(title = "WGFP Data Exploration",
             id = "tabs", 
             theme = shinytheme("sandstone"),
             tabPanel("Daily Movements Map, Plot, and Data",
                      value = "MovementsTab",
                      movements_UI("MovementsTab1", Movements_df, df_list)
             ),
             tabPanel("Weekly States",
                      value = "StatesTab",
                      States_UI("StatesTab1", states_data_list)
             ),#end of States ui Tab
             tabPanel("test", 
                      sidebarLayout(
                        sidebarPanel(
                          textInput("anim_Title", "Animation Title"),
                          
                          radioButtons("radio2", "Timeframe", 
                                       choices = c("days", "weeks"), 
                                       selected = "weeks"),
                          sliderInput("pointSize_Slider", "Select Size of Point", 
                                      min = 1, 
                                      max = 25, 
                                      value = 10),
                          sliderInput("fps_Slider", "Select frames per Second", 
                                      min = 0, 
                                      max = 6, 
                                      value = 2, 
                                      step = .2),
                          actionButton("button9", "Render Animation: Need to click 'Render Map and Data' button in Sidebar first. Takes a couple minutes to render usually"), 
                          
                        ),
                        
                        mainPanel(imageOutput("plot12"))
                      )
                    )

  
) #end of navbar page
)
server <- function(input, output) {
  movements_Server("MovementsTab1", Movements_df, WeeklyMovementsbyType2 )
  
    States_Server("StatesTab1", states_data_list, weeks)
  
  
  observeEvent(input$button9, {
    #animationDatalist <- Animation_function(filtered_movements_data())
    set_defaults(map_service = "esri", map_type = "world_imagery")
    
    map_with_data <- ggplot() +
      basemap_gglayer(animationDatalist$coords1) +
      scale_fill_identity() +
      coord_sf() +
      theme_classic() +
      
      shadow_wake(wake_length = 0.1, alpha = FALSE) +
      guides(size = FALSE, color = guide_legend(title = "Movement"))
    
    
    
  output$plot12 <- renderImage(
    {
      
      
      if (input$radio2 == "weeks"){
        map_with_data <- map_with_data + 
          geom_point(data = animationDatalist$data, aes(x = animationDatalist$data$X.1, y = animationDatalist$data$Y.1,
                                                        size = input$pointSize_Slider,
                                                        color = animationDatalist$data$movement_only, group = animationDatalist$data$weeks_since))+
          transition_time(weeks_since) +
          transition_reveal(Date) +
          ggtitle(
            #paste("Date", m3$Date),
            paste(input$anim_Title, '{frame_time}'),
            subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist$data$Date) ))
        map_with_data
        anim_save("outfile.gif", animate(map_with_data, nframes = animationDatalist$num_weeks, fps = input$fps_Slider, height = 1200, width =1200)) # New
        
        
      } else if (input$radio2 == "days"){
        map_with_data <- map_with_data + 
          geom_point(data = animationDatalist$data, aes(x = animationDatalist$data$X.1, y = animationDatalist$data$Y.1,
                                                        size = input$pointSize_Slider,
                                                        color = animationDatalist$data$movement_only, group = animationDatalist$data$days_since))+
          transition_time(days_since) + 
          labs(title = "Days") +
          ggtitle(
            
            paste(input$anim_Title, '{frame_time}'),
            subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(animationDatalist$data$Date) ))
        map_with_data
        anim_save("WindyGapFishMovements.gif", animate(map_with_data, nframes = animationDatalist$num_days, fps = input$fps_Slider, height = 1200, width =1200)) # New
        
      }
      
      
      list(src = "WindyGapFishMovements.gif", contentType = "image/gif")
 #end of isolate
      #if radiobuttons$ideo selected:
      #animate(map_with_animation, nframes = num_weeks, fps = 4, renderer = av_renderer())
      # anim_save("example2.mpg", animate(map_with_data, nframes = num_weeks, fps = 2,  renderer = av_renderer())) # New
      # list(src = "example2.mpg", contentType = "video/mpg")
      # 
      #anim_save("example2.mpg")
    },
    deleteFile = TRUE
  )
  })
}

shinyApp(ui, server)
