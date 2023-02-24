#movementsMod
## movements module
movements_UI <- function(id, Movements_df, df_list) { #could just get dates in UI and then just pass dates instead but no bigggie
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   textInput(ns("textinput3"), label = "Filter by TAG"),
                   pickerInput(ns("picker6"),
                               label = "Select Movement Type:",
                               choices = sort(unique(Movements_df$movement_only)),
                               selected = unique(Movements_df$movement_only),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 6 input
                   
                   pickerInput(ns("picker7"),
                               label = "Select Detection Type",
                               choices = sort(unique(Movements_df$det_type)),
                               selected = unique(Movements_df$det_type),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 7 
                   pickerInput(ns("picker10"),
                               label = "Select Species Type",
                               choices = sort(unique(Movements_df$Species)),
                               selected = unique(Movements_df$Species),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 10 
                   
                   sliderInput(ns("slider10"), "Fish Release Length",
                               min = min(Movements_df$Release_Length, na.rm = TRUE),
                               max = max(Movements_df$Release_Length, na.rm = TRUE),  
                               value = c(min(Movements_df$Release_Length, na.rm = TRUE),max(Movements_df$Release_Length, na.rm = TRUE)),
                               step = 1,
                               
                   ), #end of slider8
                   
                   
                   sliderInput(ns("slider2"), "Date",
                               min = min(df_list$All_Events$Date -1),
                               max = max(df_list$All_Events$Date +1),  
                               value = c(min(df_list$All_Events$Date -1),max(df_list$All_Events$Date +1)),
                               step = 1,
                               timeFormat = "%d %b %y",
                               #animate = animationOptions(interval = 500, loop = FALSE)
                   ),
                   
                   sliderInput(ns("slider9"), "Total distance travelled (m)",
                               min = min(Movements_df$sum_dist, na.rm = TRUE),
                               max = max(Movements_df$sum_dist, na.rm = TRUE),  
                               value = c(min(Movements_df$sum_dist, na.rm = TRUE),max(Movements_df$sum_dist, na.rm = TRUE)),
                               step = 1,
                               
                   ), #end of slider8
                   actionButton(ns("button7"), label = "Render Map and Data"), 
                   hr(),
      ),#end of sidebar panel
      mainPanel(width = 10,
                tabsetPanel(
                  tabPanel("Map and Table",
                           hr(),
                           splitLayout(cellWidths = c("40%", "60%"),
                                       
                                       withSpinner(DT::dataTableOutput(ns("movements1"))),
                                       withSpinner(leafletOutput(ns("map1"), height = 600))
                           ), #end of splitLayout
                           hr(),
                           # downloadButton(ns("download6"), label = "Save movements data as CSV"),
                           # hr(),
                  ), # end of Map and table tabPanel
                  tabPanel("Movement Graphs",
                           withSpinner(plotlyOutput(ns("plot1"))),
                           hr(),
                           withSpinner(plotlyOutput(ns("plot6"))),
                           # downloadButton(ns("download8"), label = "Save seasonal plot data as CSV"),
                           hr(),
                           withSpinner(plotlyOutput(ns("plot7"))),
                           hr(),
                           withSpinner(plotlyOutput(ns("plot8"))),
                           hr(),
                           withSpinner(plotlyOutput(ns("plot9"))),
                           hr(),
                           
                           verbatimTextOutput(ns("text2")),
                  ), #end of movement graphs tabpanel
                  tabPanel("Animation",
                           br(),
                           fluidRow(
                             column(width = 4,
                                    radioButtons(ns("radio2"), "Timeframe", 
                                                 choices = c("days", "weeks"), 
                                                 selected = "weeks"),
                                    # sliderInput("pointSize_Slider", "Select Size of Point", 
                                    #             min = 1, 
                                    #             max = 12, 
                                    #             value = 4),
                                    sliderInput(ns("fps_Slider"), "Select frames per Second",
                                                min = 0,
                                                max = 15,
                                                value = 2,
                                                step = .2),
                             ),#end of column
                             column(width = 4, 
                                    textInput(ns("anim_Title"), "Animation Title"),
                                    # radioButtons("renderOption", "Render as GIF or Video", 
                                    #              choices = c("GIF","Video"))
                             )
                           ),#end of fluidrow
                           
                           actionButton(ns("button9"), "Render Animation: Need to click 'Render Map and Data' button in Sidebar first. Takes a couple minutes to render usually"), 
                           imageOutput(ns("plot12"))
                  ) #end of animation tabPanel
                ), # end of tabset panel
      )#end of mainPanel
    )#end of sidebarLayout including sidebarPanel and Mainpanel
    )
}

movements_Server <- function(id, Movements_df) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      filtered_movements_data <- eventReactive(input$button7,{
        
        
        if(input$textinput3 != ''){
          movements_data1 <- Movements_df %>%
            filter(TAG %in% trimws(input$textinput3),
                   Release_Length >= input$slider10[1] & Release_Length <= input$slider10[2],
                   Date >= input$slider2[1] & Date <= input$slider2[2],
                   movement_only %in% c(input$picker6),
                   det_type %in% c(input$picker7),
                   Species %in% c(input$picker10),
                   sum_dist >= input$slider9[1] & sum_dist <= input$slider9[2],
                   
            ) %>%
            arrange(Datetime)
          #this id column is used for the map and datatable proxy and needs to be redone each time a filter is applied
          movements_data1$id <- seq.int(nrow(movements_data1))
          
        } else {
          movements_data1 <- Movements_df  %>% 
            filter(
              Release_Length >= input$slider10[1] & Release_Length <= input$slider10[2],
              Date >= input$slider2[1] & Date <= input$slider2[2],
              movement_only %in% c(input$picker6),
              det_type %in% c(input$picker7),
              Species %in% c(input$picker10),
              sum_dist >= input$slider9[1] & sum_dist <= input$slider9[2]
              
            ) %>%
            arrange(Datetime)
          movements_data1$id <- seq.int(nrow(movements_data1))
          
        }
        
        
        return(movements_data1)
      }) 
      #seasonally
      seasonal_movts <- reactive({filtered_movements_data() %>%
          group_by(month(Date), day(Date), movement_only) %>%
          summarise(total_events = n())
      })
      
      

# Map/Table ---------------------------------------------------------------


        # to keep track of previously selected row
        #setting to nothing for now
      # to keep track of previously selected row
      #setting to nothing for now
      row_selected1 <- reactiveVal()
      #this is what the new icon for selected rows looks like
      my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'lightblue', iconColor = 'red')
      
      icons <- reactive({
        awesomeIcons(
          icon = 'ios-close',
          iconColor = filtered_movements_data()$icon_color,
          library = 'ion',
          markerColor = filtered_movements_data()$marker_color
        )
      })
      output$movements1 <- renderDT({
        
        
        datatable(filtered_movements_data(),
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  extensions = c(
                    "Buttons"
                  ),
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                    
                  )
        ) 
        
        
      })
      
      observeEvent(input$movements1_rows_selected, {
        row_selected = filtered_movements_data()[input$movements1_rows_selected,]
        
        proxy <- leafletProxy('map1')
        
        proxy %>%
          #clearing the group removes previous marker from previuous row before making a new one
          clearGroup(group = "markers") %>%
          
          addAwesomeMarkers(
            clusterOptions = markerClusterOptions(),
            group = "markers",
            popup = paste(
              "TAG:", row_selected$TAG, "<br>",
              "Release Site:", row_selected$ReleaseSite, "<br>",
              "Detection Event:", row_selected$det_type, "<br>",
              "Date:", row_selected$Datetime),
            
            layerId = as.character(row_selected$id),
            lng=row_selected$X, 
            lat=row_selected$Y,
            icon = my_icon)
        
        
      })
      
      
      
      
      
# Movements Map Output ----------------------------------------------------
      
      output$map1 <- renderLeaflet({
        
        
        
        
        leaflet(filtered_movements_data()) %>% #Warning: Error in UseMethod: no applicable method for 'metaData' applied to an object of class "NULL"  solved becuase leaflet() needs an arg leaflet(x)
          addProviderTiles(providers$Esri.WorldImagery,
                           options = providerTileOptions(maxZoom = 19.5)
          ) %>%
          ##detections: based off reactives
          addAwesomeMarkers(
            group = "Detections",
            clusterOptions = markerClusterOptions(),
            lng=~X, 
            lat = ~Y,
            icon = icons(),
            label = paste(filtered_movements_data()$movement_only, "\n",
                          filtered_movements_data()$Date),
            layerId = as.character(filtered_movements_data()$id),
            popup = paste(
              "TAG:", filtered_movements_data()$TAG, "<br>",
              "Release Site:", filtered_movements_data()$ReleaseSite, "<br>",
              "Detection Event:", filtered_movements_data()$det_type, "<br>",
              "Date:", as.character(filtered_movements_data()$Datetime))
          ) %>%
          
          ###polylines and points: obtained from GISdb from this study
          addAwesomeMarkers(data = antenna_sites@coords,
                            icon = Station_icons,
                            clusterOptions = markerClusterOptions(),
                            label = paste(antenna_sites@data$SiteLabel),
                            popup = paste(antenna_sites@data$SiteName, "<br>",
                                          "Channel Width:", antenna_sites@data$ChannelWid, "feet"),
                            group = "Antennas") %>% # error: don't know jow to get path Data from x....solved by specifying coordinate location with @ within data
          addPolylines(data = stream_centerline@lines[[1]], 
                       color = "blue",
                       opacity = 1,
                       popup = paste("Colorado River Centerline"),
                       group = "Stream Centerlines") %>%
          addPolylines(data = stream_centerline@lines[[2]],
                       color = "blue",
                       opacity = 1,
                       popup = paste("Fraser River Centerline"),
                       group = "Stream Centerlines") %>%
          addPolylines(data = mobile_reaches,
                       color = "yellow",
                       opacity = 1,
                       label = mobile_reaches@data$River,
                       popup = paste("Mobile Run:", mobile_reaches@data$River, 
                                     "<br>"),
                       group = "Mobile Reaches") %>%
          addAwesomeMarkers(data = releasesites@coords,
                            icon = release_icons,
                            clusterOptions = markerClusterOptions(),
                            label = releasesites@data$ReleaseSit, 
                            popup = paste("Release Date1:", releasesites@data$ReleaseDat, "<br>","Release Date 2:",  releasesites@data$ReleaseD_1),
                            group = "Release Sites") %>%
          addPolylines(data = simple_stations2, 
                       label = simple_stations2@data$ET_STATION,
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                       group = "Stations (m)") %>%
          addLayersControl(overlayGroups = c("Detections", "Antennas", "Release Sites", "Stream Centerlines", "Stations (m)", "Mobile Reaches")) %>%
          hideGroup(c("Stream Centerlines", "Stations (m)", "Antennas", "Release Sites", "Mobile Reaches"))
        
        
      })
      
      #when map is clicked, go to that icon in the dataTable
      # pagination wasn't working bc ID being assigned was different than row number; 
      #so when a icon was clicked, it was selecting the right row, but the row number was different than Id so it was going to the wrong page
      #fixed by assigning a new Id column evyertime the data is filtered; might be a more efficient way to do this but idk/idc
      observeEvent(input$map1_marker_click, {
        #need to assign layer ID's in leafletrender to have an id associated with the click
        #clicking the map gives info in the form of a list, including the layer id assigned in leaflet
        clickId <- input$map1_marker_click$id
        
        #saying get the rows in the data with the same id as clickId; clickId is the row number
        dataTableProxy("movements1") %>%
          selectRows(which(filtered_movements_data()$id == clickId)) %>%
          selectPage(which(input$movements1_rows_all == clickId) %/% input$movements1_state$length + 1)
      })


# Movements Animation Output ----------------------------------------------
      observeEvent(input$button9, {
        animationDatalist <- Animation_function(filtered_movements_data())
        set_defaults(map_service = "esri", map_type = "world_imagery")
        
        map_with_data <- ggplot() +
          basemap_gglayer(animationDatalist$coords1) +
          scale_fill_identity() +
          coord_sf() +
          theme_classic() +
          guides(size = FALSE, color = guide_legend(title = "Movement"))
        
        
        
        output$plot12 <- renderImage(
          {
            if (input$radio2 == "weeks"){
              map_with_data <- map_with_data + 
                geom_point(data = animationDatalist$data, aes(x = animationDatalist$data$X.1, y = animationDatalist$data$Y.1,
                                                              size = input$pointSize_Slider,
                                                              color = animationDatalist$data$movement_only, group = animationDatalist$data$weeks_since))+
                transition_time(weeks_since) +
                ggtitle(
                  #paste("Date", m3$Date),
                  paste(input$anim_Title, '{frame_time}'),
                  subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist$data$Date) ))
              map_with_data
              anim_save("WindyGapFishMovements.gif", animate(map_with_data, nframes = animationDatalist$num_weeks, fps = input$fps_Slider, height = 1200, width =1200)) # New
              
              
            } else if (input$radio2 == "days"){
              map_with_data <- map_with_data + 
                geom_point(data = animationDatalist$data, aes(x = animationDatalist$data$X.1, y = animationDatalist$data$Y.1,
                                                              size = 10,
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
          },
          deleteFile = FALSE
        )
      })
        
# Movement Plots Output ----------------------------------------------------
        observe({
        #daily
        output$plot1 <- renderPlotly({
          plot <- filtered_movements_data() %>%
            ggplot(aes(x = Date, fill = movement_only,
                       text = paste('Date: ', as.character(Date), '\n'))
            ) +
            geom_bar(stat = "count", position = "dodge") +
            theme_classic() +
            labs(title="Fish Movement by Day",
                 x ="Date", y = "Count") +
            scale_fill_manual(values = c("Downstream Movement" = "red",
                                         "Upstream Movement" = "chartreuse3",
                                         "No Movement" = "black",
                                         "Initial Release" = "darkorange",
                                         "Changed Rivers" = "purple"))
          
        })    
        
        




        output$plot6 <- renderPlotly({

          plot <- seasonal_movts() %>%
            mutate(merged = (parse_date_time(paste(`month(Date)`, `day(Date)`), "md"))) %>%
            ggplot(aes(x = merged, y = total_events, fill = movement_only)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_classic() +
            labs(title = "Seasonal Daily Movements", x = "Day", y = "Counts",
                 caption = "Currently No download option for this data.") +
            scale_x_datetime(date_labels = "%b") +
            scale_fill_manual(values = c("Downstream Movement" = "red",
                                         "Upstream Movement" = "chartreuse3",
                                         "No Movement" = "black",
                                         "Initial Release" = "darkorange",
                                         "Changed Rivers" = "purple"))

          ggplotly(plot)
        })

        # Total movements
        output$plot7 <- renderPlotly({
          plot <- filtered_movements_data() %>%
            filter(
              !dist_moved %in% c(0)) %>%
            ggplot(aes(x = dist_moved)) +
            geom_histogram(binwidth = 50) +
            theme_classic() +
            labs(title = "Each movement detected: ('No movements' excluded)", subtitle = "Groupings are 50 m")
          ggplotly(plot)

          plotly1 <- ggplotly(p = plot)
          plotly1
        })

        #cumulative movement
        output$plot8 <- renderPlotly({
          plot <- filtered_movements_data() %>%
            ggplot(aes(x = sum_dist)) +
            geom_histogram(binwidth = 300) +
            theme_classic() +
            labs(title = "Cumulative movement", subtitle = "Groupings are 300 m")
          plotly1 <- ggplotly(p = plot)
          plotly1

        })

        output$plot9 <- renderPlotly({
          plot <- filtered_movements_data() %>%
            ggplot(aes(x = hour(Datetime), fill = movement_only)) +
            geom_histogram(binwidth = 1) +
            theme_classic() +
            labs(title = "Detections by Hour")
          plotly1 <- ggplotly(p = plot)
          plotly1

        })
        
        output$text2 <- renderPrint({
          "'Fish Movement by Day' plot renders table data shown in the 'map and table' tab. 
      'Seasonal Daily Movements' graph data can be downloaded below."
        })
        
        
      }) #end of observe
      
      
      
      
    }
  )
}