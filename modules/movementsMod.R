#movementsMod
## movements module
movements_UI <- function(id, Movements_df) { #could just get dates in UI and then just pass dates instead but no bigggie
  ns <- NS(id)
  useShinyjs()
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   movementsFiltered_UI(ns("movementModFilters"), Movements_df)
      ),#end of sidebar panel
      mainPanel(width = 10,
                tabsetPanel(
                  tabPanel("Map and Table",
                           hr(),
                           tags$head(
                             tags$style(HTML("
                                .table-container {
                                    position: absolute;
                                    bottom: 0; /* Position the table at the bottom of the map */
                                    left: 5; /* Align the table to the left */
                                    width: 40%; /* Table width is 100% of the map */
                                    height: 550px; /* Initial height of the table */
                                    background: rgba(255, 255, 255, 0.9); /* Semi-transparent background */
                                    border-top: 1px solid #ccc; /* Border at the top to separate from the map */
                                    box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.5); /* Shadow effect for visibility */
                                    z-index: 1000; /* Keeps the table on top */
                                    overflow: hidden; /* Prevents content overflow */
                                  }
                                .table-header {
                                  background: #007bff;
                                  color: white;
                                  padding: 5px 10px;
                                  font-weight: bold;
                                }
                                .toggle-container {
                                  position: absolute;
                                  top: 80px;
                                  left: 10px;
                                  z-index: 1200; /* Keeps the toggle button on top */
                                }
                              "))
                            ),
                           fluidRow(
                             column(
                               width = 12,
                               # Full-width Leaflet map
                               div(
                                 style = "position: relative;", # Ensure map is the relative parent
                                 withSpinner(leafletOutput(ns("map1"), height = "700px")),
                                 div(
                                   id = "toggle-container",
                                   #in a module, use classes instead of IDs because using ns() modifies the Id of the element (makes it movements_module-table-container)
                                   class = "toggle-container",
                                   actionButton(ns("toggle_table"), "Toggle Table") # Button remains always visible
                                 ),
                                 jqui_resizable(
                                   div(
                                     id = ns("table-container"), # Draggable and resizable container
                                     class = "table-container",
                                     div(
                                       id = "table-header", # Draggable header
                                       "Movements Table" # Label for the draggable area
                                     ),
                                     div(
                                       id = "table-wrapper",
                                       style = "width: 100%; height: calc(100% - 30px); overflow: auto;", # Adjust to account for header height
                                       withSpinner(DTOutput(ns("movements1")))
                                     )
                                   )
                                 ) 
                               )
                             )
                           ),
                           hr(),
                           downloadData_UI(ns("downloadmovements1")),
                  ), # end of Map and table tabPanel
                  tabPanel("Minicharts Map",
                           leafletOutput(ns("map2"), height="80vh"), 
                           h6("Mobile data always excluded. Movement data filtered with sidebar filters and aggregated on a weekly scale.")
                           ),
                  
                  tabPanel("Movement Graphs",
                           tabsetPanel(
                             tabPanel("Movement by Day",
                               br(),
                               withSpinner(plotlyOutput(ns("plot1"))),
                               hr(), 
                               withSpinner(DTOutput(ns("movementByDayTable"))), 
                               br(),
                               downloadData_UI(ns("downloadmovementByDayTable")), 
                             ), 
                             tabPanel("Movement by Season", 
                               br(),
                               withSpinner(plotlyOutput(ns("plot6"))),
                               hr(),
                               withSpinner(DTOutput(ns("seasonalMovementTable"))),
                               downloadData_UI(ns("downloadplot6"))
                             ), 
                             tabPanel("Individual Movement", 
                                      br(), 
                                      withSpinner(plotlyOutput(ns("plot7"))),
                                      sliderInput(ns("indMovementBinwidthSlider"), "Distance Binwidth",
                                                  min = 1, 
                                                  max = max(Movements_df$dist_moved, na.rm = TRUE),
                                                  value = 50),
                                      hr()
                                      ), 
                             tabPanel("Cumulative Movement", 
                                      br(),
                                      withSpinner(plotlyOutput(ns("plot8"))),
                                      sliderInput(ns("cumulativeMovementBinwidthSlider"), "Distance Binwidth",
                                                  min = 1, 
                                                  max = 10000,
                                                  value = 300),
                                      
                                      hr()
                                      ), 
                             tabPanel("Movement by Hour", 
                                      br(),
                                      withSpinner(plotlyOutput(ns("plot9"))),
                                      hr(), 
                                      h6("Data may be misleading; Movements are calculated by the day and if a fish has multiple detections on the same day at the same antenna,
                 the first detection will be used which can fall commonly at hour 0",
                                      )
                                      )
                           ),
                           
                  ), #end of movement graphs tabpanel
                  tabPanel("Animation",
                           br(),
                           mod_animationUI(ns("movements_animation"))

                  ) #end of animation tabPanel
                ), # end of tabset panel
      )#end of mainPanel
    )#end of sidebarLayout including sidebarPanel and Mainpanel
    )
}

movements_Server <- function(id, Movements_df, allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
      filtered_movements_data <- movementsFiltered_Server("movementModFilters", Movements_df)
      #seasonally
      seasonal_movts <- reactive({filtered_movements_data() %>%
          group_by(lubridate::month(Date), day(Date), movement_only) %>%
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
      
      downloadData_Server("downloadmovements1", filtered_movements_data(), "MovementsData")
      
      output$movements1 <- renderDT({
        
        req(filtered_movements_data())
        datatable(
          filtered_movements_data(),
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          options = list(
            scrollX = TRUE,
            stateSave = TRUE,
            pageLength = 5,
            info = TRUE,
            lengthMenu = list(c(1, 5, 10, 25, 50, 100, 200), c("1", "5", "10", "25", "50", "100", "200")),
            dom = 'lfrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "No data to display for the selected filters.")
          )
        ) %>%
          formatRound(columns = c("UTM_X", "UTM_Y"), digits = 0, mark = "")
        
        
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
      
      # Toggle table visibility
      observeEvent(input$toggle_table, {
        shinyjs::toggle(id = "table-container")
      })
      
      
      

# Minicharts Map outout ---------------------------------------------------

      WeeklyMovementsbyType <- reactive({
        Wrangleminicharts_function(filtered_movements_data())
      })
      
  output$map2 <- renderLeaflet({
    #all options for chart
    chartColumns <- c("Changed Rivers", "Downstream Movement", "Initial Release", "No Movement", "Upstream Movement")
    #these are the columns you can actually use in the data because you don't always have all movement types 
     subsetChartColumns <- chartColumns[which(chartColumns %in% names(WeeklyMovementsbyType()))]
     
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(maxZoom = 19.5)
      ) %>%
      addAwesomeMarkers(data = antenna_sites,
                        icon = Station_icons,
                        #clusterOptions = markerClusterOptions(),
                        label = paste(antenna_sites$SiteLabel),
                        popup = paste(antenna_sites$SiteName),
                        group = "Antennas") %>%
      addAwesomeMarkers(data = releasesites,
                        icon = release_icons,
                        clusterOptions = markerClusterOptions(),
                        label = releasesites$ReleaseSit, 
                        popup = paste("Release Date1:", releasesites$ReleaseDat, "<br>","Release Date 2:",  releasesites$ReleaseD_1),
                        group = "Release Sites") %>%
      ###minicharts
      addMinicharts(
        lng =  WeeklyMovementsbyType()$X,
        lat = WeeklyMovementsbyType()$Y,
        #layerId = WeeklyMovementsbyType()$det_type,
        type = "bar",
        maxValues = 50,
        height = 100,
        width = 45,
        #chartdata columns are organized the same as sort(unique(movements_list$Movements_df$movement_only)) so that movement color values will line up correctly
        chartdata = WeeklyMovementsbyType()[,subsetChartColumns],
        #gets desired colors based off movements
        colorPalette = unname(allColors[subsetChartColumns]), 
        time = WeeklyMovementsbyType()$date_week
        
      ) %>%
      addLayersControl(overlayGroups = c("Antennas", "Release Sites"), 
                       baseGroups = c("Satellite")
      ) %>%
      hideGroup(c("Antennas", "Release Sites"))
  })

      
# Movements Map Output ----------------------------------------------------
      
      output$map1 <- renderLeaflet({
        
        leaflet(filtered_movements_data()) %>% #Warning: Error in UseMethod: no applicable method for 'metaData' applied to an object of class "NULL"  solved becuase leaflet() needs an arg leaflet(x)
          addProviderTiles(providers$Esri.WorldImagery,
                           options = providerTileOptions(maxZoom = 19.5), 
                           group = "Satellite"
          ) %>%
          
          setView(lng = -106.0773, lat = 40.14075, zoom = 12) %>%
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
          addAwesomeMarkers(data = antenna_sites,
                            icon = Station_icons,
                            clusterOptions = markerClusterOptions(),
                            label = paste(antenna_sites$SiteLabel),
                            popup = paste(antenna_sites$SiteName),
                            group = "Antennas") %>% # error: don't know how to get path Data from x....solved by specifying coordinate location with @ within data
          addPolylines(data = stream_centerline[stream_centerline$River == "Colorado River",], 
                       color = "blue",
                       opacity = 1,
                       popup = paste("Colorado River Centerline"),
                       group = "Stream Centerlines") %>%
          addPolylines(data = stream_centerline[stream_centerline$River == "Fraser River",],
                       color = "blue",
                       opacity = 1,
                       popup = paste("Fraser River Centerline"),
                       group = "Stream Centerlines") %>%
          addPolylines(data = mobile_reaches,
                       color = "yellow",
                       opacity = 1,
                       label = mobile_reaches$River,
                       popup = paste("Mobile Run:", mobile_reaches$River, 
                                     "<br>"),
                       group = "Mobile Reaches") %>%
          addAwesomeMarkers(data = releasesites,
                            icon = release_icons,
                            clusterOptions = markerClusterOptions(),
                            label = releasesites$ReleaseSit, 
                            popup = paste("Release Date1:", releasesites$ReleaseDat, "<br>","Release Date 2:",  releasesites$ReleaseD_1),
                            group = "Release Sites") %>%
          addPolylines(data = simpleStations, 
                       label = simpleStations$ET_STATION,
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                       group = "Stations (m)") %>%
          addPolygons(data = WGFP_States_2024,
                      label = WGFP_States_2024$State, 
                      color = "#702963", 
                      group = "States") %>%
         
          addLayersControl(overlayGroups = c("Detections", "Antennas", "Release Sites", "Stream Centerlines", "Stations (m)", "Mobile Reaches", "States"), 
                           baseGroups = c("Satellite")
                                          ) %>%
          hideGroup(c("Stream Centerlines", "Stations (m)", "Antennas", "Release Sites", "Mobile Reaches", "States")) %>%
          addMeasure(primaryLengthUnit = "meters")
        
        # leafletProxy("map") %>%
        #   ### always remove the prior minichart
        #   removeMinicharts(cems$towncode)
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
      #Even though filtered_movements_data is reactive, when I pass this as unreactive, I am passing the reactive object
      #then inside the module, i call is with (), getting the evaluated result
      mod_animationServer("movements_animation", filtered_movements_data = filtered_movements_data, allColors = allColors) 

        
# Movement Plots Output ----------------------------------------------------
      observe({
        #daily
        output$plot1 <- renderPlotly({
          filtered_movements_data() %>%
            ggplot(aes(x = Date, fill = movement_only,
                       text = paste('Date: ', as.character(Date), '\n'))
            ) +
            geom_bar(stat = "count", position = "dodge") +
            theme_classic() +
            labs(title="Fish Movement by Day",
                 x ="Date", y = "Count") +
            scale_fill_manual(values = allColors)
          
        })
        #not sure if we want to do by species but we could 
        if(nrow(filtered_movements_data() >0)){
          movementByDayDataForTable <- filtered_movements_data() %>%
            count(Date, name = "Number of Movements")
        } else{
          movementByDayDataForTable <- data.frame("Date" = character())
        }
        
        
        
        output$movementByDayTable <- renderDT({
          
          
          datatable(
            movementByDayDataForTable,
            rownames = FALSE,
            selection = "single",
            filter = 'top',
            #extensions = c("Buttons"),
            options = list(
              #statesave is restore table state on page reload
              stateSave = TRUE,
              pageLength = 10,
              info = TRUE,
              lengthMenu = list(c(10, 25, 50, 100, 200), c("10", "25", "50", "100", "200")),
              dom = 'lfrtip',
              #had to add 'lowercase L' letter to display the page length again
              language = list(emptyTable = "No data found for selected filters")
            )
          )
          
        })
        
        downloadData_Server("downloadmovementByDayTable", movementByDayDataForTable, "MovementByDayCounts")
        
        
        
        output$plot6 <- renderPlotly({
          
          plot <- seasonal_movts() %>%
            mutate(merged = (parse_date_time(paste(`lubridate::month(Date)`, `day(Date)`), "md"))) %>%
            ggplot(aes(x = merged, y = total_events, fill = movement_only)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_classic() +
            labs(title = "Seasonal Daily Movements", x = "Day", y = "Counts",
                 caption = "Currently No download option for this data.") +
            scale_x_datetime(date_labels = "%b") +
            scale_fill_manual(values = allColors)
          
          ggplotly(plot)
        })
        
        output$seasonalMovementTable <- renderDT({
          
          datatable(
            seasonal_movts(),
            rownames = FALSE,
            selection = "single",
            filter = 'top',
            options = list(
              stateSave = TRUE,
              pageLength = 10,
              info = TRUE,
              lengthMenu = list(c(10, 25, 50, 100, 200), c("10", "25", "50", "100", "200")),
              dom = 'lfrtip',
              language = list(emptyTable = "Enter inputs and press Render Table")
            )
          )
          
        })
        
        downloadData_Server("downloadplot6", seasonal_movts(), "SeasonalMovementsData")
        
        # Total movements
        output$plot7 <- renderPlotly({
          plot <- filtered_movements_data() %>%
            filter(
              !dist_moved %in% c(0)) %>%
            ggplot(aes(x = dist_moved, fill = Species)) +
            geom_histogram(binwidth = input$indMovementBinwidthSlider) +
            theme_classic() +
            labs(title = "Each movement detected: ('No movements' excluded)", subtitle = "Groupings are 50 m", 
                 x = "Distance Between Detections Moved (m)", y = "Count") +
            scale_fill_manual(values = allColors)
          
          
          ggplotly(plot)
          
        })
        
        #cumulative movement
        output$plot8 <- renderPlotly({
          plot <- filtered_movements_data() %>%
            distinct(TAG, .keep_all = TRUE) %>%
            ggplot(aes(x = sum_dist, fill = Species)) +
            geom_histogram(binwidth = input$cumulativeMovementBinwidthSlider) +
            theme_classic() +
            labs(title = "Cumulative Movement by TAG", subtitle = "Groupings are 300 m", 
                 x = "Sum Distance", y = "Total number of fish") +
            scale_fill_manual(values = allColors)
          
          ggplotly(plot)
          
        })
        
        output$plot9 <- renderPlotly({
          plot <- filtered_movements_data() %>%
            ggplot(aes(x = lubridate::hour(Datetime), fill = movement_only)) +
            geom_histogram(binwidth = 1) +
            theme_classic() +
            labs(title = "Detections by Hour", 
                 x = "Hour of Day", 
                 y = "Count") +
            scale_fill_manual(values = allColors)
          ggplotly(plot) 
          
        })
        
      }) #end of observe
      
    }
  )
}
