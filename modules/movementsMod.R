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
                  tabPanel("Minicharts Map",
                           fluidRow(
                             column(12,
                                    div("This is a work in progress, doesn't work with the sidebar filters yet and for some reason stops going after 7 ish months. Press the play button in the bottom right"))
                           ),
                           leafletOutput(ns("map2"))
                           ),
                  
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
                  # tabPanel("Animation",
                  #          br(),
                  #          mod_animationUI(ns("movements_animation"))
                  #          
                  # ) #end of animation tabPanel
                ), # end of tabset panel
      )#end of mainPanel
    )#end of sidebarLayout including sidebarPanel and Mainpanel
    )
}

movements_Server <- function(id, Movements_df, WeeklyMovementsbyType) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      filtered_movements_data <- eventReactive(input$button7, ignoreNULL = FALSE,{
        
        
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
        
        datatable(
          filtered_movements_data(),
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          extensions = c("Buttons"),
          options = list(
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50, 100, 200), c("10", "25", "50", "100", "200")),
            dom = 'Blfrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
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
      
      
      

# Minicharts Map outout ---------------------------------------------------

      #data 
      # WeeklyMovementsbyType <- reactive({
      #   req(input$button7)
      #   print(colnames(filtered_movements_data()))
      #   #this is the start of a function as I envision
      #   #WeeklyMovementsbyType <- filtered_movements_data()
      #   print("True")
      #   WeeklyMovementsbyType <- filtered_movements_data() %>%
      #     ungroup() %>%
      #     ### mobile filter
      #     filter(!det_type %in% c("Mobile Run")) %>%
      #     mutate(weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))),
      #            date_week = as.Date("2020-09-01")+weeks(weeks_since)
      #     ) %>%
      #     group_by(UTM_X, UTM_Y, date_week, movement_only) %>%
      #     mutate(total = n()) %>%
      #     distinct(UTM_X, UTM_Y, date_week, movement_only, .keep_all = TRUE) %>%
      #     pivot_wider(id_cols = c("X", "Y", "det_type", "date_week"), names_from = movement_only, values_from = total) %>%
      #     select(-9)
      #   print("got this far")
      # 
      #   WeeklyMovementsbyType[is.na(WeeklyMovementsbyType)] <- 0
      # 
      #   ## this part is making new row with UTM's to get the initla release back down to 0 to not show as a big bar graph the whole time on minicharts
      #   #just decided I'm going to put a disclaimer that it doesn't work as well with mobile detections
      #   WeeklyMovementsbyType2 <- WeeklyMovementsbyType %>%
      #     group_by(X, Y, det_type) %>%
      #     arrange(date_week) %>%
      #     mutate(nextinitialRelease = lead(`Initial Release`, order_by = date_week)
      #     )
      #   x <- WeeklyMovementsbyType2
      #   df_skeleton <- WeeklyMovementsbyType2[1,]
      #   df_skeleton[1,] <- list(-106, 55, NA, NA, 0, 0, 0, 0, 0, 0)
      # 
      #   for (row in 1:nrow(WeeklyMovementsbyType2)) {
      #     #print(x$nextinitialRelease[row])
      #     if(is.na(WeeklyMovementsbyType2$nextinitialRelease[row]) & WeeklyMovementsbyType2$`Initial Release`[row] > 0) {
      #       #gettig values to make a new row with
      #       print("na val")
      #       X1 <- WeeklyMovementsbyType2$X[row]
      # 
      #       Y1 <- WeeklyMovementsbyType2$Y[row]
      #       next_week <- WeeklyMovementsbyType2$date_week[row] + weeks(1)
      # 
      #       events <- 0
      #       det_type <- WeeklyMovementsbyType2$det_type[row]
      # 
      #       new_row <- df_skeleton
      #       new_row[1,] <- list(X1, Y1, det_type, next_week, events, NA, NA, NA, NA, NA)
      #       WeeklyMovementsbyType2 <- rbind(WeeklyMovementsbyType2, new_row)
      #     }
      #   }
      #   return(WeeklyMovementsbyType2)
      # })
      
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(maxZoom = 19.5)
      ) %>%
      ###minicharts
      addMinicharts(
        lng =  WeeklyMovementsbyType$X,
        lat = WeeklyMovementsbyType$Y,
        #layerId = WeeklyMovementsbyType$det_type,
        type = "bar",
        maxValues = 50,
        height = 45,
        width = 45,
        chartdata = WeeklyMovementsbyType[,c("Initial Release", "No Movement", "Downstream Movement", "Upstream Movement", "Changed Rivers")],
        time = WeeklyMovementsbyType$date_week

      )
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
          addAwesomeMarkers(data = antenna_sites,
                            icon = Station_icons,
                            clusterOptions = markerClusterOptions(),
                            label = paste(antenna_sites$SiteLabel),
                            popup = paste(antenna_sites$SiteName, "<br>",
                                          "Channel Width:", antenna_sites$ChannelWid, "feet"),
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
         
          addLayersControl(overlayGroups = c("Detections", "Antennas", "Release Sites", "Stream Centerlines", "Stations (m)", "Mobile Reaches")) %>%
          hideGroup(c("Stream Centerlines", "Stations (m)", "Antennas", "Release Sites", "Mobile Reaches"))
        
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
      # mod_animationServer("movements_animation", filtered_movements_data = filtered_movements_data())
      # 
        
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
          #ggplotly(plot)
          
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
          
        })

        #cumulative movement
        output$plot8 <- renderPlotly({
          plot <- filtered_movements_data() %>%
            ggplot(aes(x = sum_dist)) +
            geom_histogram(binwidth = 300) +
            theme_classic() +
            labs(title = "Cumulative movement", subtitle = "Groupings are 300 m")
          ggplotly(plot)
          

        })

        output$plot9 <- renderPlotly({
          plot <- filtered_movements_data() %>%
            ggplot(aes(x = hour(Datetime), fill = movement_only)) +
            geom_histogram(binwidth = 1) +
            theme_classic() +
            labs(title = "Detections by Hour")
          ggplotly(plot)
          

        })
        
        output$text2 <- renderPrint({
          "'Fish Movement by Day' plot renders table data shown in the 'map and table' tab. 
      'Seasonal Daily Movements' graph data can be downloaded below."
        })
        
        
      }) #end of observe
      
      
      
      
    }
  )
}