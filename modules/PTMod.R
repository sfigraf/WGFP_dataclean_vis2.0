
#allColors <- setNames(c(movementColors, rainbow_trout_colors[0:length(unique(PTData$Site))]), c(sort(unique(Movements_df$movement_only)), sort(unique(PTData$Site))))



PT_UI <- function(id, PTData, Movements_df, WGFPSiteVisitsFieldData) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Time Series",
        sidebarLayout(
          sidebarPanel(width = 2,
                       filteredPTData_UI(ns("timeSeriesPT"), PTData$PTDataLong, includeUSGS = FALSE),
                       
                       checkboxInput(ns("dischargeOverlay"), "Overlay USGS Data"
                       ),
                       uiOutput(ns("YaxisSelect")),
                       h6("Note: USGS data is measured from USGS Gauge at Hitching Post and is the same across all sites")
                       
          ),
          mainPanel(width = 10,
                    box(title = "Environmental Time Series Data",
                        width = 10,
                        withSpinner(plotlyOutput(ns("PTPlot")))
                    )
          )
        )
        
      ),
      tabPanel(
        "Movements Overlay",
        br(), 
        sidebarLayout(
          tabsetPanel(
            tabPanel("Movement Filters",
                     sidebarPanel(width = 2,
                                  movementsFiltered_UI(ns("filteredMovementData"), Movements_df)
                     )
            ), 
            tabPanel("Environmental Filters", 
                     sidebarPanel(width = 2,
                                  pickerInput(ns("sitePicker2"),
                                              label = "Select Sites:",
                                              choices = sort(unique(PTData$PTDataWide$Site)),
                                              selected = sort(unique(PTData$PTDataWide$Site)),
                                              multiple = TRUE,
                                              options = list(
                                                `actions-box` = TRUE #this makes the "select/deselect all" option
                                              )
                                              
                                  ),
                                  selectInput(ns("variableSelect2"),
                                              label = "Variable to Plot",
                                              choices = c(colnames(PTData$PTDataWide)[grepl("_", colnames(PTData$PTDataWide))], "USGSDischarge", "USGSWatertemp"),
                                              selected = colnames(PTData$PTDataWide)[grepl("_", colnames(PTData$PTDataWide))][1],
                                  ), 
                                  sliderInput(ns("dateSlider2"), "Date",
                                              min = min(lubridate::date(PTData$PTDataWide$dateTime) -1),
                                              max = max(lubridate::date(PTData$PTDataWide$dateTime) +1),  
                                              value = c(min(lubridate::date(PTData$PTDataWide$dateTime) -1), max(lubridate::date(PTData$PTDataWide$dateTime) +1)),
                                              step = 1,
                                              timeFormat = "%d %b %y"
                                  ),
                                  h6("Note: USGS data is measured from USGS Gauge at Hitching Post and is the same across all sites")                     )
            )
          ), 
          mainPanel(width = 10,
                    box(title = "Environmental and Movement Data",
                        width = 10,
                        withSpinner(plotlyOutput(ns("OverlayPlot"))), 
                        radioButtons(ns("YaxisSelect2"), 
                                     "Primary Y Axis Data",
                                     choices = c("Movement Data", 
                                                 "Environmental Data"),
                                     selected = "Movement Data")
                    )
          )
        )
        
      ), 
      
      # variable correlations ui ------------------------------------------------
      
      
      tabPanel("Variable Correlations", 
               sidebarLayout(
                 sidebarPanel(width = 2,
                              selectInput(ns("siteSelect"),
                                          label = "Select a Site",
                                          choices = sort(unique(PTData$PTDataWide$Site)),
                                          selected = sort(unique(PTData$PTDataWide$Site))[1],
                              ), 
                              selectInput(ns("variableSelectX"),
                                          label = "X Axis Variable",
                                          choices = c(colnames(PTData$PTDataWide)[grepl("_", colnames(PTData$PTDataWide))], "USGSDischarge", "USGSWatertemp"),
                                          selected = colnames(PTData$PTDataWide)[grepl("_", colnames(PTData$PTDataWide))][1],
                              ), 
                              
                              
                              selectInput(ns("variableSelectY"),
                                          label = "Y Axis Variable",
                                          choices = c(colnames(PTData$PTDataWide)[grepl("_", colnames(PTData$PTDataWide))], "USGSDischarge", "USGSWatertemp"),
                                          selected = colnames(PTData$PTDataWide)[grepl("_", colnames(PTData$PTDataWide))][2],
                              ),
                              sliderInput(ns("dateSlider3"), "Date",
                                          min = min(lubridate::date(PTData$PTDataWide$dateTime) -1),
                                          max = max(lubridate::date(PTData$PTDataWide$dateTime) +1),  
                                          value = c(min(lubridate::date(PTData$PTDataWide$dateTime) -1), max(lubridate::date(PTData$PTDataWide$dateTime) +1)),
                                          step = 1,
                                          timeFormat = "%d %b %y"
                              ), 
                              checkboxInput(ns("summarizeByTimeframe"), "Summarize by Timeframe"
                              ),
                              uiOutput(ns("timeFrameSummaryOptions")),
                              
                              h6("Note: USGS data is measured from USGS Gauge at Hitching Post and is the same across all sites")
                              
                 ), 
                 mainPanel(width = 10,
                           box(
                             width = 10,
                             br(),
                             withSpinner(plotlyOutput(ns("variableCorrelationPlot")))
                           ), 
                           box(width = 10, 
                               uiOutput(ns("caption")))
                           
                 )
               )
      ), 
      

# Detection Distances UI -----------------------------------------------------

      
      tabPanel("Detection Distance", 
               sidebarLayout(
                 tabsetPanel(
                   tabPanel("Detection Distance", 
                     sidebarPanel(
                       width = 2,
                       
                       pickerInput(ns("sitePicker3"),
                                   label = "Select Sites:",
                                   choices = sort(unique(WGFPSiteVisitsFieldData$Site)),
                                   selected = unique(WGFPSiteVisitsFieldData$Site),
                                   multiple = TRUE,
                                   options = list(
                                     `actions-box` = TRUE #this makes the "select/deselect all" option
                                   )
                       ), 
                       selectInput(ns("variableSelect3"),
                                   label = "Reading to Plot",
                                   choices = colnames(WGFPSiteVisitsFieldData)[grepl("mm", colnames(WGFPSiteVisitsFieldData))],
                                   selected = colnames(WGFPSiteVisitsFieldData)[grepl("mm", colnames(WGFPSiteVisitsFieldData))][1],
                       ),
                       sliderInput(ns("detectionDistanceSlider"), "Date",
                                   min = min(lubridate::date(WGFPSiteVisitsFieldData$Date) -1, na.rm = TRUE),
                                   max = max(lubridate::date(WGFPSiteVisitsFieldData$Date) +1, na.rm = TRUE),  
                                   value = c(min(lubridate::date(WGFPSiteVisitsFieldData$Date) -1, na.rm = TRUE), max(lubridate::date(WGFPSiteVisitsFieldData$Date) +1, na.rm = TRUE)),
                                   step = 1,
                                   timeFormat = "%d %b %y"
                       ),
                       sliderInput(ns("SiteDataOpacity"), "Line Opacity", 0, 1, step = .1, value = 1)
                       
                     )
                   ), 
                   tabPanel("PT Data", 
                            sidebarPanel(
                              width = 2,
                              filteredPTData_UI(ns("detectionDistancePT"), PTData$PTDataLong),
                              sliderInput(ns("PTDataOpacity"), "Line Opacity", 0, 1, step = .1, value = .5)
                            )
                   )
                 ),
                 
                 mainPanel(width = 10,
                           br(),
                             withSpinner(plotlyOutput(ns("DetectionDistancePlot"))), 
                             br(), 
                             h6("Detection distances recorded as 'TOUCHING' are displayed here as .001"),
                             radioButtons(ns("YaxisSelect3"), 
                                          "Primary Y Axis Data",
                                          choices = c("Detection Distance Data", 
                                                      "Environmental Data"),
                                          selected = "Detection Distance Data")
                   
                 )
               )
      )
    )
    
  )
}

PT_Server <- function(id, PTData, Movements_df, USGSData, WGFPSiteVisitsFieldData, allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      
      # PTdata Time series ------------------------------------------------------
      
      filteredPTData <- filteredPTData_Server("timeSeriesPT", PTData$PTDataLong)
      
      selectedUSGSVar <- reactiveVal(NULL)
      selectedYaxisVar <- reactiveVal(NULL)
      observeEvent(input$USGSOverlaySelect, {
        selectedYaxisVar(input$primaryYAxis)
        selectedUSGSVar(input$USGSOverlaySelect)
      }, ignoreNULL = FALSE)
       
        
        output$YaxisSelect <- renderUI({
          req(input$dischargeOverlay)
          
          # Use the previously stored value if available, otherwise use the first available choice
          # specify a fallback value if the left-hand side is Null
          selectedUSGSChoice <- selectedUSGSVar() %||% colnames(USGSData$USGS15Min)[grepl("USGS", colnames(USGSData$USGS15Min))][1]
          selectedYaxisChoice <- selectedYaxisVar() %||% "Pressure Transducer Data"
          
          
          tagList(
            selectInput(ns("USGSOverlaySelect"), 
                        "Select USGS Variable", 
                        choices = sort(c(colnames(USGSData$USGS15Min)[grepl("USGS", colnames(USGSData$USGS15Min))])), 
                        selected = selectedUSGSChoice
            ),
            
            radioButtons(ns("primaryYAxis"),
                         "Primary Y Axis Data",
                         choices = c("Pressure Transducer Data", 
                                     "USGS Data"),
                         selected = selectedYaxisChoice)
          )
        })
        
      
      
      
      
      filteredDischargeData <- reactive({
        filteredDischargeData <- USGSData$USGS15Min %>%
          dplyr::filter(lubridate::date(dateTime) >= filteredPTData$Dates[1] & lubridate::date(dateTime) <= filteredPTData$Dates[2])
        
        return(filteredDischargeData)
      })
      
      output$PTPlot <- renderPlotly({
        
        if(!input$dischargeOverlay){
          
          plot_ly() %>%
            add_trace(data = filteredPTData$filteredPTData(), x = ~dateTime, y = ~Reading, 
                      color = ~Site,
                      mode = "lines", 
                      type = "scatter", 
                      connectgaps = TRUE,
                      #marker = list(size = 5, opacity = 0.6),
                      colors = allColors) %>%
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = filteredPTData$Variable, side = "left", showgrid = FALSE)
            )
        } else {
          
          req(input$primaryYAxis)
          if (input$primaryYAxis == "Pressure Transducer Data") {
            
            plot_ly() %>%
              add_trace(data = filteredPTData$filteredPTData(), x = ~dateTime, y = ~Reading, 
                        color = ~Site,
                        colors = allColors,
                        mode = "lines", 
                        type = "scatter", 
                        connectgaps = TRUE,
                        #marker = list(size = 5, opacity = 0.6),
                        yaxis = "y1") %>%
              add_lines(data = filteredDischargeData(), x = ~dateTime, y = ~.data[[input$USGSOverlaySelect]],
                        #these gaps don't need to be connected becuase there is more consistent gaps in the data: ie no reading between december and march
                        connectgaps = FALSE,
                        color = I("#87CEEB"),
                        name = case_when(input$USGSOverlaySelect == "USGSDischarge" ~ "USGS Discharge", 
                                         input$USGSOverlaySelect == "USGSWatertemp" ~ "USGS Water Temp (F)"),
                        yaxis = "y2") %>%
              layout(legend = list(x = 1.05, y = 1),
                     xaxis = list(title = "Date"),
                     yaxis = list(title = filteredPTData$Variable, side = "left", showgrid = FALSE),
                     yaxis2 = list(title = input$USGSOverlaySelect, side = "right", overlaying = "y",
                                   showgrid = FALSE))
          } else if(input$primaryYAxis == "USGS Data") {
            plot_ly() %>%
              add_trace(data = filteredPTData$filteredPTData(), x = ~dateTime, y = ~Reading, 
                        color = ~Site,
                        colors = allColors,
                        mode = "lines", 
                        type = "scatter", 
                        connectgaps = TRUE,
                        yaxis = "y2") %>%
              add_lines(data = filteredDischargeData(), x = ~dateTime, y = ~.data[[input$USGSOverlaySelect]],
                        connectgaps = FALSE,
                        color = I("#87CEEB"),
                        name = case_when(input$USGSOverlaySelect == "USGSDischarge" ~ "USGS Discharge", 
                                         input$USGSOverlaySelect == "USGSWatertemp" ~ "USGS Water Temp (F)"),
                        yaxis = "y1") %>%
              layout(legend = list(x = 1.05, y = 1),
                     xaxis = list(title = "Date"),
                     yaxis = list(title = case_when(input$USGSOverlaySelect == "USGSDischarge" ~ "USGS Discharge", 
                                                    input$USGSOverlaySelect == "USGSWatertemp" ~ "USGS Water Temp (F)"),
                                  side = "left", showgrid = FALSE),
                     yaxis2 = list(title = filteredPTData$Variable, side = "right", overlaying = "y",
                                   showgrid = FALSE))
          }
        }
      })
      

# Movements Overlay -------------------------------------------------------

      
      
      observeEvent(input$variableSelect2, {
        
        if(input$variableSelect2 %in% c("USGSDischarge", "USGSWatertemp")){
          updatePickerInput(session, "sitePicker2", choices = character(0))
        } else{
          updatePickerInput(session, "sitePicker2", 
                            choices = sort(unique(PTData$PTDataWide$Site)),
                            selected = sort(unique(PTData$PTDataWide$Site))
                            )
          
        }
      })
      
      filteredPTData2 <- reactive({
        
        if(!input$variableSelect2 %in% c("USGSDischarge", "USGSWatertemp")){
          validate(
            need(input$sitePicker2, "Please select a site to display")
          )
          filteredData <- PTData$PTDataWide %>%
            filter(lubridate::date(dateTime) >= input$dateSlider2[1] & 
                     lubridate::date(dateTime) <= input$dateSlider2[2])
          
          filteredData <- filteredData %>%
            dplyr::filter(Site %in% input$sitePicker2) %>%
            group_by(Date = date(dateTime), Site) %>%
            summarise(dailyAverage = round(mean(!!sym(input$variableSelect2)), 2)) %>%
            #need to ungroup to get data to show for plotly plots (overlay plot)
            ungroup() 
          #makes sure there isn't links in the data 
          #this is how the connectgaps argument is funcoitnal: need to have these NAs in data
          #if the mode is lines it makes sense to connect the gaps because otherwise single data points without connection won't show up
          
          filteredData <- filteredData %>%
            group_by(Site) %>%  # Ensure this operation is done separately for each site
            complete(Date = seq(min(Date), max(Date), by = "day")) %>%
            ungroup()
          
        } else{
          filteredData <- USGSData$USGSDaily %>%
            rename(dailyAverage = case_when(input$variableSelect2 == "USGSDischarge" ~ "Flow", 
                                            input$variableSelect2 == "USGSWatertemp" ~ "WtempF"))
        }
        
        return(filteredData)
      })
      #might want this later 
      # filteredDischargeDataMovements <- reactive({
      #   
      #   filteredDischargeDataMovements <- USGSData$USGS15Min %>%
      #     dplyr::filter(lubridate::date(dateTime) >= input$dateSlider2[1] & lubridate::date(dateTime) <= input$dateSlider2[2]) %>%
      #     group_by(Date = date(dateTime)) %>%
      #     summarise(dailyAverageCFS = round(mean(USGSDischarge), 2))
      #   return(filteredDischargeDataMovements)
      # })
      
      filteredMovementsData <- movementsFiltered_Server("filteredMovementData", Movements_df)
      
      filteredMovementsDataCounts <- reactive({
        return(filteredMovementsData() %>%
                 count(Date, movement_only, name = "numberOfActivities")
        )
      })
      
      output$OverlayPlot <- renderPlotly({
        
        if(!input$variableSelect2 %in% c("USGSDischarge", "USGSWatertemp")){
          line_color = ~Site
          nameOfLine = ~Site
        } else{
          line_color = I("#87CEEB")
          nameOfLine = case_when(input$variableSelect2 == "USGSDischarge" ~ "USGS Discharge", 
                                 input$variableSelect2 == "USGSWatertemp" ~ "USGS Water Temp (F)")
        }
        
        if(input$YaxisSelect2 == "Movement Data"){
          movYaxis = "y1"
          envYaxis = "y2"
          primaryYaxisName = "Movement Data (Daily Counts)"
          SecondaryYaxisName = input$variableSelect2
          
        } else{
          movYaxis = "y2"
          envYaxis = "y1"
          primaryYaxisName = input$variableSelect2
          SecondaryYaxisName = "Movement Data (Daily Counts)"
        }
        
        plot_ly() %>%
          add_trace(data = filteredPTData2(), x = ~Date,
                    y = ~dailyAverage,
                    name = nameOfLine,
                    color = line_color, 
                    type = "scatter",
                    yaxis = envYaxis,
                    connectgaps = TRUE,
                    mode = "lines",
                    colors = allColors
          ) %>%
          add_trace(data = filteredMovementsDataCounts(), x = ~Date, y = ~numberOfActivities,
                    yaxis = movYaxis,
                    color = ~movement_only, 
                    colors = allColors,
                    hoverinfo = "text",
                    text = ~paste('Date: ', as.character(Date), '<br>Number of Activities: ', numberOfActivities),
                    type = 'bar') %>%
          layout(legend = list(x = 1.05, y = 1),
                 barmode = "overlay",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = primaryYaxisName, side = "left", showgrid = FALSE),
                 yaxis2 = list(title = SecondaryYaxisName, side = "right", overlaying = "y",
                               showgrid = FALSE))
      })
      

      
# Variable Correlations ---------------------------------------------------
      
      output$timeFrameSummaryOptions <- renderUI({
        req(input$summarizeByTimeframe)
        radioButtons(ns("timeFrameSummarizeSelect"),
                     label = NULL,
                     choices = c("Daily", "Weekly", "Monthly"))
        
        
      })
      
      
      filteredPTData3 <- reactive({
        
        validate(
          need(input$variableSelectX != input$variableSelectY, "Please select 2 different variables to compare")
        )
        
        filteredPTData <- PTData$PTDataWide %>%
          filter(Site == input$siteSelect, 
                 lubridate::date(dateTime) >= input$dateSlider3[1] & lubridate::date(dateTime) <= input$dateSlider3[2]
          ) 
        names(filteredPTData)[names(filteredPTData) == input$variableSelectX] <- "variableX"
        names(filteredPTData)[names(filteredPTData) == input$variableSelectY] <- "variableY"
        
        if(input$summarizeByTimeframe){
          req(input$timeFrameSummarizeSelect)
          if(input$timeFrameSummarizeSelect == "Daily"){
            
            filteredPTData <- filteredPTData %>%
              group_by(Date = date(dateTime)) %>%
              summarise(
                variableX = round(mean(variableX), 2),
                variableY = round(mean(variableY), 2)
              )
          } else if(input$timeFrameSummarizeSelect == "Weekly"){
            filteredPTData <- filteredPTData %>%
              mutate(weekStart = floor_date(dateTime, unit = "week")) %>%
              group_by(weekStart) %>%
              summarise(
                variableX = round(mean(variableX), 2),
                variableY = round(mean(variableY), 2)
              )
          } else if(input$timeFrameSummarizeSelect == "Monthly"){
            filteredPTData <- filteredPTData %>%
              mutate(monthStart = floor_date(dateTime, unit = "month")) %>%
              group_by(monthStart) %>%
              summarise(
                variableX = round(mean(variableX), 2),
                variableY = round(mean(variableY), 2)
              )
          }
        }
        
        return(filteredPTData)
      })
      
      output$variableCorrelationPlot <- renderPlotly({

        filteredPTData3() %>%
          ggplot(aes(x = variableX, y = variableY)) +
          geom_line(color = allColors[[input$siteSelect]]) +
          theme_classic() +
          labs(title = paste0(input$variableSelectX, " vs ", input$variableSelectY), 
               x = input$variableSelectX, 
               y = input$variableSelectY
               ) 
      })
      
      output$caption <- renderUI({
        fit <- lm(variableX ~ variableY, data = filteredPTData3())
        rsquared <- round(summary(fit)$r.squared, 2)
        caption_html <- paste("<p style='font-style: italic; font-size: 12px;'>",
                              "R-squared:",  rsquared,
                              "</p>")
        HTML(caption_html)
      })
      
      
# Detection distance filters ----------------------------------------------
      
      AllfilteredDetectionDistanceData <- reactive({
        
        filteredPTForDetectionDistance <- filteredPTData_Server("detectionDistancePT", PTData$PTDataLong, needValidation = FALSE)
        
        WGFPSiteVisitsFieldData3 <- WGFPSiteVisitsFieldData %>%
          filter(Site %in% input$sitePicker3, 
                 lubridate::date(Date) >= input$detectionDistanceSlider[1] & 
                   lubridate::date(Date) <= input$detectionDistanceSlider[2]) 
        return(list(
          "WGFPSiteVisitsFieldData3" = WGFPSiteVisitsFieldData3,
          "filteredPTForDetectionDistance" = filteredPTForDetectionDistance
        )
        )
        
      })
      
      output$DetectionDistancePlot <- renderPlotly({
        
        if(input$YaxisSelect3 == "Detection Distance Data"){
          ddYaxis = "y1"
          envYaxis = "y2"
          primaryYaxisName = input$variableSelect3
          SecondaryYaxisName = AllfilteredDetectionDistanceData()$filteredPTForDetectionDistance$Variable
          
        } else{
          ddYaxis = "y2"
          envYaxis = "y1"
          primaryYaxisName = AllfilteredDetectionDistanceData()$filteredPTForDetectionDistance$Variable
          SecondaryYaxisName = input$variableSelect3
        }
        
        if(!AllfilteredDetectionDistanceData()$filteredPTForDetectionDistance$Variable %in% c("USGSDischarge", "USGSWatertemp")){
          line_color = ~Site
          nameOfLine = ~paste0(AllfilteredDetectionDistanceData()$filteredPTForDetectionDistance$Variable, ": ", Site)
        } else{
          line_color = I("#87CEEB")
          nameOfLine = case_when(AllfilteredDetectionDistanceData()$filteredPTForDetectionDistance$Variable == "USGSDischarge" ~ "USGS Discharge", 
                                 AllfilteredDetectionDistanceData()$filteredPTForDetectionDistance$Variable == "USGSWatertemp" ~ "USGS Water Temp (F)")
        }
        
        plot_ly() %>%
          add_trace(data = AllfilteredDetectionDistanceData()$WGFPSiteVisitsFieldData3, x = ~Date, y = ~.data[[input$variableSelect3]], 
                    color = ~Site, 
                    name = ~paste0(input$variableSelect3, ": ", Site), 
                    type = "scatter", 
                    yaxis = ddYaxis,
                    opacity = input$SiteDataOpacity,
                    colors = allColors,
                    mode = "lines+markers"
          ) %>%
          add_trace(data = AllfilteredDetectionDistanceData()$filteredPTForDetectionDistance$filteredPTData(), x = ~dateTime, y = ~Reading, 
                    color = line_color,
                    name = nameOfLine, 
                    line = list(shape = 'linear', width = 2, dash = 'dash'),
                    connectgaps = FALSE,
                    opacity = input$PTDataOpacity,
                    type = "scatter", 
                    colors = allColors,
                    yaxis = envYaxis,
                    mode = "lines"
          ) %>%
          layout(legend = list(x = 1.1, y = 1),
                 title = "Detection Distances and PT Data", 
                 xaxis = list(title = "Date"),
                 yaxis = list(title = primaryYaxisName, side = "left", showgrid = FALSE), 
                 yaxis2 = list(title = SecondaryYaxisName, side = "right", overlaying = "y", 
                               showgrid = FALSE)
          )
      })
      
    }
  )
}