rainbow_trout_colors <- c("#8B8000", "#008080", "#FF69B4", "#FF4500", "#6A5ACD","#32CD32", "#20B2AA", "#FF8C00", "#4682B4")
movementColors <- c("purple", "red", "darkorange", "black", "chartreuse3")


PT_UI <- function(id, PTData, Movements_df) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Time Series",
        sidebarLayout(
          sidebarPanel(width = 2,
                       pickerInput(ns("sitePicker"),
                                   label = "Select Sites:",
                                   choices = sort(unique(PTData$Site)),
                                   selected = unique(PTData$Site)[1],
                                   multiple = TRUE,
                                   options = list(
                                     `actions-box` = TRUE #this makes the "select/deselect all" option
                                   )
                       ), 
                       selectInput(ns("variableSelect"),
                                   label = "Variable to Plot",
                                   choices = colnames(PTData)[grepl("_", colnames(PTData))],
                                   selected = colnames(PTData)[grepl("_", colnames(PTData))][1],
                       ), 
                       sliderInput(ns("dateSlider"), "Date",
                                   min = min(lubridate::date(PTData$dateTime) -1),
                                   max = max(lubridate::date(PTData$dateTime) +1),  
                                   value = c(min(lubridate::date(PTData$dateTime) -1), max(lubridate::date(PTData$dateTime) +1)),
                                   step = 1,
                                   timeFormat = "%d %b %y"
                       ), 
                       checkboxInput(ns("dischargeOverlay"), "Overlay USGS Discharge Data"
                       ),
                       uiOutput(ns("YaxisSelect")),
                       h6("Note: Discharge is measured from USGS Gauge at Hitching Post and is the same across all sites")
                       
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
                                              choices = sort(unique(PTData$Site)),
                                              selected = sort(unique(PTData$Site)),
                                              multiple = TRUE,
                                              options = list(
                                                `actions-box` = TRUE #this makes the "select/deselect all" option
                                              )
                                              
                                  ),
                                  selectInput(ns("variableSelect2"),
                                              label = "Variable to Plot",
                                              choices = c(colnames(PTData)[grepl("_", colnames(PTData))], "USGSDischarge", "USGSWatertemp"),
                                              selected = colnames(PTData)[grepl("_", colnames(PTData))][1],
                                  ), 
                                  sliderInput(ns("dateSlider2"), "Date",
                                              min = min(lubridate::date(PTData$dateTime) -1),
                                              max = max(lubridate::date(PTData$dateTime) +1),  
                                              value = c(min(lubridate::date(PTData$dateTime) -1), max(lubridate::date(PTData$dateTime) +1)),
                                              step = 1,
                                              timeFormat = "%d %b %y"
                                  ),
                                  h6("Note: Discharge is measured from USGS Gauge at Hitching Post and is the same across all sites")                     )
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
                               choices = sort(unique(PTData$Site)),
                               selected = sort(unique(PTData$Site))[1],
                   ), 
                   selectInput(ns("variableSelectX"),
                               label = "X Axis Variable",
                               choices = c(colnames(PTData)[grepl("_", colnames(PTData))], "USGSDischarge"),
                               selected = colnames(PTData)[grepl("_", colnames(PTData))][1],
                   ), 
                   
                   
                   selectInput(ns("variableSelectY"),
                               label = "Y Axis Variable",
                               choices = c(colnames(PTData)[grepl("_", colnames(PTData))], "USGSDischarge"),
                               selected = colnames(PTData)[grepl("_", colnames(PTData))][2],
                   ),
                   sliderInput(ns("dateSlider3"), "Date",
                               min = min(lubridate::date(PTData$dateTime) -1),
                               max = max(lubridate::date(PTData$dateTime) +1),  
                               value = c(min(lubridate::date(PTData$dateTime) -1), max(lubridate::date(PTData$dateTime) +1)),
                               step = 1,
                               timeFormat = "%d %b %y"
                   ), 
                   checkboxInput(ns("summarizeByTimeframe"), "Summarize by Timeframe"
                   ),
                   uiOutput(ns("timeFrameSummaryOptions")),
                   
                   h6("Note: Discharge is measured from USGS Gauge at Hitching Post and is the same across all sites")
                   
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
      )
    )
    
  )
}

PT_Server <- function(id, PTData, Movements_df, USGSData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      observeEvent(input$variableSelect2, {
        
        if(input$variableSelect2 %in% c("USGSDischarge", "USGSWatertemp")){
          updatePickerInput(session, "sitePicker2", choices = character(0))
        } else{
          updatePickerInput(session, "sitePicker2", 
                            choices = sort(unique(PTData$Site)),
                            selected = sort(unique(PTData$Site))
                            )
          
        }
      })
      
      filteredPTData <- reactive({
        
        validate(
          need(input$sitePicker, "Please select a site to display")
        )
        
        filteredPTData <- PTData %>%
          dplyr::select(Site, dateTime, input$variableSelect) %>%
          dplyr::filter(Site %in% input$sitePicker, 
                        lubridate::date(dateTime) >= input$dateSlider[1] & lubridate::date(dateTime) <= input$dateSlider[2]) #%>%
        return(filteredPTData)
      })
      
      
      output$YaxisSelect <- renderUI({
        req(input$dischargeOverlay)
        
        radioButtons(ns("primaryYAxis"),
                     "Primary Y Axis Data",
                     choices = c("Pressure Transducer Data", 
                                 "Discharge Data"),
                     selected = "Pressure Transducer Data")
      })
      
      filteredDischargeData <- reactive({
        filteredDischargeData <- USGSData$USGSDischarge15Min %>%
          dplyr::filter(lubridate::date(dateTime) >= input$dateSlider[1] & lubridate::date(dateTime) <= input$dateSlider[2])
        
        return(filteredDischargeData)
      })
      
      filteredDischargeDataMovements <- reactive({
        
        filteredDischargeDataMovements <- USGSData$USGSDischarge15Min %>%
          dplyr::filter(lubridate::date(dateTime) >= input$dateSlider2[1] & lubridate::date(dateTime) <= input$dateSlider2[2]) %>%
          group_by(Date = date(dateTime)) %>%
          summarise(dailyAverageCFS = round(mean(Flow_Inst), 2))
        return(filteredDischargeDataMovements)
      })
      
      filteredPTData2 <- reactive({

        if(!input$variableSelect2 %in% c("USGSDischarge", "USGSWatertemp")){
          validate(
            need(input$sitePicker2, "Please select a site to display")
          )
          filteredData <- PTData %>%
            filter(lubridate::date(dateTime) >= input$dateSlider2[1] & 
                     lubridate::date(dateTime) <= input$dateSlider2[2])
          
          filteredData <- filteredData %>%
            dplyr::filter(Site %in% input$sitePicker2) %>%
            group_by(Date = date(dateTime), Site) %>%
            summarise(dailyAverage = round(mean(!!sym(input$variableSelect2)), 2)) %>%
            #need to ungroup to get data to show for plotly plots (overlay plot)
            ungroup()
            
        } else{
          filteredData <- USGSData$USGSDaily %>%
            rename(dailyAverage = case_when(input$variableSelect2 == "USGSDischarge" ~ "Flow", 
                                            input$variableSelect2 == "USGSWatertemp" ~ "WtempF"))
        }
         
        
        return(filteredData)
      })
      
      
      filteredMovementsData <- movementsFiltered_Server("filteredMovementData", Movements_df)
      
      filteredMovementsDataCounts <- reactive({
        return(filteredMovementsData() %>%
                 count(Date, movement_only, name = "numberOfActivities")
        )
      })
      
      
      output$PTPlot <- renderPlotly({
        site_colors <- setNames(rainbow_trout_colors[0:length(unique(PTData$Site))], sort(unique(PTData$Site)))
        
        if(!input$dischargeOverlay){

          plot_ly() %>%
            add_lines(data = filteredPTData(), x = ~dateTime, y = ~.data[[input$variableSelect]], 
                      color = ~Site,
                      colors = site_colors) %>%
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = input$variableSelect, side = "left", showgrid = FALSE)
            )
        } else {
          
          req(input$primaryYAxis)
          if (input$primaryYAxis == "Pressure Transducer Data") {
            
            plot_ly() %>%
              add_lines(data = filteredPTData(), x = ~dateTime, y = ~.data[[input$variableSelect]], 
                        color = ~Site,
                        colors = site_colors,
                        yaxis = "y1") %>%
              add_lines(data = filteredDischargeData(), x = ~dateTime, y = ~Flow_Inst,
                        color = I("#87CEEB"),
                        name = "USGS Discharge",
                        yaxis = "y2") %>%
              layout(legend = list(x = 1.05, y = 1),
                     xaxis = list(title = "Date"),
                     yaxis = list(title = input$variableSelect, side = "left", showgrid = FALSE),
                     yaxis2 = list(title = "Discharge (CFS)", side = "right", overlaying = "y",
                                   showgrid = FALSE))
          } else if(input$primaryYAxis == "Discharge Data") {
            plot_ly() %>%
              add_lines(data = filteredPTData(), x = ~dateTime, y = ~.data[[input$variableSelect]], 
                        color = ~Site,
                        colors = site_colors,
                        yaxis = "y2") %>%
              add_lines(data = filteredDischargeData(), x = ~dateTime, y = ~Flow_Inst,
                        color = I("#87CEEB"),
                        name = "USGS Discharge",
                        yaxis = "y1") %>%
              layout(legend = list(x = 1.05, y = 1),
                     xaxis = list(title = "Date"),
                     yaxis = list(title = "Discharge (CFS)", side = "left", showgrid = FALSE),
                     yaxis2 = list(title = input$variableSelect, side = "right", overlaying = "y",
                                   showgrid = FALSE))
          }
        }
      })
      
      
      output$OverlayPlot <- renderPlotly({

        allColors <- setNames(c(movementColors, rainbow_trout_colors[0:length(unique(PTData$Site))]), c(sort(unique(Movements_df$movement_only)), sort(unique(PTData$Site))))
      
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
        
        filteredPTData <- PTData %>%
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

        site_colors <- setNames(rainbow_trout_colors[0:length(unique(PTData$Site))], sort(unique(PTData$Site)))
        
        filteredPTData3() %>%
          ggplot(aes(x = variableX, y = variableY)) +
          geom_line(color = site_colors[[input$siteSelect]]) +
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
      
    }
  )
}