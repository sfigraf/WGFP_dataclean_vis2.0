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
                       #if this needs to be used like 1 more time I would make these functions
                       checkboxInput(ns("dischargeOverlay"), "Overlay USGS Discharge Data"
                       ),
                       uiOutput(ns("dischargeScaleValue")),
                       h6("Note: Discharge is measured from USGS Gauge at Hitching Post and is the same across all sites")
                       
          ),
          mainPanel(width = 10,
                    box(
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
            tabPanel("Movements Filters",
                     sidebarPanel(width = 2,
                                  movementsFiltered_UI(ns("filteredMovementData"), Movements_df)
                     )
            ), 
            tabPanel("Variable Filters", 
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
                                              choices = colnames(PTData)[grepl("_", colnames(PTData))],
                                              selected = colnames(PTData)[grepl("_", colnames(PTData))][1],
                                  ), 
                                  sliderInput(ns("dateSlider2"), "Date",
                                              min = min(lubridate::date(PTData$dateTime) -1),
                                              max = max(lubridate::date(PTData$dateTime) +1),  
                                              value = c(min(lubridate::date(PTData$dateTime) -1), max(lubridate::date(PTData$dateTime) +1)),
                                              step = 1,
                                              timeFormat = "%d %b %y"
                                  ),
                                  #if this needs to be used like 1 more time I would make these functions
                                  checkboxInput(ns("dischargeOverlayMovements"), "Overlay USGS Discharge Data"
                                  ),
                                  uiOutput(ns("dischargeScaleValueMovements")),
                                  h6("Note: Discharge is measured from USGS Gauge at Hitching Post and is the same across all sites")                     )
            )
          ), 
          mainPanel(width = 10,
                    box(
                      width = 10,
                      withSpinner(plotlyOutput(ns("OverlayPlot")))
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

PT_Server <- function(id, PTData, Movements_df, dischargeData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      filteredPTData <- reactive({
        
        validate(
          need(input$sitePicker, "Please select a site to display")
        )
        
        filteredPTData <- PTData %>%
          dplyr::select(Site, dateTime, input$variableSelect) %>%
          dplyr::filter(Site %in% input$sitePicker, 
                        lubridate::date(dateTime) >= input$dateSlider[1] & lubridate::date(dateTime) <= input$dateSlider[2]) #%>%
        #na.omit()
        return(filteredPTData)
      })
      
      
      output$dischargeScaleValue <- renderUI({
        req(input$dischargeOverlay)
        numericInput(
          ns("dischargeScaleValueInput"),
          "Scale Discharge (dividing)",
          1,
          min = 0,
          max = NA,
          step = NA,
          width = NULL
        )
      })
      
      output$dischargeScaleValueMovements <- renderUI({
        req(input$dischargeOverlayMovements)
        numericInput(
          ns("dischargeScaleValueInputMovements"),
          "Scale Discharge (dividing)",
          1,
          min = 0,
          max = NA,
          step = NA,
          width = NULL
        )
      })
      
      filteredDischargeData <- reactive({
        filteredDischargeData <- dischargeData %>%
          dplyr::filter(lubridate::date(dateTime) >= input$dateSlider[1] & lubridate::date(dateTime) <= input$dateSlider[2])
        
        return(filteredDischargeData)
      })
      
      filteredDischargeDataMovements <- reactive({
        
        filteredDischargeDataMovements <- dischargeData %>%
          dplyr::filter(lubridate::date(dateTime) >= input$dateSlider2[1] & lubridate::date(dateTime) <= input$dateSlider2[2]) %>%
          group_by(Date = date(dateTime)) %>%
          summarise(dailyAverageCFS = round(mean(Flow_Inst), 2))
        return(filteredDischargeDataMovements)
      })
      
      filteredPTData2 <- reactive({
        req(input$sitePicker2)
        filteredPTData <- PTData %>%
          select(Site, dateTime, input$variableSelect2) %>%
          dplyr::filter(Site %in% input$sitePicker2, 
                        lubridate::date(dateTime) >= input$dateSlider2[1] & lubridate::date(dateTime) <= input$dateSlider2[2]) %>%
          
          group_by(Date = date(dateTime), Site) %>%
          summarise(dailyAverage = round(mean(!!sym(input$variableSelect2)), 2)) 
        return(filteredPTData)
      })
      
      
      filteredMovementsData <- movementsFiltered_Server("filteredMovementData", Movements_df)
      
      filteredMovementsDataCounts <- reactive({
        return(filteredMovementsData() %>%
                 count(Date, movement_only, name = "numberOfActivities")
        )
      })
      
      
      output$PTPlot <- renderPlotly({
        
        if(!input$dischargeOverlay){
          filteredPTData() %>%
            ggplot(aes_string(x = "dateTime", y = input$variableSelect, color = "Site")
            ) +
            geom_line() +
            theme_classic() +
            labs(title="Pressure Transducer Data",
                 x = "Date", y = input$variableSelect)
        } else{
          ggplot() + 
            geom_line(data = filteredPTData(), aes_string(x = "dateTime", y = input$variableSelect, color = "Site")) +
            geom_line(data = filteredDischargeData(), aes(x = dateTime, y = round(Flow_Inst/input$dischargeScaleValueInput, 2))) +
            theme_classic() +
            labs(title="Pressure Transducer and USGS Data",
                 x = "Date", y = paste0(input$variableSelect, "/CFS"))
          
        }
      })
      
      
      output$OverlayPlot <- renderPlotly({
        
        scalevalue <- round(max(filteredPTData2()$dailyAverage, na.rm = T)/max(filteredMovementsDataCounts()$numberOfActivities), 2)
        
        plot <- ggplot() +
          # # Bar plot (movements)
          geom_bar(data = filteredMovementsDataCounts(), aes(x = Date, y = numberOfActivities, 
                                                             #text = paste('Date: ', as.character(Date), '\n'),
                                                             fill = movement_only),
                   stat = "identity", position = "dodge") +
          
          # # Line plot (windyGap)
          geom_line(data = filteredPTData2(), aes(x = Date, y = round(dailyAverage/scalevalue, 0), color = Site)) +
          
          # Customize legend and fill colors
          scale_fill_manual(values = c("Downstream Movement" = "red",
                                       "Upstream Movement" = "chartreuse3",
                                       "No Movement" = "black",
                                       "Initial Release" = "darkorange",
                                       "Changed Rivers" = "purple")) +
          guides(fill = guide_legend(title = "Movement")) +  # Legend for bar plot
          theme_classic()
        
        if(input$dischargeOverlayMovements){
          
          plot <- plot +
            geom_line(data = filteredDischargeDataMovements(), aes(x = Date, y = round(dailyAverageCFS/input$dischargeScaleValueInputMovements, 2))) +
            labs(title = paste0("Daily Movements, ", input$variableSelect2, "/", scalevalue, ", and CFS/", input$dischargeScaleValueInputMovements),
                 x = "Date", y = paste0("Count, ", input$variableSelect2, ", and CFS"))
          
          
        } else{
          plot <- plot +
            labs(title = paste0("Daily Movements and ", input$variableSelect2, "/", scalevalue),
                 x = "Date", y = "Count") 
        }
        
        return(plot)
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
        
        
        
        filteredPTData3() %>%
          ggplot(aes(x = variableX, y = variableY)) +
          geom_line() +
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