PT_UI <- function(id, PTData, Movements_df, USGSDischargeData) {
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
                       )
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
                                  actionButton(ns("overlayRender"), label = "Render"), 
                                  h6("Note: Discharge is measured from USGS Gauge at Hitching Post and is the same across all sites")
                     )
            )
          ), 
          mainPanel(width = 10,
                    box(
                      width = 10,
                      withSpinner(plotlyOutput(ns("OverlayPlot")))
                    )
          )
        )
        
      )
    )
    
  )
}

PT_Server <- function(id, PTData, Movements_df, USGSDischargeData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filteredPTData <- reactive({
        req(input$sitePicker)
        filteredPTData <- PTData %>%
          select(Site, dateTime, input$variableSelect) %>%
          dplyr::filter(Site %in% input$sitePicker, 
                        lubridate::date(dateTime) >= input$dateSlider[1] & lubridate::date(dateTime) <= input$dateSlider[2]) #%>%
          #na.omit()
        return(filteredPTData)
      })
      
      filteredPTData2 <- eventReactive(input$overlayRender, {
        req(input$sitePicker2)
        filteredPTData <- PTData %>%
          select(Site, dateTime, input$variableSelect2) %>%
          dplyr::filter(Site %in% input$sitePicker2, 
                        lubridate::date(dateTime) >= input$dateSlider2[1] & lubridate::date(dateTime) <= input$dateSlider2[2]) %>%
          #na.omit() %>%
          group_by(Date = date(dateTime), Site) %>%
          summarise(dailyAverage = round(mean(!!sym(input$variableSelect2)), 2)) %>%
          na.omit()
        #x <<- filteredPTData
        return(filteredPTData)
      })
      
      
      
      filteredMovementsData <- movementsFiltered_Server("filteredMovementData", Movements_df)
      
      filteredMovementsDataCounts <- reactive({
        return(filteredMovementsData() %>%
          count(Date, movement_only, name = "numberOfActivities")
        )
        })

      
      output$PTPlot <- renderPlotly({
        req(input$sitePicker)
        filteredPTData() %>%
          ggplot(aes_string(x = "dateTime", y = input$variableSelect, color = "Site"
          )
          ) +
          geom_line() +
          theme_classic() +
          labs(title="Pressure Transducer Data",
               x = "Date", y = input$variableSelect)
        
        
      })
      
      output$OverlayPlot <- renderPlotly({
        
        scalevalue <- max(filteredPTData2()$dailyAverage, na.rm = T)/max(filteredMovementsDataCounts()$numberOfActivities)
        ggplot() +
          # # Bar plot (movements)
          geom_bar(data = filteredMovementsDataCounts(), aes(x = Date, y = numberOfActivities, 
                                                             #text = paste('Date: ', as.character(Date), '\n'),
                                                             fill = movement_only),
                   stat = "identity", position = "dodge") +
          
          # # Line plot (windyGap)
          geom_line(data = filteredPTData2(), aes(x = Date, y = round(dailyAverage/scalevalue, 0), color = Site)) +
          
          # Set labels and titles
          labs(title = paste0("Daily Movements and Scaled ", input$variableSelect2),
               x = "Date", y = "Count") +
          
          # Customize legend and fill colors
          scale_fill_manual(values = c("Downstream Movement" = "red",
                                       "Upstream Movement" = "chartreuse3",
                                       "No Movement" = "black",
                                       "Initial Release" = "darkorange",
                                       "Changed Rivers" = "purple")) +
          guides(fill = guide_legend(title = "Movement")) +  # Legend for bar plot
          
          # Adjust theme
          theme_classic()
        
      })

    }
  )
}