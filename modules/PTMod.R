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
                                   min = min(lubridate::date(PTData$DateTime) -1),
                                   max = max(lubridate::date(PTData$DateTime) +1),  
                                   value = c(min(lubridate::date(PTData$DateTime) -1), max(lubridate::date(PTData$DateTime) +1)),
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
                   sidebarPanel(width = 2
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
          select(Site, DateTime, input$variableSelect) %>%
          dplyr::filter(Site %in% input$sitePicker, 
                        lubridate::date(DateTime) >= input$dateSlider[1] & lubridate::date(DateTime) <= input$dateSlider[2])
        return(filteredPTData)
      })
      
      filteredMovementsData <- movementsFiltered_Server("filteredMovementData", Movements_df)
      
      
      filteredMovementsDataCounts <- reactive({
        return(filteredMovementsData() %>%
          count(Date, movement_only, name = "numberOfActivities")
        )
        })
      
      windyGap1 <- USGSDischargeData %>%
        group_by(Date = date(dateTime)) %>%
        summarise(dailyAverageFlow = round(mean(Flow_Inst), 2))
      
      output$PTPlot <- renderPlotly({
        req(input$sitePicker)
          filteredPTData() %>%
            ggplot(aes_string(x = "DateTime", y = input$variableSelect, color = "Site"
            )
            ) +
            geom_line() +
            theme_classic() +
            labs(title="Pressure Transducer Data",
                 x = "Date", y = input$variableSelect)
        
        
      })
      
      output$OverlayPlot <- renderPlotly({
        ggplot() +
          # # Bar plot (moves)
          geom_bar(data = filteredMovementsDataCounts(), aes(x = Date, y = numberOfActivities,  fill = movement_only),
                   stat = "identity", position = "dodge") +
          
          # # Line plot (windyGap)
          geom_line(data = windyGap1, aes(x = Date, y = dailyAverageFlow), color = "blue") +
          
          # Set labels and titles
          labs(title = "Overlay of Bar Plot and Line Plot",
               x = "Date/Time", y = "Count/Flow") +
          
          # Customize legend and fill colors
          scale_fill_manual(values = c("Downstream Movement" = "red",
                                       "Upstream Movement" = "chartreuse3",
                                       "No Movement" = "black",
                                       "Initial Release" = "darkorange",
                                       "Changed Rivers" = "purple")) +
          guides(fill = guide_legend(title = "Movement")) +  # Legend for bar plot
          
          # Adjust theme
          theme_minimal()
        
      })

    }
  )
}