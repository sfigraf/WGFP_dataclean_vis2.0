PT_UI <- function(id, PTData, Movements_df, USGSDischargeData) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Time Series",
        sidebarLayout(
          sidebarPanel(width = 2,
                       filteredPTData_UI(ns("filteredPTDataNS"), PTData)
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
      ns <- session$ns
      
      filteredPTData <- reactive({filteredPTData_Server("filteredPTDataNS", PTData)})
      # x <<- reactive({
      #   return(filteredPTData())
      # })
      filteredMovementsData <- movementsFiltered_Server("filteredMovementData", Movements_df)
      
      
      filteredMovementsDataCounts <- reactive({
        return(filteredMovementsData() %>%
          count(Date, movement_only, name = "numberOfActivities")
        )
        })
      
      #xx <<- filteredMovementsDataCounts()
      
      windyGap1 <- USGSDischargeData %>%
        group_by(Date = date(dateTime)) %>%
        summarise(dailyAverageFlow = round(mean(Flow_Inst), 2))
      
      output$PTPlot <- renderPlotly({
        req(isTruthy(filteredPTData()))
          filteredPTData()$filteredPTDischargeDataForGraph %>%
            ggplot(aes_string(x = "dateTime", y = filteredPTData()$selectedVariable, color = "Site"
            )
            ) +
            geom_line() +
            theme_classic() +
            labs(title="Pressure Transducer Data",
                 x = "Date", y = filteredPTData()$selectedVariable)
        
        
      })
      
      output$OverlayPlot <- renderPlotly({
        
        scalevalue <- max(windyGap1$dailyAverageFlow, na.rm = T)/max(filteredMovementsDataCounts()$numberOfActivities)
        
        ggplot() +
          # # Bar plot (movements)
          geom_bar(data = filteredMovementsDataCounts(), aes(x = Date, y = numberOfActivities, 
                                                             #text = paste('Date: ', as.character(Date), '\n'),
                                                             fill = movement_only),
                   stat = "identity", position = "dodge") +
          
          # # Line plot (windyGap)
          geom_line(data = windyGap1, aes(x = Date, y = round(dailyAverageFlow/scalevalue, 0)), color = "blue") +
          
          # Set labels and titles
          labs(title = "Daily Movements and Scaled USGS Discharge at Hitching Post",
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