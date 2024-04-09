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
        
        scalevalue <- max(windyGap1$dailyAverageFlow, na.rm = T)/max(filteredMovementsDataCounts()$numberOfActivities)
        
        p <- filteredMovementsDataCounts() %>%
          ggplot(aes(x = Date, y = numberOfActivities,
                     fill = movement_only,
                      text = paste('Date: ', as.character(Date), '\n'))
        ) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_classic() +
          labs(title="Fish Movement by Day",
               x ="Date", y = "Count") +
          scale_fill_manual(values = c("Downstream Movement" = "red",
                                       "Upstream Movement" = "chartreuse3",
                                       "No Movement" = "black",
                                       "Initial Release" = "darkorange",
                                       "Changed Rivers" = "purple"))
        
        ay <- list(
          tickfont = list(size=11.7),
          titlefont=list(size=14.6),
          overlaying = "y",
          nticks = 5,
          side = "right",
          title = "Second y axis"
        )
        
        ggplotly(p) %>%
          add_trace(x=~Date, y=~dailyAverageFlow/scalevalue, colors=NULL, yaxis="y2", 
                    data=windyGap1, showlegend=FALSE, inherit=FALSE) %>%
          layout(yaxis2 = ay)
        
      })

    }
  )
}