PT_UI <- function(id, PTData) {
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
                    #), 
                    #tabPanel("Overlay",
                    # box(
                    #   width = 10,
                    #   withSpinner(plotlyOutput(ns("OverlayPlot")))
          )
          #), 
          #)#end of tabset panel
          # )
        )
        
      ),
      tabPanel(
        "Movements Overlay",
        
      )
    )
    
  )
}

PT_Server <- function(id, PTData) {
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

    }
  )
}