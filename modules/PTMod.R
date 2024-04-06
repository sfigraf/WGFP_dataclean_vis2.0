PT_UI <- function(id, PTData) {
  ns <- NS(id)
  tagList(
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
                               )
                   #want to make a date range input here

      ),
      mainPanel(
        tabsetPanel(
          #tabPanel("PT Data",
        #                      br(),
        #                      
        # ),
        tabPanel("PT Data Plot",
                 br(),
                 withSpinner(plotlyOutput(ns("PTPlot"))),
                 #withSpinner(DT::dataTableOutput(ns("allevents1"))),
        )
        )#end of tabset panel

      )
    )
  )
}

PT_Server <- function(id, PTData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filteredPTData <- reactive({
        filteredPTData <- PTData %>%
          select(Site, DateTime, input$variableSelect) %>%
          dplyr::filter(Site %in% input$sitePicker)
        return(filteredPTData)
      })
      
      output$PTPlot <- renderPlotly({
        filteredPTData() %>%
          ggplot(aes(x = DateTime, y = input$variableSelect, color = Site
                     #text = paste('Date: ', as.character(Date), '\n')
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