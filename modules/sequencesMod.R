Sequences_UI <- function(id, antennaChoices) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   pickerInput(ns("antennas1"),
                               label = "Select Antenna(s):",
                               choices = antennaChoices,
                               selected = antennaChoices[1],
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), 
                   pickerInput(ns("antennas2"),
                               label = "Select Second Antenna(s):",
                               choices = antennaChoices,
                               selected = antennaChoices[2],
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), 
                   actionButton(ns("renderbutton"), label = "Render"),
        
      ), 
      mainPanel(width = 10,
                br(),
                withSpinner(DTOutput(ns("sequencesTable"))))
    )
  
  )
}

Sequences_Server <- function(id, All_Events) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filteredData <- eventReactive(input$renderbutton, {
        data <- summarizedDf(All_Events, input$antennas1, input$antennas2)
        return(data)
      })
      
      output$sequencesTable <- renderDT({
        
        datatable(
          filteredData(),
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          options = list(
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50, 100, 200), c("10", "25", "50", "100", "200")),
            dom = 'lfrtip',
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
        
      })
      
    }
  )
}