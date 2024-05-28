Sequences_UI <- function(id, antennaChoices) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   pickerInput(ns("antennas1"),
                               label = "Select Downstream Antenna(s) in Sequence:",
                               choices = antennaChoices,
                               selected = antennaChoices[1],
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE 
                               )
                   ), 
                   pickerInput(ns("antennas2"),
                               label = "Select Upstream Antenna(s) in Sequence:",
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
                uiOutput(ns("sequencestableUI"))
                )
    )
  
  )
}

Sequences_Server <- function(id, All_Events) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      boxTitle <- reactiveVal("")
      
      filteredData <- eventReactive(input$renderbutton, ignoreNULL = FALSE, {
        
        validate(
          need(length(input$antennas1) > 0, "Please select at least one antenna from Antenna(s) 1."),
          need(length(input$antennas2) > 0, "Please select at least one antenna from Antenna(s) 2.")
        )
        
        newTitle <- paste("Instances of Detections Between", paste(input$antennas1, collapse = ", "), "and", paste(input$antennas2, collapse = ", "))
        boxTitle(newTitle)
        
        data <- summarizedDf(All_Events, input$antennas1, input$antennas2)
        return(data)
      })
      
      output$sequencestableUI <- renderUI({
        tagList(
          box(title = boxTitle(), 
              withSpinner(DTOutput(ns("sequencesTable")))
          )
        )
        
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
            language = list(emptyTable = "No Instances of Detections Between Selected Antenna Sites")
          )
        )
        
      })
      
    }
  )
}