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
                   uiOutput(ns("dateSliderUI")),
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
        
        dateFilteredData <- All_Events %>%
          dplyr::filter(
            Date >= input$dateSlider[1] & Date <= input$dateSlider[2]
            )
        
        data <- summarizedDf(dateFilteredData, input$antennas1, input$antennas2)
        
        dataMovements <- data %>%
          dplyr::filter(`Upstream Or Downstream Movement` %in% input$UpstreamDownstreamFilter)
        
        return(dataMovements)
      })
      
      output$sequencestableUI <- renderUI({
        tagList(
          box(title = boxTitle(), 
              withSpinner(DTOutput(ns("sequencesTable")))
          )
        )
      })
      
      output$dateSliderUI <- renderUI({
        tagList(
          sliderInput(ns("dateSlider"), "Date",
                      min = min(All_Events$Date - 1),
                      max = max(All_Events$Date + 1),  
                      value = c(min(All_Events$Date - 1), max(All_Events$Date + 1)),
                      step = 1,
                      timeFormat = "%d %b %y"
          ), 
          pickerInput(ns("UpstreamDownstreamFilter"), 
                      "Movement Type", 
                      choices = c("Upstream", "Downstream"), 
                      selected = c("Upstream", "Downstream"), 
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE 
                      )
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