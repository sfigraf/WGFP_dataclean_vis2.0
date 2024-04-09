filteredPTData_UI <- function(id, PTData) {
  ns <- NS(id)
  tagList(
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
                min = min(lubridate::date(PTData$dateTime) - 1),
                max = max(lubridate::date(PTData$dateTime) + 1),  
                value = c(min(lubridate::date(PTData$dateTime) - 1), max(lubridate::date(PTData$dateTime) + 1)),
                step = 1,
                timeFormat = "%d %b %y"
    )
  
  )
}

filteredPTData_Server <- function(id, PTData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      filteredPTDischargeDataForGraph <- reactive({
        
        req(input$sitePicker)
        
        filteredPTDischargeDataForGraph <- PTData %>%
          select(Site, dateTime, input$variableSelect) %>%
          dplyr::filter(Site %in% input$sitePicker, 
                        lubridate::date(dateTime) >= input$dateSlider[1] & lubridate::date(dateTime) <= input$dateSlider[2])
        return(filteredPTDischargeDataForGraph)
      })
      # xx1 <<- filteredPTDischargeDataForGraph()
      # xx2 <<- input$variableSelect
      #ptList <- 
      return(
        list(
          "filteredPTDischargeDataForGraph" = filteredPTDischargeDataForGraph(), 
          "selectedVariable" = input$variableSelect
        )
        )
    }
  )
}