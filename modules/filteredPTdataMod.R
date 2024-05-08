filteredPTData_UI <- function(id, PTDataLong) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("sitePicker"),
                label = "Select Sites:",
                choices = sort(unique(PTDataLong$Site)),
                selected = unique(PTDataLong$Site)[1],
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE #this makes the "select/deselect all" option
                )
    ), 
    selectInput(ns("variableSelect"),
                label = "Variable to Plot",
                choices = sort(unique(PTDataLong$EnvVariable)),
                selected = unique(PTDataLong$EnvVariable)[1],
    ), 
    sliderInput(ns("dateSlider"), "Date",
                min = min(lubridate::date(PTDataLong$dateTime) -1),
                max = max(lubridate::date(PTDataLong$dateTime) +1),  
                value = c(min(lubridate::date(PTDataLong$dateTime) -1), max(lubridate::date(PTDataLong$dateTime) +1)),
                step = 1,
                timeFormat = "%d %b %y"
    )
  
  )
}

filteredPTData_Server <- function(id, PTDataLong, USGSData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filteredPTData <- reactive({
        
        validate(
          need(input$sitePicker, "Please select a site to display")
        )
        
        filteredPTData <- PTDataLong %>%
          #dplyr::select(Site, dateTime, input$variableSelect) %>%
          dplyr::filter(Site %in% input$sitePicker, 
                        EnvVariable %in% input$variableSelect,
                        lubridate::date(dateTime) >= input$dateSlider[1] & lubridate::date(dateTime) <= input$dateSlider[2]) #%>%
        return(filteredPTData)
      })
      
      
      
      return(list(
        "filteredPTData" = filteredPTData, 
        "Dates" = input$dateSlider
      )
      )
    }
  )
}