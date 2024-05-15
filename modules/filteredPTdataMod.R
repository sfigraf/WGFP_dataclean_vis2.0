filteredPTData_UI <- function(id, PTDataLong, includeDischarge = TRUE) {
  ns <- NS(id)
  
  if(!includeDischarge){
    PTDataLong <- PTDataLong %>%
      filter(!EnvVariable %in% c("USGSDischarge"))
  }
  
  tagList(
    pickerInput(ns("sitePicker"),
                label = "Select Sites:",
                choices = sort(unique(PTDataLong$Site)),
                selected = unique(PTDataLong$Site),
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

filteredPTData_Server <- function(id, PTDataLong, needValidation = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filteredPTData <- reactive({
        if(needValidation){
          validate(
            need(input$sitePicker, "Please select a site to display")
          )
          
        }
        
        filteredPTData <- PTDataLong %>%
          dplyr::filter(Site %in% input$sitePicker, 
                        EnvVariable %in% input$variableSelect,
                        lubridate::date(dateTime) >= input$dateSlider[1] & lubridate::date(dateTime) <= input$dateSlider[2]
                        #!is.na(EnvVariable)
          )
        filteredPTDataWithNAs <- filteredPTData %>%
          group_by(Site) %>%  # Ensure this operation is done separately for each site
          complete(dateTime = seq(min(dateTime), max(dateTime), by = "hour")) %>%
          ungroup()
        
        xx <<- filteredPTDataWithNAs
          
        return(filteredPTDataWithNAs)
      })
      
      
      
      return(list(
        "filteredPTData" = filteredPTData, 
        "Variable" = input$variableSelect,
        "Dates" = input$dateSlider
      )
      )
    }
  )
}