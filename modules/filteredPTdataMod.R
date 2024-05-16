filteredPTData_UI <- function(id, PTDataLong, includeUSGS = TRUE) {
  ns <- NS(id)
  
  if(!includeUSGS){
    PTDataLong <- PTDataLong %>%
      filter(!EnvVariable %in% c("USGSDischarge", "USGSWatertemp"))
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
      
      observeEvent(input$variableSelect, {
        
        if(input$variableSelect %in% c("USGSDischarge", "USGSWatertemp")){
          updatePickerInput(session, "sitePicker", choices = character(0))
        } else{
          updatePickerInput(session, "sitePicker", 
                            choices = sort(unique(PTDataLong$Site)),
                            selected = sort(unique(PTDataLong$Site))
          )
          
        }
      })
      
      filteredPTData <- reactive({
        if(needValidation){
          validate(
            need(input$sitePicker, "Please select a site to display")
          )
        }
        
        filteredPTData <- PTDataLong %>%
          dplyr::filter(
                        EnvVariable %in% input$variableSelect,
                        lubridate::date(dateTime) >= input$dateSlider[1] & lubridate::date(dateTime) <= input$dateSlider[2]
          )
        if(!input$variableSelect %in% c("USGSDischarge", "USGSWatertemp")){
          req(input$sitePicker)
          #complete sequence so lines don't connect in plotly
          filteredPTData <- filteredPTData %>%
            dplyr::filter(Site %in% input$sitePicker) %>%
            group_by(Site) %>%  # Ensure this operation is done separately for each site
            complete(dateTime = seq(min(dateTime), max(dateTime), by = "hour")) %>%
            ungroup()
        } else{
          filteredPTData <- filteredPTData %>%
            select(-Site) %>%
            dplyr::distinct()
        }
        
          #xx <<- filteredPTData
        return(filteredPTData)
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