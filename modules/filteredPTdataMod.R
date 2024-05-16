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
        
        # Capture current selected sites without triggering reactive dependencies
        currentSelectedSites <- isolate(input$sitePicker)
        
        # If the selected variable is either "USGSDischarge" or "USGSWatertemp",
        # clear the site picker because these variables do not require site selection
        if (input$variableSelect %in% c("USGSDischarge", "USGSWatertemp")) {
          updatePickerInput(session, "sitePicker", choices = character(0), selected = character(0))
        } else {
          # For other variables, update the picker input with available sites
          # Extract unique site names from the dataset and sort them
          availableSites <- sort(unique(PTDataLong$Site))
          
          # Check if the currently selected sites are still available in the new choices
          validSelectedSites <- currentSelectedSites[currentSelectedSites %in% availableSites]
          
          # If no valid selected sites are found, default to selecting all available sites
          if (length(validSelectedSites) == 0) {
            validSelectedSites <- availableSites
          }
          
          # Update the picker input with the available sites and the valid selected sites
          updatePickerInput(session, "sitePicker", 
                            choices = availableSites,
                            selected = validSelectedSites
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