States_UI <- function(id) {
  ns <- NS(id)
  tagList(
    br(), 
    downloadData_UI(ns("downloadStatesMARKData")),
    br(), 
    withSpinner(DT::DTOutput(ns("statesMARKData")))
  )
}

States_Server <- function(id, MARKEncounterHistories) {
  moduleServer(
    id,
    function(input, output, session) {
      
      downloadData_Server("downloadStatesMARKData", MARKEncounterHistories, "MARKEncounterHistories")
      
      
      output$statesMARKData <- renderDT({
        
        datatable(MARKEncounterHistories,
                  rownames = FALSE,
                  extensions = c("Buttons"),
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                    
                    #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
                  )
        ) %>%
          formatStyle(
            columns = grep("^\\d", names(MARKEncounterHistories), value = TRUE),   # Select columns starting with a number
            backgroundColor = styleEqual(
              levels = 0,
              default = "lightblue",
              values = c("transparent") # No color for 0
            )
          )
        
      })
    }
  )
}
