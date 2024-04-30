downloadData_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("downloadActionButton"), label = "Save Data")
  )
}

downloadData_Server <- function(id, data, fileName = "WGFPdataDownload") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$downloadActionButton, {
        showModal(modalDialog(
          title = "Data Download",
          downloadButton(ns("downloadCSV"), "Download as CSV"),
          downloadButton(ns("downloadRDS"), "Download as RDS"),
          
          #"Data 1 has been successfully downloaded."
        ))
      })
      
      output$downloadCSV <- downloadHandler(
        filename = function() {
          paste(fileName, Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write_csv(data, file, progress = TRUE)
          
        }
      )
      
      
      output$downloadRDS <- downloadHandler(
        filename = function() {
            paste(fileName, Sys.Date(), ".rds", sep = "")
        },
        content = function(file) {
            saveRDS(data, file = file)
        }
      )
    }
  )
}