downloadData_UI <- function(id, labelText = "Save Data") {
  ns <- NS(id)
  tagList(
    actionButton(ns("downloadActionButton"), label = labelText), 
    hr(),
  )
}

downloadData_Server <- function(id, data, fileName = "WGFPdataDownload") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$downloadActionButton, {
        showModal(modalDialog(
          fluidRow(
            column(
              width = 12,
              align = "center", 
              downloadButton(ns("downloadCSV"), "Download as CSV")
            )
          ), 
          br(), 
          fluidRow(
            column(
              width = 12,
              align = "center", 
              downloadButton(ns("downloadRDS"), "Download as RDS")
            )
          ),
          br(),
          
          footer = tagList(
            fluidRow(
              column(
                width = 12,
                align = "center", 
                modalButton("Cancel")
              )
            )
          ),
          
          easyClose = TRUE, 
          size = "s"
          
        ))
        #in shiny when an ObwervEvent is newly created (as it is every time it's called) it's default behavior is to run immediately 
        #if the input is anything other than NULL, which it will be in the crosstalkData mod loop after the download button has been clicked once 
        #ignoreInit tells the modal to ignore its first exectuion; aka since input$downloadActionButtonValue > 0 after the first execution, this keeps running in that crosstalk data df that re-renders the downloadData_SAerver 
      }, ignoreInit = TRUE)
      
      output$downloadCSV <- downloadHandler(
        filename = function() {
          filenameReactive <- if (shiny::is.reactive(fileName)) fileName() else fileName
          paste(filenameReactive, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          # curly braces optional on 1 line of code
          #grabs the current version of that data with this call using () 
          data <- if (shiny::is.reactive(data)) data() else data
          write_csv(data, file, progress = TRUE)
          
        }
      )
      
      
      output$downloadRDS <- downloadHandler(
        filename = function() {
          filenameReactive <- if (shiny::is.reactive(fileName)) fileName() else fileName
          
          paste(filenameReactive, "_", Sys.Date(), ".rds", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          
          data <- if (shiny::is.reactive(data)) data() else data
          saveRDS(data, file = file)
        }
      )
      
    }
  )
}