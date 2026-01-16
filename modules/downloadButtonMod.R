downloadData_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("downloadActionButton"), label = "Save Data"), 
    hr(),
  )
}

downloadData_Server <- function(id, data, fileName = "WGFPdataDownload") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$downloadActionButton, {
        print("button pressed")
        #print(is.reactive(data))
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
      }, ignoreInit = TRUE)
      
      output$downloadCSV <- downloadHandler(
        filename = function() {
          paste(fileName, "_", Sys.Date(), ".csv", sep = "")
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
            paste(fileName, "_", Sys.Date(), ".rds", sep = "")
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