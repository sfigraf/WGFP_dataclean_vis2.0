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
      
      # Check if the passed object is reactive. If not, wrap it in one.
      # This allows the rest of the module to use data() safely.
      # r_filename <- if (shiny::is.reactive(filename_base)) {
      #   data
      # } else {
      #   reactive({ data })
      # }
      # data <- if (shiny::is.reactive(data)) {
      #   #print("data is reactive")
      #   data
      # } else {
      #   print("data is not reactive")
      #   reactive({ data })
      # }
      
      observeEvent(input$downloadActionButton, {
        print(nrow(data()))
        #shiny::validate(need(nrow(data()) > 0, "No data available to download."))
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
      })
      
      output$downloadCSV <- downloadHandler(
        filename = function() {
          paste(fileName, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          # curly braces optional on 1 line of code
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