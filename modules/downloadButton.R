downloadData_UI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(outputId = ns("downloadButton"), label = "Save Combined Data")
  )
}

downloadData_Server <- function(id, data, fileName = "WGFPdataDownload") {
  moduleServer(
    id,
    function(input, output, session) {
      output$downloadButton <- downloadHandler(
        filename = function() {
          #if (endsWith(inFile$name, ".TXT") | endsWith(inFile$name, ".csv")) {
            paste(fileName, Sys.time(), ".rds", sep = "")
            
          #} else if (endsWith(inFile$name, ".xlsx")) {
            #paste("Biomark_Raw", str_sub(inFile,-14,-6), ".csv", sep = "")
            
          #}
        },
        content = function(file) {
          #inFile <- input$file2
          #if rds selected or csv
          #if(endsWith(inFile$name, ".rds")){
            
            saveRDS(data, file = file)
          #} else if(endsWith(inFile$name, ".csv")){
            #write_csv(updated_data(), file,  progress = TRUE)
          #}
        }
      )
    }
  )
}