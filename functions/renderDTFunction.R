renderDTFunction <- function(output, outputID, outputData, dataCaption, matching_tags){
  
  
  output[[outputID]] <- renderDT({
    
    datatable(
      outputData,
      rownames = FALSE,
      selection = "single",
      filter = 'top',
      caption = dataCaption,
      
      options = list(
        #statesave is restore table state on page reload
        stateSave = TRUE,
        scrollX = TRUE,
        pageLength = 10,
        info = TRUE,
        lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
        dom = 'lrtip',
        #had to add 'lowercase L' letter to display the page length again
        language = list(emptyTable = "Enter inputs and press Render Table"), 
        rowCallback = JS(
          "function(row, data) {",
          "  var tag = data[0];",  # assuming 'Tag' is the first column (index 0)",
          "  var matching_tags = " ,matching_tags, ";", 
          "  if (matching_tags.indexOf(tag) !== -1) {",
          "    $('td', row).css('background-color', '#FFDDC1');",  # Set the background color for matching rows
          "  }",
          "}"
        )
      )
    )
  })
  
}