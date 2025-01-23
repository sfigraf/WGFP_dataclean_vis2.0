renderDTFunction <- function(output, outputID, outputData, dataCaption){

  output[[outputID]] <- renderDT({
    
    datatable(
      outputData,
      rownames = FALSE,
      selection = "single",
      filter = 'top',
      caption = dataCaption,
      
      options = list(
        columnDefs = list(list(visible=FALSE, targets="rowColor")),
        #statesave is restore table state on page reload
        #stateSave = TRUE,
        scrollX = TRUE,
        pageLength = 10,
        info = TRUE,
        lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
        dom = 'lrtip',
        #had to add 'lowercase L' letter to display the page length again
        language = list(emptyTable = "Enter inputs and press Render Table")

      )
    ) %>%
      formatStyle(
        'rowColor',
        target = 'row',
        backgroundColor = styleEqual(c("red", "green", "yellow"), c("#f4433680", "#A5D6A7", "#ffeb3b"))
      )
  })
  
}