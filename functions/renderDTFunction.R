# alreadyDetectedTags <- avianPredationList$checkedTags
# outputData <- avianPredationList$movingDownstream

renderDTFunction <- function(output, outputID, outputData, dataCaption, alreadyDetectedTags){
  
  originalColumns <- names(outputData)
  alreadyDetectedTags$TAG <- as.character(alreadyDetectedTags$TAG)
  x <- left_join(outputData, alreadyDetectedTags, by = "TAG")
  
  x1 <- x %>%
    mutate(rowColor = case_when(`SG Opinion` %in% c("Yes", "yes") ~ "red", 
                                `SG Opinion` %in% c("No", "no") ~ "green", 
                                !is.na(`SG Opinion`) ~ "yellow", 
                                is.na(`SG Opinion`) ~ "none"
                                )) %>%
    select(c(originalColumns, rowColor))
 
    
    
  output[[outputID]] <- renderDT({
    
    datatable(
      x1,
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
        language = list(emptyTable = "Enter inputs and press Render Table")
        #columnDefs = list(list(visible=FALSE, targets= which(names(x1) == "rowColor")))
        # rowCallback = JS(
        #   "function(row, data) {",
        #   "  var tag = data[0];",  # 'TAG' is the first column (index 0)",
        #   "  var opinions = ", tag_opinion_js, ";",  # Insert the tag-opinion mapping from R to JS
        #   "  var opinion = opinions[tag];",  # Get the opinion for the current tag from table 2",
        #   "  if (opinion === 'No' || opinion === 'no') {",
        #   "    $('td', row).css('background-color', 'green');",  
        #   "  } else if (opinion === 'Yes' || opinion === 'yes') {",
        #   "    $('td', row).css('background-color', 'red');",  
        #   "  } else if (opinion !== undefined) {",
        #   "    $('td', row).css('background-color', 'yellow');",  
        #   "  }",
        #   "}",
        #   sep = "\n"
        # )
      )
    ) %>%
      formatStyle(
        'rowColor', 
        target = 'row', 
        backgroundColor = styleEqual(c("red", "green", "yellow"), c("#f4433680", "#A5D6A7", "#ffeb3b"))
      )
  })
  
}