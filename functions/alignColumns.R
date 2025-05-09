#helper function to easily convert columns of different Dfs to the same columns in order to bind them
#converts columns with same names to the same type (will break if formats are incorrect, ie Dates, times)
#then creates new columns with the correct types based off dfWIthIdealColumns
alignColumns <- function(dfToChange, desiredColumns, dfWithIdealColumns) {
  
  commonColumns <- intersect(colnames(dfToChange), colnames(dfWithIdealColumns))
  
  for (col in commonColumns) {
    expectedType <- class(dfWithIdealColumns[[col]])

    if(!setequal(class(dfToChange[[col]]), expectedType)){
      dfToChange[[col]] <- switch(expectedType,
                                  "integer" = as.integer(dfToChange[[col]]),
                                  "numeric" = as.numeric(dfToChange[[col]]),
                                  "character" = as.character(dfToChange[[col]]),
                                  "factor" = as.factor(dfToChange[[col]]),
                                  "Date" = as.Date(dfToChange[[col]]),
                                  dfToChange[[col]])  # Retain original if type doesn't match known types
    }
    
  }
  
  
  # Create missing columns in dfToChange with NA values
  missingColumns <- setdiff(desiredColumns, colnames(dfToChange))
  
  if (length(missingColumns) > 0) {
    for (col in missingColumns) {
      # Determine the expected column type based on desiredColumns
      expectedType <- ifelse(col %in% colnames(dfWithIdealColumns), 
                             typeof(dfWithIdealColumns[[col]]),
                             ifelse(col %in% colnames(allPressureTransducerDataWithDischarge),
                                    typeof(allPressureTransducerDataWithDischarge[[col]]),
                                    typeof(dfToChange[[col]])))  # Use existing type of dfToChange's column
      
      # Create new column with NA values and enforce the expected type
      dfToChange[[col]] <- NA  # Start with NA values
      dfToChange[[col]] <- switch(expectedType,
                                  "integer" = as.integer(dfToChange[[col]]),
                                  "numeric" = as.numeric(dfToChange[[col]]),
                                  "character" = as.character(dfToChange[[col]]),
                                  "factor" = as.factor(dfToChange[[col]]),
                                  "Date" = as.Date(dfToChange[[col]]),
                                  dfToChange[[col]])  # Retain original if type doesn't match known types
    }
  }
  
  # Reorder dataframe columns to match desiredColumns
  dfToChange <- dfToChange %>%
    select(all_of(desiredColumns))
  
  return(dfToChange)
}