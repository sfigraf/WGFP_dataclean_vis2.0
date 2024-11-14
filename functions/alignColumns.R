#helper function to easily convert columns of different Dfs to the same columns in order to bind them
#currently doesn't change the types of data though if 2 dfs had the same column name but different types
alignColumns <- function(dfToChange, desiredColumns, dfWithIdealColumns) {
  
  commonColumns <- intersect(colnames(dfToChange), colnames(dfWithIdealColumns))
  
  for (col in commonColumns) {
    expectedType <- class(dfWithIdealColumns[[col]])
    #print(paste(col, expectedType))
    # if(matches(class(dfToChange[[col]])) != matches(expectedType)){
    if(!setequal(class(dfToChange[[col]]), expectedType)){
      dfToChange[[col]] <- switch(expectedType,
                                  "integer" = as.integer(dfToChange[[col]]),
                                  "numeric" = as.numeric(dfToChange[[col]]),
                                  "character" = as.character(dfToChange[[col]]),
                                  "factor" = as.factor(dfToChange[[col]]),
                                  "Date" = as.Date(dfToChange[[col]]),
                                  dfToChange[[col]])  # Retain original if type doesn't match known types
    }
   # print(paste(col, expectedType))
    
  }
  
  
  # Create missing columns in dfToChange with NA values
  missingColumns <- setdiff(desiredColumns, colnames(dfToChange))
  #x1 <- janitor::compare_df_cols(dfToChange, dfWithIdealColumns, return = "mismatch")
  # common <- names(dfToChange)[names(dfToChange) %in% names(dfWithIdealColumns)]
  # dfToChange[common] <- lapply(common, function(x) {
  #   match.fun(paste0("as.", class(dfWithIdealColumns[[x]])[1]))(dfToChange[[x]])
  # })
  
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