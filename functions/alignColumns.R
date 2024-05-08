alignColumns <- function(detectionsWithEnvironmentalData, desiredColumns, DetectionsWithIdealColumns) {
  # Create missing columns in detectionsWithEnvironmentalData with NA values
  missingColumns <- setdiff(desiredColumns, colnames(detectionsWithEnvironmentalData))
  
  if (length(missingColumns) > 0) {
    for (col in missingColumns) {
      # Determine the expected column type based on desiredColumns
      expectedType <- ifelse(col %in% colnames(DetectionsWithIdealColumns), 
                             typeof(DetectionsWithIdealColumns[[col]]),
                             ifelse(col %in% colnames(allPressureTransducerDataWithDischarge),
                                    typeof(allPressureTransducerDataWithDischarge[[col]]),
                                    typeof(detectionsWithEnvironmentalData[[col]])))  # Use existing type of detectionsWithEnvironmentalData's column
      
      # Create new column with NA values and enforce the expected type
      detectionsWithEnvironmentalData[[col]] <- NA  # Start with NA values
      detectionsWithEnvironmentalData[[col]] <- switch(expectedType,
                                                       "integer" = as.integer(detectionsWithEnvironmentalData[[col]]),
                                                       "numeric" = as.numeric(detectionsWithEnvironmentalData[[col]]),
                                                       "character" = as.character(detectionsWithEnvironmentalData[[col]]),
                                                       "factor" = as.factor(detectionsWithEnvironmentalData[[col]]),
                                                       "Date" = as.Date(detectionsWithEnvironmentalData[[col]]),
                                                       detectionsWithEnvironmentalData[[col]])  # Retain original if type doesn't match known types
    }
  }
  
  # Reorder dataframe columns to match desiredColumns
  detectionsWithEnvironmentalData <- detectionsWithEnvironmentalData %>%
    select(all_of(desiredColumns)) %>%
    select(-Site, -dateTime) %>%
    rename(Site = SiteName)
  
  return(detectionsWithEnvironmentalData)
}