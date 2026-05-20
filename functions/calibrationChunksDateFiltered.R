#for pressure trasnducer data, function to parse and chunk up site-only pt data into claibration chunks defined in allPTData .xlsx file 
calibrationChunksDateFiltered <- function(calibrationDates = calibrationDates, siteOnlyData){
  
  siteOnlyChunk_list <- list()
  
  for(i in 1:nrow(calibrationDates)) {
    
    # Define the current window
    current_start <- calibrationDates$DataCalStart[i]
    current_end   <- calibrationDates$DataCalEnd[i]
    Site <- calibrationDates$Site[i]
    
    # Filter red barn based on this window
    chunk <- siteOnlyData[siteOnlyData$dateTime >= current_start & siteOnlyData$dateTime <= current_end, ]
    
    # Store it in the list with a name (optional)
    # Assuming df1 has a 'site_name' or 'year' column to identify the chunk
    site_label <- paste0(Site, "_", i) 
    siteOnlyChunk_list[[site_label]] <- chunk
  }
  return(siteOnlyChunk_list)
}