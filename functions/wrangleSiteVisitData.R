wrangleSiteVisitData <- function(siteVisitList){
  
  ddStationary <- siteVisitList$Stationary
  
  
  ddBiomark <- siteVisitList$Biomark %>%
    rename(`32mm Initial (Biomark)` = `32mm Initial`, 
           `32mm Final (Biomark)` = `32mm Final`, 
           `12mm Initial (Biomark)` = `12mm Initial`, 
           `12mm Final (Biomark)` = `12mm Final`)
  
  ddStationary$Time <- format(ddStationary$Time, format = "%H:%M:%S")
  ddBiomark$Time <- format(ddBiomark$Time, format = "%H:%M:%S")
  names(ddBiomark)
  
  desiredColumns = c(colnames(ddStationary), grep("mm", names(ddBiomark), value = TRUE))
  Biomarkaligned <- alignColumns(ddBiomark, desiredColumns = desiredColumns, ddStationary)
  Stationaryaligned <- alignColumns(ddStationary, desiredColumns = colnames(Biomarkaligned), Biomarkaligned)
  WGFP_SiteVisits_FieldData <- dplyr::bind_rows(Biomarkaligned, Stationaryaligned)
  
  mm_columns <- grep("[0-9]+mm", names(WGFP_SiteVisits_FieldData), value = TRUE)
  
  suppressWarnings({
    WGFP_SiteVisits_FieldData <- WGFP_SiteVisits_FieldData %>%
      mutate_at(vars(all_of(mm_columns)), ~ifelse(. == "TOUCHING", ".001", .)) %>%
      mutate(
        #there should be NAs inudced here if there are data that aren't numbers in these columns
        across(.cols = contains("[0-9]+mm"), .fns = as.numeric)
        
      )
  })
  
  return(WGFP_SiteVisits_FieldData)
}