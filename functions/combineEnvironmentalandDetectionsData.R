library(data.table)
combineEnvironmentalandDetectionsData <- function(Detections, allPressureTransducerDataWithDischarge, DischargeData){
  #########joining
  DetectionsWorking <- Detections %>%
    #this part works as long as the site name in the metadata is what is used in the PT files as well
    left_join(wgfpMetadata$AntennaMetadata[,c("FrontendSiteCode", "SiteName")], by = c("Event" = "FrontendSiteCode")) %>%
    mutate(SiteName = case_when(SiteName == "Confluence Stationary Antenna" ~ "Confluence", 
                                SiteName == "Hitching Post Stationary Antenna" ~ "Hitching Post", 
                                SiteName == "Hitching Post Stationary Antenna" ~ "Red Barn")
    ) %>%
    # add_row(Datetime = as.POSIXct("2019-09-16 07:15:00")) %>%
    # add_row(Datetime = as.POSIXct("2021-05-24 08:00:00"), SiteName = "Hitching Post")
  
  PTData <- allPressureTransducerDataWithDischarge %>%
    mutate(Datetime = as.POSIXct(dateTime)) %>%
    rename(
      #Datetime = dateTime, 
      SiteName = Site)
  
  #check same timezone
  attr(PTData$Datetime, "tzone")
  attr(DetectionsWorking$Datetime, "tzone")
  #force detection df to correct zone
  DetectionsWorking$Datetime <- lubridate::force_tz(DetectionsWorking$Datetime, tzone = "America/Denver")
  
  notExactTimestampMatchesDetections <- anti_join( DetectionsWorking,PTData, by = "Datetime") 
  
  notExactTimestampMatchesDetections <- data.table(notExactTimestampMatchesDetections)
  PTData <- data.table(PTData)
  
  keycols <- c("Datetime", "SiteName")
  setkeyv(notExactTimestampMatchesDetections, keycols)
  setkeyv(PTData, keycols)
  
  #time_window_seconds <- 60 * 60  # 15 minutes in seconds
  
  ###need to change the "EVent" field to confluence.HithcingPost/RedBarn and then set a separate key to join both on the timestamp AND by site
  #   
  # combined <- PTData[ DetectionsWorking,roll = "nearest"]
  # 
  # notExactTimestampMatchesDetectionsWithClosestEnvironmentlaReading <- PTData[DetectionsWorking, roll = "nearest", on = keycols, nomatch = NULL]
  #oerfrom a rolling join from data.table
  notExactTimestampMatchesDetectionsWithClosestEnvironmentlaReading <- PTData[notExactTimestampMatchesDetections, roll = "nearest", on = .(SiteName, Datetime), nomatch = NULL]
  #result_df <- x[abs(Datetime - i.Datetime) <= time_window_seconds]
  
  #gives df of envrionmental data and detections with closest timestamps
  notExactTimestampMatchesDetectionsWithClosestEnvironmentlaReading <- notExactTimestampMatchesDetectionsWithClosestEnvironmentlaReading %>%
    relocate(dateTime, Datetime) %>%
    rename(envrionemtalDataMeasurementTime = dateTime)
  
  #change columns to NA that are outside 1 hour time frame
  #cols to change to NA: 
  columnstoChange <- c(colnames(PTData)[grepl("_", colnames(PTData))], "USGSDischarge")
  
  notExactTimestampMatchesDetectionsWithClosestEnvironmentlaReadingWithin1Hour <- notExactTimestampMatchesDetectionsWithClosestEnvironmentlaReading %>%
    mutate(timeDifference = ifelse(difftime(envrionemtalDataMeasurementTime, Datetime, units = c("hours")) >=1, 1, 0), 
           across(all_of(columnstoChange), ~ ifelse(timeDifference == 1, NA_real_, .))
    )
  
  
  ####
  #selected data without releaseSites
  
  #gives exact matches of detections NOT at the stationary sites: release, mobile rus, biomark
  #can only join with USGS data here
  #and soon we shold differntiaite and only join on above/below the dam, since the gauge is at hitching post and isn't that useful for confluence/above the dam
  #q: how do we decide above/below? is it a station number, like anything above hitching post?
  exactMatchesNotPTdata <- DetectionsWorking %>%
    filter(!SiteName %in% c("Confluence", "Hitching Post", "Red Barn")) %>%
    inner_join(DischargeData, by = c("Datetime" = "dateTime")) %>%
    mutate(envrionemtalDataMeasurementTime = Datetime) %>%
    rename(USGSDischarge = Flow_Inst)
  
  #need to get exact time matches that ARE at the stationary sites
  exactMatchesWithPTdata <- DetectionsWorking %>%
    filter(SiteName %in% c("Confluence", "Hitching Post", "Red Barn")) %>%
    inner_join(PTData, by = c("Datetime" = "dateTime", "SiteName")) %>%
    mutate(envrionemtalDataMeasurementTime = Datetime)
  ###then bind otgether the notExactTimeMatches, exact time matches not at stationary sites, and exact time matches at Staionaety sites
  #this shoudl give df the same size as the original condensedAllEventsWithReleaseInfo (or detectoins df) with environmental variables attached
  #them maybe from there we can filter based off time: within 15 mintues or an hour is ok, but if the nearest one is months off then it's a no
  desiredColumns <- c(colnames(Detections), colnames(allPressureTransducerDataWithDischarge), "envrionemtalDataMeasurementTime", "SiteName")
  desiredColTypes <- c(class(Detections[,c(colnames(Detections))]))
  # align_df_columns <- function(df, desiredColumns) {
  #   # Create missing columns in df with NA values
  #   missing_columns <- setdiff(desiredColumns, colnames(df))
  #   if (length(missing_columns) > 0) {
  #     for (col in missing_columns) {
  #       df[[col]] <- NA_real_
  #     }
  #   }
  #   # Reorder dataframe columns to match desiredColumns
  #   df <- df %>%
  #     select(all_of(desiredColumns))
  #   return(df)
  # }
  
  align_df_columns <- function(df, desiredColumns) {
    # Create missing columns in df with NA values
    missing_columns <- setdiff(desiredColumns, colnames(df))
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        # Determine the expected column type based on desiredColumns
        expected_type <- ifelse(col %in% colnames(Detections), 
                                typeof(Detections[[col]]),
                                ifelse(col %in% colnames(allPressureTransducerDataWithDischarge),
                                       typeof(allPressureTransducerDataWithDischarge[[col]]),
                                       typeof(df[[col]])))  # Use existing type of df's column
        
        # Create new column with NA values and enforce the expected type
        df[[col]] <- NA  # Start with NA values
        df[[col]] <- switch(expected_type,
                            "integer" = as.integer(df[[col]]),
                            "numeric" = as.numeric(df[[col]]),
                            "character" = as.character(df[[col]]),
                            "factor" = as.factor(df[[col]]),
                            "Date" = as.Date(df[[col]]),
                            df[[col]])  # Retain original if type doesn't match known types
      }
    }
    
    # Reorder dataframe columns to match desiredColumns
    df <- df %>%
      select(all_of(desiredColumns))
    
    return(df)
  }
  
  df_list <- list(
    "exactMatchesWithPTdata1" <- align_df_columns(exactMatchesWithPTdata, desiredColumns),
    "exactMatchesNotPTdata1" <- align_df_columns(exactMatchesNotPTdata, desiredColumns),
    "notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour1" <- align_df_columns(notExactTimestampMatchesDetectionsWithClosestEnvironmentlaReadingWithin1Hour, desiredColumns = desiredColumns)
  )
  
  allData <- dplyr::bind_rows(df_list)
  return(allData)
}

x <- 