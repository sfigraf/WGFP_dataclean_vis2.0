combineEnvironmentalAndSiteVisitData <- function(WGFPSiteVisitsFieldData, PTDataWide){
  WGFPSiteVisitsFieldData1 <- WGFPSiteVisitsFieldData %>%
    mutate(dateTime = lubridate::ymd_hms(paste(Date, Time)), 
           fieldDataNotes = Notes)
  
  
  #perform rolling join with PT data
  ptdataWide <- PTDataWide %>%
    dplyr::filter(!is.na(Site)) %>%
    mutate(ptTimeRecorded = dateTime, 
           ptDataNotes = Notes)
  
  #exact time matches: 12 in total
  ExactTimeMatches <- WGFPSiteVisitsFieldData1 %>%
    inner_join(ptdataWide, by = c("Site", "dateTime")) %>%
    rename(Notes = Notes.x)
  
  #rest of them: 
  WGFPSiteVisitsFieldData2 <- WGFPSiteVisitsFieldData1 %>%
    anti_join(ExactTimeMatches, by = colnames(WGFPSiteVisitsFieldData1)) %>%
    dplyr::filter(!is.na(dateTime))
  
  # #the rolling join with 2 can only take those with the 
  # notExactTimestampMatchesDetectionsStationaryOnly <- notExactTimestampMatchesDetections %>%
  #   filter(!is.na(SiteName))
  library(data.table, quietly = TRUE,warn.conflicts	= FALSE)
  
  #make PT data and timestamps into data.table objects so that we can perform rolling join
  WGFPSiteVisitsFieldData2 <- data.table(WGFPSiteVisitsFieldData2)
  ptdataWide <- data.table(ptdataWide)
  
  # WGFPSiteVisitsFieldData2[, dateTime := as.POSIXct(dateTime, format="%m/%d/%Y %H:%M:%S")]
  # ptdataWide[, dateTime := as.POSIXct(dateTime, format="%m/%d/%Y %H:%M:%S")]
  
  
  ####need to make it so site names are prezent in both data: need to elimate kaibab stuff etc
  
  #set keycols
  #the order these are in matter
  keycols <- c("Site","dateTime")
  setkeyv(WGFPSiteVisitsFieldData2, keycols)
  setkeyv(ptdataWide, keycols)
  
  
  WGFPSiteVisitsFieldData3 <- ptdataWide[WGFPSiteVisitsFieldData2, roll = "nearest", on = .(Site, dateTime), nomatch = NULL]
  
  WGFPSiteVisitsFieldData3 <- as.data.frame(WGFPSiteVisitsFieldData3) %>%
    relocate(Site, Date, Time, dateTime, ptTimeRecorded, `32mm RR (ft) DS Initial`, `32mm Initial (Biomark)`, USGSDischarge, Water_Level_NoIce_ft)
  
  
  #get only rows with timestaps within 13 hours
  WGFPSiteVisitsFieldData4 <- WGFPSiteVisitsFieldData3 %>%
    mutate(timeDifference = ifelse(abs(difftime(ptTimeRecorded, dateTime, units = c("hours"))) > 13, 1, 0)
           #across(all_of(columnstoChange), ~ ifelse(timeDifference == 1, NA_real_, .))
    ) %>%
    dplyr::filter(timeDifference == 0)
  
  ExactTimeMatchesOrganized <- alignColumns(ExactTimeMatches, colnames(WGFPSiteVisitsFieldData4), WGFPSiteVisitsFieldData4)
  ###joining back up
  siteVisitDataWithPTData <- bind_rows(ExactTimeMatchesOrganized, WGFPSiteVisitsFieldData4)
  ###need to get rest of rows to join for final df
  restOfRows <- WGFPSiteVisitsFieldData %>%
    anti_join(siteVisitDataWithPTData, by = c("Site", "Date"))
  
  restOfRowsAligned <- alignColumns(restOfRows, names(siteVisitDataWithPTData), siteVisitDataWithPTData)
  allRowsPTDataSiteVisits <- bind_rows(restOfRowsAligned, siteVisitDataWithPTData)
  
  allRowsPTDataSiteVisits <- allRowsPTDataSiteVisits %>%
    mutate(dateTime = lubridate::ymd_hms(paste(Date, Time)))
  
  return(allRowsPTDataSiteVisits)
}