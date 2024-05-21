combineEnvironmentalAndSiteVisitData <- function(WGFPSiteVisitsFieldData, PTDataWide){
  
  #expect nAs here so suprressing these warnings
  suppressWarnings({
    WGFPSiteVisitsFieldData1 <- WGFPSiteVisitsFieldData %>%
      mutate(dateTime = lubridate::ymd_hms(paste(Date, Time)), 
             fieldDataNotes = Notes)
  })
  
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
  
  library(data.table, quietly = TRUE,warn.conflicts	= FALSE)
  
  #make PT data and timestamps into data.table objects so that we can perform rolling join
  WGFPSiteVisitsFieldData2 <- data.table(WGFPSiteVisitsFieldData2)
  ptdataWide <- data.table(ptdataWide)
  
  
  #set keycols
  #the order these are in matter
  keycols <- c("Site","dateTime")
  setkeyv(WGFPSiteVisitsFieldData2, keycols)
  setkeyv(ptdataWide, keycols)
  
  
  WGFPSiteVisitsFieldData3 <- ptdataWide[WGFPSiteVisitsFieldData2, roll = "nearest", on = .(Site, dateTime), nomatch = NULL]
  
  WGFPSiteVisitsFieldData3 <- as.data.frame(WGFPSiteVisitsFieldData3) #%>%
  #relocate(Site, Date, Time, dateTime, ptTimeRecorded, `32mm RR (ft) DS Initial`, `32mm Initial (Biomark)`, USGSDischarge, Water_Level_NoIce_ft)
  
  
  #get only rows with timestaps within 13 hours
  WGFPSiteVisitsFieldData4 <- WGFPSiteVisitsFieldData3 %>%
    mutate(timeDifference = ifelse(abs(difftime(ptTimeRecorded, dateTime, units = c("hours"))) > 13, 1, 0)
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
  
  #expect nAs here so suprressing these warnings
  suppressWarnings({
    allRowsPTDataSiteVisits <- allRowsPTDataSiteVisits %>%
      mutate(dateTime = lubridate::ymd_hms(paste(Date, Time)))
  })
  
  mm_columns <- grep("[0-9]+mm", names(WGFP_SiteVisits_FieldData), value = TRUE)
  
  #reorder columns so detectoin distance is first
  allRowsPTDataSiteVisits <- allRowsPTDataSiteVisits %>%
    select(Date, Time, Site, Water_Level_NoIce_ft, all_of(mm_columns), dplyr::everything())
  
  return(allRowsPTDataSiteVisits)
}