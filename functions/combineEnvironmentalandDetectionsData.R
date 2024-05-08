
combineEnvironmentalandDetectionsData <- function(Detections, allPressureTransducerDataWithDischarge, DischargeData){
  #using this package for the main nearest naighbor timestamp join, but it has a lot of cross functionality with other packages especiallly lubridate
  #so we're going to detach it at the end
  library(data.table, quietly = TRUE,warn.conflicts	= FALSE)
  #########joining to get PT siteNmae from metadata
  DetectionswithPTSiteName <- Detections %>%
    #add pressure transducer site name to have a key to join with PT data
    #this part works as long as the site name in the metadata is what is used in the PT files as well
    #as more sites are added, we'll have to make sure the names in the pressure transducer files are the same as in the metadata "PressureTransducerSiteName" field
    left_join(wgfpMetadata$AntennaMetadata[,c("FrontendSiteCode", "PressureTransducerSiteName")], by = c("Event" = "FrontendSiteCode")) %>%
    rename(SiteName = PressureTransducerSiteName)
  
  
  # df of data where PT timestamp doesn't exactly match a detection timestamp -----------------
  
  
  ###remove rows here that have NA's associated with every field except datetime
  keyColumns <- c("dateTime")
  
  #filters rows from the data frame allPressureTransducerDataWithDischarge based on whether they have missing values only in specified columns (keyColumns),
  #filtered_PTData is the dates where there all all NA values in rest of the colunms
  filtered_PTData <- allPressureTransducerDataWithDischarge[rowSums(is.na(allPressureTransducerDataWithDischarge[, setdiff(names(allPressureTransducerDataWithDischarge), keyColumns)])) == (ncol(allPressureTransducerDataWithDischarge) - length(keyColumns)), ]
  #then we anti_join that with the original PT data with discharge to get df of data with datetimes that have some sort of relevant data
  allPressureTransducerDataWithDischargeNoNA <- allPressureTransducerDataWithDischarge %>% 
    anti_join(filtered_PTData) %>%
    suppressMessages()
  
  PTData <- allPressureTransducerDataWithDischargeNoNA %>%
    mutate(Datetime = as.POSIXct(dateTime)) %>%
    rename(SiteName = Site)
  
  #check timezone
  if(attr(PTData$Datetime, "tzone") != attr(DetectionswithPTSiteName$Datetime, "tzone")){
    #force detection df to correct zone
    PTData$Datetime <- lubridate::force_tz(PTData$Datetime, tzone = "UTC")
    
  }
  #these are all the detections from the original Detection file that do not exactly match a PT or discharge recording timestamp
  notExactTimestampMatchesDetections <- DetectionswithPTSiteName %>%
    anti_join(PTData, by = c("Datetime")) %>%
    suppressMessages() 
  

# rolling join on just specific site stuff --------------------------------
  #the rolling join with 2 can only take those with the 
  notExactTimestampMatchesDetectionsStationaryOnly <- notExactTimestampMatchesDetections %>%
    filter(!is.na(SiteName))
  
  #make PT data and timestamps into data.table objects so that we can perform rolling join
  notExactTimestampMatchesDetectionsStationaryOnly <- data.table(notExactTimestampMatchesDetectionsStationaryOnly)
  PTData <- data.table(PTData)
  
  #set keycols
  keycols <- c("Datetime", "SiteName")
  setkeyv(notExactTimestampMatchesDetectionsStationaryOnly, keycols)
  setkeyv(PTData, keycols)
  
  #perform a rolling join from data.table
  #first joins on sitename then datetime
  #for data where we have a stationary site attached, discharge data is from the closest hour. Otherwise, dishcarge data is within 15 minutes
  #gives df of envrionmental data and detections with closest timestamps
  notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading <- PTData[notExactTimestampMatchesDetectionsStationaryOnly, roll = "nearest", on = .(SiteName,Datetime), nomatch = NULL]
  
  # x <- as.data.frame(notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading) %>%
  #   dplyr::filter(Datetime == as.POSIXct("2020-11-28 11:58:27"))
  
  notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading <- notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading %>%
    relocate(dateTime, Datetime) %>%
    rename(environmentalDataMeasurementTime = dateTime)
  
  #change environmental data columns to NA that are outside 1 hour time frame
  #cols to change to NA: all that include a "_" and USGSDischarge
  columnstoChange <- c(colnames(PTData)[grepl("_", colnames(PTData))], "USGSDischarge")
  
  notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour <- notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading %>%
    mutate(timeDifference = ifelse(abs(difftime(environmentalDataMeasurementTime, Datetime, units = c("hours"))) >= 1, 1, 0), 
           across(all_of(columnstoChange), ~ ifelse(timeDifference == 1, NA_real_, .))
    ) 
  #this is one of the df's to join
  #this is stationary PT readings for all detections within 1 hour, including discharge
  #if there is 1 valid reading in any of the speificed columns, the row is kept
  #but rows are removed if all the entires in the specified columns are NA
  notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1HourValidDataOnly <- as.data.frame(notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour) %>%
    filter(rowSums(is.na(.[columnstoChange])) < length(columnstoChange))
  
  
  #now we need to do a rolling join with just discharge

# rolling join 2 just discharge -------------------------------------------
  # outsideOf1hr <- notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour %>%
  #   filter(is.na(USGSDischarge))
  #want to buffer by 13 hours for just discharge
  
  
  restOftheNotExactTimestampMatches <- notExactTimestampMatchesDetections %>%
    anti_join(notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1HourValidDataOnly, 
                 by = c("Datetime", "Event", "TAG", "UTM_X", "UTM_Y")) %>%
    arrange(SiteName) %>%
    suppressMessages() 
  
  restOftheNotExactTimestampMatches <- data.table(restOftheNotExactTimestampMatches)
  DischargeDataforJoin <- data.table(DischargeData) %>%
    mutate(Datetime = dateTime)
  
  #set keycols
  keycols <- c("Datetime")
  setkeyv(restOftheNotExactTimestampMatches, keycols)
  setkeyv(DischargeDataforJoin, keycols)

  restOftheNotExactTimestampMatchesWithDischargeReading <- DischargeDataforJoin[restOftheNotExactTimestampMatches, roll = "nearest", on = .(Datetime), nomatch = NULL]

  
  restOftheNotExactTimestampMatchesWithDischargeReading <- restOftheNotExactTimestampMatchesWithDischargeReading %>%
    relocate(dateTime, Datetime) %>%
    rename(environmentalDataMeasurementTime = dateTime, 
          USGSDischarge = Flow_Inst)
  
  #change environmental data columns to NA that are outside 13 hour time frame
  #cols to change to NA: includes USGSDischarge
  columnstoChange <- c("USGSDischarge")
  
  
  ##this is the rest of the not exact time stamp entries to fill in the gaps 
  restOftheNotExactTimestampMatchesWithDischargeReading <- restOftheNotExactTimestampMatchesWithDischargeReading %>%
    mutate(timeDifference = ifelse(abs(difftime(environmentalDataMeasurementTime, Datetime, units = c("hours"))) >= 13, 1, 0), 
           across(all_of(columnstoChange), ~ ifelse(timeDifference == 1, NA_real_, .))
    ) 
  #qaqc: these should have same amount of rows as all not exact timestamps. Can check to see if they all have discharge data as well
  #nrow(restOftheNotExactTimestampMatchesWithDischargeReading) + nrow(notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1HourValidDataOnly)
  
  
  
# getting environmental data for non-stationary events with exact timestamp matches--------------------
  
  #gives exact matches of detections NOT at the stationary sites: release, mobile rus, biomark
  #can only join with USGS data here
  #currently joins discharge data to this no matter if the event is above or below the dam
  exactMatchesNotPTdata <- DetectionswithPTSiteName %>%
    filter(!SiteName %in% na.omit(unique(wgfpMetadata$AntennaMetadata$PressureTransducerSiteName))) %>%
    inner_join(DischargeData, by = c("Datetime" = "dateTime")) %>%
    mutate(environmentalDataMeasurementTime = Datetime) %>%
    rename(USGSDischarge = Flow_Inst)
  
  
  # getting environmental data for all other detections that do not  have a stationary antenna site attached --------
  
  #this should be redundant when we actually have PT data from connectivity channel
  #should be discharge only right now unless we add some stuff
  ptData_noSite <- PTData %>%
    filter(is.na(SiteName))
  
  exactMatchesatSiteNoPTSite <- DetectionswithPTSiteName %>%
    filter(SiteName %in% na.omit(unique(wgfpMetadata$AntennaMetadata$PressureTransducerSiteName))) %>%
    inner_join(ptData_noSite, by = c("Datetime" = "dateTime")) %>%
    suppressMessages() %>%
    mutate(environmentalDataMeasurementTime = Datetime)
  
  
  # gets environmental data associated with stationry antenna sites attached to stationary sites that have an exact timestamp match -------------------------------------------------
  
  
  exactMatchesWithPTdata <- DetectionswithPTSiteName %>%
    filter(SiteName %in% na.omit(unique(wgfpMetadata$AntennaMetadata$PressureTransducerSiteName))) %>%
    inner_join(PTData, by = c("Datetime" = "dateTime", "SiteName")) %>%
    mutate(environmentalDataMeasurementTime = Datetime)
  
  ###then bind together notExactTimeMatches at statoinary sites, exact time matches not at stationary sites, and exact time matches at Stationary sites
  #this should give df the same size as the original condensedAllEventsWithReleaseInfo (or Detections) with environmental variables attached
  desiredColumns <- c(colnames(Detections), colnames(allPressureTransducerDataWithDischarge), "environmentalDataMeasurementTime", "SiteName")

  #align columns is another function that gets columns to the right names 
  df_list <- list(
    "exactTimestampMatchesWithPTdata1" = alignColumns(exactMatchesWithPTdata, desiredColumns, Detections),
    "exactTimestampMatchesatSiteNoPTSite1" = alignColumns(exactMatchesatSiteNoPTSite, desiredColumns, Detections),  
    "exactTimestampMatchesNotPTdata1" = alignColumns(exactMatchesNotPTdata, desiredColumns, Detections),
    "notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour1" = alignColumns(notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1HourValidDataOnly, desiredColumns = desiredColumns, Detections), 
    "notExactTimestampMatchesDetectionsStationaryValidDataWithClosestEnvironmentalReadingWithin1Hour1" = alignColumns(restOftheNotExactTimestampMatchesWithDischargeReading, desiredColumns, Detections)
    )
  
  ## some rows have an entry for ptData at an exact timestamp, but the variables are all NA so it gets put under "notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour1"
  #however since there's an exact match it also gets put under "exactMatchesNotPTdata" 
  #this leads to duplicate entries in the final df, just with different environmental timestamp fields
  #so that's why here we filter out those rows (out of 2.6 million, it was just 3 rows) based on having a different envrionmnetal timestamp
  allData <- dplyr::bind_rows(df_list) %>%
    dplyr::distinct(across(-environmentalDataMeasurementTime), .keep_all = TRUE)
  
  detach("package:data.table", unload=TRUE)
  return(allData)
}

# x <- combineEnvironmentalandDetectionsData(Detections = condensedAllEventsWithReleaseInfo,
#                                            allPressureTransducerDataWithDischarge = allPressureTransducerDataWithDischarge,
#                                            DischargeData = windyGap
#                                            )
# ###QAQC code if there are different rows for the functions

# 
# #make sure we get all rows that we don't have any environmental data for: 
# #these are the "exact matches"
# #this is the df 
# #these rows should be the same; because in the end we just want the same df before except with environmental data
# nrow(Detections) == nrow(allData)
# #these are the rows that are different
# #have to have same timezone (see above)
# x1 <- allData %>%
#   select(all_of(colnames(Detections)))
# x2 <- anti_join(Detections, x1) #, by = "Datetime"
# x2 <- anti_join(x1, Detections)
# # y <- allData %>%
# #   filter(is.na(Datetime))
# x <- x1 %>%
#   distinct()
# df2 <- x1
# #takes awhile
# duplicate_rows <- df2[duplicated(df2) & !duplicated(df2, fromLast = TRUE), ]
# 
# x <- duplicate_rows[,c("TAG", "Datetime", "Event")] %>%
#   left_join(allData)
# 
# dupesInAlldata <- allData %>%
#   filter(TAG %in% x)
  

