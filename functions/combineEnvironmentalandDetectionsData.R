library(data.table)
combineEnvironmentalandDetectionsData <- function(Detections, allPressureTransducerDataWithDischarge, DischargeData){
  #########joining to get siteName
  DetectionsWorking <- Detections %>%
    #add pressure treansducer site name to have a key to join with PT data
    #this part works as long as the site name in the metadata is what is used in the PT files as well
    #as more sites are added, we'll have to make sure the names in the pressure transducer files are the same as in the metadata "PressureTransducerSiteName" field
    left_join(wgfpMetadata$AntennaMetadata[,c("FrontendSiteCode", "PressureTransducerSiteName")], by = c("Event" = "FrontendSiteCode")) %>%
    rename(SiteName = PressureTransducerSiteName)
    # add_row(Datetime = as.POSIXct("2019-09-16 07:15:00")) %>%
    # add_row(Datetime = as.POSIXct("2021-05-24 08:00:00"), SiteName = "Hitching Post")
  ###remove rows here that have NA's assoicatied with every field except datetime
  
  specified_columns <- c("dateTime")
  
  # Filter rows based on NA values in other columns
  #filters rows from the data frame allPressureTransducerDataWithDischarge based on whether they have missing values only in specified columns (specified_columns),
  #filtered_PTData is the dates where there all all NA values in rest of the colunms
  filtered_PTData <- allPressureTransducerDataWithDischarge[rowSums(is.na(allPressureTransducerDataWithDischarge[, setdiff(names(allPressureTransducerDataWithDischarge), specified_columns)])) == (ncol(allPressureTransducerDataWithDischarge) - length(specified_columns)), ]
 #then we anti_join that with the original PT data with discharge to get df of data with datetiems that have some sort of relevant data
  allPressureTransducerDataWithDischargeNoNA <- anti_join(allPressureTransducerDataWithDischarge, filtered_PTData)
  
  
  PTData <- allPressureTransducerDataWithDischargeNoNA %>%
    mutate(Datetime = as.POSIXct(dateTime)) %>%
    rename(
      #Datetime = dateTime, 
      SiteName = Site)
  
  #check same timezone
  if(attr(PTData$Datetime, "tzone") != attr(DetectionsWorking$Datetime, "tzone")){
    #force detection df to correct zone
    DetectionsWorking$Datetime <- lubridate::force_tz(DetectionsWorking$Datetime, tzone = "America/Denver")
    
  }
  
  notExactTimestampMatchesDetections <- anti_join(DetectionsWorking,PTData, by = "Datetime") 
  
  notExactTimestampMatchesDetections <- data.table(notExactTimestampMatchesDetections)
  PTData <- data.table(PTData)
  
  keycols <- c("Datetime", "SiteName")
  setkeyv(notExactTimestampMatchesDetections, keycols)
  setkeyv(PTData, keycols)

  #perform a rolling join from data.table
  #for data where we have a stationary site attached, discharge data is from the closest hour. Otherwise, dishcarge data is within 15 minutes
  notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading <- PTData[notExactTimestampMatchesDetections, roll = "nearest", on = .(SiteName, Datetime), nomatch = NULL]

  #gives df of envrionmental data and detections with closest timestamps
  notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading <- notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading %>%
    relocate(dateTime, Datetime) %>%
    rename(environmentalDataMeasurementTime = dateTime)
  
  #change columns to NA that are outside 1 hour time frame
  #cols to change to NA: 
  columnstoChange <- c(colnames(PTData)[grepl("_", colnames(PTData))], "USGSDischarge")
  
  notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour <- notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading %>%
    mutate(timeDifference = ifelse(abs(difftime(environmentalDataMeasurementTime, Datetime, units = c("hours"))) >= 1, 1, 0), 
           across(all_of(columnstoChange), ~ ifelse(timeDifference == 1, NA_real_, .))
    ) #%>% 
    #filter(timeDifference )
    #arrange(desc(SiteName))
  y <- notExactTimestampMatchesDetectionsWithClosestEnvironmentalReading %>% filter(Datetime == "2023-12-26 15:00:00")
  # y1 <- y %>%
  #   mutate(time = abs(difftime(environmentalDataMeasurementTime, Datetime, units = c("hours"))))
  
  #####
  #selected data without releaseSites
  
  #gives exact matches of detections NOT at the stationary sites: release, mobile rus, biomark
  #can only join with USGS data here
  #and soon we shold differntiaite and only join on above/below the dam, since the gauge is at hitching post and isn't that useful for confluence/above the dam
  #q: how do we decide above/below? is it a station number, like anything above hitching post?
  exactMatchesNotPTdata <- DetectionsWorking %>%
    filter(!SiteName %in% c("Confluence", "Hitching Post", "Red Barn")) %>%
    inner_join(DischargeData, by = c("Datetime" = "dateTime")) %>%
    mutate(environmentalDataMeasurementTime = Datetime) %>%
    rename(USGSDischarge = Flow_Inst)
  
  # x <- exactMatchesNotPTdata %>%
  #   filter(Datetime == "2023-12-26 15:00:00")
  
  #get ptdata for sites with exact matching timestamps between stationary antenna detections and PTdata
  #but there's not actually a site associated with the PT data on that side
  ptData_noSite <- PTData %>%
    filter(is.na(SiteName))
  
  exactMatchesatSiteNoPTSite <- DetectionsWorking %>%
    filter(SiteName %in% c("Confluence", "Hitching Post", "Red Barn")) %>%
    inner_join(ptData_noSite, by = c("Datetime" = "dateTime")) %>%
    mutate(environmentalDataMeasurementTime = Datetime)
  
  #need to get exact time matches that ARE at the stationary sites
  exactMatchesWithPTdata <- DetectionsWorking %>%
    filter(SiteName %in% c("Confluence", "Hitching Post", "Red Barn")) %>%
    inner_join(PTData, by = c("Datetime" = "dateTime", "SiteName")) %>%
    mutate(environmentalDataMeasurementTime = Datetime)
  
  #need to keep exact matches that don't have a siteName associated
  
  ###then bind otgether the notExactTimeMatches, exact time matches not at stationary sites, and exact time matches at Staionaety sites
  #this shoudl give df the same size as the original condensedAllEventsWithReleaseInfo (or detectoins df) with environmental variables attached
  #them maybe from there we can filter based off time: within 15 mintues or an hour is ok, but if the nearest one is months off then it's a no
  desiredColumns <- c(colnames(Detections), colnames(allPressureTransducerDataWithDischarge), "environmentalDataMeasurementTime", "SiteName")
  desiredColTypes <- c(class(Detections[,c(colnames(Detections))]))
  
  alignColumns <- function(detectionsWithEnvironmentalData, desiredColumns) {
    # Create missing columns in detectionsWithEnvironmentalData with NA values
    missing_columns <- setdiff(desiredColumns, colnames(detectionsWithEnvironmentalData))
    
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        # Determine the expected column type based on desiredColumns
        expected_type <- ifelse(col %in% colnames(Detections), 
                                typeof(Detections[[col]]),
                                ifelse(col %in% colnames(allPressureTransducerDataWithDischarge),
                                       typeof(allPressureTransducerDataWithDischarge[[col]]),
                                       typeof(detectionsWithEnvironmentalData[[col]])))  # Use existing type of detectionsWithEnvironmentalData's column
        
        # Create new column with NA values and enforce the expected type
        detectionsWithEnvironmentalData[[col]] <- NA  # Start with NA values
        detectionsWithEnvironmentalData[[col]] <- switch(expected_type,
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
  
  df_list <- list(
    "exactMatchesWithPTdata1" <- alignColumns(exactMatchesWithPTdata, desiredColumns),
    "exactMatchesatSiteNoPTSite1" <- alignColumns(exactMatchesatSiteNoPTSite, desiredColumns),  
    "exactMatchesNotPTdata1" <- alignColumns(exactMatchesNotPTdata, desiredColumns),
    "notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour1" <- alignColumns(notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour, desiredColumns = desiredColumns)
  )
  ## some rows have an entry for ptData at an exact timestamp, but the variables are all NA so it gets put under "notExactTimestampMatchesDetectionsWithClosestEnvironmentalReadingWithin1Hour1"
  #however since there's an exact match it also gets put under "exactMatchesNotPTdata" 
  #this leads to duplicate entries in the final df, just with different environmental timestamp fields
  #so that's why here we filter out those rows (out of 2.6 million, it was just 3 rows) based on having a different envrionmnetal timestamp
  allData <- dplyr::bind_rows(df_list) %>%
    distinct(across(-environmentalDataMeasurementTime), .keep_all = TRUE)
  #make sure we get all rows that we don't have any environmental data for: 
  #these are the "exact matches"
  #this is the df 
  #these rows should be the same; because in the end we just want the same df before except with environmental data
  nrow(Detections) == nrow(allData)
  #these are the rows that are different
  #have to have same timezone (see above)
  x1 <- allData %>%
    select(all_of(colnames(Detections)))
  x2 <- anti_join(Detections, x1) #, by = "Datetime"
  x2 <- anti_join(x1, Detections)
  # y <- allData %>%
  #   filter(is.na(Datetime))
  x <- x1 %>%
    distinct()
  df2 <- x1
  #
  duplicate_rows <- read_rds("diplicaterows.rds")
  duplicate_rows <- df2[duplicated(df2) & !duplicated(df2, fromLast = TRUE), ]
  
  return(allData)
}
# x <- Detections %>%
#   filter(TAG == 230000142723, 
#          Date == "2024-03-10")
x <- combineEnvironmentalandDetectionsData(Detections = condensedAllEventsWithReleaseInfo,
                                           allPressureTransducerDataWithDischarge = allPressureTransducerDataWithDischarge,
                                           DischargeData = windyGap
                                           )
x <- duplicate_rows[,c("TAG", "Datetime", "Event")] %>%
  left_join(allData)

dupesInAlldata <- allData %>%
  filter(TAG %in% x)
  
# x1 <- x %>%
#   select(all_of(colnames(condensedAllEventsWithReleaseInfo))) #%>%
#   #mutate(Datetime = as.POSIXct(Datetime))
# 
# x2 <- anti_join(condensedAllEventsWithReleaseInfo, x1, by = "Datetime")
# #time 115
# #1454 rows different
# 
# y <- x %>%
#   filter(is.na(Datetime))
# 
# are_column_classes_same <- function(df1, df2) {
#   if (ncol(df1) != ncol(df2)) {
#     return(FALSE)  # Number of columns must be the same
#   }
#   
#   # Compare column classes for each column
#   all(identical(sapply(df1, class), sapply(df2, class)))
# }
# 
# are_column_classes_same(x1, condensedAllEventsWithReleaseInfo)
#  == nrow(condensedAllEventsWithReleaseInfo) -nrow(x1)
