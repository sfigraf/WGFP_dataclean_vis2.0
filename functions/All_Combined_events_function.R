### maybe want to take all the "cleaning data" stuff out and put it into their own functions
#then this function will just be combining everything
# Create Function
## this function is up to date for new antennas 
All_combined_events_function <- function(Stationary, Mobile, Biomark, Release, Recaptures){
  
  start_time <- Sys.time()
  startMessage <- "Running All_combined_events_function: Combining and cleaning Stationary, Mobile, Biomark, Release, and Recapture csv inputs."
  print(startMessage)
    
  
  # biomark cleaning, getting dates into uniform format, 
  biomarkCleaned <- Biomark %>%
    mutate(TAG = str_replace(`DEC Tag ID`, "\\.", ""),
           # i wish this could be a join, but when there are 2 dif codes (A1, B1, etc) used for backend name, this is a bit simpler
           `Reader ID` = case_when(
             `Reader ID` %in% WindyGapBypassAntennaBackendSiteCode ~ WindyGapBypassAntennaFrontendSiteCode,
             `Reader ID` %in% WindyGapAuxiliaryAntennaBackendSiteCode ~ WindyGapAuxiliaryAntennaFrontendSiteCode,
             `Reader ID` %in% GranbyDiversionAntennaBackendSiteCode ~ GranbyDiversionAntennaFrontendSiteCode,
             `Reader ID` %in% RiverRunAntennaBackendSiteCode ~ RiverRunAntennaFrontendSiteCode,
             `Reader ID` %in% FraserRiverCanyonAntennaBackendSiteCode ~ FraserRiverCanyonAntennaFrontendSiteCode,
             TRUE ~ `Reader ID`),
           #make a column for Scan>Date if parentheses are detected in the string, that means the format is in mdy 
           # and we want to convert it to YYYYMMDD format. elsewise, leave it as is
           `Scan Date` = ifelse(str_detect(`Scan Date`, "/"), 
                              as.character(mdy(`Scan Date`)), 
                              `Scan Date`)
    ) %>%
    #we want to filter out test tags here, but not marker tags
    filter(!TAG %in% test_tags) %>%
    #get UTMs based off what is in metaDataTable
    #left_join() is faster than merge()
    left_join(wgfpMetadata$AntennaMetadata, by = c("Reader ID" = "FrontendSiteCode")) %>%
    distinct()
  
  ###Create one big clean dataset
  condensedWGFP <- Stationary %>%
    select(DTY, ARR, TAG, SCD, UTM_X, UTM_Y) %>%
    rename(Scan_Date = DTY, Scan_Time = ARR, Site_Code = SCD, UTM_X = UTM_X, UTM_Y = UTM_Y)
  
  
  condensedBiomark <- biomarkCleaned %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
    select(`Scan Date`, `Scan Time`, TAG, `Reader ID`, UTM_X, UTM_Y) %>%
    rename(Scan_Date = `Scan Date`, Scan_Time = `Scan Time`, Site_Code = `Reader ID`, UTM_X = UTM_X, UTM_Y = UTM_Y)
  
  condensedMobile <- Mobile %>% #gonna have to just change to mobile eventually
    rename(TAG = TagID) %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG),
           Date = ifelse(str_detect(Date, "/"), 
                         as.character(mdy(Date)), 
                         Date)) %>% 
    #getting backend codes to frontend codes
    left_join(wgfpMetadata$AntennaMetadata[,c("FrontendSiteCode", "BackendSiteCode")], by = c("Ant" = "BackendSiteCode")) %>%
    mutate(Ant = coalesce(FrontendSiteCode, Ant)) %>%
    select(Date, Time, TAG, Ant, UTM_X, UTM_Y) %>%
    rename(Scan_Date = Date, Scan_Time = Time, Site_Code = Ant)
  
  
  condensedMobileAndBiomark <- bind_rows(condensedMobile,condensedBiomark)
  allDetections <- bind_rows(condensedMobileAndBiomark, condensedWGFP)
  
  cleanedAllDetections <- allDetections %>%
    filter(Scan_Date >= as.Date("2020-08-06")) %>% #right before the first date of marker tag detections on stationary antennas
    mutate(
      Scan_DateTime = ymd_hms(paste(Scan_Date, Scan_Time))) %>%
    select(Scan_Date, Scan_Time, Scan_DateTime, TAG, Site_Code, UTM_X, UTM_Y)
  
  ### all detections and recaps and release "EVENTS" DF
  
  #getting timestamps in order and getting relevant columns
  cleanedRelease <- Release %>%
    rename(TAG = TagID) %>%
    mutate(TAG = str_trim(TAG),
           Species = str_trim(Species),
           Date = mdy(Date),
           DateTime = lubridate::ymd_hms(paste(Date, Time))) %>%
    select(RS_Num, River, ReleaseSite, Date, Time, DateTime, UTM_X, UTM_Y, Species, Length, Weight, TAG, TagSize, Ant, Event)
  
  #getting timestamps in order and getting relevant columns
  
  cleanedRecaptures <- Recaptures %>%
    rename(TAG = TagID) %>%
    filter(!Date %in% c("", " ", NA)) %>%
    mutate(TAG = str_trim(TAG),
           Event = str_trim(Event),
           Species = str_trim(Species),
           Date = mdy(Date),
           DateTime = ymd_hms(paste(Date, Time))) %>%
    select(RS_Num, River, RecaptureSite, DateTime, Date, Time, UTM_X, UTM_Y, Species, Length, Weight, TAG, TagSize, Ant, Event) %>%
    rename(
      Recap_Length = Length,
      Recap_Weight = Weight
    )
  
  #getting all detections file ready to merge with encounters
  allDetectionsForBinding <- cleanedAllDetections %>%
    mutate(Date = ymd(Scan_Date)) %>%
    rename(
      Time = Scan_Time,
      DateTime = Scan_DateTime,
      Event = Site_Code
    ) 
  
  # this file is used in enc_hist_summary_wide 
  recapturesAndDetections <- bind_rows(allDetectionsForBinding, cleanedRecaptures)
  
  allEvents <- bind_rows(recapturesAndDetections, cleanedRelease)
  
  # bind rows vs left join; bind rows will make it so there is a "release" or "recapture" event and also make columns with relevant info
  
  #fills in release info so it is known at any row of detection
  allEventsWithReleaseInfo <- left_join(allEvents, cleanedRelease, by = c("TAG"))
  
  
  condensedAllEventsWithReleaseInfo <- allEventsWithReleaseInfo %>%
    select(Date.x, Time.x, DateTime.x, TAG, Event.x, Species.y, Length.y, Weight.y, ReleaseSite.y, Date.y, RecaptureSite, Recap_Length, Recap_Weight, UTM_X.x, UTM_Y.x) %>%
    rename(Release_Date = Date.y,
           Date = Date.x,
           Time = Time.x,
           Datetime = DateTime.x,
           Event = Event.x,
           Species = Species.y,
           Release_Length = Length.y,
           Release_Weight = Weight.y, 
           ReleaseSite = ReleaseSite.y,
           UTM_X = UTM_X.x,
           UTM_Y = UTM_Y.x) %>%
    #gets rid of all duplicate rows but keeps all info
    distinct(Datetime, TAG, Event, .keep_all = TRUE) 
  
  Tags_only <- cleanedRelease %>%
    select(TAG)
  
  #makes sure all events are from tags ONLY in the release file
  
  condensedAllEventsWithReleaseInfo <- left_join(Tags_only, condensedAllEventsWithReleaseInfo, by = "TAG")
  #allPressureTransducerDataWithDischarge$dateTime <- lubridate::force_tz(allPressureTransducerDataWithDischarge$dateTime, tzone = "UTC")
  
  ###add temp/environmental data to that
  #these arguments come from the createFlatFilesRunscript
  condensedAllEventsWithReleaseandEnvironmentalInfo <- combineEnvironmentalandDetectionsData(Detections = condensedAllEventsWithReleaseInfo,
                                             allPressureTransducerDataWithDischarge = allPressureTransducerDataWithDischarge,
                                             DischargeData = windyGap
                                             )
  #this is the final df 
  
  #Change na to "No info" in select columns so that it will register with the Picker input in the app
  #pretty sure that's just a bug on the DT or shinyWidgets end that it can't select by NA
  # 87 rows were not even showing up on the all_events app because the Species was NA -12/14/21 SG
  # 840 rows weren't showing up on the display bc length weights are NA i nthe release file -11/6/24 SG
  condensedAllEventsWithReleaseandEnvironmentalInfo <- condensedAllEventsWithReleaseandEnvironmentalInfo %>%
    replace_na(list(Species = "No Info", ReleaseSite = "No Info", 
                    #changing release wight and length to 0 keeps the fish in the df when weight/lentgh filters are active
                    Release_Weight = 0, Release_Length = 0,
                    Site = "No Site Associated")) %>%
    dplyr::filter(!TAG %in% c("230000999999"))
  
  ### This is getting the events dataframe to only the data relevant for joining with stations
  #stattions are used for movements, distance moved
  allEventsRelevantToStations <- condensedAllEventsWithReleaseandEnvironmentalInfo %>%
    #this part is for making sure the sequence of events will make sense
    # if there's no tag input then have to group_by TAG as well
    group_by(Date, TAG) %>% 
    mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                  Datetime == max(Datetime) ~ "Last_of_day",
                                  Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
    ) %>%
    ungroup() %>%
    distinct(TAG, Event, Date, first_last,  UTM_X, UTM_Y, .keep_all = TRUE) %>%
    arrange(Datetime) 
  
  ###get growth rates for QAQC tab
  growthRates <- getGrowthRates(Release = Release, Recaptures = Recaptures)

  df_list <- list("All_Detections" = cleanedAllDetections, 
                  "All_Events_most_relevant" = allEventsRelevantToStations,
                  #allEvents has release and recapture along with detections. All Detections just has detections
                  "All_Events" = condensedAllEventsWithReleaseandEnvironmentalInfo, 
                  "Recaps_detections" = recapturesAndDetections, 
                  "growthRates" = growthRates)
  
  end_time <- Sys.time()
  endMessage <- paste("All_combined_events_function took", round(difftime(end_time, start_time, units = "mins"),2), "minutes.")
  print(endMessage)
  return(list("df_list" = df_list, 
              "endMessage" = paste(c(startMessage, endMessage), collapse = "<br>")))
}
  
