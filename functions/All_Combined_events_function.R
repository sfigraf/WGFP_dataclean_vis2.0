### maybe want to take all the "cleaning data" stuff out and put it into their own functions
#then this function will just be combining everything
# Create Function
## this function is up to date for new antennas 
All_combined_events_function <- function(Stationary, Mobile, Biomark, Release, Recaptures){
  
  start_time <- Sys.time()
  print("Running All_combined_events_function: Combining and cleaning Stationary, Mobile, Biomark, Release, and Recapture csv inputs......")
  
  
  # biomark cleaning, getting dates into uniform format, 
  biomarkCleaned <- Biomark %>%
    mutate(TAG = str_replace(DEC.Tag.ID, "\\.", ""),
           Reader.ID = case_when(Reader.ID == "A1" | Reader.ID == "B1" ~ WindyGapAntennaSiteCode,
                                 Reader.ID == "A2" | Reader.ID == "B2" ~ KaibabParkAntennaSiteCode,
                                 Reader.ID == "A3" ~ RiverRunAntennaSiteCode,
                                 Reader.ID == "A4" ~ FraserRiverCanyonAntennaSiteCode,
                                 TRUE ~ Reader.ID),
           #make a column for Scan>Date if parentheses are detected in the string, that means the format is in mdy 
           # and we want to convert it to YYYYMMDD format. elsewise, leave it as is
           Scan.Date = ifelse(str_detect(Scan.Date, "/"), 
                              as.character(mdy(Scan.Date)), 
                              Scan.Date)
    ) %>%
    #we want to filter out test tags here, but not marker tags
    filter(!TAG %in% test_tags) %>%
    
    # from gis: B1 416026, 4440196
    #B2: 420727.9, 4437221
    # b3 is wg, b4 is kaibab
    # b5 is river run
    # b6 is fraser river canyon
    mutate(UTM_X =case_when(Reader.ID == "B3" ~ "416026",
                            Reader.ID == "B4" ~ "420728",
                            Reader.ID == "B5" ~ "419210",
                            Reader.ID == "B6" ~ "424543"),
           UTM_Y = case_when(Reader.ID == "B3" ~ "4440196",
                             Reader.ID == "B4" ~ "4437221",
                             Reader.ID == "B5" ~ "4439961",
                             Reader.ID == "B6" ~ "4435559")) %>%
    distinct()
  
  ###Create one big clean dataset
  condensedWGFP <- Stationary %>%
    select(DTY, ARR, TAG, SCD, UTM_X, UTM_Y) %>%
    rename(Scan_Date = DTY, Scan_Time = ARR, Site_Code = SCD, UTM_X = UTM_X, UTM_Y = UTM_Y)
  
  
  condensedBiomark <- biomarkCleaned %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
    select(Scan.Date, Scan.Time, TAG, Reader.ID, UTM_X, UTM_Y) %>%
    rename(Scan_Date = Scan.Date, Scan_Time = Scan.Time, Site_Code = Reader.ID, UTM_X = UTM_X, UTM_Y = UTM_Y)
  
  condensedMobile <- Mobile %>% #gonna have to just change to mobile eventually
    rename(TAG = TagID) %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG),
           Date = ifelse(str_detect(Date, "/"), 
                         as.character(mdy(Date)), 
                         Date)) %>% #end of mutate
    select(Date, Time, TAG, Ant, UTM_X, UTM_Y) %>%
    rename(Scan_Date = Date, Scan_Time = Time, Site_Code = Ant)
  
  
  condensedMobileAndBiomark <- bind_rows(condensedMobile,condensedBiomark)
  allDetections <- bind_rows(condensedMobileAndBiomark, condensedWGFP)
  
  cleanedAllDetections <- allDetections %>%
    filter(Scan_Date >= as.Date("2020-08-06")) %>% #right before the first date of marker tag detections on stationary antennas
    mutate(
      Scan_DateTime = ymd_hms(paste(Scan_Date, Scan_Time))) %>%
    select(Scan_Date, Scan_Time, Scan_DateTime, TAG, Site_Code, UTM_X, UTM_Y )
  
  ### all detections and recaps and release "EVENTS" DF
  
  #getting timestamps in order and getting relevant columns
  cleanedRelease <- Release %>%
    rename(TAG = TagID) %>%
    mutate(TAG = str_trim(TAG),
           Date = mdy(Date),
           DateTime = lubridate::ymd_hms(paste(Date, Time))) %>%
    select(RS_Num, River, ReleaseSite, Date, Time, DateTime, UTM_X, UTM_Y, Species, Length, Weight, TAG, TagSize, Ant, Event)# %>%
  
  #getting timestamps in order and getting relevant columns
  
  cleanedRecaptures <- Recaptures %>%
    rename(TAG = TagID) %>%
    filter(!Date %in% c("", " ", NA)) %>%
    mutate(TAG = str_trim(TAG),
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
  
  
  #this is the final df 
  
  #Change na to "No info" in select columns so that it will register with the Picker input in the app
  #pretty sure that's just a bug on the DT or shinyWidgets end that it can't select by NA
  # 87 rows were not even showing up on the all_events app because the Species was NA -12/14/21 SG
  
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
    distinct(Datetime, TAG, Event, .keep_all = TRUE) %>%
    replace_na(list(Species = "No Info", ReleaseSite = "No Info"))
  
  Tags_only <- cleanedRelease %>%
    select(TAG)
  
  #makes sure all events are from tags ONLY in the release file
  
  condensedAllEventsWithReleaseInfo <- left_join(Tags_only, condensedAllEventsWithReleaseInfo, by = "TAG")
  
  ### This is getting the events dataframe to only the data relevant for joining with stations
  
  allEventsRelevantToStations <- condensedAllEventsWithReleaseInfo %>%
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
  
  df_list <- list("All_Detections" = cleanedAllDetections, 
                  "All_Events_most_relevant" = allEventsRelevantToStations,
                  #allEvents has release and recapture along with detections. All Detections just has detections
                  "All_Events" = condensedAllEventsWithReleaseInfo, 
                  "Recaps_detections" = recapturesAndDetections)
  
  end_time <- Sys.time()
  print(paste("All_combined_events_function took", round((end_time-start_time),2)))
  
  return(df_list)
}
  
