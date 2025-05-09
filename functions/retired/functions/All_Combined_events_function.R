

# Create Function
## this function is up to date for new antennas 
All_combined_events_function <- function(Stationary, Mobile, Biomark, Release, Recaptures){
  
  start_time <- Sys.time()
  print("Running All_combined_events_function: Combining and cleaning Stationary, Mobile, Biomark, Release, and Recapture csv inputs.")
  
  
  WGFP_NoMarkers <- Stationary %>%
    mutate(TAG = str_replace(str_trim(TAG), "\\_", "")) %>%
    filter(str_detect(TAG, "^900"), 
           !TAG %in% test_tags)

#marker tag only file 
  Markers_only <- Stationary %>%
    mutate(TAG = str_replace(str_trim(TAG), "\\_", "")) %>%
    filter(
      str_detect(TAG, "^0000000")
    )
  
  Markers_only1 <- Markers_only %>%
    mutate(
      DTY = 
        ifelse(str_detect(DTY, "/"), 
                   as.character(mdy(DTY)), 
                   DTY),
      DTY2 = as.Date(DTY))
      
  
  Markers_only2 <- Markers_only1 %>%
    #this is the same process that all_detections goes through
    mutate(Scan_Time1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ hms(ARR) - hours(12),
                                 str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ hms(ARR),
                                 
                                 str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR),
                                 str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR) + hours(12),
                                 #if it doesn't detect PM or AM just do hms(ARR)
                                 str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR)),
           Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
           CleanARR = str_trim(str_sub(Scan_Time2, start = 11, end = -1))
    ) %>%
    
    select(Code, DTY2, ARR, CleanARR, TRF, DUR, TTY, TAG, SCD, ANT, NCD, EFA) %>%
    rename(DTY = DTY2)
    
  ## rest of getting "clean" windy gap stationary data
  ### Subset Detection Type "Codes" to only include Summary (S) and Individual (I) ###
  WGFP_Clean= data.frame(WGFP_NoMarkers[which(WGFP_NoMarkers$Code == "I" | WGFP_NoMarkers$Code == "S"),])
  
  #### Add Lat Longs to detections ###
   
  # takes out 900 from TAG in WGFP Clean
  # also takes out duplicate rows
  WGFP_Clean <- WGFP_Clean %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG),
           SCD = case_when(SCD == "CD7" & ANT == "A1" ~ "CD7",
                           SCD == "CD7" & ANT == "A2" ~ "CD8",
                           SCD == "CD7" & ANT == "A3" ~ "CD9",
                           SCD == "CD7" & ANT == "A4" ~ "CD10",
                           TRUE ~ SCD),
           DTY = ifelse(str_detect(DTY, "/"), 
                         as.character(mdy(DTY)), 
                         DTY)) %>%
    
    # mutate(TAG = case_when(str_detect(TAG, "^900") ~ str_sub(TAG, 4,-1),
    #                        str_detect(TAG, "!^900") ~ TAG)) %>%
    # assigning UTM's are important because they are plotted later when getting stations file in GIS
    mutate(UTM_X =case_when(SCD == "RB1" | SCD == "RB2" ~ "412489",
                            SCD == "HP3" | SCD == "HP4" ~ "414375",
                            SCD == "CF5" | SCD == "CF6" ~ "416965",
                            SCD == "CD7" | SCD == "CD8" | SCD == "CD9" | SCD == "CD10" ~ "415801",
                            SCD == "CU11" | SCD == "CU12" ~ "416802"),
           UTM_Y = case_when(SCD == "RB1" | SCD == "RB2" ~ "4439413",
                             SCD == "HP3" | SCD == "HP4" ~ "4440241",
                             SCD == "CF5" | SCD == "CF6" ~ "4439369",
                             SCD == "CD7" | SCD == "CD8" | SCD == "CD9" | SCD == "CD10" ~ "4439899",
                             SCD == "CU11" | SCD == "CU12" ~ "4439507")) %>%
    distinct()
  
  # biomark cleaning, getting dates into uniform format, 
  biomark2 <- Biomark %>%
    mutate(TAG = str_replace(`DEC Tag ID`, "\\.", ""),
           `Reader ID` = case_when(`Reader ID` == "A1" | `Reader ID` == "B1" ~ "B3",
                                 `Reader ID` == "A2" | `Reader ID` == "B2" ~ "B4",
                                 `Reader ID` == "A3" ~ "B5",
                                 `Reader ID` == "A4" ~ "B6",
                                 TRUE ~ `Reader ID`),
           #make a column for Scan>Date if parentheses are detected in the string, that means the format is in mdy 
           # and we want to convert it to YYYYMMDD format. elsewise, leave it as is
           `Scan Date` = ifelse(str_detect(`Scan Date`, "/"), 
                              as.character(mdy(`Scan Date`)), 
                              `Scan Date`) ) %>%
    
    filter(!TAG %in% c("900230000102751", "900226001581072", "999000000007586", "999000000007585", "999000000007601",
                       "999000000007602", "900230000087405", "900230000087408", "900226001546996", "900230000088083", 
                       "900230000087402", "900230000087403", "900230000088084", "900230000228791", "900230000088082",
                       "900230000087401" ) ) %>%
    
    # from gis: B1 416026, 4440196
    #B2: 420727.9, 4437221
    # b3 is wg, b4 is kaibab
    # b5 is river run
    # b6 is fraser river canyon
    mutate(UTM_X =case_when(`Reader ID` == "B3" ~ "416026",
                            `Reader ID` == "B4" ~ "420728",
                            `Reader ID` == "B5" ~ "419210",
                            `Reader ID` == "B6" ~ "424543"),
           UTM_Y = case_when(`Reader ID` == "B3" ~ "4440196",
                             `Reader ID` == "B4" ~ "4437221",
                             `Reader ID` == "B5" ~ "4439961",
                             `Reader ID` == "B6" ~ "4435559")) %>%
    distinct()
  
  ###Create one big clean dataset
  WGFP_condensed <- WGFP_Clean %>%
    select(DTY, ARR, TAG, SCD, UTM_X, UTM_Y) %>%
    rename(Scan_Date = DTY, Scan_Time = ARR, Site_Code = SCD, UTM_X = UTM_X, UTM_Y = UTM_Y)
  
  
  Biomark_condensed <- biomark2 %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
    select(`Scan Date`, `Scan Time`, TAG, `Reader ID`, UTM_X, UTM_Y) %>%
    rename(Scan_Date = `Scan Date`, Scan_Time = `Scan Time`, Site_Code = `Reader ID`, UTM_X = UTM_X, UTM_Y = UTM_Y)
  
  Mobile_condensed <- Mobile %>% #gonna have to just change to mobile eventually
    rename(TAG = TagID) %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG),
           Date = ifelse(str_detect(Date, "/"), 
                         as.character(mdy(Date)), 
                         Date)) %>% #end of mutate
    select(Date, Time, TAG, Ant, UTM_X, UTM_Y) %>%
    rename(Scan_Date = Date, Scan_Time = Time, Site_Code = Ant)
  
  
  WG_bio <- bind_rows(WGFP_condensed,Biomark_condensed)
  All_detections <- bind_rows(WG_bio, Mobile_condensed)
  
  #cleaning timestamps for mobile and old stationary detections mainly
  if (    length(unique( str_detect(All_detections$Scan_Time, "PM|AM"))) > 1) {
    All_detections1 <- All_detections %>%
      mutate(Scan_Time1 = case_when(str_detect(Scan_Time, "AM") & str_detect(Scan_Time, "^12:") ~ hms(Scan_Time) - hours(12),
                                    str_detect(Scan_Time, "PM") & str_detect(Scan_Time, "^12:") ~ hms(Scan_Time),
                                    
                                    str_detect(Scan_Time, "AM") & str_detect(Scan_Time, "^12:", negate = TRUE) ~ hms(Scan_Time),
                                    str_detect(Scan_Time, "PM") & str_detect(Scan_Time, "^12:", negate = TRUE) ~ hms(Scan_Time) + hours(12),
                                    #if it doesn't detect PM or AM just do hms(Scan_Time)
                                    str_detect(Scan_Time, "PM|AM") == FALSE ~ hms(Scan_Time)),
      ) %>%
      mutate(Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
             clean_time = str_trim(str_sub(Scan_Time2, start = 11, end = -1))) %>%
      
      select(Scan_Date, clean_time, TAG, Site_Code, UTM_X, UTM_Y ) #%>%
      #rename(Scan_Time = (clean_time))
  }
  
  All_detections2 <- All_detections1 %>%
    filter(Scan_Date >= as.Date("2020-08-06")) %>% #right before the first date of marker tag detections on stationary antennas
    mutate(
      #datetime1 = as.POSIXct(paste(Scan_Date, Scan_Time),format="%Y-%m-%d %H:%M:%S"), #this line works too
      Scan_DateTime = ymd_hms(paste(Scan_Date, clean_time))) %>%
    #rename(Scan_DateTime = datetime2) %>%
    select(Scan_Date, clean_time, Scan_DateTime, TAG, Site_Code, UTM_X, UTM_Y )
  
### all detections and recaps and release "EVENTS" DF
  
  #getting timestamps in order and getting relevant columns
  Release1 <- Release %>%
    rename(TAG = TagID) %>%
    mutate(TAG = str_trim(TAG),
           Date = mdy(Date),
           Time1 = case_when(str_length(Time) > 5 ~ as_datetime(hms(Time)),
                             str_length(Time) <= 5 ~ as_datetime(hm(Time))), #warning message: problem with mutate(time1, some strings failed to parse); same with recaps # fixed by changing to hms() newest release file 20211229 has times with seconds 
           Time2 = str_sub(Time1, start = 11, end = -1),
           DateTime = ymd_hms(paste(Date, Time2))) %>%
    select(RS_Num,River,ReleaseSite,Date, Time2, DateTime,UTM_X,UTM_Y,Species,Length,Weight,TAG,TagSize,Ant,Event) %>%
    rename(Time = Time2) 
  
  #getting timestamps in order and getting relevant columns
  
  recaps1 <- Recaptures %>%
    rename(TAG = TagID) %>%
    filter(!Date %in% c("", " ", NA)) %>%
    mutate(TAG = str_trim(TAG),
           Date = mdy(Date),
           # added in like I did the release file, functionality for when the time contains just HH:mm and hh:mm:ss
           Time1 = case_when(str_length(Time) > 5 ~ as_datetime(hms(Time)),
                             str_length(Time) <= 5 ~ as_datetime(hm(Time))),
           Time2 = str_sub(Time1, start = 11, end = -1),
           DateTime = ymd_hms(paste(Date, Time2))) %>%
    select(RS_Num,River,RecaptureSite,DateTime,Date,Time2,UTM_X,UTM_Y,Species,Length,Weight,TAG,TagSize,Ant,Event) %>%
    rename(Time = Time2,
           Recap_Length = Length,
           Recap_Weight = Weight
    )
  
  #getting all detections file ready to merge with encounters
  All_Detections_1_merge <- All_detections2 %>%
    mutate(Date = as.Date(Scan_Date)) %>%
    rename(
      Time = clean_time,
      DateTime = Scan_DateTime,
      Event = Site_Code) 
  
  ## 
  
  
  # this file is used in enc_hist_summary_wide 
  recaps_detections <- bind_rows(All_Detections_1_merge, recaps1)
  
  detections_release_recaps <- bind_rows(recaps_detections, Release1)
  
  
  # bind rows vs left join; bind rows will make it so there is a "release" or "recapture" event and also make columns with relevant info
  

  #fills in release info so it is known at any row of detection
  filled_in_release_rows <- left_join(detections_release_recaps, Release1, by = c("TAG"))
  
  
  #this is the final df 
  
  #Change na to "No info" in select columns so that it will register with the Picker input in the app
  #pretty sure that's just a bug on the DT or shinyWidgets end that it can't select by NA
  # 87 rows were not even showing up on the all_events app because the Species was NA -12/14/21 SG
  
  filled_in_release_rows_condensed <- filled_in_release_rows %>%
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
  
  Tags_only <- Release1 %>%
    select(TAG)
  
  #makes sure all events are from tags ONLY in the release file
  
  filled_in_release_rows_condensed <- left_join(Tags_only, filled_in_release_rows_condensed, by = "TAG")
  
  
  
  ### This is getting the events dataframe to only the data relevant for joining with stations
  
  all_events_relevant_to_stations <- filled_in_release_rows_condensed %>%
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
  
  

  df_list <- list( "WGFP_Clean" = WGFP_Clean, "All_Detections" = All_detections2, 
                   "All_Events_most_relevant" = all_events_relevant_to_stations,
                  "All_Events" = filled_in_release_rows_condensed, "Marker_Tag_data" = Markers_only2, "Recaps_detections" = recaps_detections)
  
  end_time <- Sys.time()
  print(paste("All_combined_events_function took", round((end_time-start_time)/60,2), "Minutes"))
  
  return(df_list)
}
  
