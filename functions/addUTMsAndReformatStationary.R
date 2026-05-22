#### stationary data utm assigning function
#marker tags are included in the return, but test tags are filtered out

addUTMsAndCorrectDSTStationary <- function(Stationary, test_tags){
  start_time <- Sys.time()
  print("Adding UTMs and correcting DST to Stationary Detection File......")
  
  Stationary_withUTMS <- Stationary %>%
    #taking out test_tags
    #this variable comes from the metadata
    filter(!TAG %in% test_tags) %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
    #the frontend/backend part should be unnecessary since the backend and frontend site code names should be the same with stationary antennas but it's good to be consistent
    ## getting UTM's are important because they are plotted later when getting stations file in GIS
    left_join(wgfpMetadata$AntennaMetadata[,c("FrontendSiteCode", "BackendSiteCode", "UTM_X", "UTM_Y")], by = c("SCD" = "BackendSiteCode")) %>%
    mutate(SCD = coalesce(FrontendSiteCode, SCD)) %>%
    select(-FrontendSiteCode) %>%
    distinct() 
  
  ###daylight savings accounted for
  Stationary_withUTMSDSTCorrected <- Stationary_withUTMS %>%
    mutate(
      # 1. Parse using Phoenix time (which is UTC-7 year-round and NEVER observes DST).
      # Because Arizona doesn't skip an hour, you will get ZERO "failed to parse" warnings!
      datetime_mst = ymd_hms(paste(DTY, ARR), tz = "America/Phoenix"),

      # 2. Convert to Denver time.
      # This automatically adds 1 hour to every single row between March and November,
      # but leaves the winter months exactly the same.
      datetime_denver = with_tz(datetime_mst, tzone = "America/Denver")
    ) %>%
    #getting these columns back to desired format for later combining with mobile, biomark etc
    mutate(
      DTY_corrected = as.character(as.Date(datetime_denver)),
      ARR_corrected = format(datetime_denver, "%H:%M:%S")
    ) %>%
    select(-datetime_mst, -datetime_denver) %>%
    relocate(DTY_corrected, .after = DTY) %>%
    relocate(ARR_corrected, .after = ARR)
  # df_corrected <- Stationary_withUTMS %>%
  #   # Combine DTY and ARR into a temporary datetime column with your local timezone
  #   mutate(
  #     # 1. First, parse as standard date-time *without* a timezone (keeps it neutral)
  #     datetime_naive = ymd_hms(paste(DTY, ARR)),
  #     
  #     # 2. Force the Denver timezone, telling R how to handle the DST gaps.
  #     # 'boundary' rolls non-existent clock times forward to the post-gap time.
  #     datetime_denver = force_tz(
  #       datetime_naive, 
  #       tzone = "America/Denver", 
  #       roll_dst = "boundary"
  #     )      #dateTime = ymd_hms(paste(DTY, ARR), tz = "America/Denver")
  #   ) %>%
  #   mutate(
  #     # Example: Converting the corrected time to UTC to normalize the data
  #     #datetime_utc = with_tz(datetime_local, tzone = "UTC"),
  #     
  #     # Overwriting or creating new columns based on the corrected timeline
  #     DTY_corrected = as.Date(datetime_denver),
  #     ARR_corrected = format(datetime_denver, "%H:%M:%S")
  #   )
  
  # x <- df_corrected %>%
  #   filter(DTY == "2024-03-10")
  # daylugthsavginsRows <- Stationary_withUTMS %>%
  #   # Create a temporary column to check the parsing
  #   mutate(parsed_test = ymd_hms(paste(DTY, ARR), tz = "America/Denver")) %>%
  #   # Keep ONLY the rows where the parsing turned into an NA...
  #   # ...but ensure the original ARR wasn't already NA or blank
  #   filter(is.na(parsed_test) & !is.na(ARR) & ARR != "")
  #write_csv(daylugthsavginsRows, "daylightSavingsStationaryRows.csv")
    
  end_time = Sys.time()
  print(paste("Adding UTMs and correcting daylight savings for Stationary Detection file took", round(difftime(end_time, start_time, units = "mins"),2), "minutes"))
  
  return(Stationary_withUTMSDSTCorrected)
}
