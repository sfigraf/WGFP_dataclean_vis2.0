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
      # Parse using Phoenix time (which is UTC-7 year-round and NEVER observes DST)
      datetime_mst = ymd_hms(paste(DTY, ARR), tz = "America/Phoenix"),

      # Convert to Denver time.
      # This automatically adds 1 hour to every single row between March and November,
      # but leaves the winter months exactly the same.
      datetime_denver = with_tz(datetime_mst, tzone = "America/Denver")
    ) %>%
    #getting these columns back to desired format for later combining with mobile, biomark etc
    mutate(
      DTY_DSTcorrected = as.character(as.Date(datetime_denver)),
      ARR_DSTcorrected = format(datetime_denver, "%H:%M:%S")
    ) %>%
    select(-datetime_mst, -datetime_denver) %>%
    relocate(DTY_DSTcorrected, .after = DTY) %>%
    relocate(ARR_DSTcorrected, .after = ARR)

    
  end_time = Sys.time()
  print(paste("Adding UTMs and correcting daylight savings for Stationary Detection file took", round(difftime(end_time, start_time, units = "mins"),2), "minutes"))
  
  return(Stationary_withUTMSDSTCorrected)
}
