#### stationary data cleaning runscript

##not sure if we want to return marker tags too

addUTMsAndReformatStationary <- function(Stationary){
  start_time <- Sys.time()
  print("Reformatting and adding UTMs to Stationary Detection File......")
  
  Stationary_withUTMS <- Stationary %>%
    #taking out test_tags
    #this variable comes from the metadata
    filter(!TAG %in% test_tags) %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
    # mutate(TAG = gsub("\\_", "", str_trim(TAG)), 
    #        DTY = ifelse(str_detect(DTY, "/"),
    #                 as.character(mdy(DTY)),
    #                 DTY)
    #        ) %>%
    
    #the frontend/backend part should be unnecessary since the backend and frontend site code names should be the same with stationary antennas but it's good to be consistent
    #the UTMs are v necessary
    left_join(wgfpMetadata$AntennaMetadata[,c("FrontendSiteCode", "BackendSiteCode", "UTM_X", "UTM_Y")], by = c("SCD" = "BackendSiteCode")) %>%
    mutate(SCD = coalesce(FrontendSiteCode, SCD)) %>%
    select(-FrontendSiteCode) %>%
    distinct() 
  

  # takes out 900 from TAG in WGFP Clean
  # also takes out duplicate rows
  # Stationary_withUTMS <- Stationary_cleanedTime1 %>%
  #   #this change
  #   
  #   # assigning UTM's are important because they are plotted later when getting stations file in GIS
  #   left_join(selectedMetaData, by = c("SCD" = "FrontendSiteCode")) %>%
    
  end_time = Sys.time()
  print(paste("Adding UTMs and reformatting Statoinary Detection file took", round((end_time-start_time),2), "seconds."))
  
  return(Stationary_withUTMS)
}
