#### stationary data utm assigning function
#marker tags are included in the return, but test tags are filtered out

addUTMsAndReformatStationary <- function(Stationary){
  start_time <- Sys.time()
  print("Reformatting and adding UTMs to Stationary Detection File......")
  
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
    
  end_time = Sys.time()
  print(paste("Adding UTMs and reformatting Statoinary Detection file took", round(difftime(end_time, start_time, units = "mins"),2), "minutes"))
  
  return(Stationary_withUTMS)
}
