#### stationary data cleaning runscript

##not sure if we want to return marker tags too

cleanStationary <- function(Stationary){
  start_time <- Sys.time()
  print("Cleaning Raw Stationary Detection File......")
  Stationary <- Stationary %>%
    mutate(TAG = gsub("\\_", "", str_trim(TAG)), 
           DTY = ifelse(str_detect(DTY, "/"),
                    as.character(mdy(DTY)),
                    DTY)) %>%
    #taking out test_tags
    filter(!TAG %in% test_tags)
  
  #cleaning timestamps for mobile and old stationary detections mainly
  if (any(grepl("PM|AM", Stationary$ARR))) {
    Stationary_cleanedTime <- Stationary %>%
      mutate(ARR1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ hms(ARR) - hours(12),
                              str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ hms(ARR),
                              
                              str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR),
                              str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR) + hours(12),
                              #if it doesn't detect PM or AM just do hms(ARR)
                              str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR)),
      ) %>%
      mutate(ARR2 = as.character(as_datetime(ARR1)), 
             ARR = str_trim(str_sub(ARR2, start = 11, end = -1))) %>%
      select(-c(ARR1, ARR2))
    #rename(Scan_Time = (clean_time))
  } else{
    Stationary_cleanedTime <- Stationary %>%
      mutate(ARR = hms(ARR))
  }
  
  Stationary_cleanedTime1 <- Stationary_cleanedTime %>%
    filter(nchar(DTY) == 10, 
           DTY >= as.Date("2020-08-06"), 
           Code %in% c("I", "S"))
  
  #### Add UTMS to detections ###
  
  # takes out 900 from TAG in WGFP Clean
  # also takes out duplicate rows
  Stationary_withUTMS <- Stationary_cleanedTime1 %>%
    #this change
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG),
           SCD = case_when(SCD == "CD7" & ANT == "A1" ~ "CD7",
                           SCD == "CD7" & ANT == "A2" ~ "CD8",
                           SCD == "CD7" & ANT == "A3" ~ "CD9",
                           SCD == "CD7" & ANT == "A4" ~ "CD10",
                           TRUE ~ SCD)) %>%
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
  end_time = Sys.time()
  print(paste("Reading in files took", round((end_time-start_time),2)))
  
  return(Stationary_withUTMS)
}
