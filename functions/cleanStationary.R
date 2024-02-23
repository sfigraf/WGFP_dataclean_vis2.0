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
    #this variable comes from the metadata
    filter(!TAG %in% test_tags)
  
  #cleaning timestamps for mobile and old stationary detections mainly
  #currently we are converting to periods so that it is easier to add and subtract intervals
  if (any(grepl("PM|AM", Stationary$ARR))) {
    Stationary_cleanedTime <- Stationary %>%
      mutate(ARR1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ lubridate::hms(ARR) - hours(12),
                              str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ lubridate::hms(ARR),
                              
                              str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ lubridate::hms(ARR),
                              str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ lubridate::hms(ARR) + hours(12),
                              #if it doesn't detect PM or AM just do lubridate::hms(ARR)
                              str_detect(ARR, "PM|AM") == FALSE ~ lubridate::hms(ARR)),
      ) %>%
      #but that also means that as_datetime reads those as periods and doesn't play well with the 0s interval specifically
      #so we need to convert that plain "1970-01-01" to midnight
      mutate(ARR2 = as.character(as_datetime(ARR1)), 
             ARR = ifelse(ARR2 == "1970-01-01", 
                          "00:00:00",
                          str_trim(str_sub(ARR2, start = 11, end = -1))
             )
    ) %>%
      select(-c(ARR1, ARR2))
  } else{
    Stationary_cleanedTime <- Stationary %>%
      mutate(ARR = lubridate::hms(ARR))
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
           SCD = case_when(SCD == "CD7" & ANT == "A1" ~ "CD1",
                           SCD == "CD7" & ANT == "A2" ~ "CD2",
                           SCD == "CD7" & ANT == "A3" ~ "CS1",
                           SCD == "CD7" & ANT == "A4" ~ "CS2",
                           TRUE ~ SCD)) %>%
    # assigning UTM's are important because they are plotted later when getting stations file in GIS
    mutate(UTM_X =case_when(SCD == "RB1" | SCD == "RB2" ~ "412489",
                            SCD == "HP3" | SCD == "HP4" ~ "414375",
                            SCD == "CF5" | SCD == "CF6" ~ "416965",
                            SCD == "CD1" | SCD == "CD2" ~ "415802",
                            SCD == "CS1" | SCD == "CS2" ~ "415787",
                            SCD == "CU1" | SCD == "CU2" ~ "416723"),
           UTM_Y = case_when(SCD == "RB1" | SCD == "RB2" ~ "4439413",
                             SCD == "HP3" | SCD == "HP4" ~ "4440241",
                             SCD == "CF5" | SCD == "CF6" ~ "4439369",
                             SCD == "CD1" | SCD == "CD2" ~ "4439907",
                             SCD == "CS1" | SCD == "CS2" ~ "4439908",
                             SCD == "CU1" | SCD == "CU2" ~ "4439443")) %>%
    distinct()
  end_time = Sys.time()
  print(paste("Cleaning Stationary file took", round((end_time-start_time),2)))
  
  return(Stationary_withUTMS)
}
