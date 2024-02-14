### stationary clean function
CleanStationary <- function(Stationary) {
  
  Stationary_NoMarkers <- Stationary %>%
    mutate(TAG = str_replace(str_trim(TAG), "\\_", "")) %>%
    filter(str_detect(TAG, "^900"), 
           !TAG %in% c("900230000102751","900226001581072","900230000004000", "900230000088082", "900230000088083"))
  
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
  Stationary_Clean= data.frame(Stationary_NoMarkers[which(Stationary_NoMarkers$Code == "I" | Stationary_NoMarkers$Code == "S"),])
  
  #### Add Lat Longs to detections ###
  
  # takes out 900 from TAG in Stationary Clean
  # also takes out duplicate rows
  Stationary_Clean <- Stationary_Clean %>%
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
  
  # if (length(unique( str_detect(All_detections$Scan_Time, "PM|AM"))) > 1) {
  #   All_detections1 <- All_detections %>%
  #     mutate(Scan_Time1 = case_when(str_detect(Scan_Time, "AM") & str_detect(Scan_Time, "^12:") ~ hms(Scan_Time) - hours(12),
  #                                   str_detect(Scan_Time, "PM") & str_detect(Scan_Time, "^12:") ~ hms(Scan_Time),
  #                                   
  #                                   str_detect(Scan_Time, "AM") & str_detect(Scan_Time, "^12:", negate = TRUE) ~ hms(Scan_Time),
  #                                   str_detect(Scan_Time, "PM") & str_detect(Scan_Time, "^12:", negate = TRUE) ~ hms(Scan_Time) + hours(12),
  #                                   #if it doesn't detect PM or AM just do hms(Scan_Time)
  #                                   str_detect(Scan_Time, "PM|AM") == FALSE ~ hms(Scan_Time)),
  #     ) %>%
  #     mutate(Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
  #            clean_time = str_trim(str_sub(Scan_Time2, start = 11, end = -1))) %>%
  #     
  #     select(Scan_Date, clean_time, TAG, Site_Code, UTM_X, UTM_Y ) #%>%
    #rename(Scan_Time = (clean_time))
  #}
  return(Stationary_Clean)
}