#### stationary data cleaning runscript

##not sure if we want to return marker tags too

#Stationary_june <- read.csv(paste0("./data/WGFP_Raw_20230623.csv"))

saveRDS(Stationary_june, "data/stationaryRaw_20230623.rds")
cleanStationary <- function(Stationary){
  
  Stationary <- Stationary %>%
    mutate(TAG = gsub("\\_", "", str_trim(TAG)), 
           DTY = ifelse(str_detect(DTY, "/"),
                    as.character(mdy(DTY)),
                    DTY))
  
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
  
  ##separating marker tags and detections
  # 
  # Cleaned_Stationary_detectionsOnly <- Stationary_cleanedTime %>%
  #   filter(grepl("^900", TAG),
  #          Code %in% c("I", "S"), 
  #          DTY >= as.Date("2020-08-06")) %>% 
  #   mutate(
  #          TAG = ifelse(grepl("^900", TAG), gsub("^900", "", TAG)),
  #          SCD = case_when(SCD == "CD7" & ANT == "A1" ~ "CD7",
  #                          SCD == "CD7" & ANT == "A2" ~ "CD8",
  #                          SCD == "CD7" & ANT == "A3" ~ "CD9",
  #                          SCD == "CD7" & ANT == "A4" ~ "CD10",
  #                          TRUE ~ SCD),
  #          DTY = ifelse(str_detect(DTY, "/"), 
  #                       as.character(mdy(DTY)), 
  #                       DTY))
  # 
  # #marker tag only file 
  # Markers_only <- Stationary_cleanedTime %>%
  #   filter(str_detect(TAG, "^0000000"))
  
  
  return(Stationary_cleanedTime1)
}
# 
# x <- cleanStationary(Stationary_june)
# # # 
# saveRDS(x, file = "data/WGFP_StationaryCleaned_20230623.rds")
# still want to save a pure raw .rds file as well as the cleaned one
