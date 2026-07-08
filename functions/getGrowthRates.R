# Recaptures = indiv_datasets_list$recapdata# %>%
#   #select(-`X.2`)
# Release <- indiv_datasets_list$releasedata
# 
# combined_data <- Recaptures %>%
#   inner_join(Release, by = "TagID") 
# 
# # 2. Calculate time at large and filter
# filtered_data <- combined_data %>%
#   mutate(
#     # Calculate difference in days. Ensure dates are Date or POSIXct objects!
#     time_at_large = as.numeric(difftime(recapture_date, release_date, units = "days"))
#   )



getGrowthRates <- function(Recaptures, Release){
  
  #this is 
  Release <- Release %>%
    mutate(Date = as.character(lubridate::mdy(Date)), 
           Species = str_trim(Species))
  
  Recaptures <- Recaptures %>%
    mutate(Date = as.character(lubridate::mdy(Date)), 
           Species = str_trim(Species)
    )
  
  RecapturesforBind <- alignColumns(Recaptures, names(Release), Release) %>%
    left_join(Recaptures[,c("TagID", "Length", "Weight", "RecaptureSite")], by = c("TagID", "Length", "Weight"))
  
  ReleaseforBind <- alignColumns(Release, names(RecapturesforBind), RecapturesforBind)
  
  ReleaseRecaps <- bind_rows(ReleaseforBind, RecapturesforBind)
  
  DFforGrowthRates <- ReleaseRecaps %>%
    mutate(Date = lubridate::ymd(Date), 
           AgeClass = case_when(
             #age classes based on examination of age frequency graph and looking at eaks and vallyes and talking with eric fetherman
             !Species %in% c("RBT", "LOC") ~ "Unknown",
             
             #LOC
             Species == "LOC" & Length <= 150 ~ "0-1 Years",
             Species == "LOC" & Length > 150 & Length <= 230 ~ "2 Years",
             Species == "LOC" & Length > 230 & Length <= 360 ~ "3 Years",
             Species == "LOC" & Length > 360 ~ "3+ Years",
             
             ##RBT
             Species == "RBT" & Length <= 140 ~ "0-1 Years",
             Species == "RBT" & Length > 140 & Length <= 330 ~ "2 Years",
             Species == "RBT" & Length > 330 & Length <= 430 ~ "3 Years",
             Species == "RBT" & Length > 430 ~ "3+ Years",
             TRUE ~ "Unknown" # Catch-all for NA or missing lengths
           )
    ) %>%
    group_by(TagID) %>%
    arrange(Date, .by_group = TRUE) %>%
    #use 52.25 weeks to account for leap years
    mutate(daysSince = as.numeric(difftime(Date, lag(Date), units = "days")), 
           yearsSince = daysSince/365.25,
           previousLength = lag(Length), 
           previousWeight = lag(Weight),
           previousYear = lag(year(Date)),
           previousAgeClass = lag(AgeClass), 
           previousRiver = lag(River)
    ) %>%
    mutate(
      `Length Growth Rate mm per Year`= round((Length - previousLength)/yearsSince, 2), 
      `Weight Growth Rate g per Year`= round((Weight - previousWeight)/yearsSince, 2)
    )
  return(DFforGrowthRates)
}
