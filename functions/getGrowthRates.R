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
  
  # Recaptures = Recaptures %>%
  #   select(-`X.2`)
  #align columns in preparation for binding
  # print("startt of function")
  # print(names(Release))
  RecapturesforBind <- alignColumns(Recaptures, names(Release), Release) %>%
    left_join(Recaptures[,c("TagID", "Length", "Weight", "RecaptureSite")], by = c("TagID", "Length", "Weight"))
  
  ReleaseforBind <- alignColumns(Release, names(RecapturesforBind), RecapturesforBind)
  
  ReleaseRecaps <- bind_rows(ReleaseforBind, RecapturesforBind)

  GrowthRatesDF <- ReleaseRecaps %>%
    mutate(Date = lubridate::ymd(Date)) %>%
    group_by(TagID) %>%
    arrange(Date, .by_group = TRUE) %>%
    #use 52.25 weeks to account for leap years
    mutate(yearsSince = as.numeric(difftime(Date, lag(Date), units = "weeks"))/52.25, 
           daysSince = as.numeric(difftime(Date, lag(Date), units = "days")), 
           previousLength = lag(Length), 
           previousWeight = lag(Weight),
           #), 
           `Length Growth Rate mm per Year`= round((Length - previousLength)/yearsSince, 2), 
           `Weight Growth Rate g per Year`= round((Weight - previousWeight)/yearsSince, 2)
    )
  return(GrowthRatesDF)
}
