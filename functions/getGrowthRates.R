# Recaptures = indiv_datasets_list$recapdata# %>%
#   #select(-`X.2`)
# Release <- indiv_datasets_list$releasedata



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
  print("middle of function")
  
  GrowthRatesDF <- ReleaseRecaps %>%
    mutate(Date = lubridate::ymd(Date)) %>%
    group_by(TagID) %>%
    arrange(Date, .by_group = TRUE) %>%
    #use 52.25 weeks to account for leap years
    mutate(yearsSince = as.numeric(difftime(Date, lag(Date), units = "weeks"))/52.25, 
           `Length Growth Rate mm per Year`= round((Length - lag(Length))/yearsSince, 2), 
           `Weight Growth Rate g per Year`= round((Weight - lag(Weight))/yearsSince, 2)
    )
  return(GrowthRatesDF)
}
