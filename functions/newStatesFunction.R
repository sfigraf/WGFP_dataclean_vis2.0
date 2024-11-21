#this comes from runscript after spatial join detections stations function
detectionsWithStates <- DailyDetectionsStationsStates$spatialList$stationData
timePeriods <- wgfpMetadata$TimePeriods
#release and recaps
recapsAndRelease <- as.data.frame(detectionsWithStates) %>%
  filter(Event %in% c("Recapture", "Recapture and Release", "Release", "Recapture "), 
         !Species %in% "TGM")

# x45 <- x4 %>%
#   left_join(recapsAndRelease[, c("TAG", "Datetime", "Release_Length", "Release_Weight", "Recap_Length", "Recap_Weight")], 
#             by = c("TAG", "Datetime"))
#need to add the datetime to make sure the <= and >= filtering later is interpreting correctly
timePeriodsCorrect <- timePeriods %>%
  mutate(`start date` = janitor::excel_numeric_to_date(as.numeric(`start date`)), 
         `end date` = ymd_hms(paste(janitor::excel_numeric_to_date(as.numeric(`end date`)), "23:59:59"))
  )

# Perform the join and filtering
#no muskie
df1_with_period <- as.data.frame(detectionsWithStates) %>%
  filter(Species != "TGM") %>%
  rowwise() %>%
  mutate(
    TimePeriod = timePeriodsCorrect %>%
      filter(Datetime >= `start date` & Datetime <= `end date`) %>%
      pull(periods) %>%
      first()
  )

x1 <- df1_with_period %>%
  #filter(TAG %in% c("230000143362", "230000142543")) %>%
  select(TAG, Datetime, Event, TimePeriod, State) #%>%
# group_by(TAG, TimePeriod) %>%
# mutate(
#   # Identify groups around "Recapture"
#   recapture_found = str_trim(Event) == "Recapture",
#   group_id = cumsum(recapture_found)
# )
# 
x2 <- x1 %>%
  ungroup() %>%
  group_by(TAG) %>%
  mutate(Event = str_trim(Event)) %>%
  # bind_rows(x1 %>%
  #            filter(str_trim(Event) == "Recapture")) %>%
  arrange(TAG, Datetime) 

x3 <- x2 %>%
  
  mutate(
    is_recapture = ifelse(str_trim(Event) == "Recapture", 1, 0),
    group = cumsum(lag(is_recapture, default = 0)) + 1
  ) 

x4 <- x3 %>%
  # left_join(cleanedRelease[,c("TAG", "DateTime", "Length", "Weight")] %>%
  #             rename(Release_Length = Length,
  #                    Release_Weight = Weight), by = c("TAG", "Datetime" = "DateTime")) %>%
  # left_join(cleanedRecaptures[,c("TAG", "DateTime", "Recap_Length", "Recap_Weight")], by = c("TAG", "Datetime" = "DateTime")) %>%
  bind_rows(x3 %>%
              filter(str_trim(Event) == "Recapture") %>%
              mutate(group = group + 1)
  ) %>%
  arrange(TAG, Datetime)
  
  # left_join(recapsAndRelease[, c("TAG", "Datetime", "Event", "Release_Length", "Release_Weight", "Recap_Length", "Recap_Weight")], 
  #           by = c("TAG", "Datetime", "Event"))
# x45 <- x4 %>%
#   mutate(Length = coalesce(Release_Length, Recap_Length), 
#          Weight = coalesce(Release_Weight,Recap_Weight)) %>%
#   select(-c(Release_Length, Release_Weight, Recap_Length, Recap_Weight))

  
x5 <- x4 %>%
  group_by(TAG, TimePeriod, group) %>%
  summarize(condensedStates = gsub('([[:alpha:]])\\1+', '\\1', paste(State, collapse = ""))
  ) %>%
  mutate(newState = str_sub(condensedStates,-1,-1)) %>%
  select(-condensedStates) 

x6 <- x5 %>%
  arrange(as.numeric(TimePeriod)) %>%
  pivot_wider(names_from = TimePeriod, values_from = newState) %>%
  group_by(TAG) %>%
  mutate(number1 = ifelse(group == max(group), 1, -1))

recapsAndReleaseWithGroup <- recapsAndRelease %>%
  group_by(TAG) %>%
  arrange(Datetime) %>%
  mutate(group = row_number(), 
         RBT = ifelse(Species == "RBT", 1, 0),
         LOC = ifelse(Species == "LOC", 1, 0), 
         MTS = ifelse(Species == "MTS", 1, 0))

x7 <- x6 %>%
  #left_join(x45[,c("TAG", "group", "")])
  #adding length and weight columns on
  #coalescing recap dat first so that will take priority
  left_join(recapsAndReleaseWithGroup[,c("TAG", "group", "Release_Length", "Release_Weight", "Recap_Length", "Recap_Weight", "RBT", "LOC", "MTS")] %>%
              mutate(Length = coalesce(Recap_Length, Release_Length),
              Weight = coalesce(Recap_Weight, Release_Weight)), by = c("TAG", "group")) %>%
  select(-c(Release_Length, Release_Weight, Recap_Length, Recap_Weight))
#### need ghost tags/avian predation now
