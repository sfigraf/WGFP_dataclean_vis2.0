# TimePeriods <- wgfpMetadata$TimePeriods 
# 
# # ### this comes from runscript after spatial join detections stations function
# DailyDetectionsStationsStates1 <- DailyDetectionsStationsStates$spatialList$stationData
# encounterDF <- createMARKEncounterHistories(DailyDetectionsStationsStates1, GhostTags, AvianPredation, TimePeriods)
# x <- encounterDF$MARKEncounterHistories
library(janitor)
createMARKEncounterHistories <- function(DailyDetectionsStationsStates1, GhostTags, AvianPredation, TimePeriods){
  
  start_time <- Sys.time()
  startMessage <- "Running createMARKEncounterHistories: Taking events, ghost/predation, and time periods and creating MARK ready dataframe."
  print(startMessage)
  
  #don't need TGM in analysis
  detectionsWithStates <- as.data.frame(DailyDetectionsStationsStates1) %>%
    filter(Species != "TGM")
  #getting most pertinent info
  GhostTagsForJoining <- GhostTags %>%
    rename(TAG = TagID) %>%
    select(TAG, GhostDate) 
  
  AvianPredationForJoining <- AvianPredation %>%
    rename(TAG = TagID) %>%
    select(TAG, PredationDate)
  
  #joining with ghost tag and predation dfs
  eventsWithGhostDates <- left_join(detectionsWithStates, GhostTagsForJoining, by = c("TAG"))
  eventsWithGhostDatesAndAvianPredation <- left_join(eventsWithGhostDates, AvianPredationForJoining, by = c("TAG")) 
  
  #combining ghost and predation just to 1 state "G"
  #if it's predated and ghost, predated will always come first
  eventsWithGhostDatesAndAvianPredation <- eventsWithGhostDatesAndAvianPredation %>%
    mutate(GhostOrPredationDate = coalesce(PredationDate, GhostDate), 
           State = case_when(Date >= GhostOrPredationDate ~ "G", 
                             TRUE ~ State)
    ) %>%
    #getting rid of data for tags after their ghost/predation date
    #makes sure to keep non-predated tags if they don'thave a ghost/predation date
    filter(is.na(GhostOrPredationDate) |
             Date <= GhostOrPredationDate)
  # we don't need duplicated rows of states and tags that fall on the same day. 

  #getting rid of same-day ghost data, keeping just the first detection that day that qualifies as ghost/predation
  eventswithOneGhostEvent <- eventsWithGhostDatesAndAvianPredation %>%
    arrange(TAG, Datetime) %>%
    group_by(TAG, State) %>%
    mutate(firstDatetime = dplyr::if_else(!is.na(GhostOrPredationDate) & State == "G", first(Datetime), NA)) %>% #first(Datetime)) %>%
    relocate(TAG, Datetime, GhostOrPredationDate, firstDatetime) %>%
    filter(is.na(GhostOrPredationDate) | is.na(firstDatetime) |
             Datetime == firstDatetime)
  
  
  #getting start/end dates to correct format
  timePeriodsClean <- TimePeriods %>%
    mutate(`start date` = janitor::excel_numeric_to_date(as.numeric(`start date`)), 
           `end date` = janitor::excel_numeric_to_date(as.numeric(`end date`))
    )
  #release and recaps only
  recapsAndRelease <- detectionsWithStates %>%
    filter(Event %in% c("Recapture", "Recapture and Release", "Release", "Recapture ")) %>%
    mutate(NeedsLogWeight = ifelse((Release_Length > 0 & Release_Weight == 0) | (Event %in% c("Recapture") & is.na(Recap_Weight)), TRUE, FALSE))

  #need to add the datetime to make sure the <= and >= filtering later is interpreted correctly
  timePeriodsCorrect <- timePeriodsClean %>%
    mutate(`end date` = ymd_hms(paste(`end date`, "23:59:59")))
  
  # Gets DF with row of time periods based on when the detection was
  eventsWithPeriods <- eventswithOneGhostEvent  %>%
    rowwise() %>%
    mutate(
      TimePeriod = timePeriodsCorrect %>%
        filter(Datetime >= `start date` & Datetime <= `end date`) %>%
        pull(periods) %>%
        first()
    ) %>%
    ungroup() #stops rowwise()
  #for qaqc: this should be a blank df, otherwise it means some data don't fall within the time periods sepecified in the csv
  dataWithoutPeriods <- eventsWithPeriods %>%
    filter(is.na(TimePeriod))
  
  #select pertinent columns
  eventsWithPeriodsSelect <- eventsWithPeriods %>%
    select(TAG, Datetime, Event, TimePeriod, State) 
  
  ####There are some fish that were recaptured in the same time period they were released. 
  #for these fish, we just want their second event (the recapture) as their "Release" event.
  #this code finds fish where this is the case and removes rows previous to the "Recapture" event
  
  eventsReleaseRecapSameTimePeriodCorrection <- eventsWithPeriodsSelect %>%
    group_by(TAG, TimePeriod) %>%
    arrange(Datetime) %>%
    mutate(releasedAndRecappedInSamePeriod = any(Event %in% c("Release", "Recapture and Release")) & any(Event %in% c("Recapture"))#, 
           #recapTime = as_datetime(ifelse(Event == "Recapture" & releasedAndRecappedInSamePeriod == TRUE, Datetime, NA))
           ) %>%
    filter(!(releasedAndRecappedInSamePeriod & row_number() < max(which(Event == "Recapture"), default = 0))) %>%
    #this changes recap Event to Release when there is a release/recap in the same time period
    mutate(Event = ifelse(Event == "Recapture" & releasedAndRecappedInSamePeriod, "Release", Event))
    # tidyr::fill(recapTime, .direction = "downup") %>%
  #   # filter(is.na(recapTime) | Datetime >= recapTime)
  # y <- setdiff(x, originalX[,-which(names(originalX) == "recapTime")])
  
  # Now getting times where a fish was recapped twice in the same period
  multipleRecapInstancesSamePeriod <- eventsReleaseRecapSameTimePeriodCorrection %>%
    filter(Event == "Recapture") %>%
    group_by(TAG, TimePeriod) %>%
    count(name = "recapsInSinglePeriod") %>%
    filter(recapsInSinglePeriod > 1) %>%
    mutate(multipleRecapsInPeriod = TRUE) %>%
    select(-recapsInSinglePeriod)
  
  #joins to get instances of multiple "Recaptures" in the same period
  eventsWithRecapReleaseSameTimePeriodCorrection <- eventsReleaseRecapSameTimePeriodCorrection %>%
    left_join(multipleRecapInstancesSamePeriod, by = c("TAG", "TimePeriod")) %>%
    group_by(TAG, TimePeriod) %>%
    arrange(Datetime) %>%
    #removes rows where there are 2 instances of recaps in the same period
    #it's already arranged by datetime, so it finds where the max (most recent) recap is within that grouping of TAG and timePeriod, 
    # and removes instances of Recap that have row numbers beofre the max recap instance within that time period
    #using default = 0 makes sure there's a value when there's no recap events. Speeds up the code, ensures there's no warnings.
    filter(!(Event == "Recapture" & row_number() < max(which(Event == "Recapture"), default = 0))) %>%
    ungroup()

    
  ### makes Identifier (group) for TAGs based on number of times they've been recapped
  groupedRecapEventsByTag <- eventsWithRecapReleaseSameTimePeriodCorrection %>%
    group_by(TAG) %>%
    mutate(
      isRecapture = ifelse(str_trim(Event) == "Recapture", 1, 0),
      group = cumsum(lag(isRecapture, default = 0)) + 1
    )
  
  
  
  #####
  
  #230000143396

  #adds another row for recapture with a new group so that later the new recap will start the new line of data
  additionalRecapInstance <- groupedRecapEventsByTag %>%
    bind_rows(groupedRecapEventsByTag %>%
                filter(str_trim(Event) == "Recapture") %>%
                mutate(group = group + 1)
    ) %>%
    arrange(TAG, Datetime)
  
  # ghost state cannot occur the same state as a fish is released. So this code adds 1 period where the ghost state occurs as the same time period as the release. 

  releaseGhostCorrection <- additionalRecapInstance %>%
    mutate(TimePeriod = as.numeric(TimePeriod)) %>%
    group_by(TAG, TimePeriod) %>%
    arrange(Datetime) %>%
    mutate(releaseAndGhostSamePeriod = State == "G" & any(Event %in% c("Release", "Recapture and Release")),
      TimePeriod = ifelse(releaseAndGhostSamePeriod, TimePeriod + 1, TimePeriod))
  
  #grabs the last state the tag appeared in for that time period
  lastStateInTimePeriod <- releaseGhostCorrection %>%
    group_by(TAG, TimePeriod, group) %>%
    summarize(condensedStates = gsub('([[:alpha:]])\\1+', '\\1', paste(State, collapse = ""))
              #releaseAndGhostSamePeriod = releaseAndGhostSamePeriod
              # releasedAndRecappedInSamePeriod = releasedAndRecappedInSamePeriod,
              # multipleRecapsInPeriod = multipleRecapsInPeriod 
              
    ) %>%
    mutate(newState = str_sub(condensedStates,-1,-1)) %>%
    select(-condensedStates) 
  
  #puts the data to wide format and assigns the number based on if the Tag's history ended or not (group identifier)
  tagsEventsWideFormatWithCount <- lastStateInTimePeriod %>% 
    group_by(TAG, TimePeriod, group) %>%
    arrange(as.numeric(TimePeriod)) %>% 
    pivot_wider(names_from = TimePeriod, values_from = newState) %>% 
    group_by(TAG) %>% 
    mutate(Count = ifelse(group == max(group), 1, -1)) 
  
  #modifies the data checking for state "G", if the fish has a G in the history its automatically a -1
  tagsEventsWideWithGhost <- tagsEventsWideFormatWithCount %>%
    rowwise() %>%
    mutate(Count = case_when(any(c_across(matches("^[0-9]+$")) == "G") ~ -1, 
                               TRUE ~ Count)
    ) %>%
    ungroup() #stops rowwise()
  
  ###### 
  QAQCColumnsDF <- releaseGhostCorrection %>%
    # filter(releaseAndGhostSamePeriod ==T |
    #   releasedAndRecappedInSamePeriod==T |
    #   multipleRecapsInPeriod ==T) %>%
    group_by(TAG) %>%
    summarize(releaseAndGhostSamePeriod = any(releaseAndGhostSamePeriod), 
              releasedAndRecappedInSamePeriod = any(releasedAndRecappedInSamePeriod),
              multipleRecapsInPeriod = any(multipleRecapsInPeriod)) %>%
    
     mutate(multipleRecapsInPeriod = replace_na(multipleRecapsInPeriod, FALSE) )
  
  
  #getting recap and release info ready to join with MARK data; 
  # using the same identifier (group) to help join
  #can't just create new group() because group number has already been corrected for in groupedRecapEventsByTag (release/recaps in same time period)
  #but there hasn't been additoinal recap instance added yet in groupedRecapEventsByTag so we should get that we need
  
  recapsAndReleaseWithGroup <- recapsAndRelease %>%
    
    mutate( 
           RBT = ifelse(Species == "RBT", 1, 0),
           LOC = ifelse(Species == "LOC", 1, 0), 
           MTS = ifelse(Species == "MTS", 1, 0)) %>%
    #first, filter out which recaps/release we don't want based off group assinings from earlier
    left_join(groupedRecapEventsByTag[,c("TAG", "Datetime", "group")], by = c("TAG", "Datetime")) %>%
    filter(!is.na(group)) %>%
    #then make new group numbers
    group_by(TAG) %>%
    arrange(Datetime) %>%
    mutate(group = row_number())
    # distinct(.keep_all = TRUE)
  # #can't join by event because 2nd recap events have already been changed to "Release" now
  # recapsAndReleaseWithGroup <- recapsAndReleaseWithGroup %>%
    
  
  #joining by TAG and group, merging all LEgnth/weight columns and deselecting redundant ones
  tagsEventsWideLW <- tagsEventsWideWithGhost %>%
    #adding length and weight columns
    #coalescing recap dat first so that will take priority
    left_join(recapsAndReleaseWithGroup[,c("TAG", "group", "Release_Length", "Release_Weight", "Recap_Length", "Recap_Weight", "RBT", "LOC", "MTS", "NeedsLogWeight")] %>%
                mutate(Length = coalesce(Recap_Length, Release_Length),
                       Weight = ifelse(NeedsLogWeight, NA, coalesce(Recap_Weight, Release_Weight))), 
              by = c("TAG", "group")) %>%
    select(-c(Release_Length, Release_Weight, Recap_Length, Recap_Weight))
  
  ## replace NA with 0
  tagsEventsWide0s <- tagsEventsWideLW %>%
    rowwise() %>%
    mutate(across(matches("^[0-9]+$"), ~ replace_na(.x, "0"))) %>%
    ungroup() #stops rowwise()
  

  ### getting the column names to the time periods described
  # Reshape the wide df to long format
  tagsEventsLong <- tagsEventsWide0s %>%
    pivot_longer(matches("^[0-9]+$"), names_to = "periods", values_to = "State") %>%
    mutate(periods = as.character(periods))  # Convert Number to integer for matching
  
  # Join with the time periods dataframe to get the date range for each Number
  tagsEventsLongwithTpDates <- tagsEventsLong %>%
    left_join(timePeriodsClean[,c("periods", "start date", "end date")], by = "periods") %>%
    mutate(NewColumnName = paste(`start date`, "to", `end date`))
  
  #getting columns in desired order
  colsToMov <- c("Count", "Length", "Weight", "RBT", "LOC", "MTS", "NeedsLogWeight")
  # Reshape back to wide format with new column names
  # this df will not be the same number of rows as recaps and releases because some fish were released/recapped in the same time periods, 
  # so for those fish, the most recent instance is taken as that recap/release
  tagsEventsWideCorrectTpLabels <- tagsEventsLongwithTpDates %>%
    select(-`start date`, -`end date`, -periods) %>%
    pivot_wider(names_from = NewColumnName, values_from = "State") %>%
    select(-all_of(colsToMov), all_of(colsToMov))
  
  #adding on QAQC columns 
  # this is the final DF
  tagsEventsWideCorrectTpLabelswithQAQC <- tagsEventsWideCorrectTpLabels %>%
    left_join(QAQCColumnsDF, by = "TAG") 
  
  ##### Potential Avian Predation 
  DailyMovements_withStationsWeeksSince <- as.data.frame(DailyDetectionsStationsStates1) %>%
    ungroup() %>%
    mutate(
      #makes sense to use floor not ceiling with weeks because then there are are more fish in week 0
      # if you want to start at week 1 instead of week 0, add +1 to the end of expression
      # when you change this too, it changes the number of entries in the states dataframe
      weeks_since = as.numeric(floor(difftime(Datetime, min(Datetime), units = "weeks")))
    )
  weeklyStates <- DailyMovements_withStationsWeeksSince %>% 
    group_by(weeks_since, TAG) %>%
    arrange(Datetime) %>%
    mutate(
      allWeeklyStates = paste(State, collapse = ""),
      condensedWeeklyStates = gsub('([[:alpha:]])\\1+', '\\1', allWeeklyStates), #removes consecutive letters
      weekly_unique_events = length(unique(Event))
    )
  #this is now a weekly chart
  cleanedWeeklyStates <- weeklyStates %>%
    distinct(weeks_since, TAG, condensedWeeklyStates, .keep_all = TRUE) %>%
    select(-State) %>%
    rename(State = condensedWeeklyStates)
  
  weeklyActiveFish <- cleanedWeeklyStates %>%
    filter(weekly_unique_events > 4 | str_length(State) > 6) %>%
    select(TAG, Date, weeks_since, State, allWeeklyStates, weekly_unique_events)
  #used for avian predation and also in encounter histories summary wide
  #we want to keep TGM here so using original DF
  summarizedStates <- as.data.frame(DailyDetectionsStationsStates1) %>%
    group_by(TAG) %>%
    arrange(Datetime) %>%
    mutate(allStates = paste(State, collapse = ""),
           condensedAllStates = gsub('([[:alpha:]])\\1+', '\\1', allStates), #removes consecutive letters
           channelSummary = case_when(str_detect(condensedAllStates, "D") ~ "Used Connectivity Channel", 
                                      TRUE ~ "Didn't Use Channel"), 
           
           #new columns to say if fish stayed above or below?
           went_above_dam_noChannel = str_detect(condensedAllStates, "CE|BE|AE|CF|BF|AF|CH|BH|AH"),
           went_below_dam_noChannel = str_detect(condensedAllStates, "EC|EB|EA|FC|FB|FA|HC|HB|HA"),
           went_below_dam_throughChannel = str_detect(condensedAllStates, "EDC|EDB|EDA|FDC|FDB|FDA|HDC|HDB|HDA"),
           went_above_dam_throughChannel = str_detect(condensedAllStates, "CDE|BDE|ADE|CDF|BDF|ADF|CDH|BDH|DAH"),
           entered_channel_from_DS = str_detect(condensedAllStates, "AD|BD|CD"),
           entered_channel_from_US = str_detect(condensedAllStates, "ED|FD|HD"),
           
    ) %>%
    select(TAG, condensedAllStates, channelSummary, went_above_dam_noChannel, went_below_dam_noChannel,went_below_dam_throughChannel,went_above_dam_throughChannel,entered_channel_from_DS,entered_channel_from_US) %>%
    distinct(TAG, .keep_all = TRUE)
  
  overAllActiveFish <- summarizedStates %>%
    filter(str_length(condensedAllStates) >6)
  
  overAllActiveFishNotinWeeklyDF <- anti_join(overAllActiveFish, weeklyActiveFish, by = "TAG")
  possibleAvianPredation <- list(
    "weeklyActiveFish" = weeklyActiveFish,
    "overAllActiveFishNotinWeeklyDF" = overAllActiveFishNotinWeeklyDF
  )
  
  end_time <- Sys.time()
  endMessage <- paste("createMARKEncounterHistories took", round(difftime(end_time, start_time, units = "mins"),2), "minutes.")
  print(endMessage)
  return(list("MARKEncounterHistories" = tagsEventsWideCorrectTpLabelswithQAQC,
              "States_summarized" = summarizedStates,
              "possibleAvianPredation" = possibleAvianPredation,
              "endMessage" = paste(c(startMessage, endMessage), collapse = "<br>"), 
              "timePeriodsMessage" = ifelse(nrow(dataWithoutPeriods) > 0, paste0("When assigning states for program MARK, some data don't have a time period assigned to them, date ranges ", as.Date(min(dataWithoutPeriods$Datetime)), " to ", as.Date(max(dataWithoutPeriods$Datetime)),". Add or adjust time periods in the csv."), "")
              )
         )
}


