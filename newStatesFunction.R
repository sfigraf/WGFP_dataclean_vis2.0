#TimePeriods <- wgfpMetadata$TimePeriods
#this comes from runscript after spatial join detections stations function
#DailyDetectionsStationsStates <- DailyDetectionsStationsStates$spatialList$stationData
#encounterDF <- createMARKEncounterHistories(DailyDetectionsStationsStates, GhostTags, AvianPredation, TimePeriods)

library(janitor)
createMARKEncounterHistories <- function(DailyDetectionsStationsStates, GhostTags, AvianPredation, TimePeriods){
  
  start_time <- Sys.time()
  startMessage <- "Running createMARKEncounterHistories: Taking events, ghost/predation, and time periods and creating MARK ready dataframe."
  print(startMessage)
  
  #don't need TGM in analysis
  detectionsWithStates <- as.data.frame(DailyDetectionsStationsStates) %>%
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
    filter(Event %in% c("Recapture", "Recapture and Release", "Release", "Recapture "))

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
    )
  
  #select pertinent columns
  eventsWithPeriodsSelect <- eventsWithPeriods %>%
    select(TAG, Datetime, Event, TimePeriod, State) 
  
  #makes Identifier (group) for TAGs based on number of times they've been recapped
  groupedRecapEventsByTag <- eventsWithPeriodsSelect %>%
    group_by(TAG) %>%
    mutate(
      isRecapture = ifelse(str_trim(Event) == "Recapture", 1, 0),
      group = cumsum(lag(isRecapture, default = 0)) + 1
    ) 
  
  #adds another row for recapture with a new group so that later the new recap will start the new line of data
  additionalRecapInstance <- groupedRecapEventsByTag %>%
    bind_rows(x3 %>%
                filter(str_trim(Event) == "Recapture") %>%
                mutate(group = group + 1)
    ) %>%
    arrange(TAG, Datetime)

  
  #grabs the last state the tag appeared in for that time period
  lastStateInTimePeriod <- additionalRecapInstance %>%
    group_by(TAG, TimePeriod, group) %>%
    summarize(condensedStates = gsub('([[:alpha:]])\\1+', '\\1', paste(State, collapse = ""))
    ) %>%
    mutate(newState = str_sub(condensedStates,-1,-1)) %>%
    select(-condensedStates) 
  
  #puts the data to wide format and assigns the number based on if the Tag's history ended or not (group identifier)
  tagsEventsWideFormatWithCount <- lastStateInTimePeriod %>% 
    arrange(as.numeric(TimePeriod)) %>% 
    pivot_wider(names_from = TimePeriod, values_from = newState) %>% 
    group_by(TAG) %>% 
    mutate(Count = ifelse(group == max(group), 1, -1)) 
  
  #modifies the data checking for state "G", if the fish has a G in the history its automatically a -1
  tagsEventsWideWithGhost <- tagsEventsWideFormatWithCount %>%
    rowwise() %>%
    mutate(Count = case_when(any(c_across(matches("^[0-9]+$")) == "G") ~ -1, 
                               TRUE ~ Count)
    )
  
  #getting recap and release info ready to join with MARK data; 
  # creating the same identifier (group) to help join
  recapsAndReleaseWithGroup <- recapsAndRelease %>%
    group_by(TAG) %>%
    arrange(Datetime) %>%
    mutate(group = row_number(), 
           RBT = ifelse(Species == "RBT", 1, 0),
           LOC = ifelse(Species == "LOC", 1, 0), 
           MTS = ifelse(Species == "MTS", 1, 0))
  
  #joining by TAG and group, merging all LEgnth/weight columns and deselecting redundant ones
  tagsEventsWideLW <- tagsEventsWideWithGhost %>%
    #adding length and weight columns
    #coalescing recap dat first so that will take priority
    left_join(recapsAndReleaseWithGroup[,c("TAG", "group", "Release_Length", "Release_Weight", "Recap_Length", "Recap_Weight", "RBT", "LOC", "MTS")] %>%
                mutate(Length = coalesce(Recap_Length, Release_Length),
                       Weight = coalesce(Recap_Weight, Release_Weight)), by = c("TAG", "group")) %>%
    select(-c(Release_Length, Release_Weight, Recap_Length, Recap_Weight))
  
  ## replace NA with 0
  tagsEventsWide0s <- tagsEventsWideLW %>%
    rowwise() %>%
    mutate(across(matches("^[0-9]+$"), ~ replace_na(.x, "0")))
  

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
  colsToMov <- c("Count", "Length", "Weight", "RBT", "LOC", "MTS")
  # Reshape back to wide format with new column names
  tagsEventsWideCorrectTpLabels <- tagsEventsLongwithTpDates %>%
    select(-`start date`, -`end date`, -periods) %>%
    pivot_wider(names_from = NewColumnName, values_from = "State") %>%
    select(-all_of(colsToMov), all_of(colsToMov))
  
  end_time <- Sys.time()
  endMessage <- paste("createMARKEncounterHistories took", round(difftime(end_time, start_time, units = "mins"),2), "minutes.")
  print(endMessage)
  return(list("MARKEncounterHistories" = tagsEventsWideCorrectTpLabels, 
              "endMessage" = paste(c(startMessage, endMessage), collapse = "<br>")))
}


