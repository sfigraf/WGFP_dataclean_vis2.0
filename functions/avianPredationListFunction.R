avianPredationListFunction <- function(All_Events = AllCombinedEvents$df_list$All_Events, 
                                       AvianPredation = AvianPredation, enc_hist_wide_listPossibleAvianPredation = enc_hist_wide_list$enc_wide_list$possibleAvianPredation, 
                                       encounterMARKStates = encounterMARKStates, 
                                       Movements_list = Movements_list, ghostdata = GhostTags, 
                                       checkedTags = checkedTags) {
  ### Avian Predation: grouping together different methods of finding avian predated fish
  
  #takes out avian predated tags, this is used for sequences
  allEventsAvianPredationFiltered <-  All_Events %>%
    dplyr::filter(!TAG %in% unique(AvianPredation$TagID)
    )
  
  movingDownstream <- getSequences(allEventsAvianPredationFiltered, c("CF", "GD1", "RR1"), "", c("HP", "RB", "WG1", "WG2"), c("M1", "M2"))
  movingUpstream <- getSequences(allEventsAvianPredationFiltered, c("HP", "RB", "WG1", "WG2") , "", c("CF", "GD1", "RR1"), c("M1", "M2"))
  
  
  avianPredationList <- list(
    "movingDownstream" = movingDownstream, 
    "movingUpstream" = movingUpstream, 
    "largeMovementsWithoutChannel" = enc_hist_wide_listPossibleAvianPredation, 
    "statesWeeklyActiveFish" = encounterMARKStates$possibleAvianPredation$weeklyActiveFish, 
    "statesAllActiveFish" = encounterMARKStates$possibleAvianPredation$overAllActiveFishNotinWeeklyDF, 
    "fastMovements" = Movements_list$avianPredationDFS$fastMovements,
    "longMovements" = Movements_list$avianPredationDFS$longMovements
  )
  #removing all tags already discerned as avian predation from each df
  avianPredationList <- purrr::map(avianPredationList, ~anti_join(.x, AvianPredation, by = c("TAG" = "TagID")))
  
  ###making df of frequency counts for each time a fish appears on a df to get "priority" list
  tagCountsNoPredation <- purrr::map_dfr(avianPredationList, ~ {
    # Count occurrences within this dataframe
    .x %>%
      count(TAG)
  }) %>%
    # Summarize counts across all dataframes
    group_by(TAG) %>%
    summarize(Frequency = sum(n), .groups = 'drop') %>%
    arrange(desc(Frequency))
  
  avianPredationList[["tagCountsNoPredation"]] = tagCountsNoPredation
  #adding checked tags to the list
  
  checkedTags$TAG <- as.character(checkedTags$TAG)
  
  ##filter out MERG tags
  #get tag data first bc no species field in avian predation dfs
  taggedBirdEventsOnly = AllCombinedEvents$df_list$All_Events %>%
    #add more bird species if that arises
    filter(Species %in% c("MERG"))
  taggedBirds <- unique(taggedBirdEventsOnly$TAG)
  
  
  avianPredationList <- purrr::map(avianPredationList, ~{
    originalColumns <- names(.x)
    outputDatWithCheckedTags <- left_join(.x, checkedTags, by = "TAG")
    outputDatWithRowColor <- outputDatWithCheckedTags %>%
      dplyr::filter(!TAG %in% taggedBirds) %>%
      mutate(rowColor = case_when(Opinion %in% c("Yes", "yes") ~ "red", 
                                  Opinion %in% c("No", "no") ~ "green", 
                                  !is.na(Opinion) ~ "yellow", 
                                  is.na(Opinion) ~ "none"
      )) %>%
      select(c(all_of(originalColumns), rowColor))
    return(outputDatWithRowColor)
  }
  )
  
  ##gets df of tags that have moved/have detections over a month after their predation date
  #qaqc check to make sure we didn't mislabel a predated tag
  #some are mobile detections that have just been pooped out so if they're already in ghost tag folder, shade green
  avianPredatedTagsMovementsMonthAfterPD <- Movements_list$dailyMovementsTable1 %>%
    filter(TAG %in% AvianPredation$TagID) %>%
    left_join(AvianPredation[,c("TagID", "PredationDate")], by = c("TAG" = "TagID")) %>%
    group_by(TAG) %>%
    filter(Datetime >= (PredationDate +months(1))) %>%
    left_join(ghostdata[,c("TagID", "Event")], by = c("TAG" = "TagID")) %>%
    rename(InGhostTagDF = Event) %>%
    relocate(TAG, PredationDate, Date, Datetime, InGhostTagDF) %>%
    #if the tag is already in the ghost tag folder, shade green
    mutate(rowColor = case_when(InGhostTagDF %in% c("Ghost") ~ "green", 
                                TRUE ~ "none")
    ) 
  
  avianPredationList[["avianPredatedTagsMovementsMonthAfterPD"]] = avianPredatedTagsMovementsMonthAfterPD
  
  return(avianPredationList)
}