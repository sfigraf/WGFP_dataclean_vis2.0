#### ENC HIST summary table function
# allDetectionsAndRecaptures <- df_list$Recaps_detections
# Release <- Release
# combined_events_stations <- combined_events_stations #resulting df from combined_events and stations function
# States_summarized <- states_data_list$States_summarized
#recaps and all detreitons comes from WGFP ENC_hist_function, release data is a read_in csv, all_events_condensed with stations comes from combine_stations_events function
Ind_tag_enc_hist_wide_summary_function <- function(allDetectionsAndRecaptures, Release, combined_events_stations, States_summarized, markerTags){
  
  start_time <- Sys.time()
  print("Running Ind_tag_enc_hist_wide_summary_function: Summarizes detection and movement data from each released Tag.")
  
  all_enc12 <- allDetectionsAndRecaptures %>%
    count(TAG, Event, name = "Encounters") 
  
  all_enc12wide  <- all_enc12 %>%
    pivot_wider(id_cols = TAG, names_from = Event, values_from = Encounters) %>%
    rename_with(~ paste0(., "_n"), -TAG) 
  
  #ordering correctly for calculations later on
  columnOrder <- c(RedBarnCodes, HitchingPostCodes, ConfluenceCodes, ConnectivityChannelDownstreamCodes, ConnectivityChannelSideChannelCodes, ConnectivityChannelUpstreamCodes, 
                   MobileRunCodes, WindyGapAntennaSiteCode, KaibabParkAntennaSiteCode, RiverRunAntennaSiteCode, FraserRiverCanyonAntennaSiteCode)
  all_enc12wideOrdered <- all_enc12wide %>%
    select(TAG, one_of(paste0(columnOrder, "_n")), Recapture_n)
    #select(TAG, RB1_n,RB2_n,HP3_n, HP4_n, CF5_n, CF6_n, CD1_n, CD2_n, CS1_n, CS2_n, CU1_n, CU2_n, M1_n, M2_n, B3_n, B4_n, B5_n, B6_n, Recap_n)
    
  #    
  # x <- all_enc12 %>%
  #   replace_na(list(Species = "No Info", ReleaseSite = "No Info"))
  # 
  # ENC_ALL <- all_enc12 %>%
  #   rename(RB1_n = RB1,
  #          RB2_n = RB2,
  #          HP3_n = HP3,
  #          HP4_n = HP4,
  #          CF5_n = CF5,
  #          CF6_n = CF6,
  #          CD1_n = CD1,
  #          CD2_n = CD2,
  #          CS1_n = CS1,
  #          CS2_n = CS2,
  #          CU1_n = CU1,
  #          CU2_n = CU2,
  #          M1_n = M1,
  #          M2_n = M2,
  #          B3_n = B3,
  #          B4_n = B4,
  #          B5_n = B5,
  #          B6_n = B6,
  #          Recap_n = Recapture
  #   ) %>%
  #   select(TAG, RB1_n,RB2_n,HP3_n, HP4_n, CF5_n, CF6_n, CD1_n, CD2_n, CS1_n, CS2_n, CU1_n, CU2_n, M1_n, M2_n, B3_n, B4_n, B5_n, B6_n, Recap_n)
  # 
  
  #### Combine Release data ###
  Release1 <- Release %>%
    mutate(TAG = str_trim(TagID)) %>%
    replace_na(list(Species = "No Info", ReleaseSite = "No Info")) #replaced species and releasesite to follow the same convention as AllEvents
  
  # was geting a massive dataframe because the Release df is called TAGid not TAG.
  #need to actually join on full join not merge
  ENC_Release <- full_join(Release1, all_enc12wideOrdered,  by = "TAG")
  
  #gets tag list that wasn't in release file or markerTags
  unknown_tags <- ENC_Release %>%
    filter(is.na(ReleaseSite), 
           !TAG %in% markerTags) %>%
    select(TAG,where(is.numeric))
  
  ENC_Release[is.na(ENC_Release)] = 0 #gets rest of the number count columns to 0 from NA
  
  ENC_Release1 <- ENC_Release %>%
    #applies the mutation logic to all columns that end with "_n". 
    #It checks if each value is greater than 0 
    #creates a new column with the name obtained by removing "_n" from the original column name.
    mutate(across(ends_with("_n"), ~ (. > 0), .names = "{sub('_n', '', .col)}"))
  
  
  #### Make 1 or 0 for encounter history rather than counts ###
  #gets df with TF of whether a fish was detected at a antenna
  # ENC_Release1 <- ENC_Release %>%
  #   mutate(RB1 = (RB1_n >0),
  #          RB2 = (RB2_n >0),
  #          HP3 = (HP3_n >0),
  #          HP4 = (HP4_n >0),
  #          CF5 = (CF5_n >0),
  #          CF6 = (CF6_n >0),
  #          CD1 = (CD1_n >0),
  #          CD2 = (CD2_n >0),
  #          CS1 = (CS1_n >0),
  #          CS2 = (CS2_n >0),
  #          CU1 = (CU1_n >0),
  #          CU2 = (CU2_n >0),
  #          M1 = (M1_n >0),
  #          M2 = (M2_n >0),
  #          B3 = (B3_n >0),
  #          B4 = (B4_n>0),
  #          B5 = (B5_n >0),
  #          B6 = (B6_n>0),
  #          Recapture = (Recap_n > 0))
  
  
  #summary stats of each antenna encounter
  #a little precariously built because Row numbers are used
  #but also has release data
  totalcols <- ncol(ENC_Release1)
  
  ENC_Release2 <- ENC_Release1 %>%
    #counts number of TRUE across specified rows. -SG
    # need to have parentheses (totalcols-1) that's why i was getting bad numbers on biomark T/F initially
    #added 8 new columns for new antennas
    mutate(
      TotalEncounters = rowSums(select(., all_of(c(RedBarnCodes, HitchingPostCodes, ConfluenceCodes, 
                                            ConnectivityChannelDownstreamCodes, ConnectivityChannelSideChannelCodes,
                                            ConnectivityChannelUpstreamCodes, MobileRunCodes, 
                                            WindyGapAntennaSiteCode, KaibabParkAntennaSiteCode,
                                            RiverRunAntennaSiteCode, FraserRiverCanyonAntennaSiteCode, "Recapture"))) == TRUE),
      TotalAntennas1 = rowSums(select(., all_of(c(RedBarnCodes, HitchingPostCodes, ConfluenceCodes, 
                                                            ConnectivityChannelDownstreamCodes, ConnectivityChannelSideChannelCodes,
                                                            ConnectivityChannelUpstreamCodes, MobileRunCodes, 
                                                            WindyGapAntennaSiteCode, KaibabParkAntennaSiteCode,
                                                            RiverRunAntennaSiteCode, FraserRiverCanyonAntennaSiteCode))) == TRUE),
      TotalStationary = rowSums(select(., all_of(c(RedBarnCodes, HitchingPostCodes, ConfluenceCodes, 
                                                             ConnectivityChannelDownstreamCodes, ConnectivityChannelSideChannelCodes,
                                                             ConnectivityChannelUpstreamCodes))) == TRUE),

      TotalMobile = rowSums(select(.,  all_of(MobileRunCodes)) == TRUE),
      TotalBiomark = rowSums(select(., all_of(c(WindyGapAntennaSiteCode, KaibabParkAntennaSiteCode,
                                                                                     RiverRunAntennaSiteCode, FraserRiverCanyonAntennaSiteCode))) == TRUE),
      TotalRedBarn =rowSums(select(.,  all_of(RedBarnCodes)) == TRUE),
      TotalHitchingPost = rowSums(select(.,  all_of(HitchingPostCodes)) == TRUE),
      TotalConfluence = rowSums(select(.,  all_of(ConfluenceCodes)) == TRUE),
      TotalConnectivityDownstream = rowSums(select(.,  all_of(ConnectivityChannelDownstreamCodes)) == TRUE),
      TotalConnectivitySideChannel = rowSums(select(.,  all_of(ConnectivityChannelSideChannelCodes)) == TRUE),
      TotalConnectivityUpstream = rowSums(select(.,  all_of(ConnectivityChannelUpstreamCodes)) == TRUE)
    ) %>%
    # just says if the fish was ever detected at these sites
    mutate(RedBarn = TotalRedBarn > 0,
           HitchingPost = TotalHitchingPost > 0,
           Confluence = TotalConfluence > 0,
           ConnectivityDownstream = TotalConnectivityDownstream > 0,
           ConnectivitySideChannel = TotalConnectivitySideChannel > 0,
           ConnectivityUpstream = TotalConnectivityUpstream > 0,
           Biomark = TotalBiomark > 0,
           Mobile = TotalMobile >0
    ) %>%
    filter(TAG %in% unique(Release1$TAG)) 
  
  setdiff(ENC_Release22$TotalHitchingPost, ENC_Release2$TotalHitchingPost)
  ###Bringing in Station data with info about ABOVE/BELOW dam for joining
  ### the release data isn't being brought in well; The tags aren't being brought in as full numbers, so when the release data is joined,
  # it can't match up 23000088888 to 2.3E+11; so release site gets put in as "no info", and
  #therefore when the columns join, it doesn't make a column called "release above dam" (should I cahnge to subset by number instead of column name?)
  #SOLVED: needed to change the format in release csv file
  
  # trying to go based on movements
  ### thinking of disbanding this and doing the same process but with the states in order to say if fish went above/below
  
  above_below_counts <- combined_events_stations %>%
    count(TAG, det_type, above_below, name = "Encounters") %>%
    mutate(combined_event = paste(det_type, above_below),
           EncountersTF = ifelse(Encounters > 0, 
                                 TRUE,
                                 FALSE))
  
  above_below_counts1 <- pivot_wider(data = above_below_counts, id_cols = TAG, names_from = combined_event, values_from = EncountersTF)
  above_below_counts2 <- above_below_counts1 %>%
    select(TAG, `Release Above the Dam`, `Release Below the Dam`,`Recapture Above the Dam`,`Recapture Below the Dam`,`Recapture and Release Above the Dam`,`Recapture and Release Below the Dam`, `Mobile Run Above the Dam`, `Mobile Run Below the Dam`)
  #turns all the NA's made to FALSE
  above_below_counts2[is.na(above_below_counts2)] = FALSE

  ENC_Release3 <- left_join(ENC_Release2, above_below_counts2, by = "TAG")
  ### need to figure out how connectivity channel fits into this part?
  ENC_Release4 <- ENC_Release3 %>%
    mutate(through_dam = case_when(
      (RB1|RB2|HP3|HP4|B3|`Release Below the Dam`|`Recapture Below the Dam`|`Recapture and Release Below the Dam`|`Mobile Run Below the Dam`) == TRUE & (CF5|CF6|B4|B5|B6|`Release Above the Dam`|`Recapture Above the Dam`|`Recapture and Release Above the Dam`|`Mobile Run Above the Dam`) == TRUE ~ "Went through dam",
      (RB1|RB2|HP3|HP4|B3|`Release Below the Dam`|`Recapture Below the Dam`|`Recapture and Release Below the Dam`|`Mobile Run Below the Dam`) == TRUE & (CF5&CF6&B4&B5&B6&`Release Above the Dam`&`Recapture Above the Dam`&`Recapture and Release Above the Dam`&`Mobile Run Above the Dam`) == FALSE ~ "Stayed Below the Dam",
      (RB1&RB2&HP3&HP4&B3&`Release Below the Dam`&`Recapture Below the Dam`&`Recapture and Release Below the Dam`&`Mobile Run Below the Dam`) == FALSE & (CF5|CF6|B4|B5|B6|`Release Above the Dam`|`Recapture Above the Dam`|`Recapture and Release Above the Dam`|`Mobile Run Above the Dam`) == TRUE ~ "Stayed Above the Dam",
      
    ))
  # left joining states summary to enc_release
  ENC_Release5 <- left_join(ENC_Release4, States_summarized, by = "TAG")
  #rearranging so that Tag is first column shown
  ENC_Release5<- ENC_Release5 %>%
    select(TAG, 1:ncol(ENC_Release5))
  ###joining on column with sum data
  #same code appears in movements function
  sum_dist1 <- combined_events_stations %>%
    group_by(TAG) %>%
    arrange(Datetime) %>%
    mutate(dist_moved = ET_STATION - lag(ET_STATION, order_by = Datetime),
           sum_dist = (sum(abs(diff(ET_STATION, na.rm = TRUE))))
    ) %>% #end of mutate
    distinct(TAG, .keep_all = TRUE) %>%
    select(TAG, sum_dist)
  
  ENC_Release6 <- ENC_Release5 %>%
    left_join(sum_dist1, by = "TAG") %>%
    mutate(Date = ifelse(str_detect(Date, "/"),
                         as.character(mdy(Date)),
                         Date))
  
  #### dummy rows removal: 1/14/23
  ENC_Release6 <- ENC_Release6 %>%
    filter(!TAG %in% c("230000999999"))
  
  
  
  
  enc_wide_list <- list(
    "ENC_Release_wide_summary" = ENC_Release6, "Unknown_Tags" = unknown_tags
  )
  
  end_time <- Sys.time()
  print(paste("Encounter Histories Summary Wide Function took", round(end_time-start_time,2), "Seconds"))
  
  return(enc_wide_list)
}
