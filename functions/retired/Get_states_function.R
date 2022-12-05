# this should be get_states_function
### current assumptions: 
# if a fish hits the upstream antenna and not the downstream one on a given day, it is assumed to  have swam back upstream and NOT just missed the downstream antenna
# vice versa for downstream antenna
# so also if a fish is assumed to be downstream of RB1, and last hit RB1 only, then hits RB1 again, that is assumed as an upstream movement

# if a fish is released upstream of an antenna 

# hitching post release site is below the antennas and if a fish released at hithcing post hits the hithcin post antennas, it's an upstream movement

# no mobile detections incorporated for now
#All_events <- df_list$All_Events
# Stationdata1 <- read_csv("EncounterHistory_AllData_wStations_20220114.csv", 
#                          col_types = cols(
#                            #OBJECTID = col_skip(), Join_Count = col_skip(), TARGET_FID = col_skip(), 
#                                           TAG = col_character(), Release_Length = col_number(), 
#                                           UTM_X = col_character(), UTM_Y = col_character(),
#                                           #Date_ = col_date(format = "%m/%d/%Y"),
#                                           Release_Weight = col_number()))



#station_data <- Stationdata1
#All_events_stations_combined <- combined_events_stations

Get_states_function <- function(All_events_stations_combined) {
  library(tidyverse) 
  library(lubridate)
  
  start_time <- Sys.time()
  


  r1 <- All_events_stations_combined %>%
    # no need to group_by date until states will be consolidated
    # need to group_by tag though so that the Lag(Date) will get the last date that that fish was detected
    # some movements weren't being recorded correctly because it was grouping by both date and Tag
    group_by(TAG) %>%
    
    mutate(
      det_type = case_when(str_detect(Event, "RB1|RB2") ~ "Red Barn Stationary Antenna",
                           str_detect(Event, "HP3|HP4") ~ "Hitching Post Stationary Antenna",
                           str_detect(Event, "CF5|CF6") ~ "Confluence Stationary Antenna",
                           str_detect(Event, "B3") ~ "Windy Gap Dam Biomark Antenna",
                           str_detect(Event, "B4") ~ "Kaibab Park Biomark Antenna",
                           str_detect(Event, "M1|M2") ~ "Mobile Run",
                           Event == "Recapture" ~ "Recapture",
                           TRUE ~ Event),
      
      current_event_vals = case_when(Event == "RB1" ~ 11.9,
                                     Event == "RB2" ~ 11.1,
                                     Event == "HP3" ~ 7.9,
                                     Event == "HP4" ~ 7.1,
                                     Event == "CF5" ~ 4.9,
                                     Event == "CF6" ~ 4.1,
                                     Event == "B3" ~ 6,
                                     Event == "B4" ~ .9, #this ensures that kaibab park release to dtecitons will get a slight upstream move
                                     
                                     Event == "Recapture" & RecaptureSite == "Lower River Run" ~ 4,
                                     Event == "Recapture" & RecaptureSite == "Fraser River Ranch" ~ 2,
                                     Event == "Recapture" & RecaptureSite == "Kaibab Park" ~ 1,
                                     Event == "Recapture" & RecaptureSite == "Upper River Run" ~ 3,
                                     Event == "Recapture" & RecaptureSite == "Below Confluence Antenna" ~ 5,
                                     Event == "Recapture" & RecaptureSite == "Windy Gap Dam" ~ 6,
                                     Event == "Recapture" & RecaptureSite == "Hitching Post" ~ 7,
                                     Event == "Recapture" & RecaptureSite == "Chimney Rock Above Island" ~ 8,
                                     Event == "Recapture" & RecaptureSite == "Chimney Rock Below Island" ~ 9,
                                     Event == "Recapture" & RecaptureSite == "Upper Red Barn Fry Site" ~ 10,
                                     Event == "Recapture" & RecaptureSite == "Pool Above Red Barn Antenna" ~ 11,
                                     Event == "Recapture" & RecaptureSite == "Lower Red Barn Fry Site" ~ 12,
                                     Event == "Recapture" & RecaptureSite == "Below Red Barn Diversion #1" ~ 13,
                                     Event == "Recapture" & RecaptureSite == "Below Red Barn Diversion #2" ~ 14,
                                     Event == "Recapture" & RecaptureSite == "Kinney Creek" ~ 15,
                                     Event == "Recapture" & RecaptureSite == "Dark Timber Above Railroad" ~ 16,
                                     Event == "Recapture" & RecaptureSite == "Sheriff Ranch Upper Field" ~ 17,
                                     Event == "Recapture" & RecaptureSite == "Shefiff Ranch Middle Field" ~ 18,
                                     Event == "Recapture" & RecaptureSite == "Sheriff Ranch Fry Site" ~ 19
                                     
      ),
    
      movement = case_when(
        
        Event %in% c("Release", "Recapture and Release") ~ "Initial Release",
        #if the values are more or less than previous values, it's moved upstream or downstream
        
        

        #transitions can only happen on one day
        #current station different than previous station, it's a movement
# Downstream Movements and Transitions ------------------------------------

        
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals < lead(current_event_vals, order_by = Datetime)) ~ "Downstream Movement Before continuing downstream to transition at that site", #230000228381
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals > lead(current_event_vals, order_by = Datetime)) & (!Event %in% c("M1", "M2","B3", "B4", "Recapture")) ~ "Downstream Movement and Downstream Transition before a US Transition", #means fish missed a antenna #230000228638
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & (current_event_vals < lead(current_event_vals, order_by = Datetime)) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") ~ "Downstream Movement and Inferred Downstream Transition1", #missed a antenna going downstream #230000228314
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & current_event_vals == lead(current_event_vals, order_by = Datetime) ~ "Downstream Movement with next detection on same antenna", #230000228054

# (current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION > lead(ET_STATION, order_by = Datetime)) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") ~ "Downstream Movement and Transition", #means a fish missed a antenna heading downstream #230000228771
# (current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION < lead(ET_STATION, order_by = Datetime)) & !Event %in% c("M1", "M2") ~ "Downstream Movement without Transition", #this is a fish that hits an antenna and heads right back upstream #230000228901
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0)  & !Event %in% c("B3", "B4", "M1", "M2") ~ "Downstream Movement without Transition2", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) #before hitting same antenna #230000228346
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION < lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1)  & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") ~ "Downstream Movement and Downstream Transition2 and inferred Upstream Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) # 230000272182 #230000228314
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1)  & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") ~ "Downstream Movement and Downstream Transition2", # before hitting same antenna #230000228696
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0)  & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") & (current_event_vals != lead(current_event_vals, order_by = Datetime) |  is.na(lead(current_event_vals, order_by = Datetime))) ~ "Downstream Movement and Downstream Transition3",  # before hitting same antenna #230000228275 #230000224042

# current_event_vals > lag(current_event_vals, order_by = Datetime) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0)  & !Event %in% c("B3", "B4")~ "Downstream Movement without Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) #before hitting same antenna
# current_event_vals > lag(current_event_vals, order_by = Datetime) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1)  & !Event %in% c("B3", "B4") ~ "Downstream Movement and downstream Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) # before hitting same antenna


(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & ET_STATION < lead(ET_STATION, order_by = Datetime) ~ "Downstream Transition then Upstream Transition", #230000228381
(current_event_vals >= lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION > lead(ET_STATION, order_by = Datetime) ~ "Downstream Transition Before continuing Downstream", #230000228136 #230000228631 #may need to include that previous detection was on the same day in order to do transition? 
(current_event_vals >= lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals < lead(current_event_vals, order_by = Datetime) ~ "Downstream Transition (possibly inferred) with next detection possibly at same site", #230000228956
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & (current_event_vals == lead(current_event_vals, order_by = Datetime) | is.na(lead(current_event_vals, order_by = Datetime)) ) ~ "Downstream Transition with next detection at same antenna or station", #230000229042
(current_event_vals >= lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals > lead(current_event_vals, order_by = Datetime) ~ "Downstream Transition (possibly inferred)  and inferred Upstream Transition with next detection possibly at same site", #230000229044 
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION >= lead(ET_STATION, order_by = Datetime)  & (Date == lag(Date, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) ~ "Downstream Transition (possibly inferred) with next detection possibly at same site1", # if this line were to leave and these would be coded as NA, it might be beneficial 3230000228638


(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date != lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & !Event %in% c("B3", "B4", "M1", "M2", "Recapture")  ~ "Inferred Downstream Transition with next detection possibly at same site", #230000229044


(Event %in% c("M1", "M2",  "B3", "B4","Recapture")) & (ET_STATION < lag(ET_STATION, order_by = Datetime))  ~ "Downstream Movement1", #230000228473 #`movement = case_when(...)`. x object not interpretable as a factor# solved because I was typing uppercase C for a concatenation of strings, not c

#(is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime))) & ET_STATION < lag(ET_STATION, order_by = Datetime) &  ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals == lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Downstream Movement2 without Transition", #"Downstream Movement2", #no fish for now
#(#is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime))) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals > lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Downstream Movement2 and Transition", #skipped an antenna #230000228623
#(is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime))) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals < lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Downstream Movement2 before potential Transition", #skipped an antenna #230000228623

(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0) ~ "Downstream Movement and last detection of history", #230000228796 
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1) ~ "Downstream Movement and Downstream Transition and last detection of history", #230000228891. 

(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") ~ "Downstream Transition and last detection of history",

#fish must have also transitioned if the next time it's seen it's seen on the next-most ds antenna
#if a fish is headed downstream and only hits one antenna but then is detected downstream, it must have transitioned as well as moved
#if the station values are the same though, it's a transition, not a movement

# Upstream Movements and Transitions --------------------------------------
(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals > lead(current_event_vals, order_by = Datetime)) ~ "Upstream Movement Before continuing Upstream to transition at that site", 
(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals < lead(current_event_vals, order_by = Datetime)) & !Event %in% c("M1", "M2","B3", "B4", "Recapture") ~ "Upstream Movement and Upstream Transition111", #means fish missed a antenna #before a Upstream Transition
(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & (current_event_vals > lead(current_event_vals, order_by = Datetime)) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") ~ "Upstream Movement and Inferred Upstream Transition1", #missed a antenna going Upstream #228314
(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & current_event_vals == lead(current_event_vals, order_by = Datetime) ~ "Upstream Movement with next detection on same antenna",

# (current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION < lead(ET_STATION, order_by = Datetime)) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") ~ "Upstream Movement and Transition", #means a fish missed a antenna heading Upstream
# (current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION > lead(ET_STATION, order_by = Datetime)) & !Event %in% c("M1", "M2") ~ "Upstream Movement without Transition", #this is a fish that hits an antenna and heads right back upstream
# (current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0)  & !Event %in% c("B3", "B4", "M1", "M2") ~ "Upstream Movement without Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) #before hitting same antenna
# (current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1)  & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") ~ "Upstream Movement and Upstream Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) # before hitting same antenna

(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1)  & !Event %in% c("B3", "B4", "M1", "M2") ~ "Upstream Movement without Transition2", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) #before hitting same antenna #
(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION > lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0)  & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") ~ "Upstream Movement and Upstream Transition2 and inferred downstream transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) #  #
(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0)  & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") ~ "Upstream Movement and Upstream Transition2", # before hitting same antenna #
(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1)  & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") & current_event_vals != lead(current_event_vals, order_by = Datetime) ~ "Upstream Movement and Upstream Transition3",  # before hitting same antenna # #


(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & ET_STATION > lead(ET_STATION, order_by = Datetime)  ~ "Upstream Transition then Downstream Transition", 
(current_event_vals <= lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION < lead(ET_STATION, order_by = Datetime)  ~ "Upstream Transition Before continuing Upstream", #230000228136
(current_event_vals <= lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals > lead(current_event_vals, order_by = Datetime) ~ "Upstream Transition (possibly inferred) with next detection possibly at same site", 
(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & (current_event_vals == lead(current_event_vals, order_by = Datetime) | is.na(lead(current_event_vals, order_by = Datetime)) ) ~ "Upstream Transition with next detection at same antenna or station", 
(current_event_vals <= lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals < lead(current_event_vals, order_by = Datetime) ~ "Upstream Transition (possibly inferred)  and inferred Downstream Transition with next detection possibly at same site", 
(current_event_vals < lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime))  & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) ~ "Upstream Transition (possibly inferred) with next detection possibly at same site1", #230000228638


#upstream inferred transition for fish 
(current_event_vals > lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date != lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & !Event %in% c("B3", "B4", "M1", "M2") ~ "Inferred Upstream Transition with next detection possibly at same site", #230000228862



(Event %in% c("M1", "M2", "B3", "B4", "Recapture")) & (ET_STATION > lag(ET_STATION, order_by = Datetime))  ~ "Upstream Movement1",  #`movement = case_when(...)`. x object not interpretable as a factor# solved because I was typing uppercase C for a concatenation of strings, not c

#(is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime))) & ET_STATION > lag(ET_STATION, order_by = Datetime) &  ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals == lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Upstream Movement2 without Transition", #"Upstream Movement2",
#(is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals < lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Upstream Movement2 and Transition", #skipped an antenna #230000228623
#(is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals > lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Upstream Movement2 before potential Transition", #skipped an antenna #230000228623

#(is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime))) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals < lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Upstream Movement2 before Transition", #skipped an antenna #230000228623

(current_event_vals < lag(current_event_vals, order_by = Datetime)| (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1) ~ "Upstream Movement and last detection of history",
(current_event_vals < lag(current_event_vals, order_by = Datetime)| (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0) ~ "Upstream Movement and Upstream Transition and last detection of history",

(current_event_vals < lag(current_event_vals, order_by = Datetime)| (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") ~ "Upstream Transition and last detection of history",


# Not Enough Info or No Movement ---------------------------------------------------------

(current_event_vals != lag(current_event_vals, order_by = Datetime)) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date != lag(Date, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime))  ~ "Not Enough Info to infer Movement", #might have to include that next station is the same as previous? 
(current_event_vals != lag(current_event_vals, order_by = Datetime)) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date != lag(Date, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime))  ~ "Not Enough Info to infer Movement and last detection of history", #might have to include that next station is the same as previous? 

(current_event_vals == lag(current_event_vals, order_by = Datetime) | (is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime)))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) ~ "No Movement and last detection of history",

#if a event is mobile , decide whether it's a upstream or downstream movement based on ET station

Event %in% c("M1", "M2") & (ET_STATION == lag(ET_STATION, order_by = Datetime)) ~ "No Movement",
#if current event vals is na or previous event vals are NA
(is.na(current_event_vals) | is.na(lag(current_event_vals, order_by = Datetime))) & is.na(ET_STATION) ~ "Not Enough Info to infer movement1",
# #if the values are the same and the day is the same, it means there was multiple consecutive detections at the same antenna and same day 
#current_event_vals == lag(current_event_vals, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & ET_STATION == lead(ET_STATION, order_by = Datetime) ~ "No Movement; Same Day", #current_event_vals == lead(current_event_vals, order_by = Datetime)
current_event_vals == lag(current_event_vals, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & ET_STATION == lead(ET_STATION, order_by = Datetime) & abs(current_event_vals - lead(current_event_vals, order_by = Datetime)) < 1 & Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6")  ~ "No Movement; Same Day for stationary antennas", #current_event_vals == lead(current_event_vals, order_by = Datetime)
current_event_vals == lag(current_event_vals, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & ET_STATION == lead(ET_STATION, order_by = Datetime) & (current_event_vals == lead(current_event_vals, order_by = Datetime) | is.na(lead(current_event_vals, order_by = Datetime)))  & Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6")  ~ "No Movement; Same Day for stationary antennas1", #current_event_vals == lead(current_event_vals, order_by = Datetime) #230000292262

current_event_vals == lag(current_event_vals, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & !Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6")  ~ "No Movement; Same Day for non-stationary antennas", #current_event_vals == lead(current_event_vals, order_by = Datetime)

current_event_vals == lag(current_event_vals, order_by = Datetime) & (Date != lag(Date, order_by = Datetime)) ~ "No Movement1",         
#(current_event_vals > lag(current_event_vals, order_by = Datetime)) & (ET_STATION != lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & current_event_vals lead(current_event_vals, order_by = Datetime) ~ "Downstream Movement and Downstream Transition ",

      ) #end of movement case_when
    ) #end of mutate
      
  

# States ------------------------------------------------------------------

  
    states <- r1 %>%
      mutate( 

      teststate_11 = case_when(
                               #str_detect(movement, "Not Enough Info to infer movement") ~ "NEI",
                               movement %in% c("Upstream Movement and Upstream Transition2 and inferred downstream transition", "Upstream Transition then Downstream Transition","Upstream Transition (possibly inferred)  and inferred Downstream Transition with next detection possibly at same site")  & (Event %in% c("RB1", "RB2")) ~ "GH",
                               movement %in% c("Downstream Movement and Downstream Transition2 and inferred Upstream Transition", "Downstream Transition then Upstream Transition","Downstream Transition (possibly inferred)  and inferred Upstream Transition with next detection possibly at same site")  & (Event %in% c("RB1", "RB2")) ~ "HG",
                               
                               movement %in% c("Upstream Movement and Upstream Transition2 and inferred downstream transition","Upstream Transition then Downstream Transition","Upstream Transition (possibly inferred)  and inferred Downstream Transition with next detection possibly at same site")  & (Event %in% c("HP3", "HP4")) ~ "IJ",
                               movement %in% c("Downstream Movement and Downstream Transition2 and inferred Upstream Transition","Downstream Transition then Upstream Transition","Downstream Transition (possibly inferred)  and inferred Upstream Transition with next detection possibly at same site")  & (Event %in% c("HP3", "HP4")) ~ "JI",
                               
                               movement %in% c("Upstream Movement and Upstream Transition2 and inferred downstream transition","Upstream Transition then Downstream Transition","Upstream Transition (possibly inferred)  and inferred Downstream Transition with next detection possibly at same site")  & (Event %in% c("CF5", "CF6")) ~ "KL",
                               movement %in% c("Downstream Movement and Downstream Transition2 and inferred Upstream Transition","Downstream Transition then Upstream Transition","Downstream Transition (possibly inferred)  and inferred Upstream Transition with next detection possibly at same site")  & (Event %in% c("CF5", "CF6")) ~ "LK",
                               
                               #str_detect(movement, "Not Enough Info to infer movement") ~ "NEI",
                               
                               # str_detect(movement, "Upstream Transition&Downstream Transition") & str_detect(Event, "RB1|RB2") ~ "GH",
                               # str_detect(movement, "Downstream Transition&Upstream Transition") & str_detect(Event, "RB1|RB2") ~ "HG",
                               # 
                               
                               str_detect(movement, "Upstream Transition") & str_detect(Event, "RB1|RB2") ~ "G",
                               str_detect(movement, "Downstream Transition") & str_detect(Event, "RB1|RB2") ~ "H",
                               
                               str_detect(movement, "Upstream Transition") & str_detect(Event, "HP3|HP4") ~ "I",
                               str_detect(movement, "Downstream Transition") & str_detect(Event, "HP3|HP4") ~ "J",
                               
                               str_detect(movement, "Upstream Transition") & str_detect(Event, "CF5|CF6") ~ "K",
                               str_detect(movement, "Downstream Transition") & str_detect(Event, "CF5|CF6") ~ "L",
                               
        
                               
                               ET_STATION <= 4150 & !Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6") ~ "A",
                               ET_STATION > 4150 & ET_STATION <= 6340 & !Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6") ~ "B",
                               ET_STATION > 6340 & ET_STATION <= 8330 & !Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6") ~ "C",
                               ET_STATION > 8330 & ET_STATION <= 9550 & !Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6") ~ "D",
                               ET_STATION > 9550 & River %in% c("Colorado River") & !Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6") ~ "E",
                               ET_STATION > 9550 & River %in% c("Fraser River") & !Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6") ~ "F",
                               
                              
      ) #end of case_when
     # thought about adding column for if afish misses a detection on a sister antenna; but that would take a bit more work than I'm willing to go atm
      
    ) #%>% #end of mutate
  ### will have a lot of NA's here because in the code above, a state is not coded if a fish has a no movement for that day, or hits one antenna but doesn't transition, etc
  # see unique(unkown_states$movement) for full list
  unknown_states <- states %>%
    filter(is.na(teststate_11)) %>%
    select(Datetime, TAG, Event, movement, teststate_11)
  
  states1 <- states %>%
    filter(!is.na(teststate_11)) %>%
    group_by(Date, TAG) %>%
    #arranging my datetime ensures that all states will be recorded in the correct order
    arrange(Datetime) %>%
    mutate(
      teststate_2 = paste(teststate_11, collapse = ""),
      
      
    )  %>%
    mutate(teststate_4 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2), #removes consecutive letters
           teststate_5 = case_when(teststate_4 == "GHG" ~ "G",
                                   teststate_4 == "HGH" ~ "H",
                                   teststate_4 == "GHGH" ~ "GH",
                                   teststate_4 == "HGHG" ~ "HG",
                                   teststate_4 == "IJI" ~ "I",
                                   teststate_4 == "JIJ" ~ "J",
                                   teststate_4 == "KLK" ~ "K",
                                   teststate_4 == "LKL" ~ "L",
                                   teststate_4 == "IJIJ" ~ "IJ",
                                   teststate_4 == "JIJI" ~ "JI",
                                   teststate_4 == "KLKL" ~ "KL",
                                   teststate_4 == "LKLK" ~ "LK",
                                   TRUE ~ teststate_4),
           
           ) 
  
  states_final <- states1 %>%
    distinct(Date, TAG, teststate_5, .keep_all = TRUE) %>%
    select(Date, Datetime, TAG, teststate_5, det_type, ReleaseSite, Species, Release_Length, Release_Weight, c_number_of_detections, daily_unique_events, days_since, UTM_X, UTM_Y) %>%
    rename(State = teststate_5)
  
  
    
  
    #select(Date, Datetime, first_last, Event, movement,  teststate_11, c_number_of_detections, daily_unique_events, ReleaseSite, RecaptureSite, TAG)
  # x <- r1 %>%
  #   filter(!is.na(previous_event) & !Event %in% c("Release", "Recapture and Release" ))

# Movements --------------------------------------------
  ##this hopefully should be pretty small...filled with tags with detections before official "Release" and tags without release info
   unknown_movements <- r1 %>%
    filter(
      #!ReleaseSite %in% c("Pool Above Red Barn Antenna"),
      str_detect(TAG, c("^230")) | str_detect(TAG, c("^226")),
           #!is.na(previous_event), #don't want entries 
      is.na(movement)
      )
  

# Pivot_wider -------------------------------------------------------------

  days <- data.frame(days_since = 1:max(states_final$days_since))
  
  days_and_states <- full_join(days, states_final, by = "days_since")
  
  
  days_and_states_wide <- pivot_wider(days_and_states, id_cols = TAG, names_from = days_since, values_from = State)
  
  days_and_states_wide <- days_and_states_wide %>%
    select(TAG, `0`, 2:ncol(days_and_states_wide))
  states_df_list <- list("All_States" = states_final, "Unaccounted_Movements" = unknown_movements, "Days_and_states_wide" = days_and_states_wide)
  #this just tells how long the fucntion takes
  end_time <- Sys.time()
  print(paste("States Function took", round(end_time-start_time,2)))
  
  return(states_df_list)
}


# #statesdf_list <- Get_states_function(combined_events_stations)


#####################
# statesdfwide1 <- statesdfwide[, c(5, 4, 1, 2, 3)]
# 
# statesdfwide <- statesdf_list$Days_and_states_wide
# statesdfwide1 <- statesdfwide %>%
#   select(TAG, `0`, 2:ncol(statesdfwide))