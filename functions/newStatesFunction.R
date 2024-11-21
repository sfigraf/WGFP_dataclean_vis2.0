#this comes from runscript after spatial join detections stations function
detectionsWithStates <- as.data.frame(DailyDetectionsStationsStates$spatialList$stationData) %>%
  filter(Species != "TGM")
GhostTagsForJoining <- GhostTags %>%
  rename(TAG = TagID) %>%
  select(TAG, GhostDate) 

AvianPredationForJoining <- AvianPredation %>%
  rename(TAG = TagID) %>%
  select(TAG, PredationDate)

#joining with ghost tag df
eventsWithGhostDates <- left_join(detectionsWithStates, GhostTagsForJoining, by = c("TAG"))
eventsWithGhostDatesAndAvianPredation <- left_join(eventsWithGhostDates, AvianPredationForJoining, by = c("TAG")) #%>%
  #distinct(TAG, Event, Datetime, UTM_X, UTM_Y, first_last, .keep_all = TRUE)
#if it's predated and ghost, predated will always come first
y2 <- eventsWithGhostDatesAndAvianPredation %>%
  mutate(GhostOrPredationDate = coalesce(PredationDate, GhostDate), 
         State = case_when(Date >= GhostOrPredationDate ~ "G", 
                           TRUE ~ State)
         #equal = GhostDate == PredationDate
  ) %>%
  #getting rid of data for tags after their ghost/predation date
  #makes sure to keep non-predated tags if they don'thave a ghost/predation date
  filter(is.na(GhostOrPredationDate) |
         Date <= GhostOrPredationDate)
# we don't need duplicated rows of states and tags that fall on the same day. 
# y3 <- y2 %>%
#   distinct(TAG, GhostOrPredationDate, State, Date, .keep_all = T)
#getting rid of same-day ghost data, keeping just the first detectio nthat day that qulifies as ghot/predation
y3 <- y2 %>%
  #filter(State == "G") %>%
  arrange(TAG, Datetime) %>%
  group_by(TAG, State) %>%
  mutate(firstDatetime = dplyr::if_else(!is.na(GhostOrPredationDate) & State == "G", first(Datetime), NA)) %>% #first(Datetime)) %>%
  relocate(TAG, Datetime, GhostOrPredationDate, firstDatetime) %>%
  filter(is.na(GhostOrPredationDate) | is.na(firstDatetime) |
           Datetime == firstDatetime)
  #filter(Datetime == firstDatetime)
# nrow(eventsWithGhostDatesAndAvianPredation)



timePeriods <- wgfpMetadata$TimePeriods %>%
  mutate(`start date` = janitor::excel_numeric_to_date(as.numeric(`start date`)), 
         `end date` = janitor::excel_numeric_to_date(as.numeric(`end date`))
  )
#release and recaps
recapsAndRelease <- as.data.frame(detectionsWithStates) %>%
  filter(Event %in% c("Recapture", "Recapture and Release", "Release", "Recapture "), 
         !Species %in% "TGM")

# x45 <- x4 %>%
#   left_join(recapsAndRelease[, c("TAG", "Datetime", "Release_Length", "Release_Weight", "Recap_Length", "Recap_Weight")], 
#             by = c("TAG", "Datetime"))
#need to add the datetime to make sure the <= and >= filtering later is interpreting correctly
timePeriodsCorrect <- timePeriods %>%
  mutate(`end date` = ymd_hms(paste(`end date`, "23:59:59")))

# Perform the join and filtering
#no muskie
df1_with_period <- as.data.frame(y3)  %>%
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

x65 <- x6 %>%
  rowwise() %>%
  mutate(number1 = case_when(any(c_across(matches("^[0-9]+$")) == "G") ~ -1, 
                             TRUE ~ number1)
    #        if_else(
    # any(c_across(matches("^[0-9]+$")) == "G"), -1, number1, missing = number1
    # )
  )

# x6 <- x5 %>%
#   group_by(TAG) %>%
#   mutate(number1 = ifelse(newState == "G", -1, NA)
#          
# # case_when(newState == "G" ~ -1, 
# #                              group == max(group) ~ 1, 
# #                              TRUE ~ -1)
#   ) %>%
#   arrange(as.numeric(TimePeriod)) %>%
#   pivot_wider(names_from = TimePeriod, values_from = newState) #%>%
# 
# x6Correct <- x6
  # group_by(TAG) %>%
  # mutate(number1 = case_when(State == "G" ~ -1, 
  #                            group == max(group) ~ 1, 
  #                            TRUE ~ -1)
  # )
           #ifelse(group == max(group), 1, -1))

recapsAndReleaseWithGroup <- recapsAndRelease %>%
  group_by(TAG) %>%
  arrange(Datetime) %>%
  mutate(group = row_number(), 
         RBT = ifelse(Species == "RBT", 1, 0),
         LOC = ifelse(Species == "LOC", 1, 0), 
         MTS = ifelse(Species == "MTS", 1, 0))

x7 <- x65 %>%
  #left_join(x45[,c("TAG", "group", "")])
  #adding length and weight columns on
  #coalescing recap dat first so that will take priority
  left_join(recapsAndReleaseWithGroup[,c("TAG", "group", "Release_Length", "Release_Weight", "Recap_Length", "Recap_Weight", "RBT", "LOC", "MTS")] %>%
              mutate(Length = coalesce(Recap_Length, Release_Length),
              Weight = coalesce(Recap_Weight, Release_Weight)), by = c("TAG", "group")) %>%
  select(-c(Release_Length, Release_Weight, Recap_Length, Recap_Weight))

## replcae NA with 0
x8 <- x7 %>%
  rowwise() %>%
  mutate(across(matches("^[0-9]+$"), ~ replace_na(.x, "0")))
  #replace_na(c_across(matches("^[0-9]+$") = "0")
  

# # Example wide dataframe
# df <- data.frame(
#   TAG = c("A", "B", "C"),
#   `1` = c(10, 15, 20),
#   `2` = c(5, 10, 15),
#   `3` = c(8, 16, 24),
#   `4` = c(11, 22, 33)
# )
# 
# # Example time periods dataframe
# time_periods <- data.frame(
#   Number = 1:4,
#   StartDate = as.Date(c("2023-04-05", "2023-05-01", "2023-06-01", "2023-07-01")),
#   EndDate = as.Date(c("2023-05-06", "2023-06-01", "2023-07-01", "2023-08-01"))
# )

# Reshape the wide df to long format
df_long <- x8 %>%
  pivot_longer(matches("^[0-9]+$"), names_to = "periods", values_to = "State") %>%
  mutate(periods = as.character(periods))  # Convert Number to integer for matching

# Join with the time periods dataframe to get the date range for each Number
df_long <- df_long %>%
  left_join(timePeriods[,c("periods", "start date", "end date")], by = "periods") %>%
  mutate(NewColumnName = paste(`start date`, "to", `end date`))

colsToMov <- c("number1", "RBT", "LOC", "MTS", "Length", "Weight")
# Reshape back to wide format with new column names
df_wide <- df_long %>%
  select(-`start date`, -`end date`, -periods) %>%
  pivot_wider(names_from = NewColumnName, values_from = "State") %>%
  select(-all_of(colsToMov), all_of(colsToMov))

# View the updated wide dataframe
print(df_wide)

####qaqc
test <- x7 %>%
  arrange(TAG) %>%
  ungroup() %>%
  add_row()%>%
  add_row()%>%
  add_row()%>%
  add_row()%>%
  add_row()%>%
  add_row()%>%
  add_row()%>%
  add_row()
qaqc <- recapsAndRelease %>%
  arrange(TAG) %>%
  bind_cols(test[,"TAG"])

qaqc2 <- qaqc %>%
  mutate(equal = `TAG...1` == `TAG...38`) %>%
  relocate(TAG...1, TAG...38, equal, State, Datetime, Event, )


# joining with avian predation
#Shouldn't matter really, but this just cuts down unnecesary rows. left_joining creates more rows than df1 if there are duplicate values in the key_col of df1 (in this case TAG)
#some new rows are created then when joining to the ghost tag df because many of the ghost tags have multiple detections after the ghost date in the combined_df
# I'm just cutting out these excess rows but they would get cut down anyway later in this function. As long as each ghost tag gains a row with ghost date or predation date, that's all that matters

