#Movements_df <- movements_list$Movements_df #%>%
  #filter(Date > as.Date("2024-12-01"))


Wrangleminicharts_function <- function(Movements_df){
  
  WeeklyMovementsbyType <- Movements_df %>%
    ungroup() %>%
    ### mobile filter, no mobile data
    filter(!det_type %in% c("Mobile Run")) %>%
    mutate(weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))),
           #doing this because round(), like we're doing in animation mod, doesn't take weeks as an arg
           date_week = min(Date) + weeks(weeks_since)
           #date_week2 = as.Date(as.character(round(Datetime, units = "weeks")))
    ) %>%
    #get total number of occurances at each UTM location for a given week
    group_by(UTM_X, UTM_Y, date_week, movement_only) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    #filter(!det_type %in% c("Release")) %>%
    distinct(UTM_X, UTM_Y, date_week, movement_only, .keep_all = TRUE) %>%
    mutate(Site = case_when(det_type == "Recapture" ~ RecaptureSite,
                            det_type %in% c("Release", "Recapture and Release") ~ ReleaseSite, 
                            TRUE ~ det_type)
    ) %>%
    pivot_wider(id_cols = c("X", "Y", "date_week", "Site"), names_from = movement_only, values_from = total) #%>%
    #select(-Site)
    #column 9 accouts for the NA movements so it's a NA column
  #only needed in all data, not needed in ap since NA movement will be filtered out already
    #select(-`NA`)
  
  
  
  
  
  allWeeks = seq(min(WeeklyMovementsbyType$date_week), max(WeeklyMovementsbyType$date_week), by = "week")
  allSites <- WeeklyMovementsbyType %>%
    distinct(X, Y, Site)
  
  WeeklyMovementsbyType2 <- expand_grid(allSites, date_week = allWeeks) %>%
    #left join automatically fills in empty fields with NA
    left_join(WeeklyMovementsbyType, by = c("X", "Y", "date_week", "Site")) 
  
  WeeklyMovementsbyType2[is.na(WeeklyMovementsbyType2)] <- 0

    
  return(WeeklyMovementsbyType2)
  
}
