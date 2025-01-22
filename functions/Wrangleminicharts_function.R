#Movements_df <- Movements_list$dailyMovementsTable1
Wrangleminicharts_function <- function(Movements_df){
  
  WeeklyMovementsbyType <- Movements_df %>%
    ungroup() %>%
    ### mobile filter, no mobile data
    filter(!det_type %in% c("Mobile Run")) %>%
    mutate(weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))),
           date_week = as.Date("2020-09-01")+weeks(weeks_since)
    ) %>%
    #get total number of occurances at each UTM location for a given week
    group_by(UTM_X, UTM_Y, date_week, movement_only) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    #filter(!det_type %in% c("Release")) %>%
    distinct(UTM_X, UTM_Y, date_week, movement_only, .keep_all = TRUE) %>%
    pivot_wider(id_cols = c("X", "Y", "date_week"), names_from = movement_only, values_from = total)# %>%
    #column 9 accouts for the NA movements so it's a NA column
  #only needed in all data, not needed in ap since NA movement will be filtered out already
    #select(-`NA`)
  
  
  
  allWeeks = seq(min(WeeklyMovementsbyType$date_week), max(WeeklyMovementsbyType$date_week), by = "week")
  allSites <- WeeklyMovementsbyType %>%
    distinct(X, Y)
  
  WeeklyMovementsbyType2 <- expand_grid(allSites, date_week = allWeeks) %>%
    left_join(WeeklyMovementsbyType, by = c("X", "Y", "date_week")) #%>%
    #mutate(across(c(`Initial Release`, `No Movement`, `Downstream Movement`, `Upstream Movement` , `Changed Rivers`), ~replace_na(.x, 0)))
    WeeklyMovementsbyType2[is.na(WeeklyMovementsbyType2)] <- 0
  
  return(WeeklyMovementsbyType2)
  
}