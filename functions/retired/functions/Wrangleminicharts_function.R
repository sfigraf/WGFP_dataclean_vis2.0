WrangleMinicharts_function <- function(Movements_df){
  WeeklyMovementsbyType <- Movements_df %>%
    
    
    ungroup() %>%
    ### mobile filter
    filter(!det_type %in% c("Mobile Run")) %>%
    mutate(weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))),
           date_week = as.Date("2020-09-01")+weeks(weeks_since)
    ) %>%
    group_by(UTM_X, UTM_Y, date_week, movement_only) %>%
    mutate(total = n()) %>%
    distinct(UTM_X, UTM_Y, date_week, movement_only, .keep_all = TRUE) %>%
    pivot_wider(id_cols = c("X", "Y", "det_type", "date_week"), names_from = movement_only, values_from = total) %>%
    select(-9)
  
  WeeklyMovementsbyType[is.na(WeeklyMovementsbyType)] <- 0
  
  ## this part is making new row with UTM's to get the initla release back down to 0 to not show as a big bar graph the whole time on minicharts
  #just decided I'm going to put a disclaimer that it doesn't work as well with mobile detections
  WeeklyMovementsbyType2 <- WeeklyMovementsbyType %>%
    group_by(X, Y, det_type) %>%
    arrange(date_week) %>%
    mutate(nextinitialRelease = lead(`Initial Release`, order_by = date_week)
    )
  x <- WeeklyMovementsbyType2
  df_skeleton <- WeeklyMovementsbyType2[1,]
  df_skeleton[1,] <- list(-106, 55, NA, NA, 0, 0, 0, 0, 0, 0)
  
  for (row in 1:nrow(WeeklyMovementsbyType2)) {
    #print(x$nextinitialRelease[row])
    if(is.na(WeeklyMovementsbyType2$nextinitialRelease[row]) & WeeklyMovementsbyType2$`Initial Release`[row] > 0) {
      #gettig values to make a new row with
      
      X1 <- WeeklyMovementsbyType2$X[row]
      
      Y1 <- WeeklyMovementsbyType2$Y[row]
      next_week <- WeeklyMovementsbyType2$date_week[row] + weeks(1)
      
      events <- 0
      det_type <- WeeklyMovementsbyType2$det_type[row]
      
      new_row <- df_skeleton
      new_row[1,] <- list(X1, Y1, det_type, next_week, events, NA, NA, NA, NA, NA)
      WeeklyMovementsbyType2 <- rbind(WeeklyMovementsbyType2, new_row)
    }
  }
  return(WeeklyMovementsbyType2)
  
}