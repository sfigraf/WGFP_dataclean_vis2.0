compute_time_diff <- function(start, end) {
  diff_secs <- abs(as.numeric(difftime(end, start, units = "secs")))
  if (diff_secs < 60) {
    return(paste(diff_secs, "seconds"))
  } else if (diff_secs < 3600) {
    return(paste(round(diff_secs / 60, 2), "minutes"))
  } else if (diff_secs < 86400) {
    return(paste(round(diff_secs / 3600, 2), "hours"))
  } else {
    return(paste(round(diff_secs / 86400, 2), "days"))
  }
}

extract_sequences <- function(tag_data, antennas1, antennas2) {
  sequences <- data.frame(TAG = character(), DatetimeFirstDetectedAtCDorCS = as.POSIXct(character()), DateTimeFirstDetectedAtCU = as.POSIXct(character()), stringsAsFactors = FALSE)
  i <- 1
  
  while (i < nrow(tag_data)) {
    cu_index <- which(grepl(paste0("^(", paste(antennas1, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    cdcs_index <- which(grepl(paste0("^(", paste(antennas2, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    
    if (length(cu_index) > 0 & length(cdcs_index) > 0) {
      cu_first <- i + cu_index[1] - 1
      cdcs_first <- i + cdcs_index[1] - 1
      
      if (tag_data$Datetime[cu_first] < tag_data$Datetime[cdcs_first]) {
        sequences <- rbind(sequences, data.frame(TAG = tag_data$TAG[cu_first], DatetimeFirstDetectedAtCDorCS = tag_data$Datetime[cdcs_first], DateTimeFirstDetectedAtCU = tag_data$Datetime[cu_first]))
        i <- cdcs_first + 1
      } else {
        sequences <- rbind(sequences, data.frame(TAG = tag_data$TAG[cdcs_first], DatetimeFirstDetectedAtCDorCS = tag_data$Datetime[cdcs_first], DateTimeFirstDetectedAtCU = tag_data$Datetime[cu_first]))
        i <- cu_first + 1
      }
    } else {
      break
    }
  }
  
  return(sequences)
}

##sequences function
summarizedDf <- function(All_Events, antennas1, antennas2){
  df_filtered <- All_Events %>%
    filter(grepl(paste0("^(", paste(c(antennas1, antennas2), collapse = "|"), ")"), Event)) %>%
    arrange(TAG, Datetime)
  
  
  
  # Apply the function to each TAG
  newDF <- df_filtered %>%
    group_by(TAG) %>%
    do(extract_sequences(., antennas1, antennas2))
  
  
  
  # Add timeToTravel column with appropriate units
  # sequencesDF <- newDF %>%
  #   mutate(timeToTravel = mapply(compute_time_diff, DateTimeFirstDetectedAtCU, DatetimeFirstDetectedAtCDorCS))
  
  
  newDF2 <- newDF %>%
    mutate(UpstreamOrDownstream = case_when(DatetimeFirstDetectedAtCDorCS > DateTimeFirstDetectedAtCU ~ "Downstream", 
                                            DatetimeFirstDetectedAtCDorCS < DateTimeFirstDetectedAtCU ~ "Upstream"), 
           timeToTravelToSort = difftime(DatetimeFirstDetectedAtCDorCS, DateTimeFirstDetectedAtCU, units = "secs"), 
           timeToTravelReadable = mapply(compute_time_diff, DateTimeFirstDetectedAtCU, DatetimeFirstDetectedAtCDorCS))
  return(newDF2)
  
}


