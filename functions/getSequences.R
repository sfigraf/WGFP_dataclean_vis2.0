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

extract_sequences <- function(tag_data, downstreamAntennas, upstreamAntennas) {
  sequences <- data.frame(TAG = character(), DatetimeFirstDetectedAtDownstreamAntennas = as.POSIXct(character()), DatetimeFirstDetectedAtUpstreamAntennas = as.POSIXct(character()), stringsAsFactors = FALSE)
  i <- 1
  
  while (i < nrow(tag_data)) {
    
    downstreamAntennas_index <- which(grepl(paste0("^(", paste(downstreamAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    upstreamAntennas_index <- which(grepl(paste0("^(", paste(upstreamAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    
    if (length(upstreamAntennas_index) > 0 & length(downstreamAntennas_index) > 0) {
      upstreamAntennas_first <- i + upstreamAntennas_index[1] - 1
      downstreamAntennas_first <- i + downstreamAntennas_index[1] - 1
      
      if (tag_data$Datetime[upstreamAntennas_first] < tag_data$Datetime[downstreamAntennas_first]) {
        sequences <- rbind(sequences, data.frame(TAG = tag_data$TAG[upstreamAntennas_first], DatetimeFirstDetectedAtDownstreamAntennas = tag_data$Datetime[downstreamAntennas_first], DatetimeFirstDetectedAtUpstreamAntennas = tag_data$Datetime[upstreamAntennas_first]))
        i <- downstreamAntennas_first + 1
      } else {
        sequences <- rbind(sequences, data.frame(TAG = tag_data$TAG[downstreamAntennas_first], DatetimeFirstDetectedAtDownstreamAntennas = tag_data$Datetime[downstreamAntennas_first], DatetimeFirstDetectedAtUpstreamAntennas = tag_data$Datetime[upstreamAntennas_first]))
        i <- upstreamAntennas_first + 1
      }
    } else {
      break
    }
  }
  
  return(sequences)
}

##sequences function
summarizedDf <- function(All_Events, downstreamAntennas, upstreamAntennas){
  df_filtered <- All_Events %>%
    filter(grepl(paste0("^(", paste(c(downstreamAntennas, upstreamAntennas), collapse = "|"), ")"), Event)) %>%
    arrange(TAG, Datetime)
  
  
  
  # Apply the function to each TAG
  newDF <- df_filtered %>%
    group_by(TAG) %>%
    do(extract_sequences(., downstreamAntennas, upstreamAntennas))
  
  
  
  # Add timeToTravel column with appropriate units
  # sequencesDF <- newDF %>%
  #   mutate(timeToTravel = mapply(compute_time_diff, DatetimeFirstDetectedAtUpstreamAntennas, DatetimeFirstDetectedAtDownstreamAntennas))
  
  
  newDF2 <- newDF %>%
    mutate(`Upstream Or Downstream Movement` = case_when(DatetimeFirstDetectedAtDownstreamAntennas > DatetimeFirstDetectedAtUpstreamAntennas ~ "Downstream", 
                                            DatetimeFirstDetectedAtDownstreamAntennas < DatetimeFirstDetectedAtUpstreamAntennas ~ "Upstream"), 
           `Time Between US/DS Detections (User Friendly)` = mapply(compute_time_diff, DatetimeFirstDetectedAtUpstreamAntennas, DatetimeFirstDetectedAtDownstreamAntennas),
           `Time Between US/DS Detections (For Sorting)` = difftime(DatetimeFirstDetectedAtDownstreamAntennas, DatetimeFirstDetectedAtUpstreamAntennas, units = "secs")
    )
  return(newDF2)
  
}

#All_Events <- combinedData_df_list$All_Events
