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
  sequences <- data.frame(TAG = character(), DatetimeDetectedAtDownstreamAntennas = as.POSIXct(character()), DatetimeDetectedAtUpstreamAntennas = as.POSIXct(character()), stringsAsFactors = FALSE)
  i <- 1
  
  # Initialize a while loop to iterate through the tag_data dataframe
  while (i < nrow(tag_data)) {
    
    # Find indices of events that match the chosen downstream antennas within the subset of tag_data starting from the current index i
    downstreamAntennas_index <- which(grepl(paste0("^(", paste(downstreamAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    
    # Find indices of events that match the chosen upstream antennas within the subset of tag_data starting from the current index i
    upstreamAntennas_index <- which(grepl(paste0("^(", paste(upstreamAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    
    # Check if both upstream and downstream antenna events exist in the remaining data
    if (length(upstreamAntennas_index) > 0 & length(downstreamAntennas_index) > 0) {
      
      # Get the absolute position in tag_data of the first upstream antenna event found
      upstreamAntennas_first <- i + upstreamAntennas_index[1] - 1
      
      # Find the last downstream antenna event that occurs before the first upstream antenna event
      downstreamAntennas_before <- downstreamAntennas_index[downstreamAntennas_index < (upstreamAntennas_first - i + 1)]
      
      if (length(downstreamAntennas_before) > 0) {
        downstreamAntennas_last <- i + downstreamAntennas_before[length(downstreamAntennas_before)] - 1
        
        # Append a new row to the sequences dataframe with TAG, datetime of last downstream detection, and datetime of first upstream detection
        sequences <- rbind(sequences, data.frame(
          TAG = tag_data$TAG[upstreamAntennas_first], 
          DatetimeDetectedAtDownstreamAntennas = tag_data$Datetime[downstreamAntennas_last], 
          DatetimeDetectedAtUpstreamAntennas = tag_data$Datetime[upstreamAntennas_first]
        ))
        
        # Update the index to the position after the first upstream antenna event found
        i <- upstreamAntennas_first + 1
      } else {
        # If no downstream event occurs before the upstream event, skip to the next upstream event
        i <- upstreamAntennas_first + 1
      }
    } else {
      # Exit the loop if there are no more upstream or downstream antenna events in the remaining data
      break
    }
  }
  
  return(sequences)
}

## sequences function
summarizedDf <- function(All_Events, downstreamAntennas, upstreamAntennas){
  df_filtered <- All_Events %>%
    filter(grepl(paste0("^(", paste(c(downstreamAntennas, upstreamAntennas), collapse = "|"), ")"), Event)) %>%
    arrange(TAG, Datetime)
  
  # Apply the function to each TAG
  newDF <- df_filtered %>%
    group_by(TAG) %>%
    do(extract_sequences(., downstreamAntennas, upstreamAntennas))
  
  # Add columns for movement direction and time difference
  newDF2 <- newDF %>%
    mutate(`Upstream Or Downstream Movement` = case_when(
      DatetimeDetectedAtDownstreamAntennas > DatetimeDetectedAtUpstreamAntennas ~ "Downstream", 
      DatetimeDetectedAtDownstreamAntennas < DatetimeDetectedAtUpstreamAntennas ~ "Upstream"
    ), 
    `Time Between US/DS Detections (User Friendly)` = mapply(compute_time_diff, DatetimeDetectedAtUpstreamAntennas, DatetimeDetectedAtDownstreamAntennas),
    `Time Between US/DS Detections (For Sorting)` = difftime(DatetimeDetectedAtDownstreamAntennas, DatetimeDetectedAtUpstreamAntennas, units = "secs")
    )
  return(newDF2)
}

# All_Events <- combinedData_df_list$All_Events
# upstreamAntennas <- "CU"
#downstreamAntennas <- c("CD", "CS")
data <- summarizedDf(All_Events, downstreamAntennas, upstreamAntennas)
