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
  sequences <- data.frame(TAG = character(), 
                          DatetimeDetectedAtDownstreamAntennas = as.POSIXct(character()), 
                          DatetimeDetectedAtUpstreamAntennas = as.POSIXct(character()), 
                          MovementDirection = character(), 
                          stringsAsFactors = FALSE)
  i <- 1
  
  while (i < nrow(tag_data)) {
    
    # Find indices of events that match the chosen downstream antennas within the subset of tag_data starting from the current index i
    downstreamAntennas_index <- which(grepl(paste0("^(", paste(downstreamAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    
    # Find indices of events that match the chosen upstream antennas within the subset of tag_data starting from the current index i
    upstreamAntennas_index <- which(grepl(paste0("^(", paste(upstreamAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    
    # Check if both upstream and downstream antenna events exist in the remaining data
    if (length(upstreamAntennas_index) > 0 & length(downstreamAntennas_index) > 0) {
      
      # Get the absolute position in tag_data of the first upstream antenna event found
      upstreamAntennas_first <- i + upstreamAntennas_index[1] - 1
      
      # Get the absolute position in tag_data of the first downstream antenna event found
      downstreamAntennas_first <- i + downstreamAntennas_index[1] - 1
      
      # Movement from Downstream to Upstream
      if (tag_data$Datetime[upstreamAntennas_first] > tag_data$Datetime[downstreamAntennas_first]) {
        # Find the last downstream antenna event that occurs before the first upstream antenna event
        downstreamAntennas_before <- downstreamAntennas_index[downstreamAntennas_index < (upstreamAntennas_first - i + 1)]
        
        if (length(downstreamAntennas_before) > 0) {
          downstreamAntennas_last <- i + downstreamAntennas_before[length(downstreamAntennas_before)] - 1
          
          # Append a new row to the sequences dataframe
          sequences <- rbind(sequences, data.frame(
            TAG = tag_data$TAG[upstreamAntennas_first], 
            DatetimeDetectedAtDownstreamAntennas = tag_data$Datetime[downstreamAntennas_last], 
            DatetimeDetectedAtUpstreamAntennas = tag_data$Datetime[upstreamAntennas_first],
            MovementDirection = "Upstream"
          ))
          
          # Update the index to the position after the first upstream antenna event found
          i <- upstreamAntennas_first + 1
        } else {
          # If no downstream event occurs before the upstream event, skip to the next upstream event
          i <- upstreamAntennas_first + 1
        }
      }
      
      # Movement from Upstream to Downstream
      if (tag_data$Datetime[downstreamAntennas_first] > tag_data$Datetime[upstreamAntennas_first]) {
        # Find the last upstream antenna event that occurs before the first downstream antenna event
        upstreamAntennas_before <- upstreamAntennas_index[upstreamAntennas_index < (downstreamAntennas_first - i + 1)]
        
        if (length(upstreamAntennas_before) > 0) {
          upstreamAntennas_last <- i + upstreamAntennas_before[length(upstreamAntennas_before)] - 1
          
          # Append a new row to the sequences dataframe
          sequences <- rbind(sequences, data.frame(
            TAG = tag_data$TAG[downstreamAntennas_first], 
            DatetimeDetectedAtDownstreamAntennas = tag_data$Datetime[downstreamAntennas_first], 
            DatetimeDetectedAtUpstreamAntennas = tag_data$Datetime[upstreamAntennas_last],
            MovementDirection = "Downstream"
          ))
          
          # Update the index to the position after the first downstream antenna event found
          i <- downstreamAntennas_first + 1
        } else {
          # If no upstream event occurs before the downstream event, skip to the next downstream event
          i <- downstreamAntennas_first + 1
        }
      }
    } else {
      # Exit the loop if there are no more upstream or downstream antenna events in the remaining data
      break
    }
  }
  
  return(sequences)
}

summarizedDf <- function(All_Events, downstreamAntennas, upstreamAntennas){
  df_filtered <- All_Events %>%
    filter(grepl(paste0("^(", paste(c(downstreamAntennas, upstreamAntennas), collapse = "|"), ")"), Event)) %>%
    arrange(TAG, Datetime)
  
  # Apply the function to each TAG
  newDF <- df_filtered %>%
    group_by(TAG) %>%
    do(extract_sequences(., downstreamAntennas, upstreamAntennas))
  
  # Add columns for time difference
  newDF2 <- newDF %>%
    mutate(`Time Between US/DS Detections (User Friendly)` = mapply(compute_time_diff, DatetimeDetectedAtUpstreamAntennas, DatetimeDetectedAtDownstreamAntennas),
           `Time Between US/DS Detections (For Sorting)` = difftime(DatetimeDetectedAtDownstreamAntennas, DatetimeDetectedAtUpstreamAntennas, units = "secs"))
  
  return(newDF2)
}

# All_Events <- combinedData_df_list$All_Events
# upstreamAntennas <- "CU"
#downstreamAntennas <- c("CD", "CS")
data <- summarizedDf(All_Events, downstreamAntennas, upstreamAntennas)
