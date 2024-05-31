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

is.sequential <- function(x){
  all(diff(x) == diff(x)[1])
}

extract_sequences <- function(tag_data, downstreamAntennas, middleAntennas, upstreamAntennas) {
  sequences <- data.frame(TAG = character(), 
                          DatetimeDetectedAtDownstreamAntennas = as.POSIXct(character()), 
                          DatetimeDetectedAtUpstreamAntennas = as.POSIXct(character()), 
                          MovementDirection = character(), 
                          stringsAsFactors = FALSE)
  i <- 1
  # xx <<- middleAntennas
  # sequenceAntennas <- 
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
      #if the time the fish hit the upstream antenna is mor recent than the time the fish hit the downstream one, then it was a upstream movement
      if (tag_data$Datetime[upstreamAntennas_first] > tag_data$Datetime[downstreamAntennas_first]) {
        
        # Find the last downstream antenna event that occurs before the first upstream antenna event
        #gets all downstream antenna isntances b
        downstreamAntennas_before <- downstreamAntennas_index[downstreamAntennas_index < (upstreamAntennas_first - i + 1)]
        
        if (length(downstreamAntennas_before) > 0) {
          #could i just do tail() here?
          downstreamAntennas_last <- i + downstreamAntennas_before[length(downstreamAntennas_before)] - 1
          ###for the middle antennas, check if there were any middle antenna instances in between those 2 antenna 
          if(isTruthy(middleAntennas)){
            print("Middle antennas present")
            #check if next antenna is the next middle antenna; needs to be or it breaks
            middleAntennaStartIndex <- downstreamAntennas_last
            for (antenna_i in middleAntennas) {
              print(paste("middleStart index:", middleAntennaStartIndex))
              #if the middle antenna occurs in succession, keep checkin gthe next ones, starting at the last antenna detection on the middle antennas
              if(grepl(paste0("^(", paste(antenna_i, collapse = "|"), ")"), tag_data$Event[middleAntennaStartIndex+1])){
                #get all indices between those 
                middleAntennas_before <- grep(paste0("^(", paste(antenna_i, collapse = "|"), ")"), tag_data$Event[middleAntennaStartIndex:upstreamAntennas_first])
                #adjusted indices for tag_data
                adjusted_indices <- middleAntennas_before + middleAntennaStartIndex - 1
                # if all the indices occur in a row (ie, no other antennas in between) then it's a sequence
                if(is.sequential(adjusted_indices)){
                  #this is the last instance of the middle antenna before the next antenna in the sequence
                  middleAntennaStartIndex <- tail(adjusted_indices, n = 1)
                } else{
                  print(paste("Fish hit other antennas than ", i, "after", downstreamAntennas, "so sequence doesn't work"))
                  break
                }

              } else {
                print(paste("Fish hit", tag_data$Event[middleAntennaStartIndex+1], "instead of", i, "after", downstreamAntennas, "so sequence doesn't work"))
                
                break
              }
            }
            #check if the middle antenna hit last middle antenna right before most upstream antenna (in addition to correct seqeunce above), then it's a match, should be able to add that to the df
            if(grepl(paste0("^(", paste(middleAntennas[[length(middleAntennas)]], collapse = "|"), ")"), tag_data$Event[upstreamAntennas_first -1])){
              sequences <- rbind(sequences, data.frame(
                TAG = tag_data$TAG[upstreamAntennas_first], 
                DatetimeDetectedAtDownstreamAntennas = tag_data$Datetime[downstreamAntennas_last], 
                DatetimeDetectedAtUpstreamAntennas = tag_data$Datetime[upstreamAntennas_first],
                MovementDirection = "Upstream"
              ))
            } else{
              print(paste("Tag didn't hit ", paste(middleAntennas[[length(middleAntennas)]], collapse = ", "), "immediately before ", upstreamAntennas ))
              break
            }
          } 
          
          
          
          
          #if there is no middle antennas selected, check to see if there's any events in between the upstream antenna and downstream one. 
          #if there are events, then don't add that instance to the df, if there aren't any, add it
          # # Check for any other antenna detections in between 
          in_between_events <- tag_data$Event[(downstreamAntennas_last + 1):(upstreamAntennas_first - 1)]
          if (all(grepl(paste0("^(", paste(c(downstreamAntennas, upstreamAntennas), collapse = "|"), ")"), in_between_events))) {
            # Append a new row to the sequences dataframe
            sequences <- rbind(sequences, data.frame(
              TAG = tag_data$TAG[upstreamAntennas_first],
              DatetimeDetectedAtDownstreamAntennas = tag_data$Datetime[downstreamAntennas_last],
              DatetimeDetectedAtUpstreamAntennas = tag_data$Datetime[upstreamAntennas_first],
              MovementDirection = "Upstream"
            ))
          }
          
          # Update the index to the position after the first upstream antenna event found
          i <- upstreamAntennas_first + 1
        } else {
          # If no downstream event occurs before the upstream event, skip to the next upstream event
          i <- upstreamAntennas_first + 1
        }
      }
      
      # Movement from Upstream to Downstream
      # if (tag_data$Datetime[downstreamAntennas_first] > tag_data$Datetime[upstreamAntennas_first]) {
      #   # Find the last upstream antenna event that occurs before the first downstream antenna event
      #   upstreamAntennas_before <- upstreamAntennas_index[upstreamAntennas_index < (downstreamAntennas_first - i + 1)]
      #   
      #   if (length(upstreamAntennas_before) > 0) {
      #     upstreamAntennas_last <- i + upstreamAntennas_before[length(upstreamAntennas_before)] - 1
      #     
      #     # Check for any other antenna detections in between
      #     in_between_events <- tag_data$Event[(upstreamAntennas_last + 1):(downstreamAntennas_first - 1)]
      #     if (all(grepl(paste0("^(", paste(c(downstreamAntennas, upstreamAntennas), collapse = "|"), ")"), in_between_events))) {
      #       # Append a new row to the sequences dataframe
      #       sequences <- rbind(sequences, data.frame(
      #         TAG = tag_data$TAG[downstreamAntennas_first], 
      #         DatetimeDetectedAtDownstreamAntennas = tag_data$Datetime[downstreamAntennas_first], 
      #         DatetimeDetectedAtUpstreamAntennas = tag_data$Datetime[upstreamAntennas_last],
      #         MovementDirection = "Downstream"
      #       ))
      #     }
      #     
      #     # Update the index to the position after the first downstream antenna event found
      #     i <- downstreamAntennas_first + 1
      #   } else {
      #     # If no upstream event occurs before the downstream event, skip to the next downstream event
      #     i <- downstreamAntennas_first + 1
      #   }
      # }
    } else {
      # Exit the loop if there are no more upstream or downstream antenna events in the remaining data
      break
    }
  }
  
  return(sequences)
}

summarizedDf <- function(All_Events, downstreamAntennas, middle_antennas, upstreamAntennas){
  df_filtered <- All_Events %>%
    #filter(grepl(paste0("^(", paste(c(downstreamAntennas, upstreamAntennas), collapse = "|"), ")"), Event)) %>%
    arrange(TAG, Datetime)
  
  # Apply the function to each TAG
  newDF <- df_filtered %>%
    group_by(TAG) %>%
    do(extract_sequences(., downstreamAntennas, middle_antennas, upstreamAntennas))
  
  # Add columns for time difference
  newDF2 <- newDF %>%
    mutate(`Time Between US/DS Detections (User Friendly)` = mapply(compute_time_diff, DatetimeDetectedAtUpstreamAntennas, DatetimeDetectedAtDownstreamAntennas),
           `Time Between US/DS Detections (For Sorting)` = difftime(DatetimeDetectedAtDownstreamAntennas, DatetimeDetectedAtUpstreamAntennas, units = "secs"))
  
  return(newDF2)
}

# All_Events <- combinedData_df_list$All_Events
middleAntennas <- list(c("HP"), c("CU", "CD"))
upstreamAntennas <- "CF"
downstreamAntennas <- c("RB")
# data <- summarizedDf(All_Events, downstreamAntennas, upstreamAntennas)
