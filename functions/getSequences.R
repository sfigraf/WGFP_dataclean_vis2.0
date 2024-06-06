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

# is.sequential <- function(x){
#   all(diff(x) == diff(x)[1])
# }
is.sequential <- function(x) {
  length(x) < 2 || all(diff(x) == 1)
}
findSameLastAntennaIndices <- function(firstAntennas_firstOccurrance, firstAntennas_indices) {
  # Sort the vector to ensure it's in ascending order
  sorted_indices <- sort(firstAntennas_indices)
  
  # Find the position of downstream in the sorted vector
  firstAntennas_firstOccurrance_Index <- which(sorted_indices == firstAntennas_firstOccurrance)
  
  # Initialize a list to hold the upstream positions
  lastSameAntenna_Indices <- list()
  
  # Check if firstAntennas_firstOccurrance_Index isTruthy or downstream is the last element
  #only evalkuate the seond part if tthe first part isn't true (||)
  if (length(firstAntennas_firstOccurrance_Index) == 0 || firstAntennas_firstOccurrance_Index == length(sorted_indices)) { #!isTruthy(firstAntennas_firstOccurrance_Index)
    return(lastSameAntenna_Indices)  # No upstream value if downstream is the last element
  }
  
  # Traverse from the downstream position to collect non-sequential elements
  for (i in (firstAntennas_firstOccurrance_Index + 1):length(sorted_indices)) {
    if (sorted_indices[i] != sorted_indices[i - 1] + 1) {
      lastSameAntenna_Indices <- c(lastSameAntenna_Indices, sorted_indices[i:length(sorted_indices)])
      break
    }
  }
  
  return(lastSameAntenna_Indices)
}

extract_sequences <- function(tag_data, firstAntennas, middleAntennas, lastAntennas) {
  
  sequences <- data.frame(TAG = character(),
                          DatetimeDetectedAtFirstAntennas = as.POSIXct(character()),
                          DatetimeDetectedAtLastAntennas = as.POSIXct(character()),
                          stringsAsFactors = FALSE)
  i <- 1
  tagNumber = unique(tag_data$TAG)
  print(paste("tag number",  tagNumber))
  while (i < nrow(tag_data)) {

    # Find indices of events that match the chosen downstream antennas within the subset of tag_data starting from the current index i
    firstAntennas_indices <- which(grepl(paste0("^(", paste(firstAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    #print(paste("DS index", firstAntennas_indices))
    ### if antennas aren't the same, can just run the antenna index
    #print(paste("DS and US antennas", c(firstAntennas, lastAntennas)))
    if(all(firstAntennas != lastAntennas)){
      #if middle antennas aren't in first or last
      # Find indices of events that match the chosen upstream antennas within the subset of tag_data starting from the current index i
      lastAntennas_indices <- which(grepl(paste0("^(", paste(lastAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
      #print(paste("upstream indeeexes", lastAntennas_indices))
      #if the antennas are the same, find most upstream; if 
    } else {
      firstAntennas_firstOccurrance <- i + firstAntennas_indices[1] - 1
      #print(paste("antennas are the same, downstream first:", firstAntennas_firstOccurrance))
      #gets indexes of times where the last antenna occurred
      lastAntennas_indices <- unlist(findSameLastAntennaIndices(firstAntennas_firstOccurrance = firstAntennas_firstOccurrance, firstAntennas_indices = firstAntennas_indices))
      #print(paste("upstream indxes", lastAntennas_indices))
      
    }
    # lastAntennas_firstOccurrance <- 53
    # firstAntennas_firstOccurrance <- 47

    # Check if both upstream and downstream antenna events exist in the remaining data
    #print(paste("DS length", length(firstAntennas_indices), "and US length", length(lastAntennas_indices)))
    if (length(firstAntennas_indices) > 0 & length(lastAntennas_indices) > 0) {
      # Get the absolute position in tag_data of the first downstream antenna event found
      #this is why it's corrected by addiung for i
      firstAntennas_firstOccurrance <- i + firstAntennas_indices[1] - 1
      lastAntennas_firstOccurrance <- i + lastAntennas_indices[1] - 1
      # if(isTruthy(middleAntennas)){
      #   #if there are middle antennas, check 
      # }
      

        


      # Movement from first to last
      #if the time the fish hit the upstream antenna is mor recent than the time the fish hit the downstream one, then it was a upstream movement
      if (tag_data$Datetime[firstAntennas_firstOccurrance] < tag_data$Datetime[lastAntennas_firstOccurrance]) {

        # Find the last downstream antenna event that occurs before the first upstream antenna event
        #gets all downstream antenna instances b
        firstAntennas_before <- firstAntennas_indices[firstAntennas_indices < (lastAntennas_firstOccurrance - i + 1)]

        if (length(firstAntennas_before) > 0) {
          #could i just do tail() here?
          firstAntennas_lastOccurrance <- i + firstAntennas_before[length(firstAntennas_before)] - 1
          ###for the middle antennas, check if there were any middle antenna instances in between those 2 antenna
          if(isTruthy(middleAntennas)){
            #print("Middle antennas present")
            #check if next antenna is the next middle antenna; needs to be or it breaks
            middleAntennaStartIndex <- firstAntennas_lastOccurrance
            
            middleAntennasSequential <- FALSE
            for (antenna_i in middleAntennas) {
              #antenna_i can be 1 antenna or a vector of them
              print(paste("middleStart index:", middleAntennaStartIndex))
              #if the middle antennas occurs in succession right after the last detection on the first antenna, keep checkin gthe next ones, starting at the last antenna detection on the middle antennas
              if(grepl(paste0("^(", paste(antenna_i, collapse = "|"), ")"), tag_data$Event[middleAntennaStartIndex+1])){
                #get all indices of any middle detections between those. if antenna_i is a vector, doesn't matter which antennas of that vector get hit
                middleAntennas_before <- grep(paste0("^(", paste(antenna_i, collapse = "|"), ")"), tag_data$Event[middleAntennaStartIndex:lastAntennas_firstOccurrance])
                #adjusted indices for tag_data
                adjusted_indices <- middleAntennas_before + middleAntennaStartIndex - 1
                # if all the indices occur in a row (ie, no other antennas in between) then it's a sequence
                
                if(is.sequential(adjusted_indices)){
                  #this is the last instance of the middle antenna before the next antenna in the sequence
                  middleAntennaStartIndex <- tail(adjusted_indices, n = 1)
                  middleAntennasSequential <- TRUE
                  
                } else{
                  print(paste("Fish", tagNumber, " hit other antennas than ", antenna_i, "after", firstAntennas, "so sequence doesn't work"))
                  #breaks for-loop this is in, not overarching while-loop
                  middleAntennasSequential <- FALSE
                  break
                }

              } else {
                print(paste("Fish", tagNumber, " hit", tag_data$Event[middleAntennaStartIndex+1], "instead of", antenna_i, "after", firstAntennas, "so sequence doesn't work"))
                middleAntennasSequential <- FALSE
                break
              }
            }
            #check if the middle antenna hit last middle antenna right before most upstream antenna (in addition to correct seqeunce above),
            #then it's a match, should be able to add that to the df
            if(middleAntennasSequential == TRUE){
              if(grepl(paste0("^(", paste(middleAntennas[[length(middleAntennas)]], collapse = "|"), ")"), tag_data$Event[lastAntennas_firstOccurrance -1])){
                sequences <- rbind(sequences, data.frame(
                  TAG = tag_data$TAG[lastAntennas_firstOccurrance],
                  DatetimeDetectedAtFirstAntennas = tag_data$Datetime[firstAntennas_lastOccurrance],
                  DatetimeDetectedAtLastAntennas = tag_data$Datetime[lastAntennas_firstOccurrance]
                ))
              } else{
                print(paste("Fish", tagNumber, " didn't hit ", paste(middleAntennas[[length(middleAntennas)]], collapse = ", "), "immediately before ", lastAntennas ))
                break
              }
            }
          } else{
            #if there is no middle antennas selected, check to see if there's any events in between the upstream antenna and downstream one.
            #if there are events, then don't add that instance to the df, if there aren't any, add it
            # # Check for any other antenna detections in between
            #this may be simpler if we just have a "if the difference beween rows == 1 (if the rows are right next to each other) then add to df
            #i think this thought process is from left over code from another way of doing things...something to think about. It works so why not keep using
            in_between_events <- tag_data$Event[(firstAntennas_lastOccurrance + 1):(lastAntennas_firstOccurrance - 1)]
            if (all(grepl(paste0("^(", paste(c(firstAntennas, lastAntennas), collapse = "|"), ")"), in_between_events))) {
              # Append a new row to the sequences dataframe
              sequences <- rbind(sequences, data.frame(
                TAG = tag_data$TAG[lastAntennas_firstOccurrance],
                DatetimeDetectedAtFirstAntennas = tag_data$Datetime[firstAntennas_lastOccurrance],
                DatetimeDetectedAtLastAntennas = tag_data$Datetime[lastAntennas_firstOccurrance]
              ))
            }
          }

          # Update the index to the position after the first upstream antenna event found
          i <- lastAntennas_firstOccurrance 
        } else {
          # If no downstream event occurs before the upstream event, skip to the next upstream event
          i <- lastAntennas_firstOccurrance 
        }
      } else {
        i <- firstAntennas_firstOccurrance 
      }
    } else {
      # Exit the loop if there are no more upstream or downstream antenna events in the remaining data
      break
    }
  }

  return(sequences)
}

summarizedDf <- function(All_Events, firstAntennas, middle_antennas, lastAntennas, mobileCodes){
  df_filtered <- All_Events %>%
    dplyr::filter(!Event %in% c("Recapture", "Recapture and Release", mobileCodes)) %>%
    #filter(grepl(paste0("^(", paste(c(firstAntennas, lastAntennas), collapse = "|"), ")"), Event)) %>%
    arrange(TAG, Datetime)
  
  # Apply the function to each TAG
  newDF <<- df_filtered %>%
    group_by(TAG) %>%
    do(extract_sequences(., firstAntennas, middle_antennas, lastAntennas))
  print("got to here")
  # Add columns for time difference
  newDF2 <- newDF %>%
    mutate(`Time Between US/DS Detections (User Friendly)` = mapply(compute_time_diff, DatetimeDetectedAtLastAntennas, DatetimeDetectedAtFirstAntennas),
           `Time Between US/DS Detections (For Sorting)` = difftime(DatetimeDetectedAtFirstAntennas, DatetimeDetectedAtLastAntennas, units = "secs")) %>%
    distinct()
  
  return(newDF2)
}
# All_Events <- combinedData_df_list$All_Events
# mobileCodes <- metaDataVariableNames$MobileRunFrontendCodes
# df_filtered <- All_Events %>%
#   dplyr::filter(!Event %in% c("Recapture", "Recapture and Release", mobileCodes)) %>%
#   
#   #filter(grepl(paste0("^(", paste(c(downstreamAntennas, upstreamAntennas), collapse = "|"), ")"), Event)) %>%
#   arrange(TAG, Datetime)
# 
# # Apply the function to each TAG
# tag_data <- df_filtered %>%
#   filter(TAG == c("230000143277")) #230000224056
middleAntennas <- list(c("HP"))
lastAntennas <- "CF"
firstAntennas <- c("RB")
# data <- summarizedDf(All_Events, firstAntennas, middleAntennas, lastAntennas)
# # # # middle_antennas <- middleAntennas