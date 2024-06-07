#helper funtion to make times into easy to read times
computeTimeDiff <- function(start, end) {
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
#tells if a vector is consecutive with incrememnts of 1 
#example: c(5,7), c(1,4,5), c(8,2) return false, c(2,3,4) and c(2,3) return TRUE
is.sequential <- function(x) {
  length(x) < 2 || all(diff(x) == 1)
}

#used if the last antenna selected is the same as the first antenna; 
#gets instances where the last antenna occured after the first antenna
findSameLastAntennaIndices <- function(firstAntennas_firstOccurrance, firstAntennas_indices) {
  # Sort the vector to ensure it's in ascending order
  sortedIndices <- sort(firstAntennas_indices)
  
  # Find the position of first antenna in the sorted vector
  firstAntennas_firstOccurrance_Index <- which(sortedIndices == firstAntennas_firstOccurrance)
  
  # Initialize a list to hold the last antenna positions
  lastSameAntenna_Indices <- list()
  
  # Check if firstAntennas_firstOccurrance_Index is length 0 or First is the last element
  #only evaluate the seond part if the first part isn't true (||)
  if (length(firstAntennas_firstOccurrance_Index) == 0 || firstAntennas_firstOccurrance_Index == length(sortedIndices)) {
    return(lastSameAntenna_Indices)  # No upstream value if downstream is the last element
  }
  
  # Traverse from the first position to collect non-sequential elements
  for (i in (firstAntennas_firstOccurrance_Index + 1):length(sortedIndices)) {
    if (sortedIndices[i] != sortedIndices[i - 1] + 1) {
      lastSameAntenna_Indices <- c(lastSameAntenna_Indices, sortedIndices[i:length(sortedIndices)])
      break
    }
  }
  
  return(lastSameAntenna_Indices)
}

extractSequences <- function(tag_data, firstAntennas, middleAntennas, lastAntennas) {
  
  sequences <- data.frame(TAG = character(),
                          DatetimeDetectedAtFirstAntennas = as.POSIXct(character()),
                          DatetimeDetectedAtLastAntennas = as.POSIXct(character()),
                          stringsAsFactors = FALSE)
  i <- 1
  tagNumber = unique(tag_data$TAG)
  while (i < nrow(tag_data)) {

    # Find indices of events that match the chosen downstream antennas within the subset of tag_data starting from the current index i
    firstAntennas_indices <- which(grepl(paste0("^(", paste(firstAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
    ### if first/last antennas aren't the same, can run the antenna index to get where the last antenna is after the first antenna statrtting from current index i
    if(all(firstAntennas != lastAntennas)){
      # Find indices of events that match the chosen upstream antennas within the subset of tag_data starting from the current index i
      lastAntennas_indices <- which(grepl(paste0("^(", paste(lastAntennas, collapse = "|"), ")"), tag_data$Event[i:nrow(tag_data)]))
      
    } else {
      #if the antennas are the same, find most upstream; if 
      firstAntennas_firstOccurrance <- i + firstAntennas_indices[1] - 1
      #gets indexes of times where the last antenna occurred after the first one
      lastAntennas_indices <- unlist(findSameLastAntennaIndices(firstAntennas_firstOccurrance = firstAntennas_firstOccurrance, firstAntennas_indices = firstAntennas_indices))

    }

    # Check if both upstream and downstream antenna events exist in the remaining data, and if not, move to the next tag
    if (length(firstAntennas_indices) > 0 & length(lastAntennas_indices) > 0) {
      # Get the absolute position in tag_data of the first downstream antenna event found
      #this is why it's corrected by addiung for i
      firstAntennas_firstOccurrance <- i + firstAntennas_indices[1] - 1
      lastAntennas_firstOccurrance <- i + lastAntennas_indices[1] - 1

      # Movement from first to last
      #if the time the fish hit the upstream antenna is mor recent than the time the fish hit the downstream one, then it was a upstream movement
      if (tag_data$Datetime[firstAntennas_firstOccurrance] < tag_data$Datetime[lastAntennas_firstOccurrance]) {

        # Find the last downstream antenna event that occurs before the first upstream antenna event
        #gets all downstream antenna instances before the last antenna's first occurance
        firstAntennas_before <- firstAntennas_indices[firstAntennas_indices < (lastAntennas_firstOccurrance - i + 1)]

        if (length(firstAntennas_before) > 0) {
          #could i just do tail() here?
          firstAntennas_lastOccurrance <- i + firstAntennas_before[length(firstAntennas_before)] - 1
          ###for the middle antennas, check if there were any middle antenna instances in between those 2 antenna
          if(isTruthy(middleAntennas)){
            #check if next antenna is the next middle antenna; needs to be or it breaks
            middleAntennaStartIndex <- firstAntennas_lastOccurrance
            #as soon as there is an antenna out of order the loop breaks, and this variable returns false, 
            middleAntennasSequential <- FALSE
            for (antenna_i in middleAntennas) {
              #antenna_i can be 1 antenna or a vector of them
              #if the middle antennas occurs in succession right after the last detection on the first antenna, keep checking the next ones, starting at the last antenna detection on the middle antennas
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
            #if the middle antennas passed the test (they occurred in correct sequence),
            #then check if the middle antenna hit last middle antenna right before the last antenna selected,
            #then it's a match, add that to the df
            if(middleAntennasSequential){
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

getSequences <- function(All_Events, firstAntennas, middle_antennas, lastAntennas, mobileCodes){
  
  allEventsArranged <- All_Events %>%
    dplyr::filter(!Event %in% c("Recapture", "Recapture and Release", mobileCodes)) %>%
    #can add in this line if we want it to be ok with having other antennas in between first/last antennas
    #filter(grepl(paste0("^(", paste(c(firstAntennas, lastAntennas), collapse = "|"), ")"), Event)) %>%
    arrange(TAG, Datetime)
  
  # Apply the extractSequences function to each TAG
  sequences <- allEventsArranged %>%
    group_by(TAG) %>%
    do(extractSequences(., firstAntennas, middle_antennas, lastAntennas))
  # Add columns for time difference
  sequencesWithTiming <- sequences %>%
    mutate(`Time Between First/Last Detections (User Friendly)` = mapply(computeTimeDiff, DatetimeDetectedAtLastAntennas, DatetimeDetectedAtFirstAntennas),
           `Time Between First/Last Detections (For Sorting)` = difftime(DatetimeDetectedAtFirstAntennas, DatetimeDetectedAtLastAntennas, units = "secs")) %>%
    distinct()
  
  return(sequencesWithTiming)
}

##variables used to just help troubleshoot
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
# middleAntennas <- list(c("HP"))
# lastAntennas <- "CF"
# firstAntennas <- c("RB")
# data <- summarizedDf(All_Events, firstAntennas, middleAntennas, lastAntennas)
