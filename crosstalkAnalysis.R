####crosstalk analysis to see duplicate detections/exact same timestamp
#want to add this qaqc tab
crosstalkDF <- data.frame(
  "AntennaCodes" = character(),
  "PercentageOfDetectionsWithSameTimestamp" = numeric(),
  stringsAsFactors = FALSE
)
# SelectedAllEvents = All_Events
# antennaCodes <- metaDataVariableNames$RedBarnFrontendCodes
calculateCrosstalkProportion <- function(SelectedAllEvents, antennaCodes){
  
  antennaSpecificDetections <- SelectedAllEvents %>%
    filter(Event %in% antennaCodes) 
  
  antennaCrosstalk <- antennaSpecificDetections %>%
    group_by(Datetime, TAG) %>%
    summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
    filter(grepl(",", antennas))
  
  percentageOccurance <- round((nrow(antennaCrosstalk)/nrow(antennaSpecificDetections))*100, 3)
  
  return(percentageOccurance)
}

for(codes in list(metaDataVariableNames$RedBarnFrontendCodes, metaDataVariableNames$HitchingPostFrontendCodes,
                  metaDataVariableNames$ConfluenceFrontendCodes, metaDataVariableNames$ConnectivityChannelDownstreamFrontendCodes,
                  metaDataVariableNames$ConnectivityChannelSideChannelFrontendCodes, metaDataVariableNames$ConnectivityChannelUpstreamFrontendCodes)){

  crosstalkDF <- crosstalkDF %>%
    add_row(
      AntennaCodes = paste(codes, collapse = ", "), 
      PercentageOfDetectionsWithSameTimestamp = calculateCrosstalkProportion(SelectedAllEvents = All_Events, antennaCodes = codes)
    )
}


crosstalkDFFilled <- crosstalkDF %>%
  add_row(
    AntennaCodes = paste(antennaCodes, collapse = ", "), 
    PercentageOfDetectionsWithSameTimestamp = percentageOccurance
  )

metaDataVariableNames$ConnectivityChannelDownstreamFrontendCodes

#allEvents doesn't have marker tags in it
All_Events <- combinedData_df_list$All_Events

cd1cd2 <- All_Events %>%
  filter(Event %in% metaDataVariableNames$ConnectivityChannelDownstreamFrontendCodes) 

cd1cd2Crosstalk <- cd1cd2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(nrow(cd1cd2Crosstalk)/nrow(cd1cd2))*100

(30/3280)*100

########rb2=1, 2
rb1rb2 <- All_Events %>%
  filter(Event %in% metaDataVariableNames$RedBarnFrontendCodes) 

rb1rb2Crosstalk <- rb1rb2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(81/168327)*100

#######
hp1hp2 <- All_Events %>%
  filter(Event %in% c("HP3", "HP4"), 
         !str_detect(TAG, "^0000000|^999")) 

hp1hp2Crosstalk <- hp1hp2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(72/1105413)*100

##
CF1CF2 <- All_Events %>%
  filter(Event %in% c("CF5", "CF6"), 
         !str_detect(TAG, "^0000000|^999")) 

CF1CF2Crosstalk <- CF1CF2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(1/417614)*100

##CS Crosstalk
CS1CS2 <- All_Events %>%
  filter(Event %in% c("CS1", "CS2"), 
         !str_detect(TAG, "^0000000|^999")) 

CS1CS2Crosstalk <- CS1CS2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(1/417614)*100
