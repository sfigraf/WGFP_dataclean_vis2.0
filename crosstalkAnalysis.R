####crosstalk analysis to see duplicate detections/exact same timestamp
#want to add this qaqc tab

allDetections <- combinedData_df_list$All_Events
cd1cd2 <- allDetections %>%
  filter(Event %in% c("CD1", "CD2"), 
         !str_detect(TAG, "^0000000|^999")) 

cd1cd2Crosstalk <- cd1cd2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(30/3280)*100

########rb2=1, 2
rb1rb2 <- allDetections %>%
  filter(Event %in% c("RB1", "RB2"), 
         !str_detect(TAG, "^0000000|^999")) 

rb1rb2Crosstalk <- rb1rb2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(81/168327)*100

#######
hp1hp2 <- allDetections %>%
  filter(Event %in% c("HP3", "HP4"), 
         !str_detect(TAG, "^0000000|^999")) 

hp1hp2Crosstalk <- hp1hp2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(72/1105413)*100

##
CF1CF2 <- allDetections %>%
  filter(Event %in% c("CF5", "CF6"), 
         !str_detect(TAG, "^0000000|^999")) 

CF1CF2Crosstalk <- CF1CF2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(1/417614)*100

##CS Crosstalk
CS1CS2 <- allDetections %>%
  filter(Event %in% c("CS1", "CS2"), 
         !str_detect(TAG, "^0000000|^999")) 

CS1CS2Crosstalk <- CS1CS2 %>%
  group_by(Datetime, TAG) %>%
  summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
  filter(nchar(antennas) > 3)

(1/417614)*100
