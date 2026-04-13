###Pressure Transducer Corrections
##note: usgs data and all events data in utc
#  lubridate::tz(ptdDataWide$dateTime)
#usgs15Min <- USGSData$USGS15Min
ptdDataWide <- PTData$PTDataWide

x <- ptdDataWide %>%
  mutate(gageDif = USGSGageHeightFt - Water_Level_NoIce_ft)
rbOnly <- x %>%
  filter(Site == "Red Barn") %>%
  select(dateTime, USGSDischarge, USGSGageHeightFt, Water_Level_NoIce_ft, gageDif)

plot <- rbOnly %>%
  ggplot(aes(x = gageDif, text = as.character(gageDif))) +
  geom_histogram()  +
  ggtitle("RB Only Difs") +
  theme_classic()
ggplotly(plot)

#rite_xlsx(rbOnly, "RedBarnUSGSandPTData.xlsx")
#make reverse rating curves for each year
##divide data into chunks based on calibration dates

calibrationDates <- read_csv("calibrationDates.csv", 
                             col_types = cols(DataCalStart = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                              DataCalEnd = col_datetime(format = "%m/%d/%Y %H:%M")))

calibrationDates <- calibrationDates %>%
  arrange(DataCalStart)

redBarn_list <- list()
for(i in 1:nrow(calibrationDates)) {
  
  # Define the current window
  current_start <- calibrationDates$DataCalStart[i]
  current_end   <- calibrationDates$DataCalEnd[i]
  Site <- calibrationDates$Site[i]
  
  # Filter red barn based on this window
  chunk <- rbOnly[rbOnly$dateTime >= current_start & rbOnly$dateTime <= current_end, ]
  
  # Store it in the list with a name (optional)
  # Assuming df1 has a 'site_name' or 'year' column to identify the chunk
  site_label <- paste0(Site, "_", i) 
  redBarn_list[[site_label]] <- chunk
}

allRBTransducerQAQC <- lapply(redBarn_list, pressureTransducerQAQCFunction)

# RED BARN 2020 -----------------------------------------------------------

allRBTransducerQAQC$`Red Barn_1`$plotIwthModels
intercept <- allRBTransducerQAQC$`Red Barn_1`$baseModelList$intercept_val
slope <- allRBTransducerQAQC$`Red Barn_1`$baseModelList$slope_val

summary <- allRBTransducerQAQC$`Red Barn_1`$baseModelList$summary
### Red bArn 2021 -----------------------------------------------------------
allRBTransducerQAQC$`Red Barn_2`$plotIwthModels
intercept <- allRBTransducerQAQC$`Red Barn_2`$baseModelList$intercept_val
slope <- allRBTransducerQAQC$`Red Barn_2`$baseModelList$slope_val

# red barn 2022 -----------------------------------------------------------
allRBTransducerQAQC$`Red Barn_3`$plotIwthModels
intercept <- allRBTransducerQAQC$`Red Barn_3`$noOutliersModelList$intercept_val

allRBTransducerQAQC$`Red Barn_3`$noOutliersModelList$summary
allRBTransducerQAQC$`Red Barn_3`$baseModelList$summary

slopeNoOutliers <- coef(allRBTransducerQAQC$`Red Barn_3`$noOutliersModelList$noOutliersModel)[2]
slopeWithOutliers <- coef(allRBTransducerQAQC$`Red Barn_3`$baseModelList$baseModel)[2]

### oart 2
allRBTransducerQAQC$`Red Barn_4`$plotIwthModels
intercept <- allRBTransducerQAQC$`Red Barn_4`$baseModelList$intercept_val
slope <- allRBTransducerQAQC$`Red Barn_4`$baseModelList$slope_val

intercept <- allRBTransducerQAQC$`Red Barn_4`$noOutliersModelList$intercept_val
slope <- coef(allRBTransducerQAQC$`Red Barn_4`$noOutliersModelList$noOutliersModel)[2]
# rbOnly2022 <- rbOnly %>%
#   filter(year(dateTime) == 2022, 
#          Water_Level_NoIce_ft > 0,
#          !is.na(gageDif))
# 
# rb2022Analyized <- pressureTransducerQAQCFunction(rbOnly2022)
# 
# rb2022Analyized$plotIwthModels
# rb2022WithOutliers <- rb2022Analyized$subsetDataWithOutliers
# 
# aprilGrouping <- rb2022WithOutliers %>%
#   filter(predictedValueDif < -.15)
# #date range of april correction
# # 2022-04-02 00:00:00
# # 2022-04-21 08:00:00
# #specify as UTC since that's what the data is in
# start_date <- as.POSIXct("2022-04-02 00:00:00", tz = "UTC")
# end_date   <- as.POSIXct("2022-04-21 08:00:00", tz = "UTC")
# aprilGrouping2 <- rb2022WithOutliers %>%
#   filter(dateTime >= start_date & dateTime <= end_date) 
# 
# #avg predicted - actual difference 
# #this is our constant to correct for in our date range
# april2022Rbconstant <- mean(aprilGrouping2$predictedValueDif)
# 
# aprilGroupingCorrected <- aprilGrouping2 %>%
#   mutate(AprilcorrectedPtgageValue = Water_Level_NoIce_ft + april2022Rbconstant)
# 
# rb2022WithOutliersAprilGroupCorrected <- rb2022WithOutliers %>%
#   #remove old aril data
#   anti_join(aprilGroupingCorrected, by = "dateTime") %>%
#   #make new column for "corrected" data
#   mutate(AprilcorrectedPtgageValue = Water_Level_NoIce_ft) %>%
#   #add back new april data with corrected valyues
#   bind_rows(aprilGroupingCorrected)
# 
# rb2022WithOutliersAprilGroupCorrected2 <- rb2022WithOutliersAprilGroupCorrected %>%
#   rename(Water_Level_NoIce_ftBeforeCorection = Water_Level_NoIce_ft, 
#          Water_Level_NoIce_ft = AprilcorrectedPtgageValue)
# #runfunction agaon on new april corrected data
# rb2022AprilCorrectedGroup <- pressureTransducerQAQCFunction(rb2022WithOutliersAprilGroupCorrected2)
# 
# rb2022AprilCorrectedGroup$plotIwthModels

#### finding calibartion dates 2024; may or aug calibrateion?
#findings: april 2 - aug 6 was may calibration; aug 6- nov 8 was aug clibration

CR_RB_Water_20241108_Aug6Cal <- read_csv("CR_RB_Water_20241108_Aug6Cal.csv", 
                                         col_types = cols(Datetime = col_datetime(format = "%m/%d/%Y %H:%M")))
CR_RB_Water_20241108_May14Cal <- read_csv("CR_RB_Water_20241108_May14Cal.csv", 
                                          col_types = cols(dateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

rbOnly2024 <- rbOnly %>%
  filter(year(dateTime) == 2024)
augcal <- rbOnly2024 %>%
  left_join(CR_RB_Water_20241108_Aug6Cal, by = c("dateTime" = "Datetime")) %>%
  mutate(equal = Water_Level_NoIce_ft.x == round(Water_Level_NoIce_ft.y, 2)) %>%
  filter(Water_Level_NoIce_ft.x > 0)

mayCal <- rbOnly2024 %>%
  left_join(CR_RB_Water_20241108_May14Cal, by = c("dateTime")) %>%
  mutate(equal = Water_Level_NoIce_ft.x == round(Water_Level_NoIce_ft.y, 2)) %>%
  filter(Water_Level_NoIce_ft.x > 0)

#same for apr 2 data


CR_RB_Water_20240402_Apr2Cal <- read_csv("CR_RB_Water_20240402_Apr2Cal.csv", 
                                         col_types = cols(`dateTime` = col_datetime(format = "%m/%d/%Y %H:%M")))
CR_RB_Water_20240402_Oct18Cal <- read_csv("CR_RB_Water_20240402_Oct18Cal.csv", 
                                         col_types = cols(`dateTime` = col_datetime(format = "%m/%d/%Y %H:%M")))

aprcal <- rbOnly %>%
  inner_join(CR_RB_Water_20240402_Apr2Cal, by = c("dateTime")) %>%
  mutate(equal = Water_Level_NoIce_ft == round(Water_Level_NoIce_ft_apr2Cal, 2)) %>%
  filter(Water_Level_NoIce_ft > 0)

octCal <- rbOnly %>%
  right_join(CR_RB_Water_20240402_Oct18Cal, by = c("dateTime")) %>%
  mutate(equal = Water_Level_NoIce_ft == round(Water_Level_NoIce_ft_oct18Cal, 2)) %>%
  filter(Water_Level_NoIce_ft > 0)

#same for 2023 data
CR_RB_Water_20230725_Jun6Cal <- read_csv("CR_RB_Water_20230725_Jun6Cal.csv", 
                                         col_types = cols(`dateTime` = col_datetime(format = "%m/%d/%Y %H:%M")))
CR_RB_Water_20230725_May18Cal <- read_csv("CR_RB_Water_20230725_May18Cal.csv", 
                                         col_types = cols(`dateTime` = col_datetime(format = "%m/%d/%Y %H:%M")))

CR_RB_Water_20230725_Nov2Cal <- read_csv("CR_RB_Water_20230725_Nov2Cal.csv", 
                                          col_types = cols(`dateTime` = col_datetime(format = "%m/%d/%Y %H:%M")))

novcal <- rbOnly %>%
  inner_join(CR_RB_Water_20230725_Nov2Cal, by = c("dateTime")) %>%
  mutate(equal = Water_Level_NoIce_ft.x == round(Water_Level_NoIce_ft.y, 2)) %>%
  filter(Water_Level_NoIce_ft.x > 0)

mayCal <- rbOnly %>%
  inner_join(CR_RB_Water_20230725_May18Cal, by = c("dateTime")) %>%
  mutate(equal = Water_Level_NoIce_ft.x == round(Water_Level_NoIce_ft.y, 2)) %>%
  filter(Water_Level_NoIce_ft.x > 0)

junCal <- rbOnly %>%
  inner_join(CR_RB_Water_20230725_Jun6Cal, by = c("dateTime")) %>%
  mutate(equal = Water_Level_NoIce_ft.x == round(Water_Level_NoIce_ft.y, 2)) %>%
  filter(Water_Level_NoIce_ft.x > 0)
