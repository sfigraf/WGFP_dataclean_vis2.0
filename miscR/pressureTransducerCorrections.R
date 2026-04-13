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



#' # Red bArn 2021 -----------------------------------------------------------
#' rbOnly2021 <- rbOnly %>%
#'   filter(year(dateTime) == 2021, 
#'          Water_Level_NoIce_ft > 0,
#'          !is.na(gageDif))
#' #plotting against each other to decide to do a linear model (y = mx+b)
#' plot <- rbOnly2021 %>%
#'   ggplot(aes(x = USGSGageHeightFt, y = Water_Level_NoIce_ft)) +
#'   geom_point() +
#'   ggtitle("Red Barn 2021 USGS vs gage height") +
#'   theme_classic()
#' ggplotly(plot)
#' 
#' model <- lm(Water_Level_NoIce_ft ~ USGSGageHeightFt, data = rbOnly2021)
#' 
#' # See the results
#' summary(model)
#' #r squared = .8912
#' #residuals
#' plot(model)
#' resids <- residuals(model)
#' #outliers; need to recalculate for these vals?
#' #clean data; no outliers
#' #
#' 
#' ##flag outliers
#' #add residuelas as a column
#' rbOnly2021$resids <- residuals(model)
#' rbOnly2021$is_outlier <- abs(rbOnly2021$resids) > 3 * sd(rbOnly2021$resids)
#' numberOfOutliers <- rbOnly2021 %>%
#'   filter(is_outlier) %>%
#'   nrow()
#' # 3. Check how many you have
#' table(rbOnly2021$is_outlier)
#' #recalulte model with outliers removed
#' rbOnly2021_noOutliers <- rbOnly2021 %>%
#'   filter(!is_outlier)
#' modelNoOutliersFromModel1 <- lm(Water_Level_NoIce_ft ~ USGSGageHeightFt, data = rbOnly2021_noOutliers)
#' 
#' # See the results
#' summary(modelNoOutliersFromModel1)
#' 
#' ##add line to the plot from before
#' intercept_val <- coef(model)[1]
#' slope_val <- coef(model)[2]
#' #slope val .8346057
#' 
#' #'No outlier' model values
#' intercept_valNoOutliers <- coef(modelNoOutliersFromModel1)[1]
#' slope_valNoOutliers <- coef(modelNoOutliersFromModel1)[2]
#' 
#' plot <- rbOnly2021 %>%
#'   ggplot(aes(x = USGSGageHeightFt, y = Water_Level_NoIce_ft, color = is_outlier, alpha = .6)) +
#'   geom_point() +
#'   ggtitle("Red Barn 2021 USGS vs gage height") +
#'   theme_classic() +
#'   scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
#'   geom_abline(intercept = intercept_val, slope = slope_val, color = "blue", linewidth = 1) +
#'   geom_abline(intercept = intercept_valNoOutliers, slope = slope_valNoOutliers, linetype = "dashed", color = "cyan")
#' 
#' ggplotly(plot)
#' 
#' rb2021Analyized <- pressureTransducerQAQCFunction(rbOnly2021)
#' 
#' rb2021Analyized$plotIwthModels
#' rb2021Outliers <- rb2021Analyized$subsetDataWithOutliers %>%
#'   filter(is_outlier) 
#' ##predicting values ofr 2021 red barn using model without the outliers
#' rb2021Predicted <- rb2021Analyized$subsetDataWithOutliers %>%
#'   mutate(predictedValue = predict(rb2021Analyized$noOutliersModelList$noOutliersModel, newdata = .)) 
#' 
#' # #get usgs values
#' # new_usgs_values <- data.frame(USGSGageHeightFt = rb2021Outliers$USGSGageHeightFt) 
#' # 
#' # # Use the model to predict your site's height
#' # predicted_gage_heights <- predict(rb2021Analyized$noOutliersModelList$noOutliersModel, newdata = new_usgs_values)
#' # new_usgs_values$PredictedGageheight <- predicted_gage_heights
#' # predictedVals <- new_usgs_values
#' 

# red barn 2022 -----------------------------------------------------------
rbOnly2022 <- rbOnly %>%
  filter(year(dateTime) == 2022, 
         Water_Level_NoIce_ft > 0,
         !is.na(gageDif))

rb2022Analyized <- pressureTransducerQAQCFunction(rbOnly2022)

rb2022Analyized$plotIwthModels
rb2022WithOutliers <- rb2022Analyized$subsetDataWithOutliers

aprilGrouping <- rb2022WithOutliers %>%
  filter(predictedValueDif < -.15)
#date range of april correction
# 2022-04-02 00:00:00
# 2022-04-21 08:00:00
#specify as UTC since that's what the data is in
start_date <- as.POSIXct("2022-04-02 00:00:00", tz = "UTC")
end_date   <- as.POSIXct("2022-04-21 08:00:00", tz = "UTC")
aprilGrouping2 <- rb2022WithOutliers %>%
  filter(dateTime >= start_date & dateTime <= end_date) 

#avg predicted - actual difference 
#this is our constant to correct for in our date range
april2022Rbconstant <- mean(aprilGrouping2$predictedValueDif)

aprilGroupingCorrected <- aprilGrouping2 %>%
  mutate(AprilcorrectedPtgageValue = Water_Level_NoIce_ft + april2022Rbconstant)

rb2022WithOutliersAprilGroupCorrected <- rb2022WithOutliers %>%
  #remove old aril data
  anti_join(aprilGroupingCorrected, by = "dateTime") %>%
  #make new column for "corrected" data
  mutate(AprilcorrectedPtgageValue = Water_Level_NoIce_ft) %>%
  #add back new april data with corrected valyues
  bind_rows(aprilGroupingCorrected)

rb2022WithOutliersAprilGroupCorrected2 <- rb2022WithOutliersAprilGroupCorrected %>%
  rename(Water_Level_NoIce_ftBeforeCorection = Water_Level_NoIce_ft, 
         Water_Level_NoIce_ft = AprilcorrectedPtgageValue)
#runfunction agaon on new april corrected data
rb2022AprilCorrectedGroup <- pressureTransducerQAQCFunction(rb2022WithOutliersAprilGroupCorrected2)

rb2022AprilCorrectedGroup$plotIwthModels

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
