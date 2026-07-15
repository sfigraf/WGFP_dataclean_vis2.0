###Pressure Transducer Corrections
##note: usgs data and all events data in utc
#  lubridate::tz(ptdDataWide$dateTime)
#usgs15Min <- USGSData$USGS15Min
source("functions/pressureTransducerQAQCFunction.R")
source("functions/calibrationChunksDateFiltered.R")
source("functions/modelListData.R")

library(tidyverse)
library(plotly)
library(readxl)

## read in reconstructed upper c, lower c, and fraser flow
reconstructedWGFPDailyFlow <- read_excel("data/PressureTransducer/PressureTransducerCorrections/reconstructedWGFPDailyFlow_2026-07-14.xlsx", 
                                         sheet = "Assumed and Actual Flow Data")
reconFlow1 <- reconstructedWGFPDailyFlow %>%
  mutate(CFFlow = `Assumed/Actual UpperC Flow` + `Assumed/Actual Fraser Flow`)
#bring in PT data already sourced from app page
PTData <- readRDS("data/flatFilesforApp/PTData.rds")
ptdDataWide <- PTData$PTDataWide
#check gage difference
ptdDataWide_1 <- ptdDataWide %>%
  mutate(gageDif = USGSGageHeightFt - Water_Level_NoIce_ft)
#get red barn only data
rbOnly <- ptdDataWide_1 %>%
  filter(Site == "Red Barn") %>%
  select(dateTime, USGSDischarge, USGSGageHeightFt, Water_Level_NoIce_ft, gageDif)
#plot to see what difs look like
plot <- rbOnly %>%
  ggplot(aes(x = gageDif, text = as.character(gageDif))) +
  geom_histogram()  +
  ggtitle("RB Only Difs") +
  theme_classic()
ggplotly(plot)

#rite_xlsx(rbOnly, "RedBarnUSGSandPTData.xlsx")
#make reverse rating curves for each year
##divide data into chunks based on calibration dates
#from "U:\Projects\Colorado_River\Windy_Gap_FishMovementStudy\Data\Pressure_Transducer\WGFP_PressureTransducer_AllData.xlsx", sheet calibration dates but saved as csv
calibrationDates <- read_csv("data/PressureTransducer/PressureTransducerCorrections/calibrationDates.csv", 
                             col_types = cols(DataCalStart = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                              DataCalEnd = col_datetime(format = "%m/%d/%Y %H:%M")))

calibrationDates <- calibrationDates %>%
  arrange(Site, DataCalStart)




#get just for red barn 
rbCalibatraioDates <- calibrationDates %>%
  filter(Site == "Red Barn")
#get list of red barn DFs that align with each calbration chunk
redBarn_list <- calibrationChunksDateFiltered(calibrationDates = rbCalibatraioDates, siteOnlyData = rbOnly)

#allRBTransducerQAQC <- lapply(redBarn_list, pressureTransducerQAQCFunction)
#apply pressureTransducerQAQCFunction() to all Dfs in the red barn list
allRBTransducerQAQC <- lapply(redBarn_list, function(x) {
  pressureTransducerQAQCFunction(
    subsetData = x, 
    controlVariable = "USGSGageHeightFt", 
    SiteName = "Red Barn"
  )
})

# RED BARN just Looking at resulting data ---------------------------------


# RED BARN 2020 -----------------------------------------------------------
# 
# allRBTransducerQAQC$`Red Barn_1`$USGSGageHeightList$plotIwthModels
# intercept <- allRBTransducerQAQC$`Red Barn_1`$USGSGageHeightList$baseModelList$intercept_val
# slope <- allRBTransducerQAQC$`Red Barn_1`$USGSGageHeightList$baseModelList$slope_val
# 
# summary <- allRBTransducerQAQC$`Red Barn_1`$USGSGageHeightList$baseModelList$summary
# ### Red bArn 2021 -----------------------------------------------------------
# allRBTransducerQAQC$`Red Barn_2`$USGSGageHeightList$plotIwthModels
# intercept <- allRBTransducerQAQC$`Red Barn_2`$USGSGageHeightList$baseModelList$intercept_val
# slope <- allRBTransducerQAQC$`Red Barn_2`$USGSGageHeightList$baseModelList$slope_val

# red barn 2022 -----------------------------------------------------------
# allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$plotIwthModels
# intercept <- allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$noOutliersModelList$intercept_val
# 
# allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$noOutliersModelList$summary
# allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$baseModelList$summary
# 
# slopeNoOutliers <- coef(allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$noOutliersModelList$noOutliersModel)[2]
# slopeWithOutliers <- coef(allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$baseModelList$baseModel)[2]
# 
# ### oart 2
# allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$plotIwthModels
# intercept <- allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$baseModelList$intercept_val
# slope <- allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$baseModelList$slope_val
# 
# intercept <- allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$noOutliersModelList$intercept_val
# slope <- coef(allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$noOutliersModelList$noOutliersModel)[2]
# 
# # november 2022
# allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$plotIwthModels
# intercept <- allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$baseModelList$intercept_val
# slope <- allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$baseModelList$slope_val
# 
# intercept <- allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$noOutliersModelList$intercept_val
# slope <- coef(allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$noOutliersModelList$noOutliersModel)[2]
# library(purrr)


#gets results of the modeled data into a neat table for easy viewing
redBarnModelTableResults <- modelListData(allRBTransducerQAQC)

#### correcting the RB  data so that each gage reading can be compared to itself 1:1
redBarnModelTableResultsBaseModelOnly <- redBarnModelTableResults %>%
  filter(model_type == "baseModelList")
#baseline to calibrate the rest of th edata to
baseline_slope <- redBarnModelTableResultsBaseModelOnly$slope_val[redBarnModelTableResultsBaseModelOnly$site_name == "Red Barn_1"]
baseline_intercept <- redBarnModelTableResultsBaseModelOnly$intercept_val[redBarnModelTableResultsBaseModelOnly$site_name == "Red Barn_1"]


rbOnlyWithChunksInfo <- rbOnly %>%
  # Join based on matching site AND the datetime falling within the date range
  left_join(
    redBarnModelTableResultsBaseModelOnly, 
    by = join_by(
      #site_name == site_name,
      dateTime >= DataCalStart,
      dateTime <= DataCalEnd
    )
  ) 
#make new columns with 2 dif normalization methods based on "Guidelines and Standard Procedures for Continuous Water-Quality Monitors: Station Operation, Record Computation, and Data Reporting" from USGS pg 24 
rbOnlyCorrected <- rbOnlyWithChunksInfo %>%
  mutate(
    #slope and offset correction (aka '2 point' correction).
    #applied when a sensor's error is proportional to the measured value, meaning the sensitivity (slope) has drifted alongside the baseline (intercept). It corrects both the "zero" (the intercept) and the "span" (the slope) to bring the data back into alignment with reference gage
    normalizedWaterLevelBySlopeNormalized = baseline_slope * ((Water_Level_NoIce_ft - intercept_val) / slope_val) + baseline_intercept, 
    #"constant shift" or "zero-point correction": the entire dataset is moved up or down by a static value (baseline_intercept)
    normalizedWaterLevelByOffset = Water_Level_NoIce_ft - intercept_val + baseline_intercept
  )
#plot
plotReady <- rbOnlyCorrected %>%
  #mutate(normalizedWaterLevelBySlopeNormalized = ifelse(Water_Level_NoIce_ft == 0, 0, normalizedWaterLevelBySlopeNormalized)) %>%
  select(dateTime, 
         USGSGageHeightFt,
         Water_Level_NoIce_ft, 
         normalizedWaterLevelBySlopeNormalized, 
         normalizedWaterLevelByOffset) %>%
  pivot_longer(
    cols = -dateTime, # Pivot everything EXCEPT the datetime column
    names_to = "Normalization_Method",
    values_to = "Water_Level"
  )

# 2. Build the Scatterplot
plot <- plotReady %>%
  ggplot(aes(x = dateTime, y = Water_Level, color = Normalization_Method)) +
  geom_point(alpha = 0.5, size = 1) + # alpha = 0.5 makes points slightly transparent so you can see overlap
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Water_Level_NoIce_ft" = "gray60",                     # Raw data in gray
      "USGSGageHeightFt" = "cyan",
      "normalizedWaterLevelBySlopeNormalized" = "blue",      # Option B in blue
      "normalizedWaterLevelByOffset" = "red"                 # Option A in red
    ),
    labels = c(
      "Water_Level_NoIce_ft" = "Raw Data (NoIce)",                     # Raw data in gray
      "USGSGageHeightFt" = "USGS Hitching Post Gage",
      "normalizedWaterLevelBySlopeNormalized" = "Slope & Offset Correction",      # Option B in blue
      "normalizedWaterLevelByOffset" = "Offset Correction Only" 
    )
  ) +
  labs(
    title = "Comparison of Normalization Methods Over Time",
    x = "Date",
    y = "Water Level (ft)",
    color = "Legend"
  ) +
  theme(legend.position = "bottom", legend.direction = "vertical")

plot
ggplotly(plot)
#subset because that one plot is huge
plotReady2020 <- plotReady %>%
  filter(year(dateTime) == 2020)

plot <- plotReady2020 %>%
  ggplot(aes(x = dateTime, y = Water_Level, color = Normalization_Method)) +
  geom_point(alpha = 0.5, size = 1) + # alpha = 0.5 makes points slightly transparent so you can see overlap
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Water_Level_NoIce_ft" = "gray60",                     # Raw data in gray
      "USGSGageHeightFt" = "cyan",
      "normalizedWaterLevelBySlopeNormalized" = "blue",      # Option B in blue
      "normalizedWaterLevelByOffset" = "red"                 # Option A in red
    ),
    labels = c(
      "Raw Data (NoIce)", 
      "USGS Hitching Post Gage",
      "Slope & Offset Correction",
      "Offset Correction Only"
    )
  ) +
  labs(
    title = "Comparison of Normalization Methods Over Time",
    x = "Date",
    y = "Water Level (ft)",
    color = "Legend"
  ) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggplotly(plot)

plotReady2021 <- plotReady %>%
  filter(year(dateTime) == 2021)

plot <- plotReady2021 %>%
  ggplot(aes(x = dateTime, y = Water_Level, color = Normalization_Method)) +
  geom_point(alpha = 0.5, size = 1) + # alpha = 0.5 makes points slightly transparent so you can see overlap
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Water_Level_NoIce_ft" = "gray60",                     # Raw data in gray
      "USGSGageHeightFt" = "cyan",
      "normalizedWaterLevelBySlopeNormalized" = "blue",      # Option B in blue
      "normalizedWaterLevelByOffset" = "red"                 # Option A in red
    ),
    labels = c(
      "Raw Data (NoIce)", 
      "USGS Hitching Post Gage",
      "Slope & Offset Correction",
      "Offset Correction Only"
    )
  ) +
  labs(
    title = "Comparison of Normalization Methods: 2021",
    x = "Date",
    y = "Water Level (ft)",
    color = "Legend"
  ) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggplotly(plot)
#######2022

plotReady2022 <- plotReady %>%
  filter(year(dateTime) == 2022)
plot <- plotReady2022 %>%
  ggplot(aes(x = dateTime, y = Water_Level, color = Normalization_Method)) +
  geom_point(alpha = 0.5, size = 1) + # alpha = 0.5 makes points slightly transparent so you can see overlap
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Water_Level_NoIce_ft" = "gray60",                     # Raw data in gray
      "USGSGageHeightFt" = "cyan",
      "normalizedWaterLevelBySlopeNormalized" = "blue",      # Option B in blue
      "normalizedWaterLevelByOffset" = "red"                 # Option A in red
    ),
    labels = c(
      "Raw Data (NoIce)", 
      "USGS Hitching Post Gage",
      "Slope & Offset Correction",
      "Offset Correction Only"
    )
  ) +
  labs(
    title = "Comparison of Normalization Methods Over Time",
    x = "Date",
    y = "Water Level (ft)",
    color = "Legend"
  ) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggplotly(plot)
#######2023
plotReady2023 <- plotReady %>%
  filter(year(dateTime) == 2023)

plot <- plotReady2023 %>%
  ggplot(aes(x = dateTime, y = Water_Level, color = Normalization_Method)) +
  geom_point(alpha = 0.5, size = 1) + # alpha = 0.5 makes points slightly transparent so you can see overlap
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Water_Level_NoIce_ft" = "gray60",                     # Raw data in gray
      "USGSGageHeightFt" = "cyan",
      "normalizedWaterLevelBySlopeNormalized" = "blue",      # Option B in blue
      "normalizedWaterLevelByOffset" = "red"                 # Option A in red
    ),
    labels = c(
      "Raw Data (NoIce)", 
      "USGS Hitching Post Gage",
      "Slope & Offset Correction",
      "Offset Correction Only"
    )
  ) +
  labs(
    title = "Comparison of Normalization Methods Over Time",
    x = "Date",
    y = "Water Level (ft)",
    color = "Legend"
  ) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggplotly(plot)
####### 2024
plotReady2024 <- plotReady %>%
  filter(year(dateTime) == 2024)

plot <- plotReady2024 %>%
  ggplot(aes(x = dateTime, y = Water_Level, color = Normalization_Method)) +
  geom_point(alpha = 0.5, size = 1) + # alpha = 0.5 makes points slightly transparent so you can see overlap
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Water_Level_NoIce_ft" = "gray60",                     # Raw data in gray
      "USGSGageHeightFt" = "cyan",
      "normalizedWaterLevelBySlopeNormalized" = "blue",      # Option B in blue
      "normalizedWaterLevelByOffset" = "red"                 # Option A in red
    ),
    labels = c(
      "Raw Data (NoIce)", 
      "USGS Hitching Post Gage",
      "Slope & Offset Correction",
      "Offset Correction Only"
    )
  ) +
  labs(
    title = "Comparison of Normalization Methods Over Time",
    x = "Date",
    y = "Water Level (ft)",
    color = "Legend"
  ) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggplotly(plot)

plotReady2025 <- plotReady %>%
  filter(year(dateTime) == 2025)

plot <- plotReady2025 %>%
  ggplot(aes(x = dateTime, y = Water_Level, color = Normalization_Method)) +
  geom_point(alpha = 0.5, size = 1) + # alpha = 0.5 makes points slightly transparent so you can see overlap
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Water_Level_NoIce_ft" = "gray60",                     # Raw data in gray
      "USGSGageHeightFt" = "cyan",
      "normalizedWaterLevelBySlopeNormalized" = "blue",      # Option B in blue
      "normalizedWaterLevelByOffset" = "red"                 # Option A in red
    ),
    labels = c(
      "Raw Data (NoIce)", 
      "USGS Hitching Post Gage",
      "Slope & Offset Correction",
      "Offset Correction Only"
    )
  ) +
  labs(
    title = "Comparison of Normalization Methods Over Time",
    x = "Date",
    y = "Water Level (ft)",
    color = "Legend"
  ) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggplotly(plot)

plotReady2020 <- plotReady %>%
  filter(year(dateTime) == 2020)
# print(final_table)
# write_csv(final_table, "firstModelResultsNoOutliersRedBarn.csv")
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


# Hitching Post -----------------------------------------------------------

hpOnly <- ptdDataWide_1 %>%
  filter(Site == "Hitching Post") %>%
  select(dateTime, USGSDischarge, USGSGageHeightFt, Water_Level_NoIce_ft, gageDif)


##finding calibration dates starting with 2025
#doubel checking work in archive, to database file worksheet 2025 with clkibration dates

#april
start_date <- as.POSIXct("2025-04-01 10:00:00", tz = "UTC")
end_date   <- as.POSIXct("2025-04-14 14:00:00", tz = "UTC")
hp2025_april <- hpOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date)
x <- pressureTransducerQAQCFunction(hp2025_april) #hpOnly 
x$plotIwthModels

#may 

start_date <- as.POSIXct("2025-04-14 15:00:00", tz = "UTC")
end_date   <- as.POSIXct("2025-08-19 11:00:00", tz = "UTC")
hp2025_mayCal <- hpOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date)
x <- pressureTransducerQAQCFunction(hp2025_mayCal) #hpOnly 
x$plotIwthModels

#sep cal

start_date <- as.POSIXct("2025-08-19 12:00:00", tz = "UTC")
end_date   <- as.POSIXct("2025-11-03 14:00:00", tz = "UTC")
hp2025_SepCal <- hpOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date)
x <- pressureTransducerQAQCFunction(hp2025_SepCal) #hpOnly 
x$plotIwthModels

##quick qaqc to see if the calibration dates line up with what's in the data
start_date <- as.POSIXct("2023-07-27 12:00:00", tz = "UTC")
end_date   <- as.POSIXct("2023-08-29 13:00:00", tz = "UTC")

CR_HP_Water_20231018_Jul27Cal <- read_csv("hpwaterLevels/CR_HP_Water_20231018_Jul27Cal.csv", 
                                          col_types = cols(`#` = col_skip(), `Date Time, GMT-06:00` = col_datetime(format = "%m/%d/%y %I:%M:%S %p")), 
                                          skip = 1)
x <- CR_HP_Water_20231018_Jul27Cal %>%
  filter(`Date Time, GMT-06:00` >= start_date & `Date Time, GMT-06:00` <= end_date) %>%
  rename(dateTime = `Date Time, GMT-06:00`)

# CR_HP_Water_20230727_Apr4Cal <- read_csv("hpwaterLevels/CR_HP_Water_20230727_Apr4Cal.csv", 
#                                          col_types = cols(`Date Time, GMT-06:00` = col_datetime(format = "%m/%d/%y %I:%M:%S %p")), 
#                                          skip = 1)
#checking 2023 dates etc if i need to break them up
start_date <- as.POSIXct("2022-11-02 11:00:00", tz = "UTC")
end_date   <- as.POSIXct("2023-07-27 11:00:00", tz = "UTC")
hp2023 <- hpOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date)
x <- pressureTransducerQAQCFunction(hp2023) #hpOnly 
x$plotIwthModels

#checking 2022 now
start_date <- as.POSIXct("2022-04-21 12:00:00", tz = "UTC")
end_date   <- as.POSIXct("2022-11-02 10:00:00", tz = "UTC")
hp2022 <- hpOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date)
x <- pressureTransducerQAQCFunction(hp2022) #hpOnly 
x$plotIwthModels

###analyzing and getting models

hpCalibrationDates <- calibrationDates %>%
  filter(Site == "Hitching Post")

HP_list <- calibrationChunksDateFiltered(calibrationDates = hpCalibrationDates, siteOnlyData = hpOnly)

#can pass named arguemnts after the function or use an anonymous function to be explicit like i did in red barn above
allHPTransducerQAQC <- lapply(HP_list, pressureTransducerQAQCFunction, SiteName = "Hitching Post", flowModel = TRUE, flowVariable = "USGSDischarge")

hitchingPostModelTableResults <- modelListData(allHPTransducerQAQC)

allHPTransducerQAQC$`Hitching Post_4`$flowModelList$dailyFlowModelList$ggplotly
allHPTransducerQAQC$`Hitching Post_4`$USGSGageHeightList$plotIwthModels
# Confluence --------------------------------------------------------------

#correlated to hydrology


cfOnly <- ptdDataWide_1 %>%
  filter(Site == "Confluence") %>%
  select(dateTime, USGSDischarge, USGSGageHeightFt, Water_Level_NoIce_ft, gageDif) %>%
  mutate(Date = date(dateTime)) %>%
  left_join(reconFlow1, by = "Date")


start_date <- as.POSIXct("2023-03-05 12:00:00", tz = "UTC")
end_date   <- as.POSIXct("2023-11-05 10:00:00", tz = "UTC")
###what's the correlation when water_level_no_ice is averaged on daily?
cf2025Filtered <- cfOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date, 
         Water_Level_NoIce_ft > 0) %>%
  group_by(date1 = date(dateTime)) %>%
  summarise(dailyGageHegith = mean(Water_Level_NoIce_ft))

together <- cf2025Filtered %>%
  left_join(reconFlow1, by = c("date1" = "Date")) %>%
  ggplot(aes(x = CFFlow, y = dailyGageHegith)) +
  geom_point()

### correlation when not averaged at all?
cf2025Filtered <- cfOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date, 
         Water_Level_NoIce_ft > 0) %>%
  mutate(Date = date(dateTime))

together <- cf2025Filtered %>%
  left_join(reconFlow1, by = "Date") %>%
  ggplot(aes(x = CFFlow, y = Water_Level_NoIce_ft)) +
  geom_point()

###testing function on comfulence data
start_date <- as.POSIXct("2023-03-05 12:00:00", tz = "UTC")
end_date   <- as.POSIXct("2023-11-05 10:00:00", tz = "UTC")

cfFiltered <- cfOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date, 
         Water_Level_NoIce_ft > 0) #%>%
  # ggplot(aes(x = USGSGageHeightFt, y = Water_Level_NoIce_ft)) +
  # geom_point()
cfFiltered
x <- pressureTransducerQAQCFunction(cfFiltered, SiteName = "Confluence", flowModel = TRUE)

#x$
## have funciotn 
# x <- pressureTransducerQAQCFunction(cf2025) #hpOnly 
x$flowModelList$dailyFlowModelList$ggplotly

#4/21/2022  1:00:00 PM
#2022 data
start_date <- as.POSIXct("2022-04-21 13:00:00", tz = "UTC")
end_date   <- as.POSIXct("2022-11-02 12:00:00", tz = "UTC")

cfFiltered <- cfOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date, 
         Water_Level_NoIce_ft > 0)
x <- pressureTransducerQAQCFunction(cfFiltered, SiteName = "Confluence", flowModel = TRUE)
x$flowModelList$dailyFlowModelList$intercept_val
x$flowModelList$allDataFlowModelList$ggplotly
CR_CF_Water_20221102 <- read_csv("CR_CF_Water_20221102.csv", 
                                 col_types = cols(`Date Time, GMT-06:00` = col_character())) %>%
  mutate(dateTime = lubridate::mdy_hm(`Date Time, GMT-06:00`))
together <- cfFiltered %>%
  left_join(CR_CF_Water_20221102, by = c("dateTime")) %>%
  mutate(euqlas = round(Water_Level_NoIce_ft, 2) == round(waterLevel, 2))
## winter 2022 data
CR_CF_WaterLevel_20220421 <- read_csv("CR_CF_WaterLevel_20220421.csv") %>%
  mutate(dateTime = lubridate::mdy_hm(dateTime))

start_date <- as.POSIXct(min(CR_CF_WaterLevel_20220421$dateTime), tz = "UTC")
end_date   <- as.POSIXct(max(CR_CF_WaterLevel_20220421$dateTime), tz = "UTC")

cfFiltered <- cfOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date, 
         Water_Level_NoIce_ft > 0)
x <- pressureTransducerQAQCFunction(cfFiltered, SiteName = "Confluence", flowModel = TRUE, showDiagnosticPlots = TRUE)
x$flowModelList$dailyFlowModelList$intercept_val
x$flowModelList$allDataFlowModelList$ggplotly
x$USGSGageHeightList$plotIwthModels
together <- cfFiltered %>%
  left_join(CR_CF_WaterLevel_20220421, by = c("dateTime")) %>%
  mutate(euqlas = round(Water_Level_NoIce_ft, 2) == round(waterLevel, 2))
####2021 data
start_date <- as.POSIXct("2021-04-06 15:00:00", tz = "UTC")
end_date   <- as.POSIXct("2021-11-09 14:00:00", tz = "UTC")

cfFiltered <- cfOnly %>%
  filter(dateTime >= start_date & dateTime <= end_date, 
         Water_Level_NoIce_ft > 0)
x <- pressureTransducerQAQCFunction(cfFiltered, SiteName = "Confluence", flowModel = TRUE, showDiagnosticPlots = TRUE)
x$flowModelList$allDataFlowModelList$ggplotly
x$flowModelList$dailyFlowModelList$ggplotly
x$USGSGageHeightList$plotIwthModels
x1 <- x$flowModelList$dailyFlowModelList$dataWithDailyPredictionsBasedonFlow

##running off all calibration dates
CFCalibrationDates <- calibrationDates %>%
  filter(Site == "Confluence")

CF_list <- calibrationChunksDateFiltered(calibrationDates = CFCalibrationDates, siteOnlyData = cfOnly)

#can pass named arguemnts after the function or use an anonymous function to be explicit like i did in red barn above
allCFTransducerQAQC <- lapply(CF_list, pressureTransducerQAQCFunction, SiteName = "Confluence", flowModel = TRUE)
#allCFTransducerQAQC$Confluence_18$flowModelList$dailyFlowModelList$ggplotly
conlfuenceModelTableResults <- modelListData(allCFTransducerQAQC, flowModel = TRUE)

x <- conlfuenceModelTableResults %>%
  filter(model_type %in% c("dailyFlowModelList", "allDataFlowModelList")) %>%
  pivot_wider(id_cols = site_name, names_from = model_type, values_from = c("slope_val", "intercept_val"))

# CS ----------------------------------------------------------------------

CSOnly <- ptdDataWide_1 %>%
  filter(Site == "Connectivity Side Channel") %>%
  select(dateTime, USGSDischarge, USGSGageHeightFt, Water_Level_NoIce_ft, gageDif) %>%
  mutate(Date = date(dateTime)) %>%
  left_join(reconFlow1, by = "Date")
##running off all calibration dates
CSCalibrationDates <- calibrationDates %>%
  filter(Site == "Connectivity Side Channel")

CS_list <- calibrationChunksDateFiltered(calibrationDates = CSCalibrationDates, siteOnlyData = CSOnly)

#can pass named arguemnts after the function or use an anonymous function to be explicit like i did in red barn above
allCSTransducerQAQC <- lapply(CS_list, pressureTransducerQAQCFunction, SiteName = "Connectivity Side Channel", flowModel = TRUE)
#allCSTransducerQAQC$Confluence_18$flowModelList$dailyFlowModelList$ggplotly
#siteQAQCList <- allCSTransducerQAQC
CSModelTableResults <- modelListData(allCSTransducerQAQC, flowModel = TRUE)

CSModelTableResults2 <- CSModelTableResults %>%
  filter(model_type %in% c("dailyFlowModelList", "allDataFlowModelList")) %>%
  pivot_wider(id_cols = site_name, names_from = model_type, values_from = c("slope_val", "intercept_val"))
#allCSTransducerQAQC$`Connectivity Side Channel_13`$flowModelList$dailyFlowModelList$ggplotly

# CD ----------------------------------------------------------------------


CDOnly <- ptdDataWide_1 %>%
  filter(Site == "Connectivity Downstream") %>%
  select(dateTime, USGSDischarge, USGSGageHeightFt, Water_Level_NoIce_ft, gageDif) %>%
  mutate(Date = date(dateTime)) %>%
  left_join(reconFlow1, by = "Date")
##running off all calibration dates
CDCalibrationDates <- calibrationDates %>%
  filter(Site == "Connectivity Downstream")

CD_list <- calibrationChunksDateFiltered(calibrationDates = CDCalibrationDates, siteOnlyData = CDOnly)

#can pass named arguemnts after the function or use an anonymous function to be explicit like i did in red barn above
allCDTransducerQAQC <- lapply(CD_list, pressureTransducerQAQCFunction, SiteName = "Connectivity Downstream", flowModel = TRUE)
#allCDTransducerQAQC$Confluence_18$flowModelList$dailyFlowModelList$ggplotly
#siteQAQCList <- allCDTransducerQAQC
CDModelTableResults <- modelListData(allCDTransducerQAQC, flowModel = TRUE)

CDModelTableResults2 <- CDModelTableResults %>%
  filter(model_type %in% c("dailyFlowModelList", "allDataFlowModelList")) %>%
  pivot_wider(id_cols = site_name, names_from = model_type, values_from = c("slope_val", "intercept_val"))

# CU ----------------------------------------------------------------------


CUOnly <- ptdDataWide_1 %>%
  filter(Site == "Connectivity Upstream") %>%
  select(dateTime, USGSDischarge, USGSGageHeightFt, Water_Level_NoIce_ft, gageDif) %>%
  mutate(Date = date(dateTime)) %>%
  left_join(reconFlow1, by = "Date")
##running off all calibration dates
CUCalibrationDates <- calibrationDates %>%
  filter(Site == "Connectivity Upstream")

CU_list <- calibrationChunksDateFiltered(calibrationDates = CUCalibrationDates, siteOnlyData = CUOnly)

#can pass named arguemnts after the function or use an anonymous function to be explicit like i did in red barn above
allCUTransducerQAQC <- lapply(CU_list, pressureTransducerQAQCFunction, SiteName = "Connectivity Upstream", flowModel = TRUE)
#allCUTransducerQAQC$Confluence_18$flowModelList$dailyFlowModelList$ggplotly
#siteQAQCList <- allCUTransducerQAQC
CUModelTableResults <- modelListData(allCUTransducerQAQC, flowModel = TRUE)

CUModelTableResults2 <- CUModelTableResults %>%
  filter(model_type %in% c("dailyFlowModelList", "allDataFlowModelList")) %>%
  pivot_wider(id_cols = site_name, names_from = model_type, values_from = c("slope_val", "intercept_val"))
