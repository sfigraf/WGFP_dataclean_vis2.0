###Pressure Transducer Corrections
##note: usgs data and all events data in utc
#  lubridate::tz(ptdDataWide$dateTime)
#usgs15Min <- USGSData$USGS15Min
source("functions/pressureTransducerQAQCFunction.R")
ptdDataWide <- PTData$PTDataWide

ptdDataWide_1 <- ptdDataWide %>%
  mutate(gageDif = USGSGageHeightFt - Water_Level_NoIce_ft)
rbOnly <- ptdDataWide_1 %>%
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
  arrange(Site, DataCalStart)



calibrationChunksDateFiltered <- function(calibrationDates = calibrationDates, siteOnlyData){
  
  siteOnlyChunk_list <- list()
  
  for(i in 1:nrow(calibrationDates)) {
    
    # Define the current window
    current_start <- calibrationDates$DataCalStart[i]
    current_end   <- calibrationDates$DataCalEnd[i]
    Site <- calibrationDates$Site[i]
    
    # Filter red barn based on this window
    chunk <- siteOnlyData[siteOnlyData$dateTime >= current_start & siteOnlyData$dateTime <= current_end, ]
    
    # Store it in the list with a name (optional)
    # Assuming df1 has a 'site_name' or 'year' column to identify the chunk
    site_label <- paste0(Site, "_", i) 
    siteOnlyChunk_list[[site_label]] <- chunk
  }
  return(siteOnlyChunk_list)
}

rbCalibatraioDates <- calibrationDates %>%
  filter(Site == "Red Barn")

redBarn_list <- calibrationChunksDateFiltered(calibrationDates = rbCalibatraioDates, siteOnlyData = rbOnly)

#allRBTransducerQAQC <- lapply(redBarn_list, pressureTransducerQAQCFunction)
allRBTransducerQAQC <- lapply(redBarn_list, function(x) {
  pressureTransducerQAQCFunction(
    subsetData = x, 
    controlVariable = "USGSGageHeightFt", 
    SiteName = "Red Barn"
  )
})

# RED BARN 2020 -----------------------------------------------------------

allRBTransducerQAQC$`Red Barn_1`$USGSGageHeightList$plotIwthModels
intercept <- allRBTransducerQAQC$`Red Barn_1`$USGSGageHeightList$baseModelList$intercept_val
slope <- allRBTransducerQAQC$`Red Barn_1`$USGSGageHeightList$baseModelList$slope_val

summary <- allRBTransducerQAQC$`Red Barn_1`$USGSGageHeightList$baseModelList$summary
### Red bArn 2021 -----------------------------------------------------------
allRBTransducerQAQC$`Red Barn_2`$USGSGageHeightList$plotIwthModels
intercept <- allRBTransducerQAQC$`Red Barn_2`$USGSGageHeightList$baseModelList$intercept_val
slope <- allRBTransducerQAQC$`Red Barn_2`$USGSGageHeightList$baseModelList$slope_val

# red barn 2022 -----------------------------------------------------------
allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$plotIwthModels
intercept <- allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$noOutliersModelList$intercept_val

allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$noOutliersModelList$summary
allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$baseModelList$summary

slopeNoOutliers <- coef(allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$noOutliersModelList$noOutliersModel)[2]
slopeWithOutliers <- coef(allRBTransducerQAQC$`Red Barn_3`$USGSGageHeightList$baseModelList$baseModel)[2]

### oart 2
allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$plotIwthModels
intercept <- allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$baseModelList$intercept_val
slope <- allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$baseModelList$slope_val

intercept <- allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$noOutliersModelList$intercept_val
slope <- coef(allRBTransducerQAQC$`Red Barn_4`$USGSGageHeightList$noOutliersModelList$noOutliersModel)[2]

# november 2022
allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$plotIwthModels
intercept <- allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$baseModelList$intercept_val
slope <- allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$baseModelList$slope_val

intercept <- allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$noOutliersModelList$intercept_val
slope <- coef(allRBTransducerQAQC$`Red Barn_5`$USGSGageHeightList$noOutliersModelList$noOutliersModel)[2]
library(purrr)

# final_table <- purrr::map_df(allRBTransducerQAQC, function(site_data) {
#   
#   # 2. Extract the two models and combine them into a small dataframe
#   # We use bind_rows to stack 'base' and 'noOutliers'
#   if(!is.character(site_data)){
#     
#     baseModelType <- 
#     
#     bind_rows(
#       data.frame(
#         model_type = "baseModelList",
#         intercept_val = site_data$baseModelList$intercept_val,
#         slope_val = site_data$baseModelList$slope_val
#       ),
#       data.frame(
#         model_type = "noOutliersModelList",
#         intercept_val = site_data$noOutliersModelList$intercept_val,
#         slope_val = coef(site_data$noOutliersModelList$noOutliersModel)[2]
#       )
#     )
#   } else{
#     
#   }
#   
# }, .id = "site_name")

library(tidyverse)

modelListData <- function(siteQAQCList){
  final_table <- purrr::map_df(siteQAQCList, function(site_data) {
    
    # 1. Check if site_data is just a character string (the "empty" case)
    if (is.character(site_data)) {
      return(
        data.frame(
          model_type = c("baseModelList", "noOutliersModelList"),
          intercept_val = c(NA_real_, NA_real_),
          slope_val = c(NA_real_, NA_real_), 
          DataCalStart = c(NA, NA), 
          DataCalEnd = c(NA, NA)
        )
      )
    }
    
    # 2. If it's not a character, proceed with data extraction
    # We use tryCatch or basic NULL checks to ensure the coef() call doesn't break
    cal_start <- min(site_data$USGSGageHeightList$subsetDataWithOutliersPredicted$dateTime, na.rm = TRUE)
    cal_end   <- max(site_data$USGSGageHeightList$subsetDataWithOutliersPredicted$dateTime, na.rm = TRUE)
    
    # Base Model Row
    base_row <- data.frame(
      model_type = "baseModelList",
      intercept_val = site_data$USGSGageHeightList$baseModelList$intercept_val %||% NA,
      slope_val = site_data$USGSGageHeightList$baseModelList$slope_val %||% NA, 
      DataCalStart = cal_start,
      DataCalEnd = cal_end
    )
    
    # No Outliers Model Row (using your specific coef logic)
    # We check if the model object exists before trying to index [2]
    no_outlier_slope <- NA
    if (!is.null(site_data$USGSGageHeightList$noOutliersModelList$noOutliersModel)) {
      no_outlier_slope <- coef(site_data$USGSGageHeightList$noOutliersModelList$noOutliersModel)[2]
    }
    
    no_outliers_row <- data.frame(
      model_type = "noOutliersModelList",
      intercept_val = site_data$USGSGageHeightList$noOutliersModelList$intercept_val %||% NA,
      slope_val = no_outlier_slope, 
      DataCalStart = cal_start,
      DataCalEnd = cal_end
    )
    
    # Combine the two rows for this site
    bind_rows(base_row, no_outliers_row)
    
  }, .id = "site_name")
  
  # Clean up the names (optional: removes the 'slope_val' name if coef() kept it)
  final_table$slope_val <- as.numeric(final_table$slope_val)
  return(final_table)
}

redBarnModelTableResults <- modelListData(allRBTransducerQAQC)

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
cfOnly <- ptdDataWide_1 %>%
  filter(Site == "Confluence") %>%
  select(dateTime, USGSDischarge, USGSGageHeightFt, Water_Level_NoIce_ft, gageDif) %>%
  mutate(Date = date(dateTime)) %>%
  left_join(reconFlow1, by = "Date")
#correlate to hydrology?
library(readxl)
reconstructedWGFPDailyFlow <- read_excel("reconstructedWGFPDailyFlow.xlsx", 
                                         sheet = "Assumed and Actual Flow Data")
reconFlow1 <- reconstructedWGFPDailyFlow %>%
  mutate(CFFlow = `Assumed/Actual UpperC Flow` + `Assumed/Actual Fraser Flow`)


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
# x$plotIwthModels



