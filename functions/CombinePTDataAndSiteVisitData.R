##Detection Distance wrangling
WGFP_SiteVisits_FieldData <- read_csv("data/WGFP_SiteVisits_FieldData.csv", 
                                      skip = 2)
PTData <- readRDS("data/flatFilesforApp/PTData.rds")


#######need to get a df of 
library(data.table, quietly = TRUE,warn.conflicts	= FALSE)
x <- WGFP_SiteVisits_FieldData %>%
  mutate(Datetime = lubridate::ymd_hms(paste(mdy(Date), Time)),
         Date = mdy(Date)
        
  )
#exact timestamp matches
x1 <- x %>%
  left_join(PTData, by = c("Datetime" = "dateTime", "Site"))

###need rest of data: rolling join
rowsThatNeedData <- x1 %>%
  filter(is.na(USGSDischarge))

PTData2 <- PTData %>%
  rename(Datetime = dateTime) %>%
  filter(!is.na(Site)) %>%
  mutate(environmentalDataMeasurementTime = Datetime)

rowsThatNeedData <- data.table(rowsThatNeedData)
PTData2 <- data.table(PTData2)
#set keycols
keycols <- c("Datetime", "Site")
setkeyv(rowsThatNeedData, keycols)
setkeyv(PTData2, keycols)

#perform a rolling join from data.table
#first joins on sitename then datetime
#for data where we have a stationary site attached, discharge data is from the closest hour. Otherwise, dishcarge data is within 15 minutes
#gives df of envrionmental data and detections with closest timestamps
test <- PTData2[rowsThatNeedData, roll = "nearest", on = .(Site,Datetime), nomatch = NULL]

columnstoChange <- c(colnames(PTData)[grepl("_", colnames(PTData))], "USGSDischarge")

testReadingWithin1Hour <- test %>%
  relocate(Datetime, environmentalDataMeasurementTime) %>%
  mutate(timeDifference = ifelse(abs(difftime(environmentalDataMeasurementTime, Datetime, units = c("hours"))) >= 1, 1, 0), 
         across(all_of(columnstoChange), ~ ifelse(timeDifference == 1, NA_real_, .))
  ) 

###now all that's needed is make sure you get the columns you want, rbind this with the exact time stamp matches above, and make sure you have all rows from the original df, and
#

testReadingWithin1Hour