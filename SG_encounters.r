#########this is just a working playground for random code stuff
library(readr)
mobileDetect_2022 <- read_csv("mobileDetect_2022.csv")
View(mobileDetect_2022)

library(readr)
mobileDetect_2023 <- read_csv("mobileDetect_2023.csv")
View(mobileDetect_2023)

mobileDetectDifTags <- mobileDetect_2023 %>%
  anti_join(mobileDetect_2022, by = c("TagID","Fall 2020","Spring 2021", "Summer 2021", "Fall 2021","Spring 2022", "Fall 2022" )) %>%
  mutate(Notes = "New tag from 2022")

mobileDetectSameTags <- left_join(mobileDetect_2022, mobileDetect_2023, by = c("TagID","Fall 2020","Spring 2021", "Summer 2021", "Fall 2021",   "Spring 2022", "Fall 2022" ))

mobileDetect2023 <- mobileDetectSameTags %>%
  dplyr::rename(Total = Total.y, Ghost = Ghost.y, Notes = Notes.x) %>%
  dplyr::select(TagID, `Fall 2020`, `Spring 2021`, `Summer 2021`, `Fall 2021`, `Spring 2022`, `Fall 2022`,`Summer 2023`, Total, Ghost, Notes) %>%
  rbind(mobileDetectDifTags)

write_csv(mobileDetect2023, "mobileDetect2023_with2022Data.csv")

GhostTags <- read_csv("./data/WGFP_GhostTags.csv", 
                      col_types = cols(TagID = col_character()))
x <- GhostTags %>%
  mutate(TagID = as.character(TagID)) %>%
  select(TagID, Event, GhostDate, Comments) 

ghostOrNot <- mobileDetect2023 %>%
  mutate(TagID = as.character(TagID)) %>%
  left_join(x, by = "TagID")


confused <- ghostOrNot %>%
  filter(is.na(Event), 
         grepl("Confirmed Ghost", Notes))

differences <- ghostTagsQAQC %>%
  anti_join(confused, by = "TagID")

tagsThatAreNotGhostTags <- unique(ghostTagsQAQC$TagID)

ghostOrNotWithRevisedTags <- ghostOrNot %>%
  mutate(Notes = ifelse(TagID %in% tagsThatAreNotGhostTags, "Not a Ghost Tag; revised based off movement after Ghost Date or other.", Notes)) %>%
  mutate(Notes = ifelse(Notes == "New tag from 2022" & Event == "Ghost", "Confirmed Ghost", Notes)) %>%
  arrange(Notes)



test <- GhostTags  %>%
  anti_join(ghostOrNotWithRevisedTags , by = "TagID")
write_csv(test, "tagsToCheck.csv")

write_csv(ghostOrNotWithRevisedTags, "ghostTagsUpToDateWithComments.csv")

####check if qaqc function has notes
x <- qaqcGhostTagMovements(GhostTags = GhostTags, Movements_df = Movements_df)

#ghost tag df addin on release data
GhostTags <- read_csv("./data/WGFP_GhostTags.csv", 
                      col_types = cols(TagID = col_character()))

x <- GhostTags %>%
  select(TagID, Event, GhostDate, Comments) %>%
  left_join(Release[,-which(names(Release) %in% c("Comments", "Event"))], by = "TagID")

## get all comments from mobile sheet
ghostTagsmobile2023ForCOmments <- read_csv("ghostTagsmobile2023ForCOmments.csv")

ghostTagsJoined <- ghostTagsmobile2023ForCOmments %>%
  select(TagID, Notes) %>%
  mutate(TagID = as.character(TagID)) %>%
  right_join(x, by = "TagID") %>%
  mutate(Notes2 = ifelse(nchar(Notes) > nchar(Comments), Notes, Comments)) 


write_csv(ghostTagsJoined, "ghostTagsJoined.csv")

GhostTags <- GhostTags %>%
  mutate(GhostDate = lubridate::mdy(GhostDate))

movements <- movements_list$Movements_df
ghosttagsCondensed <- GhostTags %>%
  select(TagID, GhostDate) %>%
  left_join(movements, by = c("TagID" = "TAG"))
length(unique(ghsottagsCondensed$TagID))

x <- ghosttagsCondensed %>%
  group_by(TagID) %>%
  filter(Date > GhostDate) %>%
  summarise(total_distmovedAfterGhostDate = (sum(abs(dist_moved), na.rm = TRUE))) %>%
  arrange(desc(total_distmovedAfterGhostDate))

mobileDetect_2023 <- read_csv("mobileDetect_2023.csv")

filtered <- mobileDetect_2023 %>%
  filter(`Fall 2022` == 1, 
         `Summer 2023` == 1) %>%
  select(TagID, Total, Notes) 


filteredMovemnetsGhostDate <- filtered  %>%
  mutate(TagID = as.character(TagID)) %>%
  left_join(ghosttagsCondensed, by = "TagID") %>%
  group_by(TagID) %>%
  filter(Date > "2022-10-25") %>%
  summarise(
    GhostDate = unique(GhostDate),
    #antennasDetectedAfterGhostDate = paste(unique(det_type), collapse = ", "),
    total_distmovedBetweenFall2022AndSummer2023 = (sum(dist_moved, na.rm = TRUE)),
    #maxUpstreamDistMovedAfterGhost = max(dist_moved)
  )

write_csv(filteredMovemnetsGhostDate, "total_distmovedBetweenFall2022AndSummer2023.csv")
movements <- movements_list$Movements_df
################
library(readr)
WGFP_GhostTags_upTodate <- read_csv("WGFP_GhostTags_upTodate.csv")
library(readr)
WGFP_GhostTags_20220227 <- read_csv("WGFP_GhostTags_20220227.csv")
#x <- setdiff(WGFP_GhostTags_upTodate, WGFP_GhostTags_20220227)
ghostTagsInOldFileThatwereRemoved <- WGFP_GhostTags_20220227 %>%
  anti_join(WGFP_GhostTags_upTodate, by = "TagID") %>%
  distinct(TagID, .keep_all = TRUE)
# x1 <- x %>%
#    na.omit(ReleaseSite)
temp_ghostTags <- ghostTagsInOldFileThatwereRemoved %>%
  mutate(TagID = as.character(TagID)) %>%
  mutate(GhostDate = lubridate::mdy(GhostDate))

GhostTags <- temp_ghostTags
ghostTagsQAQC <- qaqcGhostTagMovements(temp_ghostTags, Movements_df =Movements_df)

write_csv(ghostTagsQAQC, "ghostTagsRemovedFromGhostTagFile.csv")

#get tags between 2 ranges
range1_tags <- ghosttagsCondensed %>%
  filter(Date >= "2022-10-15" & Date <= "2022-10-21") %>%
  distinct(TagID)

# Filter the dataframe for the second date range
range2_tags <- ghosttagsCondensed %>%
  filter(Date >= "2023-07-20" & Date <= "2023-07-28") %>%
  distinct(TagID)

# Find the tags that appear in both ranges
common_tags <- intersect(range1_tags$TagID, range2_tags$TagID)

filtered_df <- ghosttagsCondensed %>%
  filter(TagID %in% common_tags) %>%
  # filter(det_type == "Mobile Run" & 
  #          ((Date >= "2022-10-15" & Date <= "2022-10-21") & 
  #             (Date >= "2023-05-03" & Date <= "2023-05-06"))) %>%
  group_by(TagID) %>%
  filter(Date > GhostDate) %>%
  summarise(
    GhostDate = unique(GhostDate),
    antennasDetectedAfterGhostDate = paste(unique(det_type), collapse = ", "),
    total_distmovedAfterGhostDate = (sum(dist_moved, na.rm = TRUE)),
    maxUpstreamDistMovedAfterGhost = max(dist_moved))

write_csv(filtered_df, "fall2022_Summer2023GhostDetectionsAndMovements.csv")
################
x <- Stationary %>%
  dplyr::filter(TAG %in% c("900_230000004000"))

x1 <- combinedData_df_list$All_Detections %>%
  filter(str_detect(TAG, "228468"))
x1 <- Biomark %>%
  filter(str_detect(DEC.Tag.ID, "^999"))

tag2323AtCD1 <- Stationary_Marker_tags %>%
  dplyr::filter(TAG %in% c("00000000000000002323"),
                SCD == "CD1")# %>%
#dplyr::distinct(DTY)

plot <- tag2323AtCD1 %>%
  ggplot(aes(x = DTY, text = as.character(DTY))) +
  geom_histogram()  +
  ggtitle("Tag 2323 on CD1, Oct 18 2023 - Jan 27 2024") +
  theme_classic()
ggplotly(plot)

############
tag2323AtCD2 <- Stationary_Marker_tags %>%
  dplyr::filter(TAG %in% c("00000000000000002323"),
                SCD == "CD2") #%>%
#dplyr::distinct(DTY)

plot <- tag2323AtCD2 %>%
  ggplot(aes(x = DTY, text = as.character(DTY))) +
  geom_histogram()  +
  ggtitle("Tag 2323 on CD2, Oct 18 2023 - Jan 27 2024") +
  theme_classic()
ggplotly(plot)

############
tag4948AtCD1 <- Stationary_Marker_tags %>%
  dplyr::filter(TAG %in% c("00000000000000004948"),
                SCD == "CD1"
  )
#library(plotly)
plot <- tag4948AtCD1 %>%
  ggplot(aes(x = DTY, text = as.character(DTY))) +
  geom_histogram() +
  ggtitle("Tag 4948 on CD1, Oct 18 2023 - Jan 27 2024") +
  theme_classic()

ggplotly(plot)

tag4948AtCD2 <- Stationary_Marker_tags %>%
  dplyr::filter(TAG %in% c("00000000000000004948"),
                SCD == "CD2"
  )
plot <- tag4948AtCD2 %>%
  ggplot(aes(x = DTY, text = as.character(DTY))) +
  geom_histogram() +
  ggtitle("Tag 4948 on CD2, Oct 18 2023 - Jan 27 2024") +
  theme_classic()

ggplotly(plot)
###########3

x <- Stationary_Marker_tags %>%
  filter(SCD %in% c("CD1", "CD2"))

#########
ogStatoinary <- readRDS("./data/WGFP_Raw_20240213.rds")
Stationary <- ogStatoinary %>%
  filter(TAG == "900_226001581653",
         SCD == "CF5",
         DTY >= "2021-05-26" & DTY <= "2021-05-28")
Stationary <- Stationary %>%
  mutate(TAG = gsub("\\_", "", str_trim(TAG)), 
         DTY = ifelse(str_detect(DTY, "/"),
                      as.character(mdy(DTY)),
                      DTY)) %>%
  #taking out test_tags
  filter(!TAG %in% test_tags)

xx <- Stationary[Stationary$DTY == "2021-05-27" | Stationary$DTY == "2021-05-28",]

# xx1 <- xx %>%
#   mutate(ARR1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ lubridate::hms(ARR) - hours(12),
#                           str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ lubridate::hms(ARR),
#                           
#                           str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ lubridate::hms(ARR),
#                           str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ lubridate::hms(ARR) + hours(12),
#                           #if it doesn't detect PM or AM just do lubridate::hms(ARR)
#                           str_detect(ARR, "PM|AM") == FALSE ~ lubridate::hms(ARR)),
#   ) %>%
#   #but that also means that as_datetime reads those as periods and doesn't play well with the 0s interval specifically
#   #so we need to con
#   mutate(ARR2 = as.character(as_datetime(ARR1))), 
#          ARR = ifelse(ARR2 == "1970-01-01", "00:00:00", str_trim(str_sub(ARR2, start = 11, end = -1))) %>%
#   select(-c(ARR1, ARR2))
# mutate(ARR1 = dplyr::case_when(stringr::str_detect(ARR, "AM") & stringr::str_detect(ARR, "^12:") ~ lubridate::hms(ARR) - hours(12),
#                         stringr::str_detect(ARR, "PM") & stringr::str_detect(ARR, "^12:") ~ lubridate::hms(ARR),
#                         
#                         stringr::str_detect(ARR, "AM") & stringr::str_detect(ARR, "^12:", negate = TRUE) ~ lubridate::hms(ARR),
#                         stringr::str_detect(ARR, "PM") & stringr::str_detect(ARR, "^12:", negate = TRUE) ~ lubridate::hms(ARR) + hours(12),
#                         #if it doesn't detect PM or AM just do lubridate::hms(ARR)
#                         stringr::str_detect(ARR, "PM|AM") == FALSE ~ lubridate::hms(ARR)),
# ) %>%
# mutate(ARR2 = as.character(as_datetime(ARR1)))
# , 

#########
Marker_Tag_data <- Stationary_Marker_tags

x <- Stationary_Marker_tags %>%
  dplyr::group_by(TAG, SCD) %>%
  dplyr::summarise(totalDetectionsSinceProjectInception = n())

y <- Stationary_Marker_tags %>%
  dplyr::filter(SCD == "HP3") %>%
  dplyr::count(SCD, TAG, name = "totalDetectionsSinceProjectInception")

##########
x = select(ENC_Release1, dplyr::matches(paste0("(", c(RedBarnCodes, HitchingPostCodes), ")_n")))
x = rowSums(select(ENC_Release1, ends_with("_n")) == TRUE)



x <- ENC_Release1 %>%
  select(c(RedBarnCodes, HitchingPostCodes, ConfluenceCodes, 
           ConnectivityChannelDownstreamCodes, ConnectivityChannelSideChannelCodes,
           ConnectivityChannelUpstreamCodes, MobileRunCodes, 
           WindyGapAntennaSiteCode, KaibabParkAntennaSiteCode,
           RiverRunAntennaSiteCode, FraserRiverCanyonAntennaSiteCode, "Recapture"), TAG) %>%
  mutate(
    TotalEncounters = rowSums(select(., dplyr::ends_with(c(RedBarnCodes, HitchingPostCodes, ConfluenceCodes, 
                                                           ConnectivityChannelDownstreamCodes, ConnectivityChannelSideChannelCodes,
                                                           ConnectivityChannelUpstreamCodes, MobileRunCodes, 
                                                           WindyGapAntennaSiteCode, KaibabParkAntennaSiteCode,
                                                           RiverRunAntennaSiteCode, FraserRiverCanyonAntennaSiteCode, "Recapture"))) == TRUE)
  ) %>%
  mutate(test = case_when(select(c(RedBarnCodes)) == TRUE ~ "go"))

x <- ENC_Release22 %>%
  filter(TAG == "230000143638")

og <- ENC_ReleaseOG %>%
  select(TAG, through_dam) %>%
  filter(through_dam == "Went through dam or Connectivity Channel")
joined <- ENC_ReleaseJoined %>%
  select(TAG, through_dam) %>%
  filter(through_dam == "Went through dam or Connectivity Channel")

xxx1 <- anti_join(og, joined, by = c("through_dam"))

setdiff(joined, og)
#########
##separate b3 into 2 new antennas: schmuck channel (current location?) and auxiliary/dam
Biomark_cleanDate <- Biomark %>%
  #make a column for Scan>Date if parentheses are detected in the string, that means the format is in mdy
  # and we want to convert it to YYYYMMDD format. elsewise, leave it as is
  mutate(Scan.Date = ifelse(str_detect(Scan.Date, "/"),
                            as.character(lubridate::mdy(Scan.Date)),
                            Scan.Date)) 

Biomark_cleanDateWG <- Biomark_cleanDate %>%
  filter(!Reader.ID %in% c("A2", "B2"))

SchmuckChannel <- Biomark_cleanDateWG %>%
  filter(Scan.Date >= "2021-12-31") %>%
  mutate(Reader.ID = "B1")

Auxiliary <- Biomark_cleanDateWG %>%
  filter(Scan.Date < "2021-12-31") %>%
  mutate(Reader.ID = "A1")

allBiom <- bind_rows(SchmuckChannel, Auxiliary)

kaibabPark <- Biomark_cleanDate %>%
  filter(Reader.ID %in% c("A2", "B2"))

allBiom2 <- bind_rows(allBiom, kaibabPark)

write_csv(allBiom2, "Biomark_Raw_20221102.csv")
#####
allDetectionsAndRecaptures %>%
  filter(Event %in% c("WG1"))


######stuff from 2021 and beyond
library(tidyverse)
library(readxl)
library(lubridate)
library(fishualize)
library(randomcoloR)
library(ColorPalette)

start_color <- randomColor()

comp_cols <- c(start_color, 
               complementColors(start_color, 5))
tetra_cols <- c(start_color, tetradicColors(start_color, 5))
#library(threadr) #needed for period to string function



#Stationary = read.csv(paste0("WGFP_Raw_20211109.csv"), colClasses = )
Stationary = read.csv(paste0("WGFP_Raw_20211130.csv"))
#Stationary11 = read.csv(paste0("WGFP_Raw_20211122_1.csv"), colClasses = c(rep("character",11)))

# Read mobile antenna detections
Mobile = read.csv("WGFP_MobileDetections.csv", colClasses=c(rep("character",10)))

#Read Biomark
# need to be put in with decimal registering as "," because otherwise it won't bring in the full DEC.Id tag
#biomark1 <- read_csv("Biomark_Raw_20211109.csv", col_types = "cccccccccccccccc")
# biomark_col_names <- c("Scan.Date","Scan.Time","Download.Date", "Download.Time" ,"Reader.ID","Antenna.ID","HEX.Tag.ID","DEC.Tag.ID","Temperature.C",
#     "Signal.mV","Is.Duplicate","Latitude","Longitude","File.Name")
# b <- read_csv("Biomark_Raw_20211109.csv", col_types = "ctcccccccccccccc", col_names = biomark_col_names)
# 
# b <- b[-1,]

Biomark <- read.csv("Biomark_Raw_20211109_1.csv", dec = ","
                    #, Scan.Time= strptime(Scan.Time)
                    #colClasses = c("character", "character", rep("character", 12))
                    )

# Biomark1 <- read.csv("Biomark_Raw_1.csv", dec = ","
#                     #, Scan.Time= strptime(Scan.Time)
#                     #colClasses = c("character", "character", rep("character", 12))
# )
#biomark11 <- read.csv("Biomark_Raw_2.csv", dec = ",")

# Read release data
#Release = read.csv("WGFP_ReleaseData.csv",colClasses=c(rep("character",19)))
Release = read.csv("WGFP_ReleaseData_Master.csv",colClasses=c(rep("character",18)))


#allll <- merge(Stationary, Mobile, all = TRUE)

# xRelease$Date <- as_date(mdy(xRelease$Date))
# 
# yRelease$Date <- as_date(mdy(yRelease$Date))
#list of dataframes that the function returns


#in DF list: ENC_ALL is df with just tags and all the raw numbers of the detections at each site.
# WGFP Clean is the clean data file for windy gap (i guess I should also include clean biomark?)
# ENC_release2 has all tags, including release site info, and which antennas they were seen at, as well as other summary info
## good to have to filter and view one spefic tag's history
# All Detecitons is just one df of mobile, biomark and stationary antennas including UTMs, and date and time of detection. 
## good to have to be able to make a encounter history by week, or by day like matt did. Can make new column with "state" and work from there
## will need to look at lubridate sheet a lot probably
# unknown tags is a list of tags that were detected on antennas that aren't in the release file. 
df_list <- WGFP_Encounter_FUN(Stationary = Stationary, Mobile = Mobile, Release= Release, Biomark = Biomark)

ENC_Release2_1 <-  df_list$ENC_Release2
All_Detections_1 <- df_list$All_Detections
WGFP_Clean_1 <- df_list$WGFP_Clean

unknown_tags_1 <-df_list$Unknown_Tags
# ENC_Release2_2 <-  df_list$ENC_Release2
# All_Detections_2 <- df_list$All_Detections
# WGFP_Clean_2 <- df_list$WGFP_Clean
x <- ENC_Release2_1 %>%
  filter(TotalStationary >= 5)
# x <- All_Detections_1 %>%
#   select(-Scan_Time)
# 
# y <- All_Detections_2 %>%
#   select(-Scan_Time)
# 
# z <- anti_join(y, x)

unknown_tags_2 <-df_list$Unknown_Tags

#### data exploratoin/filtering

# x <- df_list$All_Detections %>%
#   filter(TAG == 230000292262) %>%
#   distinct(Scan_Date, .keep_all=TRUE)
# 
# x$Scan_Date
# 
# y <- df_list$All_Detections %>%
#   filter(Scan_Date >= as.Date("2020-10-16") & Scan_Date <= as.Date("2021-04-04"),
#          Site_Code == "RB1")
# max(as.numeric(WGFP_Clean$NCD), na.rm = TRUE)
# NCDlist <- unique(as.numeric(WGFP_Clean$NCD))
# 
x <- WGFP_Clean %>%
  # filter(TAG == 900230000228822)
  filter(DTY >= as.Date("2020-12-03") & DTY <= as.Date("2021-08-15"),
         TAG == "230000228929")

max(df_list$All_Detections$Scan_Date)
# x <- Stationary %>%
#   mutate(duplicated1 = duplicated(Stationary))
# 
# duplicate_rows_stationary <- subset(x, duplicated1 == TRUE)
# u_dup_tags <- unique(duplicate_rows_stationary$TAG)

# Combine Biomark ---------------------------------------------------------
# don't need to do this piece again
#bring in csvs
below_kaibab_1 <- read_excel("Biomark\\Kaibab_Park\\CR_KB_B2_20210916.xlsx", sheet = "Downloaded Tag IDs")
below_kaibab_2 <- read_excel("Biomark\\Kaibab_Park\\CR_KB_B2_20211006.xlsx", sheet = "Downloaded Tag IDs")
below_kaibab_3 <- read_excel("Biomark\\Kaibab_Park\\CR_KB_B2_20211019.xlsx", sheet = "Downloaded Tag IDs")


below_WG_1 <- read_excel("Biomark\\Below_Windy_Gap\\CR_WG_B1_20210916.xlsx", sheet = "Downloaded Tag IDs")
below_WG_2 <- read_excel("Biomark\\Below_Windy_Gap\\CR_WG_B1_20211006.xlsx", sheet = "Downloaded Tag IDs")
below_WG_3 <- read_excel("Biomark\\Below_Windy_Gap\\CR_WG_B1_20211021.xlsx", sheet = "Downloaded Tag IDs")

biomark <- bind_rows(below_kaibab_1,below_kaibab_2,below_kaibab_3,below_WG_1,below_WG_2,below_WG_3)
biomark$`Scan Date` <- as_date(mdy(biomark$`Scan Date`))

biomark1 <- biomark %>%
  distinct()

#str_replace(biomark$`DEC Tag ID`, "\\.", "")

# gets rid of period in tag number/makes new column TAG
# and gets rid of duplicate rows that were
# x <- duplicated(biomark)
# x <- biomark %>%
#   mutate(duplicated1 = duplicated(biomark))
# 
# duplicate_rows <- subset(x, duplicated1 == TRUE)

#write_csv(biomark1, "Biomark_Raw_1.csv")


biomark1 <- read_csv("Biomark_Raw_20211109.csv", col_types = "Dccccccccccccccc")

#this line is in the wgfp_encounters function
# biomark1 <- biomark22 %>%
#   mutate(TAG = str_replace(biomark$`DEC Tag ID`, "\\.", "")) %>%
#   distinct()

# x <- Stationary %>%
#   distinct()

Stationary2 <- Stationary %>%
  select(DTY, ARR, TAG, SCD, ANT) %>%
  rename()

Mobile2 <- Mobile %>%
  select()


#999.000000007585


# Intervals ---------------------------------------------------------------

library(fishualize)
All_events <- df_list$All_Events
Enc_release_data <- df_list$ENC_Release2
# x <- All_detections$Scan_Date
# earliest_date <- min(x)
# recent_date <- max(x)
# strptime(min(x), format = "%y/%m/%d")
# ymd(min(x))
# interval(start = min(x), end = max(x))
# difftime(as.POSIXct(recent_date), as.POSIXct(earliest_date))

# Error in as.POSIXct.default(time1) : 
#   do not know how to convert 'time1' to class "POSIXct"
# solved because I was trying to substract the dates(-) not give 2 arguments with comma
#ceiling rounds up to nearest integer larger than x. 
#weeks_since_launch<- as.numeric(ceiling(difftime(max(x), min(x), units = "weeks")))
##Weeks
all_events_05 <- All_events %>%
  mutate(weeks_since = as.numeric(ceiling(difftime(Date, min(Date), units = "weeks")))
         )

#unique tags by site and Day

# don't need this part anymore since all_detections now contains release data
# spc_enc_only <- Enc_release_data %>%
#   select(Species, Length, Weight, TAG)
# 
# detections_and_species <- left_join(All_detections_05, spc_enc_only, by = "TAG")

All_events_05 %>%
  count(Event, Species, weeks_since) %>%
  ggplot(aes(x = weeks_since, y = n, fill = Event)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Weekly detections by Site")

All_events_05 %>%
  count(Event, Species) %>%
  ggplot(aes(x = Species, y = n, fill = Event)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(title = "Raw Number of detections by Species and Site")

All_events_05 %>%
  distinct(TAG, Event, .keep_all = TRUE) %>%
  count(Event,Species) %>%
  ggplot(aes(x = Event, y = n, fill = Species)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Species Detections by Site, Controlled by Day", subtitle = "A fish with a million detections on one day only registers once") +
  ylab("Number of Unique Days") +
  xlab("Site") +
  scale_fill_fish_d(option = "Oncorhynchus_mykiss", begin = .1, end = 1) + #Oncorhynchus_mykiss #beginning and end adjust the color pallete 
  add_fishape(family = "Salmonidae",
              option = "Oncorhynchus_nerka",
               xmin = 1, xmax = 3, ymin = 200, ymax = 400,
              fill = fish(option = "Oncorhynchus_nerka", n = 4)[2],
               alpha = 0.8
              )

spp <- fishualize::fish_palettes()
library(rfishbase)
# 2. Get data on the included species from FishBase using the rfishbase package
dt <- rfishbase::species(gsub("_"," ", spp))

###Weeks since datawrangling
All_events_05 <- All_events %>%
  mutate(weeks_since = as.numeric(ceiling(difftime(Date, min(Date), units = "weeks")))
  )

All_events_1 <- All_events_05 %>%
  distinct(TAG, Event, weeks_since, .keep_all = TRUE)

All_events_weeks <- pivot_wider(data = All_events_1, id_cols = TAG, names_from = weeks_since, values_from = Event)

## Days
All_events <- df_list$All_Events

All_events_days <- All_events %>%
  mutate(days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days")))
  )

test <- All_events_days %>%
  filter(TAG %in% c("230000224371"))

All_events_days1 <- All_events_days %>%
  distinct(TAG, Event, days_since, .keep_all = TRUE)

test_days <- pivot_wider(data = All_events_days1, id_cols = TAG, names_from = days_since, values_from = Event)


single_tag <- All_events_days1 %>%
  select(Date, Datetime,TAG,Event,ReleaseSite,RecaptureSite, days_since) %>%
  filter(TAG %in% "230000224371") 

all_tags <- All_events_days1 %>%
  select(Date, Datetime,TAG,Event,ReleaseSite,RecaptureSite, days_since) 

days <- data.frame(days_since = 1:455)

x <- full_join(days, single_tag, by = "days_since")

all_tags_combined <- full_join(days, all_tags, by = "days_since")

x1 <- x %>%
  group_by(days_since) %>%
  filter(is.na(Datetime)|Datetime == max(Datetime)) %>% ## need to keep NA entries
  ungroup() #need to ungroup in order to have "lag" work with making new column
    # mutate(
    #   #Date = Date,
    #           Datetime = max(Datetime),
    #           #TAG = TAG,
    #           #Event = Event
    #           )
  #distinct(days_since, .keep_all = TRUE)
    
#these designations work for after a grouping by day and getting the event the fish was last at; 
    # but if a fish used to be in state A, and missed a antenna and only was seen on HP3, that should be a US movement but as of now is classified as Upstream

    
x2 <- x1 %>%
  mutate(State = case_when(Event == "Release" & ReleaseSite == "Fraser River Ranch" ~ "F",
                           Event == "RB1" ~ "H", #downstream movement
                           Event == "RB2" ~ "G", #upstream movet
                           Event == "HP3" ~ "J", #DS
                           Event == "HP4" ~ "I", #US
                           Event == "CF5" ~ "L", #DS
                           Event == "CF6" ~ "K", #US
                           Event == "B3" ~ "C",
                           ))
         #                   lag(State) == "RF" ~ "F",
         #                   Event == "CF5" & lag(State) == "F" ~ "L")
         #   #lag(days_since, n = 1L)
         # ) #end of mutate

# can try and pivot wider and 
test_wider <- pivot_wider(data = x1, id_cols = TAG, names_from = days_since, values_from = Event)
test_wider <- pivot_wider(data = data1, id_cols = TAG, names_from = days_since, values_from = Event)



non_na_rows <- which(!is.na(x3$State))

x3$Event[non_na_rows[1]]

#create list assigning states values 
#then can create simple if statement checking if value is > than other
# but will still need to see which exact states those are 
# because you have to know which state it resides
x3 <- x2 %>%
  filter(days_since %in% (364:418))

length(non_na_rows)
#x3$State[3:30] <- 0
x3$State[(non_na_rows[1]+1):(non_na_rows[1+1]-1)] <- 0

#x3 is a df where there is only 1 event per days_since
#now that I know you only need 0's in between, makes it easier
States_function <- function(x3) {
  
  #getting a days df to bind on
  days1 <- data.frame(days_since = 1:455, TAG = rep(NA, 455), State = rep(NA, 455))
  days2<- pivot_wider(data = days1, id_cols = TAG, names_from = days_since, values_from = State)
  # gets a list of which rows aren't NA in the State column
  non_na_rows <- which(!is.na(x3$State))
  
  #replaces the NA entries in x3$TAG with the tag number. needed for row binding with TAG column later
  #works for now because there's only 1 tag in the df used (x3)
  #tag1 <- unique(x3$TAG)[2]
  x3 <- x3 %>%
    replace_na(list(TAG = unique(x3$TAG)[2]))
  
  
  for (i in 1:length(non_na_rows)) {
    if (x3$State[non_na_rows[i]] == "F" & x3$State[non_na_rows[i+1]] == "L") {
      x3$State[(non_na_rows[i]+1):(non_na_rows[i+1]-1)] <- 0
      #x3$State[non_na_rows[i]+1:non_na_rows[i+1]-1]
    } 
    
    # if (x3$State[non_na_rows[i]] == "RF" & x3$State[non_na_rows[i+1]] == "L") {
    #   x3$State[non_na_rows[i]+1:non_na_rows[i+1]-1]
    # } 
    # 
    else {
      #print("False")
    }
  }
  
  new_df <- pivot_wider(data = x3, id_cols = TAG, names_from = days_since, values_from = State) 
  
  new_df1 <- bind_rows(days2, new_df)
  return(new_df1)
  
}

y <- States_function(x3)

y1 <- y %>%
  select(TAG, 360:417)

# rowShift <- function(x, shiftLen = 1L) {
#   r <- (1L + shiftLen):(length(x) + shiftLen)
#   r[r<1] <- NA
#   return(x[r])
# }
    
all_tags_combined1 <- all_tags_combined %>%
  group_by(days_since, TAG) %>%
  filter(is.na(Datetime)|Datetime == max(Datetime)) ## need to keep NA entries

all_tags_combined2 <- all_tags_combined1 %>%
  mutate(State = case_when(Event == "Release" & ReleaseSite == "Fraser River Ranch" ~ "F",
                           Event == "RB1" ~ "H", #downstream movement
                           Event == "RB2" ~ "G", #upstream movet
                           Event == "HP3" ~ "J", #DS
                           Event == "HP4" ~ "I", #US
                           Event == "CF5" ~ "L", #DS
                           Event == "CF6" ~ "K", #US
                           Event == "B3" ~ "C",
  ))

test11 <- pivot_wider(data = all_tags_combined2, id_cols = TAG, names_from = days_since, values_from = State,)
# mutate(
#   #Date = Date,
#   Datetime = max(Datetime),
#   TAG = TAG
#   #Event = Event
# )

df <- tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)  

y1 <- distinct(df, diff = abs(x - y))

y1 <- distinct(starwars, across(contains("color")))
starwars
# function that will tell if something is upstream of something else with 2 inputs; returns True or False (or maybe upstream or downstream)
#     or maybe could even by done with a list or df
# if release site is "x" and event is "y", state is "z"
# if no string detected in Event, continue putting previous state in
# if string detected: 
#   if return of upstream function is "upstream", put a upstream moving/"dummy" state
#   if return is downstream, put the downstream moving state  for that specific spot 
# if the string detected is a specific upstream moving state, then the next state is going to be another specific state because that's the only one it can be. 
#     same with downstream

# if there are 2 events on the same day, see which one occurred later in the day and take that one. 

# once df is filled out, convert to wide format and bind rows with other df



#just filtering days stuff
x <- All_detections %>%
  distinct(TAG, Site_Code, Scan_Date, .keep_all = TRUE)

### Getting times correct

x <- Biomark$Scan.Time[71755]
y <- mdy(x)
str_length(x)
str_detect(x, "/")
# if the string sonctains //
# Stationary1 <- Stationary %>%
#   filter(TAG == "900_230000228791")
#   #mutate(ARR1 = hms(ARR))


# Correcting bad timestamps in stationary file ----------------------------


# this is typically the problematic date range
Stationary12 <- Stationary %>%
  filter(
    DTY >= as.Date("2021-03-02") & DTY <= as.Date("2021-04-06"),
         SCD == "RB1",
         TAG != "900_230000228791")
         #str_length(ARR) <8)

# #this is the same as the filter right now in WGFP function
# WGFP_NoMarkers_1 <- Stationary %>%
#   mutate(TAG = str_replace(TAG, "\\_", "")) %>%
#   filter(str_detect(TAG, "^900"), 
#          !TAG %in% c("900230000102751","900226001581072","900230000004000"))
#            # Stationary$TAG !=  &
#            # Stationary$TAG !=  &
#            # Stationary$TAG !=  &
#            #this one is the ghost tag removed 4/6 from RB1
#            # (Stationary$TAG != "900230000228791" | DTY <= as.Date("2021-12-01"))
#   
# 
# WGFP_NoMarkers_11 <- Stationary11 %>%
#   mutate(TAG = str_replace(TAG, "\\_", "")) %>%
#   filter(str_detect(TAG, "^900"), 
#          !TAG %in% c("900230000102751","900226001581072","900230000004000"),
#          # Stationary$TAG !=  &
#          # Stationary$TAG !=  &
#          # Stationary$TAG !=  &
#          #this one is the ghost tag removed 4/6 from RB1
#          (Stationary$TAG != "900230000228791" | DTY <= as.Date("2021-03-02"))
#   )
# 
# 
# #see which rows in x are different from those in Y
# diferences <- anti_join(WGFP_NoMarkers, WGFP_NoMarkers_1)

#gets the rows that are have probematic ARR
problem_times <- Stationary %>%
  filter(str_length(ARR) < 8) %>%
  mutate(month111 = month(DTY)) 

#gets which ecat days are problematic
problem_times %>%
  distinct(DTY) 
  #distinct(month111, SCD,  .keep_all = TRUE)




write_csv(Stationary1, "WGFP_Raw_20211122.csv")
# there was a problem with the Timestamps used in the files where 791 (the ghost tag) was present and also in the APril detections. So this part and above
# is a 1 time solution to figure that all out
new_times <- read.csv("new_times.csv", colClasses = c(rep("character",11)))
#should be about 30030 entries
new_times1 <- new_times %>%
  filter(!TAG %in% c("900_230000228791"))

no_problem_times <- Stationary %>%
  filter(str_length(ARR) >= 8)

new_Stationary <- bind_rows(new_times1, no_problem_times)

#should be 30030 entries that are different
diferences <- anti_join(new_Stationary, Stationary)

write_csv(new_Stationary, "New_Stationary.csv")

# 
#   mutate(month111 = month(DTY)) %>%
#   distinct(month111, SCD, DTY, .keep_all = TRUE)

new_Stationary <- read.csv("New_Stationary.csv", colClasses = c(rep("character",11)))
x <- new_Stationary %>%
  filter(str_length(ARR) < 8)

new_Stationary1 <- new_Stationary %>%
  mutate(ARR1 = case_when(str_detect(ARR, "AM") ~ hms(ARR) ,
                          str_detect(ARR, "PM") ~ hms(ARR) + hours(12),
                          #if it doesn't detect PM or AM just do hms(ARR)
                          str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR))
         ) %>%
  mutate(ARR2 = as.character(as_datetime(ARR1)), 
         ARR3 = str_sub(ARR2, start = 11, end = -1)) %>%
  select(Code, DTY, ARR3,  TRF,  DUR,  TTY,  TAG,  SCD,  ANT,  NCD,  EFA ) %>%
  rename(ARR = ARR3)

x <-unique( str_detect(Stationary$ARR, "PM|AM") )
length(x) >1
y <- unique(str_detect(new_Stationary1$ARR, "PM|AM"))
length(y) >1

#as_datetime(as.character(new_Stationary1$ARR1))

# library(threadr)
# #this funtion doesn't deal with decimal seconds very
# period_to_string(new_Stationary1$ARR1)
# 
# period_to_seconds(new_Stationary1$ARR1)
# str_detect(new_Stationary1$ARR[147013], "PM|AM") == FALSE
# x <- format(new_Stationary1$ARR1, "%H:%M:%S")
# 
# new_Stationary1 <- new_Stationary1 %>%
#   mutate(ARR2 = as.POSIXct(ARR1, origin = "1960-01-01")) %>%
#   mutate(ARR3 = str_sub(ARR1, start = 11, end = -3))
# as.POSIXct(new_Stationary1$ARR1, origin = "1960-01-01")
# as.character(new_Stationary1$ARR1)

Release1 <- Release %>%
  rename(TAG = TagID)
x <- anti_join(ENC_Release2_1, Release1, by = "TAG")

All_Detections_1 %>%
  filter(DTY >= "2020-12-03" & DTY <= "2021-04-15")
         
list1 <- list(All_Detections_1$TAG, All_Detections_1$Scan_Date, All_Detections_1$Site_Code)
sapply(list1, `[`, 1)


x<-x %>%
  #distinct(TAG, Site_Code, Scan_Date,.keep_all=TRUE) %>%
  distinct(TAG, .keep_all = TRUE) %>%
  filter(Species == "MTS")
  
  #unique(All_Detections_1[,c("TAG", "Scan_Date", "Site_Code")])
x <- unique(All_Detections_1[,c("TAG", "Scan_Date", "Site_Code")])

unique(df_list$All_Detections$ReleaseSite)

All_Detections_21 <- anti_join(All_detections, Release1, by = "TAG")
unique(All_Detections_21$TAG)

#gets x colors of the theme for that fish species
x <- fish(n = 5,
  
  option = "Oncorhynchus_mykiss"
)


# encounter histories condensing ------------------------------------------

all_enc1 <- All_detections %>%
  count(TAG, Site_Code, name = "Encounters")

# x <- all_enc1 %>%
#   filter(Site_Code == "B3")

all_enc12 <- All_detections %>%
  count(TAG, Site_Code, name = "Encounters") 

all_enc12 <- pivot_wider(data = all_enc12, id_cols = TAG, names_from = Site_Code, values_from = Encounters)
all_enc12[is.na(all_enc12)]=0

ENC_ALL <- all_enc12 %>%
  rename(RB1_n = RB1,
         RB2_n = RB2,
         HP3_n = HP3,
         HP4_n = HP4,
         CF5_n = CF5,
         CF6_n = CF6,
         M1_n = M1,
         M2_n = M2,
         B1_n = B3,
         B2_n = B4
         ) %>%
  select(TAG, RB1_n,RB2_n,HP3_n, HP4_n, CF5_n, CF6_n, M1_n, M2_n, B1_n, B2_n)


#x <- anti_join(all_enc123, ENC_ALL)

  # mutate(RB1_n = ifelse(Site_Code == "RB1", Encounters, 0),
  #        RB2_n = ifelse(Site_Code == "RB2", Encounters, 0),
  #        HP3_n = ifelse(Site_Code == "HP3", Encounters, 0),
  #        HP4_n = ifelse(Site_Code == "HP4", Encounters, 0),
  #        CF5_n = ifelse(Site_Code == "CF5", Encounters, 0),
  #        CF6_n = ifelse(Site_Code == "CF6", Encounters, 0),
  #        M1_n = ifelse(Site_Code == "M1", Encounters, 0),
  #        M2_n = ifelse(Site_Code == "M2", Encounters, 0),
  #        B1_n = ifelse(Site_Code == "B3", Encounters, 0),
  #        B2_n = ifelse(Site_Code == "B4", Encounters, 0),
  #        
  #        
  #        )# end of mutate

# library(reshape2)
# x <- all_enc1 %>%
#   dcast(TAG~RB1_n + RB2_n, fun.aggregate = sum)
  #cast(TAG~c("RB1_n"))
  #extract(Encounters, into = "A")

# # Stationary Antennas
# StationaryEnc= WGFP_Clean %>%
#   count(TAG,SCD, name = "Encounters")
# 
# # Mobile Antennas
# MobileEnc= Mobile %>%
#   mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
#   count(TAG,MobileAnt, name = "Encounters")
# 
# # Biomark Antennas
# BiomarkEnc <- biomark2 %>%
#   mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
#   count(TAG, Reader.ID, name = "Encounters")

### Separate Encounter histories by Antenna ###
# Enc_RB1 = StationaryEnc%>%
#   filter(SCD == "RB1")
# sum(Enc_RB1$Encounters)
# 
# Enc_RB2 = StationaryEnc%>%
#   filter(SCD == "RB2")
# sum(Enc_RB2$Encounters)
# 
# Enc_HP3 = StationaryEnc%>%
#   filter(SCD == "HP3")
# sum(Enc_HP3$Encounters)
# 
# Enc_HP4 = StationaryEnc%>%
#   filter(SCD == "HP4")
# sum(Enc_HP4$Encounters)
# 
# Enc_CF5 = StationaryEnc%>%
#   filter(SCD == "CF5")
# sum(Enc_CF5$Encounters)
# 
# Enc_CF6 = StationaryEnc%>%
#   filter(SCD == "CF6")
# sum(Enc_CF6$Encounters)
# 
# Mob_M1 = MobileEnc%>%
#   filter(MobileAnt == "M1")
# sum(Mob_M1$Encounters)
# 
# Mob_M2 = MobileEnc%>%
#   filter(MobileAnt == "M2")
# sum(Mob_M2$Encounters)
# 
# Bio_B1 <- BiomarkEnc %>%
#   filter(Reader.ID == "B3")
# 
# Bio_B2 <- BiomarkEnc %>%
#   filter(Reader.ID == "B4")
# # Make Individual Encounter tables
# 
# RB1=Enc_RB1 %>%
#   select(TAG,Encounters)%>%
#   rename(RB1_n = Encounters)
# 
# RB2=Enc_RB2 %>%
#   select(TAG,Encounters)%>%
#   rename(RB2_n = Encounters)
# 
# HP3=Enc_HP3 %>%
#   select(TAG,Encounters)%>%
#   rename(HP3_n = Encounters)
# 
# HP4=Enc_HP4 %>%
#   select(TAG,Encounters)%>%
#   rename(HP4_n = Encounters)
# 
# CF5=Enc_CF5 %>%
#   select(TAG,Encounters)%>%
#   rename(CF5_n = Encounters)
# 
# CF6=Enc_CF6 %>%
#   select(TAG,Encounters)%>%
#   rename(CF6_n = Encounters)
# 
# M1=Mob_M1 %>%
#   select(TAG,Encounters)%>%
#   rename(M1_n = Encounters)
# 
# M2=Mob_M2 %>%
#   select(TAG,Encounters)%>%
#   rename(M2_n = Encounters)
# 
# B1=Bio_B1 %>%
#   select(TAG,Encounters)%>%
#   rename(B1_n = Encounters)
# 
# B2=Bio_B2 %>%
#   select(TAG,Encounters)%>%
#   rename(B2_n = Encounters)
# 
# ### Merge All Encounter Histories by antenna ###
# 
# # Merge only takes 2 values
# #RB
# ENC_RB= merge(RB1,RB2, all=TRUE)
# ENC_RB[is.na(ENC_RB)]=0
# #HP
# ENC_HP= merge(HP3,HP4, all=TRUE)
# ENC_HP[is.na(ENC_HP)]=0
# #CF
# ENC_CF= merge(CF5,CF6, all=TRUE)
# ENC_CF[is.na(ENC_CF)]=0
# #Mobile
# ENC_M1M2 = merge(M1,M2, all=TRUE)
# ENC_M1M2[is.na(ENC_M1M2)]=0
# #Biomark
# ENC_B1B2 = merge(B1,B2, all=TRUE)
# ENC_B1B2[is.na(ENC_B1B2)]=0
# 
# 
# # Merge RB HP
# ENC_RBHP= merge(ENC_RB,ENC_HP, all=TRUE)
# ENC_RBHP[is.na(ENC_RBHP)]=0
# 
# 
# # Merge RBHP with CF
# ENC_ALLStationary= merge(ENC_RBHP,ENC_CF, all=TRUE)
# ENC_ALLStationary[is.na(ENC_ALLStationary)]=0
# 
# 
# # Merge ENC_AllStationary with ENC_M1M2
# # was getting dupicate tag numbers at the very end bc I wasn't stripping the 900 from the TAG at the very beginning of the function for BIOmark and Mobile
# # so it wasn't merging correctly. Then the 900 was stripped later but by then it didn't make a dif
# ENC_Stationary_M1M2= merge(ENC_ALLStationary,ENC_M1M2, all=TRUE)
# ENC_Stationary_M1M2[is.na(ENC_Stationary_M1M2)]=0
# 
# # Merge ENC_AllStationary with ENC_B1B2
# #gets dataset of all encounters on all antennas
# ENC_ALL= merge(ENC_Stationary_M1M2,ENC_B1B2, all=TRUE)
# ENC_ALL[is.na(ENC_ALL)]=0
# 

#### Merge Release data ###
Release <- Release %>%
  rename(TAG = TagID) %>%
  mutate(TAG = str_trim(TAG))

# was geting a massive dataframe because the Release df is called TAGid not TAG.
#need to actually join on full join not merge
ENC_Release <- full_join(Release, ENC_ALL,  by = "TAG")
#gets tag list that wasn't in release file
x <- ENC_Release %>%
  filter(is.na(ReleaseSite))

unknown_tags <- x$TAG
#ENC_Release11$TAG[3433:nrow(ENC_Release11)]
#ENC_Release= merge(Release,ENC_ALL, all=TRUE)
ENC_Release[is.na(ENC_Release)]=0

#### Make 1 or 0 for encounter history rather than counts ###
#gets df with TF of whether a fish was detected at a antenna
ENC_Release1 <- ENC_Release %>%
  mutate(RB1 = (RB1_n >0),
         RB2 = (RB2_n >0),
         HP3 = (HP3_n >0),
         HP4 = (HP4_n >0),
         CF5 = (CF5_n >0),
         CF6 = (CF6_n >0),
         M1 = (M1_n >0),
         M2 = (M2_n >0),
         B1 = (B1_n >0),
         B2 = (B2_n>0)) 

#summary stats of each antenna encounter
#precariously built because Row numbers are used
#but also has release data
totalcols <- ncol(ENC_Release1)

ENC_Release2 <- ENC_Release1 %>%
  #counts number opf TRUE across specified rows. negates subsequent lines of code -SG
  mutate(TotalAntennas1 = rowSums(ENC_Release1[(totalcols-9):totalcols] == TRUE),
         TotalStationary = rowSums(ENC_Release1[(totalcols-9):(totalcols-4)] == TRUE),
         TotalMobile = rowSums(ENC_Release1[(totalcols-3):(totalcols-2)] == TRUE),
         TotalBiomark = rowSums(ENC_Release1[(totalcols-1):totalcols] == TRUE),
         TotalRB = rowSums(ENC_Release1[(totalcols-9):(totalcols-8)] == TRUE),
         TotalHP = rowSums(ENC_Release1[(totalcols-7):(totalcols-6)] == TRUE),
         TotalCf = rowSums(ENC_Release1[(totalcols-5):(totalcols-4)] == TRUE)
  ) %>%
  # just says if the fish was ever detected at these sites
  mutate(RB = (RB1_n > 0 | RB2_n >0),
         HP = (HP3_n > 0 | HP4_n >0),
         CF = (CF5_n > 0 | CF6_n >0),
         Biomark = (B1_n > 0 | B2_n >0),
         Mobile = (M1_n > 0 | M2_n >0)) %>%
  filter(!ReleaseSite %in% 0)



# RECAPTURES --------------------------------------------------------------

#This section reads in recaptures, takes a df of all detection info sans release info, and the relase info datasheet
#it gets timstamps in order and corrects some column names in prep to combine
# it first binds_rows() between all_detecitons and release to get an Event called "Release" that begins the journey for the ish
# then binds_rows() on recaptures() to do the same for event "recaputre" 
# then it left joins release info so that the whole dataframe will have release info
# so now it's basically like the ALldetections df except now it includes recaptures.
# next: can replace All_detections df

# when saving csv from excel file in excel, make sure to specify TAG as a number otherwise it will try and save just the first part of TAG
# which is bullshit
recaps <- read.csv("WGFP_RecaptureData_Master.csv", colClasses = c(rep("character", 9), rep("numeric", 2), rep("character", 8)))
#recaps <- read_csv("WGFP_RecaptureData_Master.csv", col_select = c(-1),col_types = "cccccccccnncccccccc" )
#takes first column off bc for some reason there's a weird one going on
#recaps <- recaps[,-c(1, 14)]

recaps <- recaps  %>%
  select(-Num, -QAQC)

df_list <- WGFP_Encounter_FUN(Stationary = Stationary, Mobile = Mobile, Release= Release, Biomark = Biomark)

All_Detections_1 <- df_list$All_Detections

#wrangling release data times
#getting"12:00" to read 12:00:00
Release1 <- Release %>%
  rename(TAG = TagID) %>%
  mutate(TAG = str_trim(TAG),
         Event = str_trim(Event),
         Date = mdy(Date),
         Time1 = as_datetime(hm(Time)),
         Time2 = str_sub(Time1, start = 11, end = -1),
         DateTime = ymd_hms(paste(Date, Time2))) %>%
  select(RS_Num,River,ReleaseSite,Date,DateTime,UTM_X,UTM_Y,Species,Length,Weight,TAG,TagSize,Ant,Event) 

recaps1 <- Recaptures %>%
  rename(TAG = TagID) %>%
  mutate(TAG = str_trim(TAG),
         Event = str_trim(Event),
         Date = mdy(Date),
         Time1 = as_datetime(hm(Time)),
         Time2 = str_sub(Time1, start = 11, end = -1),
         DateTime = ymd_hms(paste(Date, Time2))) %>%
  select(RS_Num,River,RecaptureSite,DateTime,Date,Time2,UTM_X,UTM_Y,Species,Length,Weight,TAG,TagSize,Ant,Event) %>%
  rename(Time = Time2,
         Recap_Length = Length,
         Recap_Weight = Weight
         )

#df without release info
All_Detections_1 <- df_list$All_Detections

All_Detections_1_merge <- All_Detections_1 %>%
  mutate(Date = as.Date(Scan_Date)) %>%
  rename(
         DateTime = Scan_DateTime,
         Event = Site_Code) 

#gets a df with a event "Release"
#successfully puts 3436 rows onto the end 
all_detections_release <- bind_rows(All_Detections_1_merge, Release1)

# merge vs join; will want to do both 

detections_release_recaps <- bind_rows(all_detections_release, recaps1)

# x <- detections_release_recaps %>%
#   filter(Event %in% c("Release","Recapture and Release"))
# 
# y <- anti_join( Release1,x, by = "TAG")


#fills in release info so it is known at any row of detection
filled_in_release_rows <- left_join(detections_release_recaps, Release1, by = c("TAG"))


#this is the final df 

filled_in_release_rows_condensed <- filled_in_release_rows %>%
  select(Date.x, DateTime.x, TAG, Event.x, Species.y, Length.y, Weight.y, ReleaseSite.y, Date.y, RecaptureSite, Recap_Length, Recap_Weight, UTM_X.x, UTM_Y.x) %>%
  rename(Release_Date = Date.y,
         Date = Date.x,
         Datetime = DateTime.x,
         Event = Event.x,
         Species = Species.y,
         Release_Length = Length.y,
         Release_Weight = Weight.y, 
         ReleaseSite = ReleaseSite.y,
         UTM_X = UTM_X.x,
         UTM_Y = UTM_Y.x)


x <- filled_in_release_rows_condensed %>%
  filter(Event %in% c("Recapture and Release"))

#539086-538465 = 621: number of rows currently getting lost between condensed rows and df displaying in app
3436 - 2915
# detections_release_recaps1 <- detections_release_recaps %>%
#   select(Date, DateTime, TAG, Event, Species, Length, Weight, ReleaseSite, RecaptureSite, Recap_Length, Recap_Weight)

# detections_release_recaps11 <- detections_release_recaps1 %>%
#   filter(Event == "Release" | Event == "Recapture")
# 
# 
# x <- left_join(Release1, recaps1, by = "TAG")
# #DF wih just release and recaputres made to try and compare growth rates
# xx <- x %>%
#   select(TAG, DateTime.x, DateTime.y, Length, Weight, ReleaseSite, RecaptureSite, Recap_Length, Recap_Weight)

# x <- all_events %>%
#   filter(Event %in% c("Recapture")) %>%
#   distinct(TAG, .keep_all = TRUE)
# 
# #trying to figure out why what is being displayed in allevents tab in rshiny app is 500-600 rows diferent than what is made in the function
###SOLVED: my date range started at 2020-09-03 instead of 2020-09-01 when fish were first released

app_file <- read.csv("allevents.csv")

app_file1 <- app_file %>%
  mutate(TAG = as.character(TAG))



# differences <- anti_join(all_events, app_file1, by = "TAG")
# unique_tags <- data.frame(unique(differences$TAG))
# 
# unique_tags <- unique_tags %>%
#   rename(TAG = unique.differences.TAG.)
# #colnames(unique_tags)
# 
# #these are unique tags that don't show up in all_events in the shiny app
# x <- left_join(unique_tags, Release1, by = "TAG")
# 
# y <- left_join(x, all_events, by = "TAG")


### trying to see the dif rows from release to no relase

x <- Release1 %>%
  distinct(TAG, .keep_all = TRUE)

all_events1 <- all_events %>%
  filter(Event %in% c("Release", "Recapture and Release")) %>%
  distinct(TAG, .keep_all =TRUE)

#gets freq table of how much each one occurs
allevents1_table <- data.frame(table(all_events1$TAG))

Release1_table <- data.frame(table(Release1$TAG))

differences <- anti_join(all_events1, Release1, by = c("TAG","Event"))

#trying to see where differences are in main all events dataframe
testevents <- read_csv("allevents2.csv", col_types = cols(.default = "?", TAG = "c", UTM_X = "c", UTM_Y = "c"))
#c("TcccnncDc")
x <- anti_join(all_events, testevents)
                         
u_tags <- data.frame(unique(x$TAG))
events_list <- unique(all_events$Event)
species_list <- unique(all_events$Species)
release_site_list <- unique(all_events$ReleaseSite)

y <- x %>%
  filter(Datetime >= "2020-08-01" & Datetime <= "2021-12-14",
        Event %in% events_list,
        Species %in% species_list,
        ReleaseSite %in% release_site_list) %>%
  select(-Date)

all_events1 <- all_events %>%
  replace_na(list(Species = "No Info", ReleaseSite = "No Info"))

  #replace(is.na("Species"), "No Info")


all_detections12 <- df_list$All_Detections_Release

all_detections12 %>%
  filter(is.na(Species))

sort(unique(df_list$All_Events$ReleaseSite))

x <- df_list$ENC_Release2 %>%
  filter(TAG == "226001581749")

### incorporating recaps into enc_releases file
all_enc12 <- recaps_detections %>%
  count(TAG, Event, name = "Encounters") 

all_enc12 <- pivot_wider(data = all_enc12, id_cols = TAG, names_from = Event, values_from = Encounters)

x <- all_enc12 %>%
  replace_na(list(Species = "No Info", ReleaseSite = "No Info"))

#all_enc12[is.na(all_enc12)]=0

ENC_ALL <- all_enc12 %>%
  rename(RB1_n = RB1,
         RB2_n = RB2,
         HP3_n = HP3,
         HP4_n = HP4,
         CF5_n = CF5,
         CF6_n = CF6,
         M1_n = M1,
         M2_n = M2,
         B3_n = B3,
         B4_n = B4,
         Recap_n = Recapture
  ) %>%
  select(TAG, RB1_n,RB2_n,HP3_n, HP4_n, CF5_n, CF6_n, M1_n, M2_n, B3_n, B4_n, Recap_n)


#### Merge Release data ###
Release1 <- Release %>%
  rename(TAG = TagID) %>%
  mutate(TAG = str_trim(TAG)) %>%
  replace_na(list(Species = "No Info", ReleaseSite = "No Info")) #replaced species and releasesite to follow the same convention as AllEvents

# was geting a massive dataframe because the Release df is called TAGid not TAG.
#need to actually join on full join not merge
ENC_Release <- full_join(Release1, ENC_ALL,  by = "TAG")
#gets tag list that wasn't in release file
x <- ENC_Release %>%
  filter(is.na(ReleaseSite))

unknown_tags <- x$TAG
#ENC_Release11$TAG[3433:nrow(ENC_Release11)]
#ENC_Release= merge(Release,ENC_ALL, all=TRUE)
ENC_Release[is.na(ENC_Release)]=0

#### Make 1 or 0 for encounter history rather than counts ###
#gets df with TF of whether a fish was detected at a antenna
ENC_Release1 <- ENC_Release %>%
  mutate(RB1 = (RB1_n >0),
         RB2 = (RB2_n >0),
         HP3 = (HP3_n >0),
         HP4 = (HP4_n >0),
         CF5 = (CF5_n >0),
         CF6 = (CF6_n >0),
         M1 = (M1_n >0),
         M2 = (M2_n >0),
         B3 = (B3_n >0),
         B4 = (B4_n>0),
         Recapture = (Recap_n > 0))
  

#summary stats of each antenna encounter
#precariously built because Row numbers are used
#but also has release data
totalcols <- ncol(ENC_Release1)

ENC_Release2 <- ENC_Release1 %>%
  #counts number of TRUE across specified rows. negates subsequent lines of code -SG
  mutate(
    TotalEncounters = rowSums(ENC_Release1[(totalcols-10):totalcols] == TRUE),

    TotalAntennas1 = rowSums(ENC_Release1[(totalcols-10):totalcols-1] == TRUE),
         TotalStationary = rowSums(ENC_Release1[(totalcols-10):(totalcols-5)] == TRUE),
         TotalMobile = rowSums(ENC_Release1[(totalcols-4):(totalcols-3)] == TRUE),
         TotalBiomark = rowSums(ENC_Release1[(totalcols-2):totalcols-1] == TRUE),
         TotalRB = rowSums(ENC_Release1[(totalcols-10):(totalcols-9)] == TRUE),
         TotalHP = rowSums(ENC_Release1[(totalcols-8):(totalcols-7)] == TRUE),
         TotalCf = rowSums(ENC_Release1[(totalcols-6):(totalcols-5)] == TRUE)
  ) %>%
  # just says if the fish was ever detected at these sites
  mutate(RB = (RB1_n > 0 | RB2_n >0),
         HP = (HP3_n > 0 | HP4_n >0),
         CF = (CF5_n > 0 | CF6_n >0),
         Biomark = (B3_n > 0 | B4_n >0),
         Mobile = (M1_n > 0 | M2_n >0)) %>%
  filter(!UTM_X %in% 0) #


# Condensed ENC_hist correct ----------------------------------------------
#224371

tag_only <- df_list$All_Events %>%
  filter(TAG %in% c("230000224371"))

#makes it so when you filter on distinct, you get the first and last events for each day and every distinct event in between

a1 <- tag_only %>%
  filter(!Event %in% "Release") %>%
  group_by(Date) %>%
  mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                Datetime == max(Datetime) ~ "Last_of_day",
                                Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
         ) %>%
  ungroup()

a2 <- a1 %>%
  #group_by(min_datetTime, max_dateTime, Event) %>%
  distinct(Date, Event, TAG, first_last, .keep_all = TRUE)
# %>%
#   filter(min_datetTime = min(min))
  
# if (there are more than one antenna on one day ) {
#   display the first and last antennas that were hit that day
#   and any unique antennas in the middle of those
# }
  

# Enc Hist Wide discrepancies 11/23/22 ------------------------------------
#ReleaseEncounters_2022_11_02 <- read_csv("ReleaseEncounters_2022-11-02.csv")
ReleaseEncounters_2022_11_02 <- read_csv("ReleaseEncounters_2022-11-02.csv", 
                                         col_types = cols(TAG = col_character(), 
                                                          RS_Num = col_character(),
                                                          UTM_X = col_character(),
                                                          UTM_Y = col_character(),
                                                          TagSize = col_character(),
                                                          Mortality = col_character(),
                                                          Date = col_character(), Time = col_character()))
#View(ReleaseEncounters_2022_11_02)

real <- enc_hist_wide_list$ENC_Release_wide_summary
# 475 entries missing because of NA's though_dam
x <- anti_join(real,ReleaseEncounters_2022_11_02, by = c("through_dam"))
library(janitor)
x <- janitor::compare_df_cols(real,ReleaseEncounters_2022_11_02)
3863+613 # comes from above_below not getting assigned stations
li
x <- is.na(real)
real1 <- real %>%
  filter(is.na(through_dam) | through_dam == "Went through dam")
unique(real$through_dam)
#vroom for statoinary ?
all_events_stations_2 <- inner_join( All_events,stations, by = c( "UTM_X", "UTM_Y", "Event", "ReleaseSite")) # "Species", "Release_Length", "Release_Weight", "Event", "Date", "Time", "ReleaseSite", "Release_Date", "RecaptureSite", "Recap_Length", "Recap_Weight"

x <- all_events_stations_2 %>%
  filter(is.na(ET_STATION))

problem_entries <- All_events_days1 %>%
  filter(is.na(above_below))

sd1 <- station_data %>%
  distinct(ET_STATION, .keep_all = TRUE) %>%
  select(ReleaseSite, ET_STATION, UTM_X, UTM_Y)
test <- left_join(Release, sd1, by = c("UTM_X", "UTM_Y", "ReleaseSite"))

test1 <- test %>%
  distinct(TagID, .keep_all = TRUE)
library(purrr)

rrr <- map(Release, station_data)

stations <- station_data %>%
  rename(
    Datetime = Datetime_,
    Time = Time_) %>%
  mutate(
    Date = mdy(Date_)
  ) %>%
  distinct(ET_STATION, .keep_all = TRUE) %>%
  select(-Date_)


#massive datafrmae occurs when there are multiple rows in B for which the key columns (same-name columns by default) match the same, single row in A
#usually this means you have to make sure you join by the fields which will not have any differenitation: iun this case, "TAG", UTM_X", "UTM_Y", and "Event". The other fields are just to help keep the dataframe more concise
# date doesn't matter toi join; no matter what day the detection/event happens, the station is the same depending on UTM
#because the stations are added by joining these columns instead of site, 


all_events_stations_2 <- left_join( All_events,stations, by = c("UTM_X", "UTM_Y")) # "Species", "Release_Length", "Release_Weight", "Event", "Date", "Time", "ReleaseSite", "Release_Date", "RecaptureSite", "Recap_Length", "Recap_Weight"

testt<- all_events_stations_2 %>%
  filter(ReleaseSite.x == "Sheriff Ranch Upper Field",
         Event.x == "Release") %>%
  unique(UTM_X)



#these are the data that aren't up to date
nanss <- All_events_days1 %>%
  filter(is.na(ET_STATION), 
         #ReleaseSite == "Sheriff Ranch Upper Field",
         #Event == "Release"
         )
sheriff_ranch_upp <- station_data %>%
  filter(ReleaseSite == "Sheriff Ranch Upper Field",
         Event == "Release")


# Station NA probs --------------------------------------------------------


stations <- station_data %>%
  rename(
    Datetime = Datetime_,
    Time = Time_) %>%
  mutate(
    Date = mdy(Date_)
  ) %>%
  distinct(UTM_X, UTM_Y, .keep_all = TRUE) %>% #can't do it by ET_station because Sherriff ranch upper field and fraser river ranch the same station initially
  select(-Date_)


#massive datafrmae occurs when there are multiple rows in B for which the key columns (same-name columns by default) match the same, single row in A
#usually this means you have to make sure you join by the fields which will not have any differenitation: iun this case, "TAG", UTM_X", "UTM_Y", and "Event". The other fields are just to help keep the dataframe more concise
# date doesn't matter toi join; no matter what day the detection/event happens, the station is the same depending on UTM
#because the stations are added by joining these columns instead of site, 


all_events_stations_2 <- left_join( All_events,stations, by = c("UTM_X", "UTM_Y")) # "Species", "Release_Length", "Release_Weight", "Event", "Date", "Time", "ReleaseSite", "Release_Date", "RecaptureSite", "Recap_Length", "Recap_Weight"

yy <- all_events_stations_2 %>%
  filter(TAG.x == "230000272155") %>%
  arrange(Datetime.x)
#will get NA's based on when the station dataset was made; 
#if the station data is outdated, there will be new recent tags that have events but aren't accounted for in Station Data;
# includes release data and mobile runs
all_events_stations_21 <- all_events_stations_2 %>%
  filter(is.na(ET_STATION))

All_events_stations_3 <- all_events_stations_2 %>%
  
  rename(
    TAG = TAG.x,
    Date = Date.x,
    Time = Time.x,
    Datetime = Datetime.x,
    Event = Event.x,
    Species = Species.x,
    Release_Length = Release_Length.x,
    Release_Weight = Release_Weight.x, 
    ReleaseSite = ReleaseSite.x,
    Release_Date = Release_Date.x,
    RecaptureSite = RecaptureSite.x,
    Recap_Length = Recap_Length.x,
    Recap_Weight = Recap_Weight.x) %>%
  
  mutate(
    #River also needs to be assigned for new detections 
    River = case_when(
      (Event %in% c("RB1", "RB2")) ~ "Colorado River", # there is no is.na here because RB UTM
      (Event %in% c("HP3", "HP4")) ~ "Colorado River",
      (Event %in% c("CF5", "CF6")) ~ "Colorado River",
      (Event %in% c("B3")) ~ "Colorado River",
      (Event %in% c("B4")) ~ "Fraser River",
      TRUE ~ River
    ),
    #this fills in the NA rows so therefore accounts for new stationary and biomark detections that weren't captured when stationdata was made
    
    #if UTM's were correctly assigned initially and stationdata is up to date, this part is unnesseccary because this will all already be done with the left_join
    
    ET_STATION = case_when(
      (Event %in% c("RB1", "RB2")) ~ 4150, # there is no is.na here because RB UTM
      is.na(ET_STATION) & (Event %in% c("HP3", "HP4")) ~ 6340,
      is.na(ET_STATION) & (Event %in% c("CF5", "CF6")) ~ 9550,
      is.na(ET_STATION) & (Event %in% c("B3")) ~ 8290,
      is.na(ET_STATION) & (Event %in% c("B4")) ~ 6050,
      !is.na(ET_STATION) & (!Event %in% c("RB1", "RB2")) ~ ET_STATION),
    
    ET_STATION = case_when(River %in% "Fraser River" ~ ET_STATION + 9566, #9566 is above Fraser River Confluence
                           River %in% "Colorado River" ~ ET_STATION)
  ) %>%
  
  # mutate(
  #   ET_STATION = case_when(River %in% "Fraser River" ~ ET_STATION + 9566, #9566 is above Fraser River Confluence
  #                          River %in% "Colorado River" ~ ET_STATION)
  # )
  # this line just makes the df smaller if htere are duplicates; usually doesn't change anything since All_events has a line that does this also in the WGFP ENC hist_function
  distinct(Datetime, Event, TAG, .keep_all =TRUE) %>%
  
  select(Date, Time, Datetime, TAG, Event, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, Recap_Length, Recap_Weight, UTM_X, UTM_Y, ET_STATION)

no_stations <- All_events_stations_3 %>%
  filter(is.na(ET_STATION)) %>%
  arrange(Datetime) %>%
  distinct(TAG, .keep_all = TRUE)

# Days_since and Prev_event -----------------------------------------------

# making these columns prepares the data for making states and pivoting wider to days
All_events_days <- All_events_stations_3 %>%
  mutate(days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days")))
  )

#getting all_events down to most essential info: how a unique fish/Tag began the day, how it ended the day, and if there were events different than that in between
All_events_days1 <- All_events_days %>%
  
  group_by(Date, TAG) %>%
  mutate(first_last = case_when(Datetime == min(Datetime) & Event != "Release" ~ "First_of_day",
                                Datetime == max(Datetime) ~ "Last_of_day",
                                Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0",
                                #Event == "Release" ~ "Last_of_day"
  ),
  c_number_of_detections = n(),
  daily_unique_events = length(unique(Event))
  ) %>%
  ungroup() %>%
  #getting all_events down to most essential info: how a unique fish/Tag began the day, how it ended the day, and if there were events different than that in between
  
  distinct(TAG, Event, Date, first_last, UTM_X, UTM_Y, .keep_all = TRUE) %>%
  
  group_by(TAG) %>%
  mutate(
    #this is used in getting states
    previous_event = lag(Event, order_by = Datetime),
    #this part is used in movemnets map
    det_type = case_when(str_detect(Event, "RB1|RB2") ~ "Red Barn Stationary Antenna",
                         str_detect(Event, "HP3|HP4") ~ "Hitching Post Stationary Antenna",
                         str_detect(Event, "CF5|CF6") ~ "Confluence Stationary Antenna",
                         str_detect(Event, "B3") ~ "Windy Gap Dam Biomark Antenna",
                         str_detect(Event, "B4") ~ "Kaibab Park Biomark Antenna",
                         str_detect(Event, "M1|M2") ~ "Mobile Run",
                         Event == "Recapture" ~ "Recapture",
                         TRUE ~ Event),
    above_below = case_when(
      ET_STATION >= 8330 ~ "Above the Dam",
      ET_STATION< 8330 ~ "Below the Dam"
    )
    
  ) %>%
  
  select(Date, Datetime, TAG, Event, det_type, ReleaseSite,Species, Release_Length, Release_Weight, Release_Date, RecaptureSite, River, days_since, first_last, previous_event,  c_number_of_detections, daily_unique_events, ET_STATION, above_below, UTM_X, UTM_Y) #next_event, next_event_2, same_day_next_events,

#ReleaseEncounters_2022_11_02 <- read_csv("ReleaseEncounters_2022-11-02.csv")
ReleaseEncounters_2022_11_02 <- read_csv("ReleaseEncounters_2022-11-02.csv", 
                                         col_types = cols(TAG = col_character(), 
                                                          RS_Num = col_character(),
                                                          UTM_X = col_character(),
                                                          UTM_Y = col_character(),
                                                          TagSize = col_character(),
                                                          Mortality = col_character(),
                                                          Date = col_character(), Time = col_character()))
colnames(ReleaseEncounters_2022_11_02)[1] = c("TagID")
#tags in the release file that don't get summarized for some reason
x <- anti_join(Release, ReleaseEncounters_2022_11_02, by = "TagID")

station_data <- Stationdata1 %>%
  distinct(TAG, .keep_all = TRUE) %>%
  rename(TagID = TAG)
#colnames(station_data)[4] = c("TagID")
tag_difs <- anti_join(Release, station_data, by = "TagID")

All_events_ <- All_events %>%
  filter(TAG == "230000142594")


# Map ---------------------------------------------------------------------


layer_location <- file.path("./gis/")


stationary_antennas <- readOGR(dsn = layer_location, layer = "stationary_points")
stationary_antennas <- sp::spTransform(stationary_antennas, CRS("+init=epsg:4326"))

stream_centerline <- readOGR(dsn = layer_location, layer = "stream_centerline")
stream_centerline <- sp::spTransform(stream_centerline, CRS("+init=epsg:4326"))

releasesites <- readOGR(dsn = layer_location, layer = "ReleaseSites2021")
releasesites <- sp::spTransform(releasesites, CRS("+init=epsg:4326"))


mobile_reaches <- readOGR(dsn = layer_location, layer = "mobile_reaches")
mobile_reaches <- sp::spTransform(mobile_reaches, CRS("+init=epsg:4326"))

###no need to change files other than stations to .rds becuase the others aren't slow to bring in and convert to correct coordinate system

simple_stations2 <- read_rds(file.path("./gis/simple_stations.rds"))

Station_icons <- awesomeIcons(
  icon = 'add',
  iconColor = 'black',
  library = 'ion',
  #iconHeight = 20,
  markerColor = "purple"
)

release_icons <- awesomeIcons(
  icon = 'add',
  iconColor = 'black',
  library = 'ion',
  #iconHeight = 20,
  markerColor = "white"
)

label_style <- list(
  "color" = "white",
  "font-size" = "12px",
  "border-color" = "black"
)

leaflet(Movements_df) %>% #Warning: Error in UseMethod: no applicable method for 'metaData' applied to an object of class "NULL"  solved becuase leaflet() needs an arg leaflet(x)
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(maxZoom = 19.5)
  ) %>%
  ##detections: based off reactives
  addAwesomeMarkers(
    group = "Detections",
    clusterOptions = markerClusterOptions(),
    lng=~X, 
    lat = ~Y,
    icon = icons(),
    label = paste(Movements_df$movement_only, "\n",
                  Movements_df$Date),
    layerId = as.character(Movements_df$id),
    popup = paste(
      "TAG:", Movements_df$TAG, "<br>",
      "Release Site:", Movements_df$ReleaseSite, "<br>",
      "Detection Event:", Movements_df$det_type, "<br>",
      "Date:", as.character(Movements_df$Datetime))
  ) %>%
  
  ###polylines and points: obtained from GISdb from this study
  addAwesomeMarkers(data = stationary_antennas@coords,
                    icon = Station_icons,
                    clusterOptions = markerClusterOptions(),
                    label = paste(stationary_antennas@data$SiteLabel),
                    popup = paste(stationary_antennas@data$SiteName, "<br>",
                                  "Channel Width:", stationary_antennas@data$ChannelWid, "feet"),
                    group = "Antennas") %>% # error: don't know jow to get path Data from x....solved by specifying coordinate location with @ within data
  addPolylines(data = stream_centerline@lines[[1]], 
               color = "blue",
               opacity = 1,
               popup = paste("Colorado River Centerline"),
               group = "Stream Centerlines") %>%
  addPolylines(data = stream_centerline@lines[[2]],
               color = "blue",
               opacity = 1,
               popup = paste("Fraser River Centerline"),
               group = "Stream Centerlines") %>%
  addPolylines(data = mobile_reaches,
               color = "yellow",
               opacity = 1,
               label = mobile_reaches@data$River,
               popup = paste("Mobile Run:", mobile_reaches@data$River, 
                             "<br>"),
               group = "Mobile Reaches") %>%
  addAwesomeMarkers(data = releasesites@coords,
                    icon = release_icons,
                    clusterOptions = markerClusterOptions(),
                    label = releasesites@data$ReleaseSit, 
                    popup = paste("Release Date1:", releasesites@data$ReleaseDat, "<br>","Release Date 2:",  releasesites@data$ReleaseD_1),
                    group = "Release Sites") %>%
  addPolylines(data = simple_stations2, 
               label = simple_stations2@data$ET_STATION,
               labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
               group = "Stations (m)") %>%
  addLayersControl(overlayGroups = c("Antennas", "Detections", "Release Sites", "Stream Centerlines", "Stations (m)", "Mobile Reaches")) %>%
  hideGroup(c("Stream Centerlines", "Stations (m)", "Antennas", "Release Sites", "Mobile Reaches"))


# spatial join R ----------------------------------------------------------

source("map_polygon_readins.R")

library(readr)
#install.packages("rgeos")
library(rgeos)
allevents_2022_11_02_condensed <- read_csv("spatial_join/allevents_2022-11-02_condensed.csv")

small_sample <- allevents_2022_11_02_condensed %>%
  slice_sample(n = 30)

movement_table_notrans <- small_sample %>%
  
  mutate(
         X = as.numeric(UTM_X),
         Y = as.numeric(UTM_Y)
  ) #end of mutate

# assigning projection to ready df lat/longs for plotting
attr(movement_table_notrans, "zone") = "13"
attr(movement_table_notrans, "projection") = "UTM"
attr(movement_table_notrans, "datum") = "GRS80"

# need a column that has x and Y for this 
# converts lutms to lat/long
movement_table_notrans <- convUL(movement_table_notrans, km=FALSE, southern=NULL)
#converting lat/long entries to spatial points dataframe
xy <- movement_table_notrans %>%
  select(X, Y)
simple_stations2@proj4string
spdf@proj4string
spdf <- SpatialPointsDataFrame(coords = xy, data = movement_table_notrans,
                               proj4string = CRS("+init=epsg:4326"))

x <- sp::over(simple_stations2,spdf )
#install.packages("sf")
#library(spatialEco)
library(sf)
## making sf objects
xx <- st_as_sf(spdf)
yy <- st_as_sf(simple_stations2)

joined <- st_join(xx, yy, st_nearest_feature)
leaflet(joined) %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(maxZoom = 19.5)
  ) %>%
  addMarkers(label = joined$ET_STATION)

distances <- as.data.frame(st_distance(xx, yy))

x <- st_intersection(xx, yy )
xxx <- st_transform(xx, 3857)
yyy <- st_transform(yy, 3857)

x <- st_snap(xxx, yyy, tolerance = 1)

x1 <- st_distance(xx, yy)

st_is_within_distance(xx,yy, 60000) 

tees <- st_nearest_points(yy, xx) %>%
  {. ->> my_linestring}
my_linestring %>% 
  st_cast('POINT') %>% 
  .[2] %>%
  {. ->> closest_point}

closest_point





leaflet(movement_table_notrans) %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(maxZoom = 19.5)
  ) %>%
  addPolylines(tees)
  # addPolylines(data = simple_stations2, 
  #              label = simple_stations2@data$ET_STATION,
  #              labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
  #              group = "Stations (m)") %>%
  addMarkers(data = spdf@coords)
  
  
  
  my.df <- read.table(text="
                    longitude    latitude
                    128.6979    -7.4197
                    153.0046    -4.7089
                    104.3261    -6.7541
                    124.9019    4.7817
                    126.7328    2.1643
                    153.2439    -5.6500
                    142.8673    23.3882
                    152.6890    -5.5710",
                     header=TRUE)
  
  # Convert data frame to sf object
  my.sf.point <- st_as_sf(x = my.df, 
                          coords = c("longitude", "latitude"),
                          crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  # simple plot
  plot(my.sf.point)
  
  # interactive map:
  library(mapview)
  mapview(my.sf.point)
  
  # convert to sp object if needed
  my.sp.point <- as(my.sf.point, "Spatial")
  
  

# yearly graphs -----------------------------------------------------------
install.packages("forecast")
library(forecast)
  ggseasonplot(AirPassengers, col=rainbow(12), year.labels=TRUE)
  
  
x1 <- Movements_df %>%
  # ungroup() %>%
  # spread(movement_only, n()) 
  # pivot_wider(id_cols = Date, names_from = movement_only)
  #mutate(test = mday(Date))
  group_by(month(Date), day(Date), movement_only) %>%
  #mutate(total_release = movement_only)
  summarise(total_events = n())
  # # mutate(m_1 = month(Date),
  # #        d_1 = day(Date))
  
plot <- x1 %>%
  # select(Datetime,`Downstream Movement`, `Initial Release`,`No Movement`,`Upstream Movement`) %>%
  # group_by(month(Datetime), day(Datetime)) %>%
  # summarise(n = n())
  #count(`Downstream Movement`, `Initial Release`,`No Movement`,`Upstream Movement`)
  mutate(merged = (parse_date_time(paste(`month(Date)`, `day(Date)`), "md")), 
         ) %>%
  ggplot(aes(x = merged, y = total_events, fill = movement_only)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  labs(title = "Seasonal Daily Movements", x = "Day", y = "Counts") +
  scale_x_datetime(
    date_labels = "%b"
    
    ) +
  scale_fill_manual(values = c("Downstream Movement" = "red",
                               "Upstream Movement" = "chartreuse3",
                               "No Movement" = "black",
                               "Initial Release" = "darkorange"))

ggplotly(plot)


# New antenna sites -------------------------------------------------------

# new_sites <- readOGR(dsn = "./antenna_sites/", layer = "antenna_sites")
# new_sites <- sp::spTransform(new_sites, CRS("+init=epsg:4326"))

leaflet(Movements_df) %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(maxZoom = 19.5)
  ) %>%
  addMarkers(data = new_sites@coords,
             label = new_sites@data$SiteLabel,
             popup = new_sites@data$SiteName)
# addPolylines(data = simple_stations2, 
#              label = simple_stations2@data$ET_STATION,
#              labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
#              group = "Stations (m)") %>%
addMarkers(data = spdf@coords,
           label = )


# including new antennas --------------------------------------------------
add_row1 <- function(variable1) {
  for (i in length(variable1)) {
    stationary1_2 <- Stationary %>%
      add_row(Code = "S",
              DTY = "2022-12-03",
              ARR = "07:58:58.900",
              TRF = "G",
              DUR = "00:00.9",
              TTY = "A",
              TAG = "900_230000999999",
              SCD = variable1[i],
              ANT = "A1",
              NCD = "30",
              EFA = .6)
  }
  
  return(stationary1_2)
}

vars = c("CD7", "CD8","CD9","CD10","CU11", "CU12")
x <- map(vars, add_row1)
x <- as.data.frame(x)

Stationary <- Stationary %>%
  add_row(Code = "S",
          DTY = "2022-12-03",
          ARR = "07:58:58.900",
          TRF = "G",
          DUR = "00:00.9",
          TTY = "A",
          TAG = "900_230000999999",
          SCD = "CD7",
          ANT = "A1",
          NCD = "30",
          EFA = .6) %>%
  add_row(Code = "S",
          DTY = "2022-12-03",
          ARR = "07:58:58.900",
          TRF = "G",
          DUR = "00:00.9",
          TTY = "A",
          TAG = "900_230000999999",
          SCD = "CD8",
          ANT = "A1",
          NCD = "30",
          EFA = .6) %>%
  add_row(Code = "S",
          DTY = "2022-12-03",
          ARR = "07:58:58.900",
          TRF = "G",
          DUR = "00:00.9",
          TTY = "A",
          TAG = "900_230000999999",
          SCD = "CD9",
          ANT = "A1",
          NCD = "30",
          EFA = .6) %>%
  add_row(Code = "S",
          DTY = "2022-12-03",
          ARR = "07:58:58.900",
          TRF = "G",
          DUR = "00:00.9",
          TTY = "A",
          TAG = "900_230000999999",
          SCD = "CD10",
          ANT = "A1",
          NCD = "30",
          EFA = .6) %>%
  add_row(Code = "S",
          DTY = "2022-12-03",
          ARR = "07:58:58.900",
          TRF = "G",
          DUR = "00:00.9",
          TTY = "A",
          TAG = "900_230000999999",
          SCD = "CU11",
          ANT = "A1",
          NCD = "30",
          EFA = .6) %>%
  add_row(Code = "S",
          DTY = "2022-12-03",
          ARR = "07:58:58.900",
          TRF = "G",
          DUR = "00:00.9",
          TTY = "A",
          TAG = "900_230000999999",
          SCD = "CU12",
          ANT = "A1",
          NCD = "30",
          EFA = .6)


### Biomark

Biomark <- Biomark %>%
  add_row(Scan.Date = "2022-12-03",Scan.Time = "59:59.9", Download.Date = "9/16/2022",Download.Time = "11:30:41",Reader.ID = "A3",Antenna.ID = 1,HEX.Tag.ID = "384.358D14F739",
          DEC.Tag.ID = "900.230000999999",Temperature.C = NA,Signal.mV= NA,Is.Duplicate= "Yes",Latitude = NA,Longitude= NA,File.Name= NA) %>%
  add_row(Scan.Date = "2022-12-03",Scan.Time = "59:59.9", Download.Date = "9/16/2022",Download.Time = "11:30:41",Reader.ID = "A4",Antenna.ID = 1,HEX.Tag.ID = "384.358D14F739",
          DEC.Tag.ID = "900.230000999999",Temperature.C = NA,Signal.mV= NA,Is.Duplicate= "Yes",Latitude = NA,Longitude= NA,File.Name= NA)
  
x <- combined_events_stations %>%
  filter(Event %in% c("B5", "B6", "CD7", "CD8","CD9","CD10","CU11", "CU12"))

All_events_stations_3 <- all_events_stations_2 %>%
  
  rename(
    TAG = TAG.x,
    Date = Date.x,
    Time = Time.x,
    Datetime = Datetime.x,
    Event = Event.x,
    Species = Species.x,
    Release_Length = Release_Length.x,
    Release_Weight = Release_Weight.x, 
    ReleaseSite = ReleaseSite.x,
    Release_Date = Release_Date.x,
    RecaptureSite = RecaptureSite.x,
    Recap_Length = Recap_Length.x,
    Recap_Weight = Recap_Weight.x) %>%
  
  mutate(
    #River also needs to be assigned for new detections 
    River = case_when(
      (Event %in% c("RB1", "RB2")) ~ "Colorado River", # there is no is.na here because RB UTM
      (Event %in% c("HP3", "HP4")) ~ "Colorado River",
      (Event %in% c("CF5", "CF6")) ~ "Colorado River",
      (Event %in% c("CD7", "CD8", "CD9", "CD10", "CU11", "CU12")) ~ "Connectivity Channel",
      (Event %in% c("B3", "B5")) ~ "Colorado River",
      (Event %in% c("B4", "B6")) ~ "Fraser River",
      TRUE ~ River
    ),
    #this fills in the NA rows so therefore accounts for new stationary and biomark detections that weren't captured when stationdata was made 12/1/22 note: there shouldn't be na rows since i changed the way the left_join above was performed and also now spatial join happens within the app everytime it's ran.
    
    #if UTM's were correctly assigned initially and stationdata is up to date, this part is unnesseccary because this will all already be done with the left_join
    # 12/1/22 i don't actually think it's needed anymore since the UTM's ARE all correct, but it's a good rdundant safeguard to have in place
    # just need to change this if a 
    ### NEED TO STILL ASIGN STATIONS FOR NEW ANTENNAS, aswell as deal with how to handle moving around the channels
    # put movements in the 
    ET_STATION = case_when(
      (Event %in% c("RB1", "RB2")) ~ 4150, # there is no is.na here because RB UTM
      is.na(ET_STATION) & (Event %in% c("HP3", "HP4")) ~ 6340,
      is.na(ET_STATION) & (Event %in% c("CF5", "CF6")) ~ 9550,
      is.na(ET_STATION) & (Event %in% c("B3")) ~ 8190,
      is.na(ET_STATION) & (Event %in% c("B4")) ~ 6050,
      !is.na(ET_STATION) & (!Event %in% c("RB1", "RB2")) ~ ET_STATION),
    # this part is needed because stations are assigned from 0 up the fraser river starting at the confluence
    ET_STATION = case_when(River %in% "Fraser River" ~ ET_STATION + 9566, #9566 is above Fraser River Confluence
                           River %in% c("Colorado River", "Connectivity Channel") ~ ET_STATION,
                           TRUE ~ ET_STATION
                           )
  ) %>%
  
  # mutate(
  #   ET_STATION = case_when(River %in% "Fraser River" ~ ET_STATION + 9566, #9566 is above Fraser River Confluence
  #                          River %in% "Colorado River" ~ ET_STATION)
  # )
  # this line just makes the df smaller if htere are duplicates; usually doesn't change anything since All_events has a line that does this also in the WGFP ENC hist_function
  distinct(Datetime, Event, TAG, .keep_all = TRUE) %>%
  
  select(Date, Time, Datetime, TAG, Event, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, Recap_Length, Recap_Weight, UTM_X, UTM_Y, ET_STATION)

# updated_states ----------------------------------------------------------
All_events_stations_combined <- combined_events_stations
# this gives all states
states1 <- All_events_stations_combined %>%
  # no need to group_by date until states will be consolidated
  # need to group_by tag though so that the Lag(Date) will get the last date that that fish was detected
  # some movements weren't being recorded correctly because it was grouping by both date and Tag
  #don't need this part bc not even doing any lag stuff
  # group_by(TAG) %>%
  # arrange(Datetime) %>%
  mutate(
    state1 = case_when(str_detect(Event, "CD7|CD8|CD9|CD10|CU11|CU12") ~ "C",
                       ET_STATION <= 8330 ~ "A",
                       ET_STATION > 8330 ~ "B")
  )

states2 <- states1 %>%
  #filter(!is.na(teststate_11)) %>%
  group_by(Date, TAG) %>%
  #arranging my datetime ensures that all states will be recorded in the correct order
  arrange(Datetime) %>%
  mutate(
    teststate_2 = paste(state1, collapse = ""),
    teststate_3 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2), #removes consecutive letters
  )

states_final <- states2 %>%
  distinct(Date, TAG, teststate_3, .keep_all = TRUE) %>%
  select(Date, TAG, teststate_3, det_type, ReleaseSite, Species, Release_Length, Release_Weight, c_number_of_detections, daily_unique_events, days_since, UTM_X, UTM_Y) %>%
  rename(State = teststate_3)
## pivot wider
days <- data.frame(days_since = 1:max(states_final$days_since))

days_and_states <- full_join(days, states_final, by = "days_since")


days_and_states_wide <- pivot_wider(days_and_states, id_cols = TAG, names_from = days_since, values_from = State)

days_and_states_wide <- days_and_states_wide %>%
  select(TAG, `0`, 2:ncol(days_and_states_wide))

# unknown_movements <- states1 %>%
#   filter(
#     #!ReleaseSite %in% c("Pool Above Red Barn Antenna"),
#     str_detect(TAG, c("^230")) | str_detect(TAG, c("^226")),
#     #!is.na(previous_event), #don't want entries 
#     is.na(movement)
#   )
  
  mutate(
    det_type = case_when(str_detect(Event, "RB1|RB2") ~ "Red Barn Stationary Antenna",
                         str_detect(Event, "HP3|HP4") ~ "Hitching Post Stationary Antenna",
                         str_detect(Event, "CF5|CF6") ~ "Confluence Stationary Antenna",
                         str_detect(Event, "B3") ~ "Windy Gap Dam Biomark Antenna",
                         str_detect(Event, "B4") ~ "Kaibab Park Biomark Antenna",
                         str_detect(Event, "M1|M2") ~ "Mobile Run",
                         Event == "Recapture" ~ "Recapture",
                         TRUE ~ Event),
    
    current_event_vals = case_when(Event == "RB1" ~ 11.9,
                                   Event == "RB2" ~ 11.1,
                                   Event == "HP3" ~ 7.9,
                                   Event == "HP4" ~ 7.1,
                                   Event == "CF5" ~ 4.9,
                                   Event == "CF6" ~ 4.1,
                                   Event == "B3" ~ 6,
                                   Event == "B4" ~ .9, #this ensures that kaibab park release to dtecitons will get a slight upstream move
                                   
                                   Event == "Recapture" & RecaptureSite == "Lower River Run" ~ 4,
                                   Event == "Recapture" & RecaptureSite == "Fraser River Ranch" ~ 2,
                                   Event == "Recapture" & RecaptureSite == "Kaibab Park" ~ 1,
                                   Event == "Recapture" & RecaptureSite == "Upper River Run" ~ 3,
                                   Event == "Recapture" & RecaptureSite == "Below Confluence Antenna" ~ 5,
                                   Event == "Recapture" & RecaptureSite == "Windy Gap Dam" ~ 6,
                                   Event == "Recapture" & RecaptureSite == "Hitching Post" ~ 7,
                                   Event == "Recapture" & RecaptureSite == "Chimney Rock Above Island" ~ 8,
                                   Event == "Recapture" & RecaptureSite == "Chimney Rock Below Island" ~ 9,
                                   Event == "Recapture" & RecaptureSite == "Upper Red Barn Fry Site" ~ 10,
                                   Event == "Recapture" & RecaptureSite == "Pool Above Red Barn Antenna" ~ 11,
                                   Event == "Recapture" & RecaptureSite == "Lower Red Barn Fry Site" ~ 12,
                                   Event == "Recapture" & RecaptureSite == "Below Red Barn Diversion #1" ~ 13,
                                   Event == "Recapture" & RecaptureSite == "Below Red Barn Diversion #2" ~ 14,
                                   Event == "Recapture" & RecaptureSite == "Kinney Creek" ~ 15,
                                   Event == "Recapture" & RecaptureSite == "Dark Timber Above Railroad" ~ 16,
                                   Event == "Recapture" & RecaptureSite == "Sheriff Ranch Upper Field" ~ 17,
                                   Event == "Recapture" & RecaptureSite == "Shefiff Ranch Middle Field" ~ 18,
                                   Event == "Recapture" & RecaptureSite == "Sheriff Ranch Fry Site" ~ 19
                                   
    ))

# above below station 8330
## make current event vals just -1 and 1: 
# could also just see ET station > x is above the damn
  # look at through_dam function in combine_statoins events

  
  x <- All_events %>%
    filter(TAG == "230000228314") %>%
    arrange(Datetime)
View(x)  
# df of multiple states in one day to check avian predation

checking <- states_final %>%
  group_by(TAG) %>%
  arrange(Date) %>%
  mutate(through_dam1 = case_when(det_type == "Release" ~ "Initial Release",
                                  str_sub(State,-1,-1) == "A" & lag(str_sub(State,-1,-1) %in% c("B", "C"), order_by = Date) ~ "Went Below Dam",
                                  str_sub(State,-1,-1) == "B" & lag(str_sub(State,-1,-1) %in% c("A", "C"), order_by = Date) ~ "Went Above Dam",
                                  State %in% c("BA", "CA", "BCA") ~ "Went Below Dam",
                                  State %in% c("AB", "CB", "ACB") ~ "Went Above Dam",
                                  State == lag(str_sub(State,-1,-1), order_by = Date) ~ "No state change",
                                  # TRUE ~ NA
      
                                  )
         )

unknown_states <- checking %>%
  filter(is.na(through_dam1) & !det_type %in% c("Release", "Recapture and Release", "Recapture"))


# Ghost tag and aviation predation "States" -------------------------------
# states <- states_function(combined_events_stations)
# states <- states$All_States
ghost_dummy <- data.frame(TAG = c("230000228444"), Ghost_date = as.Date(c("2021-06-03")))

# want to have all detectoins starting on this date "G" state
wg_date <- left_join(combined_events_stations, ghost_dummy, by = c("TAG"))
states1 <- wg_date %>%
  mutate(
    test = Date >= Ghost_date,
    #the case_whens also are a priority list, so important not to rearange these
    state1 = case_when(Date >= Ghost_date ~ "G",
                       str_detect(Event, "CD7|CD8|CD9|CD10|CU11|CU12") ~ "C",
                       ET_STATION <= 8330 ~ "A",
                       ET_STATION > 8330 ~ "B")
  )

test <- states1 %>%
  filter(TAG == "230000228444")

# states_wide <- states_data_list$Weeks_and_states_wide
# states_ <- states_data_list$All_States
# 
# DailyStates_2022_12_09 <- read_csv("DailyStates_2022-12-09.csv", 
#                                    col_types = cols(TAG = col_character(),
#                                                     UTM_X = col_character(), UTM_Y = col_character())) 
# x <- anti_join(DailyStates_2022_12_09, states_)
# y <- anti_join(states_, DailyStates_2022_12_09)
# z <- anti_join(y, x)

x <- All_events %>%
  mutate(x1 = as.character(Release_Date))
min(x$x1, na.rm = TRUE)
yy <- c("2020-04-04", "2022-04-04")
max(yy)


# Movements graphs --------------------------------------------------------

plot <- Movements_df %>%
  filter(
    !dist_moved %in% c(0)) %>%
  ggplot(aes(x = dist_moved)) +
  geom_histogram(binwidth = 50) +
  theme_classic() +
  labs(title = "Each movement detected: ('No movements' excluded)", subtitle = "Groupings are 50 m")
ggplotly(plot)

plot <- Movements_df %>%
  ggplot(aes(x = sum_dist)) +
  geom_histogram(binwidth = 300) +
  theme_classic() +
  labs(title = "Cumulative movement", subtitle = "Groupings are 300 m")
ggplotly(plot)

plot <- Movements_df %>%
  filter(!det_type %in% c("Mobile Run", "Release"),
         ) %>%
  # group_by(hour(Datetime)) %>%
  # summarize(x1 = n()) %>%
  ggplot(aes(x = hour(Datetime), fill = movement_only)) +
  geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(title = "Detections by Hour") 
ggplotly(plot)  

x <- Movements_df %>%
  mutate(x1 = str_sub(Datetime, 11, -1))


# release summaries -------------------------------------------------------

Release1 <- Release %>%
  count(Species)

Release %>%
  count(Species) %>%
  ggplot(aes(x = Species, y = n)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Released Fish by Species")

Release %>%
  ggplot(aes(x = Length, fill = Species) ) +
  geom_histogram(binwidth = 20)+
  theme_classic() +
  labs(title = "Released Fish by Length", caption = "Binwidth = 20mm")

Release %>%
  ggplot(aes(x = Weight, fill = Species) ) +
  geom_histogram(binwidth = 100)+
  theme_classic() +
  labs(title = "Released Fish by Weight", caption = "Binwidth = 100g")

# map saving --------------------------------------------------------------


library(leaflet)
library(htmlwidgets)

map1 <- leaflet() %>% #Warning: Error in UseMethod: no applicable method for 'metaData' applied to an object of class "NULL"  solved becuase leaflet() needs an arg leaflet(x)
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(maxZoom = 19.5)
  )
html_fl = tempfile(fileext = ".html")
png_fl = tempfile(fileext = ".png")

saveWidget(map1, "temp.html", selfcontained=TRUE)
webshot("temp.html", file="Rplot.png", cliprect="viewport")


mapshot(map1, file = "rrplot.png")
library(tidyverse)
library(gganimate)
library(gifski)
library(ggmap)

#Movements_df <- get_movements_function(combined_events_stations)

Movements_df <- read_csv
m1 <- Movements_df %>%
  mutate(
    days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
    #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
    # if you want to start at week 1 instead of week 0, add +1 to the end of expression
    # when you change this too, it changes the number of entries in the states dataframe
    weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks")))
    #months_since = as.numeric(floor(difftime(Date, min(Date), units = "months")))
  )

# which_state <- "colorado"
# county_info <- map_data("county", region=which_state)
#EPSG:4326 
base_map1 <- basemap_magick(x, map_service = "esri", map_type = "world_imagery")
set_defaults(map_service = "esri", map_type = "world_imagery")
basemap_ggplot(x)
x1 <- ggplot() + 
  basemap_gglayer(x) +
  scale_fill_identity() + 
  coord_sf()

base_map <- ggplot(data = county_info, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "white") +
  coord_quickmap() +
  theme_void()

map_with_data <- base_map +
  geom_point(data = m1, aes(x = X, y = Y), group = m1$weeks_since)
map_with_data

min_long <- min(m1$X)
max_long <- max(m1$X)
min_lat <- min(m1$Y)
max_lat <- max(m1$Y)
map_with_data <- map_with_data +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))



map_with_data <- base_map +
  geom_point(data = m1, aes(x = X, y = Y, color=movement_only, group=weeks_since)) +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))

map_with_data


map_with_animation <- map_with_data +
  transition_time(weeks_since) +
  #### this is the title
  ggtitle('Week after Project: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
num_weeks <- max(m1$weeks_since) - min(m1$weeks_since) + 1
## fps is how fast/slow the animation is
animate(map_with_animation, nframes = num_weeks, fps = 2)
#saves the last gif you generated
anim_save("example1.gif")
## if you want to save as video
library(av)
#first render as video
animate(map_with_animation, nframes = num_weeks, fps = 4, renderer = av_renderer())
anim_save("example2.mpg")


library(ggmap)
map <- get_googlemap("Montpellier, France", zoom = 8, maptype = "terrain")

#install.packages("mapedit")
library(mapedit)
library(basemaps)
x <- draw_ext()
basemap_magick(x, map_service = "esri", map_type = "world_imagery")

baylor <- "baylor university"
qmap(baylor, zoom = 14)
#######
#trying to get m1 coords to plot with x1 basemap
m1 <- m1 %>%
  ungroup() 

attr(m1, "zone") = "13"
attr(m1, "projection") = "UTM"
attr(m1, "datum") = "GRS80"

# need a column that has x and Y for this 
# converts lutms to lat/long
m2 <- convUL(m1, km=FALSE, southern=NULL)
xy <- m1 %>%
  select(X, Y)

spdf <- SpatialPointsDataFrame(coords = xy, data = m2,
                               proj4string = CRS("+init=epsg:3857"))
m3 <- as.data.frame(spdf)
map_with_data <- x1 +
  geom_point(data = m3, aes(x = X.1, y = Y.1), group = m3$weeks_since)
map_with_data

x <- "2021-03-22 00:39:05"
All_events1 <- All_events %>%
  mutate(trest = min(hour(Datetime)))
#sf instead of ogr
# condensed_events1 <- st_as_sf(df_list$All_Events_most_relevant, coords = c("UTM_X", "UTM_Y"))

## recap changes
x <- All_events %>%
  group_by(TAG, Event) %>%
  arrange(Datetime) %>%
  summarise(tot = n()) %>%
  filter(Event == "Recapture")
x1 <- All_events %>%
  filter(TAG == "230000228026")



