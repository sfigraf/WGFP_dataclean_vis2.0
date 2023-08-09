# Biomark <- read.csv("./data/Biomark_Raw_20221102.csv", dec = ",")

cleanBiomark <- function(rawBiomark){
   
    
  biomarkClean <- rawBiomark %>% 
  mutate(TAG = str_replace(DEC.Tag.ID, "\\.", ""),
         Reader.ID = case_when(Reader.ID == "A1" | Reader.ID == "B1" ~ "B3",
                               Reader.ID == "A2" | Reader.ID == "B2" ~ "B4",
                               Reader.ID == "A3" ~ "B5",
                               Reader.ID == "A4" ~ "B6",
                               TRUE ~ Reader.ID),
         #make a column for Scan>Date if parentheses are detected in the string, that means the format is in mdy 
         # and we want to convert it to YYYYMMDD format. elsewise, leave it as is
         Scan.Date = ifelse(str_detect(Scan.Date, "/"), 
                            as.character(mdy(Scan.Date)), 
                            Scan.Date))
  biomarkClean_noMarkers <- biomarkClean %>%
  filter(!TAG %in% c("900230000102751", "900226001581072", "999000000007586", "999000000007585", "999000000007601", "999000000007602" )) 
 biomarkMarkerTags <- biomarkClean %>%
    filter(TAG %in% c("999000000007586", "999000000007585", "999000000007601", "999000000007602" ))
 
 return(list("biomarkClean" = biomarkClean, "biomarkMarkerTags" = biomarkMarkerTags))
    
}
# 
# y <- cleanBiomark(Biomark)

