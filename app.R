library(shiny)
library(shinycssloaders)
library(tidyverse) #error has occ
library(lubridate)
library(leaflet)
library(PBSmapping)
library(sf)
library(plotly) #for turning maps to plotly
library(rgdal)
library(DT)
library(shinyWidgets) # for pickerinput
library(shinythemes)
library(bslib)
#animation stuff
library(mapedit)
library(basemaps)
library(gganimate)
#library(mapview)
#biomark test tags: 999000000007601, 999000000007602
# to do: put qaqc stuff from combine files app in this file as well to see when biomark shits the bed
#continue with how-to
# make mini charts on leaflet# 
#make "varaibels" file with station info of antennas based off spatial join, point of fraser river/CO river confluence, windy gap dam, antenna UTM's, 

#Biomark is temporarily labelled as B3 and B4 to make data filtering easier
# tieh the site_code %in% picker1 line, because B1 and B2 are technically "in" RB1 and Rb2, it would include them to be part of it 
# so for now this is easier. but actually idk if this is true, it could have been some other problem
# cntrl + shft + A to reformat chunks of code
# rsconnect::showLogs(appName="WGFP_dataclean_vis",streaming=TRUE) will show logs when trying to load app browser
# had "application failed to start" error and fixed both times with above command. both times because packages in local environment (tidyverse and lubridate) weren't called with library() command 

# Data Read Ins -----------------------------------------------------------
start_time <- Sys.time()
print("Reading in Stationary, Mobile, Biomark, Release, and Recapture csv files.")

# if column names change in any of these read-ins, might require some modification to code to get them to combine
# also if you change read.csv to read_csv, it should read in quicker but column names will change
# could be a later task
Stationary <- read.csv(paste0("./data/WGFP_Raw_20230201.csv")) #WGFP_Raw_20211130.csv WGFP_Raw_20220110_cf6.csv
Mobile <- read.csv("./data/WGFP_Mobile_Detect_AllData.csv" , colClasses= c(rep("character",14), rep("numeric", 4), rep("character", 3)))
Biomark <- read.csv("./data/Biomark_Raw_20221102.csv", dec = ",") 
# need to have tagID as a numeric field in the .csv file in order to be read in correctly as opposed to 2.3E+11 
Release <- read.csv("./data/WGFP_ReleaseData_Master.csv", na.strings = c(""," ","NA"), colClasses=c(rep("character",8), "numeric", "numeric",rep("character",8) ))
Recaptures <- read.csv("./data/WGFP_RecaptureData_Master.csv", na.strings = c(""," ","NA"), colClasses = c(rep("character", 9), rep("numeric", 2), rep("character", 8)))
#ghost tag df
#avitaion predation
AvianPredation <- read_csv("./data/WGFP_AvianPredation.csv", col_types = cols(TagID = col_character(),Comments = col_character()))
GhostTags <- read_csv("./data/WGFP_GhostTags.csv", 
                           col_types = cols(TagID = col_character()))



end_time = Sys.time()
print(paste("Reading in files took", round((end_time-start_time),2)))


#### This part was for checking if new antennas to be put in will work
source("functions/dummy_rows.R")
dummy_rows_list <- add_dummy_rows(stationary = Stationary, biomark = Biomark, release1 = Release)
Stationary <- dummy_rows_list$Stationary
Biomark <- dummy_rows_list$Biomark
Release <- dummy_rows_list$Release
#ghost_tag_df <- dummy_rows_list$Ghost_tags #date column is named "Ghost_date" and is a date type

# Date Wrangling ----------------------------------------------------------

# this readies raw files to be put into functions as well as displayed on Indivudal Datasets Page
Mobile <- Mobile %>%
    mutate(Date = as.character(mdy(Date)))


Biomark <- Biomark %>%
  #make a column for Scan>Date if parentheses are detected in the string, that means the format is in mdy 
  # and we want to convert it to YYYYMMDD format. elsewise, leave it as is
  mutate(Scan.Date = ifelse(str_detect(Scan.Date, "/"), 
                            as.character(mdy(Scan.Date)), 
                            Scan.Date))


Release_05 <- Release %>%
  mutate(Date = as.character(mdy(Date)))

Recaptures_05 <- Recaptures %>%
  mutate(Date = as.character(mdy(Date)))

AvianPredation <- AvianPredation %>%
  mutate(PredationDate = mdy(PredationDate))

GhostTags <- GhostTags %>%
  mutate(GhostDate = mdy(GhostDate)) 

# Functions Read-in -------------------------------------------------------


#functions
for (i in list.files("./functions/")) {
  if (grepl(".R", i)) {
    source(paste0("./functions/",i))
  }
}

for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}

for (i in list.files("./miscR/")) {
  if (grepl(".R", i)) {
    source(paste0("./miscR/",i))
  }
}

#putting detection data into a function that cleans and readies data for wrangling, display, filtering, mapping, plotting
df_list <- All_combined_events_function(Stationary = Stationary, Mobile = Mobile, Release = Release, Biomark = Biomark, Recaptures = Recaptures)
All_events <- df_list$All_Events
Marker_tags <- df_list$Marker_Tag_data
WGFP_Clean_1 <- df_list$WGFP_Clean
unknown_tags_1 <-df_list$Unknown_Tags

#spatially joins point (detection) data to lines (station) data based on nearest feature. 
#simplestatoins is from polygon_readins
##uncomment later
Stationdata1 <- spatial_join_stations_detections(df_list$All_Events_most_relevant, simple_stations2)

#prepares joined stations and relevant Movements dataset for movements summaries and states
combined_events_stations <- PrepareforStatesMovementsandSummary(Stationdata1)

# states
##uncomment later
states_data_list <- states_function(combined_events_stations, GhostTags, AvianPredation)
# aplies enc_hist_summary wide function
##uncomment later
enc_hist_wide_list <- Ind_tag_enc_hist_wide_summary_function(df_list$Recaps_detections, Release_05, combined_events_stations, states_data_list$States_summarized)
unknown_tags <- enc_hist_wide_list$Unknown_Tags
enc_hist_wide_df <- enc_hist_wide_list$ENC_Release_wide_summary
# applies get_movements_function
##uncomment later
Movements_df <- get_movements_function(combined_events_stations)

#this is used in states_data reactives
weeks <- data.frame(weeks_since = min(states_data_list$All_States$weeks_since):max(states_data_list$All_States$weeks_since))

#more formatting
Enc_release_data <- enc_hist_wide_df %>%
    mutate(Date = ifelse(str_detect(Date, "/"),
                         as.character(mdy(Date)),
                         Date))


most_recent_date <- max(df_list$All_Events$Date)  
# if someone could incorporate and expland upon this custom rainbow trout color pallette I made that would be great                    
rainbow_trout_pallette <- list(pink1 = "#E3BABBFF", olive_green1 = "#C4CFBFFF", dark_olive_green1 = "#81754EFF",
                               mustard_yellow1 = "#CBA660FF", brown_yellow = "#86551CFF" )

### taking dummy tag out

Biomark <- Biomark %>%
  filter(!DEC.Tag.ID %in% c("900.230000999999"))
Release_05 <- Release_05 %>%
  filter(!TagID %in% c("230000999999"))
Stationary <- Stationary %>%
  filter(!TAG %in% c("900_230000999999"))

#this list is taken by the individual datasets mod
indiv_datasets_list <- list(
      "stationarycleandata" = Stationary,
      "biomarkdata" = Biomark,
      "mobiledata" = Mobile,
      "recapdata" = Recaptures_05,
      "releasedata" = Release_05,
      "ghostdata" = GhostTags,
      "avian_preddata" = AvianPredation

    )

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  navbarPage(title = "WGFP Data Exploration",
             id = "tabs", 
             theme = shinytheme("sandstone"), #end of navbar page arguments; what follow is all inside it

               
             #   bs_theme(
             #   bg = rainbow_trout_pallette$dark_olive_green1,
             #   fg = rainbow_trout_pallette$pink1,
             #   primary = "#CBA660FF",
             #   secondary = "#86551CFF",
             #   base_font = "#E3BABBFF",
             #   code_font =  "#C4CFBFFF"
             #     
             # ),
             
             tabPanel("About/How to Use",
                      includeHTML(paste0("www/", "WGFP_dataclean_vis_about.html"))
                      
                      ), #end fo how to use TabPanel

# Individual Datasets UI ---------------------------------------------------

             
             tabPanel("Individual Datasets",
                      value = "IndividualDatasetsTab",
                      IndividualDatasets_UI("IndividualDatasetsTab1", df_list, Release_05)
                    ),#end of Individual data tab panel
             
# Encounter Histories UI --------------------------------------------------

             tabPanel("Encounter Histories",
                      value = "EncounterHistories",
                      tabsetPanel(
                        tabPanel("Encounter Histories Summaries Wide",
                          EncounterHistoriesSummariesWide_UI("EncounterHistoriesSummariesWideTab1", Enc_release_data)),
                      
                      tabPanel("All Encounter Histories",
                               AllEncounters_UI("AllEncountersTab1", df_list))
                      )
                      ), #end of Encounter Histories Tab
# States UI -----------------------------------------------------

            tabPanel("Weekly States",
                     value = "StatesTab",
                     States_UI("StatesTab1", states_data_list)
                    ),#end of States ui Tab

# Movements and Map UI Tab --------------------------------------------------------------

### note on map: if detectoins are close together, they'll be grouped and you can't do more resolution. But they can still be upstream/downstream movements if they're >= 10 m difference in station
#the filtering also automatically takes out NA values on movement with picker6; but all the NA movement onlys should be from fish where we have no release info for,
#and also from fish that have detections before their official "release" back in May
#if marker_color or icon_color is NA, it wont get mapped or displayed in data
#picker wasn't working becuase I had 2 different pickers named the same
            tabPanel("Daily Movements Map, Plot, and Data",
                     value = "MovementsTab",
                     movements_UI("MovementsTab1", Movements_df, df_list)
            ),
            

# QAQC UI tab -------------------------------------------------------------

          tabPanel("QAQC",
                   value = "QAQCTab",
                   QAQC_UI("QAQCTab1", df_list)
                   ) # end of tabPanel
    ) #end of navbar page
) #end of fluidpage



# Define server logic
# Warning: Error in validate_session_object: object 'session' not found solved by adding session to the part up here
server <- function(input, output, session) {
  
  observe({
    if(input$tabs == "MovementsTab") {
      movements_Server("MovementsTab1", Movements_df)
    } 
    if(input$tabs == "IndividualDatasetsTab"){
      IndividualDatasets_Server("IndividualDatasetsTab1", indiv_datasets_list)
    } 
    if(input$tabs == "EncounterHistories"){
      EncounterHistoriesSummariesWide_Server("EncounterHistoriesSummariesWideTab1", Enc_release_data)
      AllEncounters_Server("AllEncountersTab1", df_list)
    }
    if(input$tabs == "StatesTab"){
      States_Server("StatesTab1", states_data_list, weeks)
    } 
    if(input$tabs == "QAQCTab"){
      QAQC_Server("QAQCTab1", df_list = df_list, Release_05, Recaptures_05)
    } 
  })
  
}

shinyApp(ui = ui, server = server)
