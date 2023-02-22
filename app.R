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
# incorporate aviation predation and ghost tag files in
#continue with how-to


# 
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
Stationary <- read.csv(paste0("WGFP_Raw_20230201.csv")) #WGFP_Raw_20211130.csv WGFP_Raw_20220110_cf6.csv
Mobile <- read.csv("WGFP_Mobile_Detect_AllData.csv" , colClasses= c(rep("character",14), rep("numeric", 4), rep("character", 3)))
Biomark <- read.csv("Biomark_Raw_20221102.csv", dec = ",") 
# need to have tagID as a numeric field in the .csv file in order to be read in correctly as opposed to 2.3E+11 
Release <- read.csv("WGFP_ReleaseData_Master.csv", na.strings = c(""," ","NA"), colClasses=c(rep("character",8), "numeric", "numeric",rep("character",8) ))
Recaptures <- read.csv("WGFP_RecaptureData_Master.csv", na.strings = c(""," ","NA"), colClasses = c(rep("character", 9), rep("numeric", 2), rep("character", 8)))
#ghost tag df
#avitaion predation
AvianPredation <- read_csv("WGFP_AvianPredation.csv", col_types = cols(TagID = col_character(),Comments = col_character()))
GhostTags <- read_csv("WGFP_GhostTags.csv", 
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
#needs to put dates in yyyymmdd format to be filtered with drangeinput1
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

#mapping
source("map_polygon_readins.R")

#putting detection data into a function that cleans and readies data for wrangling, display, filtering, mapping, plotting
df_list <- All_combined_events_function(Stationary = Stationary, Mobile = Mobile, Release = Release, Biomark = Biomark, Recaptures = Recaptures)
All_events <- df_list$All_Events
recaps_and_detections <- df_list$Recaps_detections
Marker_tags <- df_list$Marker_Tag_data
WGFP_Clean_1 <- df_list$WGFP_Clean
unknown_tags_1 <-df_list$Unknown_Tags

#spatially joins point (detection) data to lines (station) data based on nearest feature. 
#simplestatoins is from plygon_readins
Stationdata1 <- spatial_join_stations_detections(df_list$All_Events_most_relevant, simple_stations2)

#appplies cobine_events_stations function
combined_events_stations <- combine_events_and_stations(All_events, Stationdata1)

# states
states_data_list <- states_function(combined_events_stations, GhostTags, AvianPredation)
States_summarized <- states_data_list$States_summarized
# aplies enc_hist_summary wide function
enc_hist_wide_list <- Ind_tag_enc_hist_wide_summary_function(recaps_and_detections, Release, combined_events_stations, States_summarized)
unknown_tags <- enc_hist_wide_list$Unknown_Tags
enc_hist_wide_df <- enc_hist_wide_list$ENC_Release_wide_summary
# applies get_movements_function
Movements_df <- get_movements_function(combined_events_stations)
#changes coordinates to web meractor for animations 
#animationDatalist <- Animation_function(Movements_df)

#this is used in states_data reactives; but we always want a 0 for weeks
weeks <- data.frame(weeks_since = min(states_data_list$All_States$weeks_since):max(states_data_list$All_States$weeks_since))

#more formatting
Enc_release_data <- enc_hist_wide_df %>%
    mutate(Date = ifelse(str_detect(Date, "/"),
                         as.character(mdy(Date)),
                         Date))


most_recent_date <- max(df_list$All_Events$Date)  

#want to put this in the function when ready
# Enc_release_data <- ENC_Release2_1 %>%
#     mutate(Date = ifelse(str_detect(Date, "/"),
#                          as.character(mdy(Date)),
#                          Date))
                    
rainbow_trout_pallette <- list(pink1 = "#E3BABBFF", olive_green1 = "#C4CFBFFF", dark_olive_green1 = "#81754EFF",
                               mustard_yellow1 = "#CBA660FF", brown_yellow = "#86551CFF" )

### taking dummy tag out

Biomark <- Biomark %>%
  filter(!DEC.Tag.ID %in% c("900.230000999999"))
Release_05 <- Release_05 %>%
  filter(!TagID %in% c("230000999999"))
Stationary <- Stationary %>%
  filter(!TAG %in% c("900_230000999999"))

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
             
             # tags$head(tags$style(HTML('.navbar-static-top {background-color: green;}',
             #                           '.navbar-default .navbar-nav>.active>a {background-color: green;}'))),
             # 
             tabPanel("About/How to Use",
                      includeHTML(paste0("www/", "WGFP_dataclean_vis_about.html"))
                      
                      ), #end fo how to use TabPanel
             

# Individual Datasets UI ---------------------------------------------------

             
             tabPanel("Individual Datasets",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("drangeinput1", "Select a Date Range:",
                                         #was accidnetly omitting events from the allevents tab bc the earliest release date is 2020-09-01
                                         #earliest detection was 2020-09-03, which was what it was set at before
                                         start = "2020-08-01", 
                                         end = max(df_list$All_Events$Date)+1), #end of date range input
                          actionButton("button1", label = "Render Table")
                          ),
                       
                      mainPanel(tabsetPanel(
                        tabPanel("Stationary Clean",
                                 hr(),
                                 downloadButton(outputId = "download3", label = "Save this data as CSV"),
                                 hr(),
                                 withSpinner(DT::dataTableOutput("stationary1"))),
                        tabPanel("Biomark",
                                 withSpinner(DT::dataTableOutput("biomark1"))),
                        tabPanel("Mobile",
                                 withSpinner(DT::dataTableOutput("mobile1"))),
                        tabPanel("Recaptures",
                                 withSpinner(DT::dataTableOutput("recaps1"))),
                        tabPanel("Release",
                                 withSpinner(DT::dataTableOutput("release1")),
                                 sliderInput("slider11", "Length Binwidth",
                                             min = 1, 
                                             max = max(Release_05$Length, na.rm = TRUE),
                                             value = 20),
                                 withSpinner(plotlyOutput("plot10")),
                                 hr(),
                                 sliderInput("slider12", "Weight Binwidth",
                                             min = 1, 
                                             max = max(Release_05$Weight, na.rm = TRUE), 
                                             value = 100),
                                 withSpinner(plotlyOutput("plot11")),
                                 hr(),),
                        tabPanel("Ghost Tags",
                                 withSpinner(DT::dataTableOutput("ghost1"))),
                        tabPanel("Aviation Predation Tags",
                                 withSpinner(DT::dataTableOutput("av_pred1")))
                          ) #end of sidebarlayout: incldes sidebar panel and mainpanel
                        ) #end of individual datasets tabset panel
                      )#end of individual datasets Mainpanel)
                    ),#end of Individual data tab panel
             # new Tab "Encounter Histories"
# Encounter Histories UI --------------------------------------------------

             tabPanel("Encounter Histories",
                      tabsetPanel(
                        tabPanel("Encounter Release History Summary Wide",
                                 sidebarLayout(
                                   sidebarPanel(
                                     textInput("textinput4", "Filter by Tag"),
                                     pickerInput(inputId = "picker11",
                                                 label = "Select Fish Species:",
                                                 choices = sort(unique(Enc_release_data$Species)),
                                                 selected = unique(Enc_release_data$Species),
                                                 multiple = TRUE,
                                                 options = list(
                                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                                 ),
                                     ), #end of picker 11 input
                                     sliderInput("slider4", "Fish Length (mm)",
                                                 min = min(Enc_release_data$Length, na.rm = TRUE),
                                                 max = max(Enc_release_data$Length, na.rm = TRUE),  
                                                 value = c(min(Enc_release_data$Length, na.rm = TRUE),max(Enc_release_data$Length, na.rm = TRUE)),
                                                 step = 1,
                                                 #timeFormat = "%T",
                                                 #animate = animationOptions(interval = 500, loop = FALSE)
                                     ),
                                     sliderInput("slider5", "Fish Weight (grams)",
                                                 min = min(Enc_release_data$Weight, na.rm = TRUE),
                                                 max = max(Enc_release_data$Weight, na.rm = TRUE),  
                                                 value = c(min(Enc_release_data$Weight, na.rm = TRUE),max(Enc_release_data$Weight, na.rm = TRUE)),
                                                 step = 1,
                                                 #timeFormat = "%T",
                                                 #animate = animationOptions(interval = 500, loop = FALSE)
                                     ),
                                     pickerInput(inputId = "picker12",
                                                 label = "Select Release Site:",
                                                 choices = sort(unique(Enc_release_data$ReleaseSite)),
                                                 selected = unique(Enc_release_data$ReleaseSite),
                                                 multiple = TRUE,
                                                 options = list(
                                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                                 )
                                     ), #end of picker 12 input
                                     pickerInput(inputId = "picker14",
                                                 label = "Total Number of Unique Events/Encounters:",
                                                 choices = sort(unique(Enc_release_data$TotalEncounters)),
                                                 selected = unique(Enc_release_data$TotalEncounters),
                                                 multiple = TRUE,
                                                 options = list(
                                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                                 )
                                     ), #end of picker 14 input
                                     sliderInput("slider8", "Total distance travelled (m)",
                                                 min = min(Enc_release_data$sum_dist, na.rm = TRUE),
                                                 max = max(Enc_release_data$sum_dist, na.rm = TRUE),  
                                                 value = c(min(Enc_release_data$sum_dist, na.rm = TRUE),max(Enc_release_data$sum_dist, na.rm = TRUE)),
                                                 step = 1,
                                     ), #end of slider8
                                     pickerInput(inputId = "picker13",
                                                 label = "Above/Below/Through the Dam:",
                                                 choices = sort(unique(Enc_release_data$through_dam)),
                                                 selected = unique(Enc_release_data$through_dam),
                                                 multiple = TRUE,
                                                 options = list(
                                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                                 )
                                     ), #end of picker 13 input
                                     actionButton("button6", label = "Render Table/Data", width = "100%")
                                   ), #end of sidebar panel for enc_release wide_summary
                                   mainPanel(hr(),
                                             downloadButton(outputId = "download1", label = "Save this data as CSV"),
                                             hr(),
                                             withSpinner(DT::dataTableOutput("enc_release1")),
                                             )#end of mainpanel for enc_hist_wide
                                 ),#end of enc_hist_wide sidebar_layout
                        ),#end of tabset panel for enc_release_wide summary
                        tabPanel("All Events and Plot",
                                 sidebarLayout(
                                   sidebarPanel(
                                     textInput("textinput1", "Filter by Tag"),
                                     dateRangeInput("drangeinput2", "Select a Date Range:",
                                                    start = "2020-08-01", 
                                                    end = max(df_list$All_Events$Date) + 1), #end of date range input
                                     sliderInput("slider1", "Hour of Day",
                                                 min = min(hour(All_events$Datetime)),
                                                 max = max(hour(All_events$Datetime)),  
                                                 value = c(min(hour(All_events$Datetime)),max(hour(All_events$Datetime))),
                                                 step = 1,
                                                 #timeFormat = "%T",
                                                 #animate = animationOptions(interval = 500, loop = FALSE)
                                     ),
                                     pickerInput(inputId = "picker1",
                                                 label = "Select Event",
                                                 choices = unique(df_list$All_Events$Event),
                                                 selected = unique(df_list$All_Events$Event),
                                                 multiple = TRUE,
                                                 options = list(
                                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                                 ),
                                     ), #end of picker input
                                     pickerInput(inputId = "picker2",
                                                 label = "Select Fish Species:",
                                                 choices = sort(unique(df_list$All_Events$Species)),
                                                 selected = unique(df_list$All_Events$Species),
                                                 multiple = TRUE,
                                                 options = list(
                                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                                 ),
                                     ), #end of picker 2 input
                                     sliderInput("slider6", "Fish Release Length (mm)",
                                                 min = min(df_list$All_Events$Release_Length, na.rm = TRUE),
                                                 max = max(df_list$All_Events$Release_Length, na.rm = TRUE),
                                                 value = c(min(df_list$All_Events$Release_Length, na.rm = TRUE),max(df_list$All_Events$Release_Length, na.rm = TRUE)),
                                                 step = 1,
                                     ),
                                     sliderInput("slider7", "Fish Release Weight (grams)",
                                                 min = min(df_list$All_Events$Release_Weight, na.rm = TRUE),
                                                 max = max(df_list$All_Events$Release_Weight, na.rm = TRUE),
                                                 value = c(min(df_list$All_Events$Release_Weight, na.rm = TRUE),max(df_list$All_Events$Release_Weight, na.rm = TRUE)),
                                                 step = 1,
                                     ),
                                     dateRangeInput("drangeinput3", "Release Date Range:",
                                                    start = min(df_list$All_Events$Release_Date, na.rm = TRUE) - 1, 
                                                    end = max(df_list$All_Events$Release_Date, na.rm = TRUE) + 1), #end of date range input
                                     pickerInput(inputId = "picker3",
                                                 label = "Select Release Site:",
                                                 choices = sort(unique(df_list$All_Events$ReleaseSite)),
                                                 selected = unique(df_list$All_Events$ReleaseSite),
                                                 multiple = TRUE,
                                                 options = list(
                                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                                 )
                                     ), #end of picker 3 input
                                     
                                     checkboxInput("checkbox1", "Remove Duplicate Days, TAGs, Events and UTMs"),
                                     checkboxInput("checkbox2", "Remove Duplicate TAGs: doesn't work with TAG filter"), #deliberate decision not to add another if statement to have it actually work because it doesn't make sense you would use both at the same time
                                     actionButton("button2", "Reset Filters"),
                                     tags$hr(),
                                     #submit button is limited in scope, doesn't even have a input ID , but works for controlling literally all inputs
                                     #submitButton("Update inputs", icon("sync"))
                                     
                                     actionButton("button3", label = "Render Table", width = "100%")
                                   ),#end of all events and plot sidebar panel
                                   mainPanel(
                                     tabsetPanel(tabPanel("All Events",
                                       hr(),
                                       downloadButton(outputId = "download2", label = "Save this data as CSV"),
                                       hr(),
                                       withSpinner(DT::dataTableOutput("allevents1")),
                                      ),
                                      tabPanel("Plot",
                                               plotlyOutput("plot5")
                                               )
                                     )#end of tabset panel
                                   )#end of all events and plot mainpanel
                                 ),# end of all events and plot sidebarLayout
                                 ) #end of tab panel for all events and plot
                      ), #end of tabset panel containing enc_hist-wide summary and all_events/plot tabs
             ), #end of Encounter Histories Tab
# States UI -----------------------------------------------------

            tabPanel("Weekly States",
                     sidebarLayout(
                       sidebarPanel(      
                          textInput("textinput2", label = "Filter by TAG"),
                          pickerInput(inputId = "picker4",
                                      label = "Select number of weekly unique events:",
                                      choices = sort(unique(states_data_list$All_States$weekly_unique_events)), #will need to be updated later on for uniqueness
                                      selected = unique(states_data_list$All_States$weekly_unique_events),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE #this makes the "select/deselect all" option
                                      )
                          ), #end of picker 4 input 
                          pickerInput(inputId = "picker5",
                                      label = "States:",
                                      choices = sort(unique(states_data_list$All_States$State)), #will need to be updated later on for uniqueness
                                      selected = unique(states_data_list$All_States$State),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE #this makes the "select/deselect all" option
                                      )
                          ), #end of picker 5 input 
                          actionButton("button5", label = "Render Table/Data", width = "100%")
                       ),#end of sidebar panel
                       mainPanel(tabsetPanel(
                         tabPanel("States Dataframe",
                                  hr(),
                                  downloadButton(outputId = "download4", label = "Save this data as CSV"),
                                  hr(),
                                  withSpinner(DT::dataTableOutput("states1"))),
                         tabPanel("States and Weeks Wide",
                                  hr(),
                                  downloadButton(outputId = "download5", label = "Save this data as CSV"),
                                  hr(),
                                  verbatimTextOutput("text1"),
                                  withSpinner(DT::dataTableOutput("states2"))),
                         tabPanel("Unknown States",
                                  hr(),
                                  withSpinner(DT::dataTableOutput("unknownstates1")),
                         ) #end of tabpanel
                        )#end of tabsetPanel
                      )#end of mainPanel
                     )#end of sidebarLayout including sidebarPanel and Mainpanel
                    ),#end of States and movements ui Tab

# Movements and Map UI Tab --------------------------------------------------------------

### note on map: if detectoins are close together, they'll be grouped and you can't do more resolution. But they can still be upstream/downstream movements if they're >= 10 m difference in station
#the filtering also automatically takes out NA values on movement with picker6; but all the NA movement onlys should be from fish where we have no release info for,
#and also from fish that have detections before their official "release" back in May
#if marker_color or icon_color is NA, it wont get mapped or displayed in data
#picker wasn't working becuase I had 2 different pick
            tabPanel("Daily Movements Map, Plot, and Data",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                      textInput("textinput3", label = "Filter by TAG"),
                                      pickerInput(inputId = "picker6",
                                                  label = "Select Movement Type:",
                                                  choices = sort(unique(Movements_df$movement_only)),
                                                  selected = unique(Movements_df$movement_only),
                                                  multiple = TRUE,
                                                  options = list(
                                                    `actions-box` = TRUE #this makes the "select/deselect all" option
                                                  )
                                      ), #end of picker 6 input
                                      
                                      pickerInput(inputId = "picker7",
                                                  label = "Select Detection Type",
                                                  choices = sort(unique(Movements_df$det_type)),
                                                  selected = unique(Movements_df$det_type),
                                                  multiple = TRUE,
                                                  options = list(
                                                    `actions-box` = TRUE #this makes the "select/deselect all" option
                                                  )
                                                  ), #end of picker 7 
                                    pickerInput(inputId = "picker10",
                                                label = "Select Species Type",
                                                choices = sort(unique(Movements_df$Species)),
                                                selected = unique(Movements_df$Species),
                                                multiple = TRUE,
                                                options = list(
                                                  `actions-box` = TRUE #this makes the "select/deselect all" option
                                                )
                                    ), #end of picker 10 
                                    
                                    sliderInput("slider10", "Fish Release Length",
                                                min = min(Movements_df$Release_Length, na.rm = TRUE),
                                                max = max(Movements_df$Release_Length, na.rm = TRUE),  
                                                value = c(min(Movements_df$Release_Length, na.rm = TRUE),max(Movements_df$Release_Length, na.rm = TRUE)),
                                                step = 1,
                                                
                                    ), #end of slider8
                                    
                                    
                                      sliderInput("slider2", "Date",
                                                  min = min(df_list$All_Events$Date -1),
                                                  max = max(df_list$All_Events$Date +1),  
                                                  value = c(min(df_list$All_Events$Date -1),max(df_list$All_Events$Date +1)),
                                                  step = 1,
                                                  timeFormat = "%d %b %y",
                                                  #animate = animationOptions(interval = 500, loop = FALSE)
                                      ),
                                    
                                    sliderInput("slider9", "Total distance travelled (m)",
                                                min = min(Movements_df$sum_dist, na.rm = TRUE),
                                                max = max(Movements_df$sum_dist, na.rm = TRUE),  
                                                value = c(min(Movements_df$sum_dist, na.rm = TRUE),max(Movements_df$sum_dist, na.rm = TRUE)),
                                                step = 1,
                                                
                                    ), #end of slider8
                                      actionButton("button7", label = "Render Map and Data"), 
                                      hr(),
                       ),#end of sidebar panel
                       mainPanel(width = 10,
                                 tabsetPanel(
                                   tabPanel("Map and Table",
                                            splitLayout(cellWidths = c("40%", "60%"),
                                                        withSpinner(DT::dataTableOutput("movements1")),
                                                        withSpinner(leafletOutput("map1", height = 600))
                                            ), #end of splitLayout
                                            hr(),
                                            downloadButton(outputId = "download6", label = "Save movements data as CSV"),
                                            hr(),
                                    ), # end of Map and table tabPanel
                                   tabPanel("Movement Graphs",
                                            withSpinner(plotlyOutput("plot1")),
                                            hr(),
                                            withSpinner(plotlyOutput("plot6")),
                                            downloadButton(outputId = "download8", label = "Save seasonal plot data as CSV"),
                                            hr(),
                                            withSpinner(plotlyOutput("plot7")),
                                            hr(),
                                            withSpinner(plotlyOutput("plot8")),
                                            hr(),
                                            withSpinner(plotlyOutput("plot9")),
                                            hr(),
                                            
                                            verbatimTextOutput("text2"),
                                            ), #end of movement graphs tabpanel
                                   tabPanel("Animation",
                                            br(),
                                            fluidRow(
                                              column(width = 4,
                                                     radioButtons("radio2", "Timeframe", 
                                                                  choices = c("days", "weeks"), 
                                                                  selected = "weeks"),
                                                     # sliderInput("pointSize_Slider", "Select Size of Point", 
                                                     #             min = 1, 
                                                     #             max = 12, 
                                                     #             value = 4),
                                                     sliderInput("fps_Slider", "Select frames per Second",
                                                                 min = 0,
                                                                 max = 15,
                                                                 value = 2,
                                                                 step = .2),
                                                     ),#end of column
                                              column(width = 4, 
                                                     textInput("anim_Title", "Animation Title"),
                                                     # radioButtons("renderOption", "Render as GIF or Video", 
                                                     #              choices = c("GIF","Video"))
                                                     )
                                            ),#end of fluidrow
                                            
                                            actionButton("button9", "Render Animation: Need to click 'Render Map and Data' button in Sidebar first. Takes a couple minutes to render usually"), 
                                            imageOutput("plot12")
                                            ) #end of animation tabPanel
                                 ), # end of tabset panel
                       )#end of mainPanel
                     )#end of sidebarLayout including sidebarPanel and Mainpanel
            ),#end of Map ui Tab

# QAQC UI tab -------------------------------------------------------------

          tabPanel("QAQC",
                   tabsetPanel(
                     tabPanel("Marker Tags",
                              sidebarLayout(
                                sidebarPanel(
                                  pickerInput(inputId = "picker8",
                                              label = "Select Site Code",
                                              choices = sort(unique(df_list$Marker_Tag_data$SCD)),
                                              selected = unique(df_list$Marker_Tag_data$SCD),
                                              multiple = TRUE,
                                              options = list(
                                                `actions-box` = TRUE #this makes the "select/deselect all" option
                                              )
                                ), #end of picker 8
                                  pickerInput(inputId = "picker9",
                                              label = "Select Marker Tag",
                                              choices = sort(unique(df_list$Marker_Tag_data$TAG)),
                                              selected = unique(df_list$Marker_Tag_data$TAG)[1],
                                              multiple = TRUE,
                                              options = list(
                                                `actions-box` = TRUE #this makes the "select/deselect all" option
                                              )
                                  ), #end of picker 9 
                              sliderInput("slider3", "Date",
                                          min = min(df_list$Marker_Tag_data$DTY -1),
                                          max = max(df_list$Marker_Tag_data$DTY +1),  
                                          value = c(min(df_list$Marker_Tag_data$DTY -1),max(df_list$Marker_Tag_data$DTY +1)),
                                          step = 1,
                                          timeFormat = "%d %b %y",
                                          #animate = animationOptions(interval = 500, loop = FALSE)
                              ),
                              actionButton("button8", label = "Render Marker Tag Plot")
                                ), #end of sidebar panel
                              mainPanel(
                                splitLayout(
                                  
                                  withSpinner(DT::dataTableOutput("markertags1")),
                                  withSpinner(plotlyOutput("plot2"))
                                  
                                ),
                                hr(),
                                downloadButton(outputId = "download7", label = "Save this data as CSV"),
                                hr(),
                              )#end of mainpanel
                              )#end of sidebar layout
                              ), #end of marker tag tab
                     tabPanel("Release and Recap Length/Weights",
                              fluidRow(
                                column(
                                  width = 8,
                                  withSpinner(plotlyOutput("plot3"))
                                              ),#end of column
                                column(
                                  width = 4,
                                  withSpinner(plotlyOutput("plot4"))
                                  
                                )#end of column
                                       )#end of fluidrow
                              ), #end of tabPanel
                     tabPanel("Unknown Tags",
                              withSpinner(DT::dataTableOutput("unknowntags1"))
                              )#end of tab panel
                   )#end of tabset Panel 
                   ) # end of tabPanel
          

         

    ) #end of navbar page
) #end of fluidpage



# Define server logic
# Warning: Error in validate_session_object: object 'session' not found solved by adding session to the part up here
server <- function(input, output, session) {
  

# Reset Filters Logic -----------------------------------------------------

  
  observeEvent(input$button2, {
    
    updateTextInput(session, "textinput1",
                    value = "")
    
    updateDateRangeInput(session, "drangeinput2",
                         start = "2020-08-01", 
                         end = max(df_list$All_Events$Date) + 1)
    
    updatePickerInput(session, "picker1",
                      selected = unique(df_list$All_Events$Event)
    )
    
    updatePickerInput(session, "picker2",
                      selected = unique(df_list$All_Events$Species)
    )
    
    updatePickerInput(session, "picker3",
                      selected = unique(df_list$All_Events$ReleaseSite)
    )
    
    updateCheckboxInput(session, "checkbox1",
                        value = NULL)
    
    updateCheckboxInput(session, "checkbox2",
                        value = NULL)
    
    updateSliderInput(session, "slider1", 
                      value = c(min(hour(All_events$Datetime)),max(hour(All_events$Datetime)))
    )
    
  }) #end of reset 
  
  #when button to make States DF list is pressed, update these picker options
  observeEvent(input$button5,{
    updatePickerInput(session, "picker4",
                      choices = sort(unique(filtered_states_data()$All_States$weekly_unique_events)),
                      selected = unique(filtered_states_data()$All_States$weekly_unique_events)
    )
    
    updatePickerInput(session, "picker5",
                      choices = sort(unique(filtered_states_data()$All_States$State)),
                      selected = unique(filtered_states_data()$All_States$State)
    )
   
  })
  
  
  
  #create slider input depending on data frequency
  # observe({
  #   
  #   # allDates <- unique(covidData$Date_reported)
  #   # eligibleDates <- allDates[xts::endpoints(allDates, on = input$frequency)]
  #   # 
  #   if(input$slider2 == "weeks"){
  #     stepSize = 7
  #   }else{
  #     stepSize = 1
  #   }
  #   
  #   output$dateUI <- renderUI({
  #     sliderInput("slider2", "Date",
  #                 min = min(df_list$All_Events$Date),
  #                 max = max(df_list$All_Events$Date),
  #                 value = min(df_list$All_Events$Date),
  #                 step = stepSize,
  #                 timeFormat = "%d %b %y",
  #                 animate = animationOptions(interval = 500, loop = FALSE)
  #     )
  #   })
  # })
  #   

# Ind D Reactives ---------------------------------------------------------
    
  
    indiv_datasets_list <- eventReactive(input$button1,{
      
      
      stationary_filtered <- df_list$WGFP_Clean %>%
        filter(DTY >= req(input$drangeinput1[1]) & DTY <= req(input$drangeinput1[2]),
               #TAG == input$textinput1 #not gonna do tag filtering for now
        )
      
      biomark_filtered <- Biomark %>%
        filter(Scan.Date >= input$drangeinput1[1] & Scan.Date <= input$drangeinput1[2])
      
      mobile_filtered <- Mobile %>%
        filter(Date >= input$drangeinput1[1] & Date <= input$drangeinput1[2])
      
      recaps_filtered <- Recaptures_05 %>%
        filter(Date >= input$drangeinput1[1] & Date <= input$drangeinput1[2])
      
      release_filtered <- Release_05 %>%
        filter(Date >= input$drangeinput1[1] & Date <= input$drangeinput1[2])
      
      ghost_filtered <- GhostTags %>%
        filter(GhostDate >= input$drangeinput1[1] & GhostDate <= input$drangeinput1[2])
      
      av_filtered <- AvianPredation %>%
        filter(PredationDate >= input$drangeinput1[1] & PredationDate <= input$drangeinput1[2])
      
      
      indiv_d_list <- list(
        "stationarycleandata" = stationary_filtered,
        "biomarkdata" = biomark_filtered,
        "mobiledata" = mobile_filtered,
        "recapdata" = recaps_filtered,
        "releasedata" = release_filtered,
        "ghostdata" = ghost_filtered,
        "avian_preddata" = av_filtered
        
      )
      
      return(indiv_d_list)
      
      
    })
    


# Enc Hist Wide Reactive --------------------------------------------------
  enc_hist_wide_filtered <- eventReactive(input$button6,{
    ##gona have to change a lot of outputs later based on what this is named
    
    
    # if the Tag filter is used or not 
    if(input$textinput4 !=''){
      
      Enc_release_data_filtered <- Enc_release_data %>%
        filter(
          TAG %in% c(input$textinput4),
          Species %in% input$picker11,
          ReleaseSite %in% input$picker12,
          Length >= input$slider4[1] & Length <= input$slider4[2],
          Weight >= input$slider5[1] & Weight <= input$slider5[2],
          sum_dist >= input$slider8[1] & sum_dist <= input$slider8[2],
          through_dam %in% input$picker13,
          TotalEncounters %in% input$picker14
          
          ) 
      
    } else {
      
      Enc_release_data_filtered <- Enc_release_data %>%
        filter(
          Species %in% input$picker11,
          ReleaseSite %in% input$picker12,
          Length >= input$slider4[1] & Length <= input$slider4[2],
          Weight >= input$slider5[1] & Weight <= input$slider5[2],
          sum_dist >= input$slider8[1] & sum_dist <= input$slider8[2],
          through_dam %in% input$picker13,
          TotalEncounters %in% input$picker14
          ) 
    }
    
    return(Enc_release_data_filtered)
  }) #end of ENC data list eventReactive

# ALL Events and Plot Reactive --------------------------------------------


    
    #enc_releae_data wasn't registering bc i used reactive() instead of reactive ({}).
    #i guess reactive ({}) makes it so you can make multiple expressions within a reactive context whereas reactive() can only do 1
    all_events_data <- eventReactive(input$button3,{
      # if the Tag filter is used or not 
      if(input$textinput1 !=''){
        #all events
        all_events_filtered <- df_list$All_Events  %>%
          filter(

            TAG %in% c(input$textinput1),
            Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
            hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],
            Event %in% input$picker1,
            Species %in% input$picker2,
            Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
            Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
            ReleaseSite %in% input$picker3,
            Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2]
          ) %>%
          arrange(Datetime)
        
        
      } else {
        all_events_filtered <- df_list$All_Events  %>%
          filter(
            
            Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
            hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],
            Event %in% input$picker1,
            Species %in% input$picker2,
            Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
            Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
            ReleaseSite %in% input$picker3,
            Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2]
          )%>%
          arrange(Datetime)
        
        
      }
        
        
        # error below solved because I wasn't using the correct variable names for each dataset
        # x `Site_Code` not found in `.data`.
        # x `Scan_Date` not found in `.data` 
      ### Filtering for TAG, SIte Code, and Day  
        
        #if there is a tag input along with the first box checked
    if (input$checkbox1 == TRUE & input$checkbox2 == FALSE & input$textinput1 !='') {
        
        all_events_filtered <- df_list$All_Events %>%
          
          filter(
            TAG == input$textinput1,
            Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
            hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],
            
                 Event %in% input$picker1,
                 Species %in% input$picker2,
                 ReleaseSite %in% input$picker3,
            Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
            Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
            Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2]
            ) %>%
          #this part is for making sure the sequence of events will make sense sequentially: tells where a fish started and ended the day and keeps other unique entries in between
          group_by(Date) %>%
          mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                        Datetime == max(Datetime) ~ "Last_of_day",
                                        Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
          ) %>%
          ungroup() %>%
          #need to include UTM_X and UTM_Y so that you can get multiple daily detections of mobile antennas in different locations
          distinct(TAG, Event, Date, first_last, UTM_X, UTM_Y, .keep_all = TRUE) %>%
          arrange(Datetime) %>%
          select(-first_last)
        
        
    }
        #if there isn't a tag input along with first box checked
        if (input$checkbox1 == TRUE & input$checkbox2 == FALSE & input$textinput1 =='') {
          
          all_events_filtered <- df_list$All_Events %>%
            
            filter(
              Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
              hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],
              
              Event %in% input$picker1,
              Species %in% input$picker2,
              Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
              Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
              Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2],
              ReleaseSite %in% input$picker3) %>%
            #this part is for making sure the sequence of events will make sense
            # if there's no tag input then have to group_by TAG as well
            group_by(Date, TAG) %>% 
            mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                          Datetime == max(Datetime) ~ "Last_of_day",
                                          Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
            ) %>%
            ungroup() %>%
            distinct(TAG, Event, Date, first_last,  UTM_X, UTM_Y, .keep_all = TRUE) %>%
            arrange(Datetime) %>%
            select(-first_last) 
          
          
        }
        
        if (input$checkbox2 == TRUE) {
          
          
          
          all_events_filtered <- df_list$All_Events %>%
            filter(
              #TAG == input$textinput1,
                    Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                    hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],
                    
                   Event %in% input$picker1,
                   Species %in% input$picker2,
                   Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
                   Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
                   Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2],
                   ReleaseSite %in% input$picker3) %>%
            arrange(Datetime) %>%
            #need to have distinct() at the end of the expression
            
            distinct(TAG, .keep_all = TRUE)
            
        }
      #### filter dummy row
      all_events_filtered <- all_events_filtered %>%
        filter(!TAG %in% c("230000999999"))

        
        return(all_events_filtered)
    }) #end of ENC data list eventReactive
    

# States data reactives ---------------------------------------------------

    #want it so that when the first button4 is pressed, the whole dataset is made
    #then after that i want to render the table with button5 along with filters
    # initial_states_data_list <- eventReactive(input$button4,{
    #   states_data1_list <- states_function(combined_events_stations, ghost_tag_df)
    #   
    #   
    #   return(states_data1_list)
    #     
    # })
    
    filtered_states_data <- eventReactive(input$button5,{
      
      if(input$textinput2 != ''){ 
        states_data1 <- states_data_list$All_States %>%
          filter(TAG %in% c(input$textinput2),
                 weekly_unique_events %in% input$picker4,
                 State %in% input$picker5
                 )%>%
          arrange(Date)
      } else { 
        states_data1 <- states_data_list$All_States %>%
          filter(
            weekly_unique_events %in% input$picker4,
            State %in% input$picker5
          )%>%
          arrange(Date)
      }
      
      #weeks <- data.frame(weeks_since = 1:max(states_data1$weeks_since))
      weeks_and_states <- full_join(weeks, states_data1, by = "weeks_since")
      
      weeks_and_states_wide <- pivot_wider(weeks_and_states, id_cols = TAG, names_from = weeks_since, values_from = State)
      #need to change this part if we decide to start at week 1 instead of 0
      weeks_and_states_wide <- weeks_and_states_wide %>%
        select(TAG, `0`, 2:ncol(weeks_and_states_wide))
      
      #putting all together in a list
      
      states_data_list <- list("filtered_states" = states_data1, "filtered_wide" =  weeks_and_states_wide)
      
      return(states_data_list)
    }) 

# Movement Data Reactives -------------------------------------------------

    
    filtered_movements_data <- eventReactive(input$button7,{
      # movements_data1 <- Movements_df %>%
      #   filter(movement_only %in% input$picker6)
      
      if(input$textinput3 != ''){
        movements_data1 <- Movements_df %>%
          filter(TAG %in% c(input$textinput3),
                 Release_Length >= input$slider10[1] & Release_Length <= input$slider10[2],
                 Date >= input$slider2[1] & Date <= input$slider2[2],
                 movement_only %in% c(input$picker6),
                 det_type %in% c(input$picker7),
                 Species %in% c(input$picker10),
                 sum_dist >= input$slider9[1] & sum_dist <= input$slider9[2],
                 
                 
                 # daily_unique_events %in% input$picker4,
                 # State %in% input$picker5
          ) %>%
          arrange(Datetime)
        #this id column is used for the map and datatable proxy and needs to be redone each time a filter is applied
        movements_data1$id <- seq.int(nrow(movements_data1))
        
      } else {
        movements_data1 <- Movements_df  %>% 
          filter(
            Release_Length >= input$slider10[1] & Release_Length <= input$slider10[2],
            Date >= input$slider2[1] & Date <= input$slider2[2],
            movement_only %in% c(input$picker6),
            det_type %in% c(input$picker7),
            Species %in% c(input$picker10),
            sum_dist >= input$slider9[1] & sum_dist <= input$slider9[2]
            
            
            # daily_unique_events %in% input$picker4,
            # State %in% input$picker5
          ) %>%
          arrange(Datetime)
        movements_data1$id <- seq.int(nrow(movements_data1))
        
      }
      
      
      return(movements_data1)
    }) 

# QAQC Reactives ----------------------------------------------------------
    filtered_markertag_data <- eventReactive(input$button8,{
      #pickers8 ad 9
      
      
        markertag_data1 <- df_list$Marker_Tag_data %>%
          filter(SCD %in% c(input$picker8),
                 TAG %in% c(input$picker9),
                 DTY >= input$slider3[1] & DTY <= input$slider3[2]
              )
      
      
      return(markertag_data1)
    }) 
    
      

# Individual Datatable renders -------------------------------------------------------

    
    output$stationary1 <- DT::renderDataTable(
        
      
      indiv_datasets_list()$stationarycleandata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
        
    )
    
    
    output$biomark1 <- renderDataTable(
        
      indiv_datasets_list()$biomarkdata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
    )
    
    
    output$mobile1 <- renderDataTable(
        
      indiv_datasets_list()$mobiledata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
    )
    
    output$recaps1 <- renderDataTable(
      
      indiv_datasets_list()$recapdata,
      rownames = FALSE,
      #extensions = c('Buttons'),
      #for slider filter instead of text input
      filter = 'top',
      options = list(
        pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
        dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
        language = list(emptyTable = "Enter inputs and press Render Table")
        
        #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
      )
    )
    
    output$release1 <- renderDataTable(
      
      indiv_datasets_list()$releasedata,
      rownames = FALSE,
      #extensions = c('Buttons'),
      #for slider filter instead of text input
      filter = 'top',
      options = list(
        pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
        dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
        language = list(emptyTable = "Enter inputs and press Render Table")
        
        #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
      )
    )
    
    output$plot10 <- renderPlotly({
      indiv_datasets_list()$releasedata %>%
        ggplot(aes(x = Length, fill = Species) ) +
        geom_histogram(binwidth = input$slider11)+
        theme_classic() +
        labs(title = "Released Fish by Length", caption = "Binwidth = 20mm")
    })
    
    output$plot11 <- renderPlotly({
      indiv_datasets_list()$releasedata %>%
        ggplot(aes(x = Weight, fill = Species) ) +
        geom_histogram(binwidth = input$slider12)+
        theme_classic() +
        labs(title = "Released Fish by Weight", caption = "Binwidth = 100g")
    })
    
    output$ghost1 <- renderDataTable(
      
      indiv_datasets_list()$ghostdata,
      rownames = FALSE,
      caption = ("Date filter applies to Ghost Date"),
      filter = 'top',
      options = list(
        pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
        dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
        language = list(emptyTable = "Enter inputs and press Render Table")
        
        #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
      )
    )
    
    output$av_pred1 <- renderDataTable(
      
      indiv_datasets_list()$avian_preddata,
      rownames = FALSE,
      caption = ("Date filter applies to Predation Date"),
      filter = 'top',
      options = list(
        pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
        dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
        language = list(emptyTable = "Enter inputs and press Render Table")
        
        #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
      )
    )
    
    # Dt 
    
    output$enc_release1 <- renderDataTable(
        
      enc_hist_wide_filtered(),
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
    )
    
    output$allevents1 <- renderDataTable({
      datatable(all_events_data(),
                  rownames = FALSE,
                  #extensions = c('Buttons'),
                  #for slider filter instead of text input
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  ) #end of options list
                ) %>%
        formatStyle(
          columns = 1:ncol(all_events_data())
          #rownames = FALSE
        )

      
    })

# States Datatable Renders ------------------------------------------------


    output$states1 <- renderDT({
      
      #input$button5
      
        datatable(filtered_states_data()$filtered_states, #initial_states_data_list()$All_States
                  rownames = FALSE,
                  #extensions = c('Buttons'),
                  #for slider filter instead of text input
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                    
                    #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
                  )
        ) %>%
          formatStyle(
            columns = c(1:ncol(filtered_states_data()$filtered_states))
            
          )
      
      
      
    })
    
    output$text1 <- renderPrint({
      "Sidebar filters now work on Wide Data"
    })
    
    output$states2 <- renderDT({
      #input$button5
      
      datatable(filtered_states_data()$filtered_wide,
                rownames = FALSE,
                
                filter = 'top',
                options = list(
                  pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                  dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                  language = list(emptyTable = "Enter inputs and press Render Table")
                  
                  #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
                )
      ) 
        
      
    })
    
    output$unknownstates1 <- renderDT({
      
      
      datatable(states_data_list$Flagged_movements,
                rownames = FALSE,
                caption = "2022-12-16: 'unknown'states df needs some work so don't pay too much atantion to this. -SG. this hopefully should be pretty small...filled with tags with detections before official 'Release' such as in in May 2021 and tags without release info.",
                filter = 'top',
                options = list(
                  pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                  dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                  language = list(emptyTable = "Enter inputs and press Render Table")
                  
                  #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
                )
      ) 
      
      
    })
    
    output$movements1 <- renderDT({
      
      
      datatable(filtered_movements_data(),
                rownames = FALSE,
                selection = "single",
                filter = 'top',
                options = list(
                  #statesave is restore table state on page reload
                  stateSave =TRUE,
                  pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                  dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                  language = list(emptyTable = "Enter inputs and press Render Table")
                  
                  #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
                )
      ) 
      
      
    })
    
    

# MarkerTag and Unknown Tags datatable Renders ----------------------------------------------
    output$markertags1 <- renderDT({
      datatable(filtered_markertag_data(),
                rownames = FALSE,
                selection = "single",
                filter = 'top',
                options = list(
                  #statesave is restore table state on page reload
                  stateSave =TRUE,
                  pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                  dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                  language = list(emptyTable = "Enter inputs and press Render Table")
                )
      ) 
    })
    
    output$unknowntags1 <- renderDT({
      datatable(unknown_tags,
                rownames = FALSE,
                selection = "single",
                filter = 'top',
                caption = ("Tags that initially started  with 900_ but are not in the release file."),
                options = list(
                  #statesave is restore table state on page reload
                  stateSave =TRUE,
                  pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                  dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                  language = list(emptyTable = "Enter inputs and press Render Table")
                )
      ) 
    })

# Enc Hist Plot Render ----------------------------------------------------

    output$plot5 <- renderPlotly({
      plot <- all_events_data() %>%
        ggplot(aes(x= Date, fill = Event, 
                   text = paste('Date: ', as.character(Date), '\n')
        )) +
        geom_bar(stat = "count", position = "dodge") +
        theme_classic() +
        labs(title = "Raw Detections Frequency")
      
      
      plotly5 <- ggplotly(p = plot)
      plotly5  
      
    })
    
# Map proxy for Icons -----------------------------------------------------
    
    
    # to keep track of previously selected row
    #setting to nothing for now
    row_selected1 <- reactiveVal()
    #this is what the new icon for selected rows looks like
    my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'lightblue', iconColor = 'red')
    
    icons <- reactive({
        awesomeIcons(
          icon = 'ios-close',
          iconColor = filtered_movements_data()$icon_color,
          library = 'ion',
          markerColor = filtered_movements_data()$marker_color
        )
    })
    
    #group_name <- "my_additons"
    #230000228991
    
    # get_data <- reactive({
    #   event.data <- event_data("plotly_selected", source = "subset")
    #   data <- data %>% mutate(show_id = FALSE)
    #   if (!is.null(event.data)) {
    #     data$show_id[event.data$pointNumber + 1] <- TRUE
    #   }
    #   data
    # })
    
    observeEvent(input$movements1_rows_selected, {
      row_selected = filtered_movements_data()[input$movements1_rows_selected,]
      
      proxy <- leafletProxy('map1')
      #print(row_selected)
      #print(input$movements1_rows_selected)
      proxy %>%
        #clearing the group removes previous marker from previuous row before making a new one
        clearGroup(group = "markers") %>%
      
        addAwesomeMarkers(
          clusterOptions = markerClusterOptions(),
          group = "markers",
          popup = paste(
            "TAG:", row_selected$TAG, "<br>",
            "Release Site:", row_selected$ReleaseSite, "<br>",
            "Detection Event:", row_selected$det_type, "<br>",
            "Date:", row_selected$Datetime),
          
          layerId = as.character(row_selected$id),
          lng=row_selected$X, 
          lat=row_selected$Y,
          icon = my_icon)
      
     
     })
    
    
  
    

# Movements Map Output ----------------------------------------------------

    output$map1 <- renderLeaflet({
      
      
      
      
      leaflet(filtered_movements_data()) %>% #Warning: Error in UseMethod: no applicable method for 'metaData' applied to an object of class "NULL"  solved becuase leaflet() needs an arg leaflet(x)
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
          label = paste(filtered_movements_data()$movement_only, "\n",
                        filtered_movements_data()$Date),
          layerId = as.character(filtered_movements_data()$id),
          popup = paste(
            "TAG:", filtered_movements_data()$TAG, "<br>",
            "Release Site:", filtered_movements_data()$ReleaseSite, "<br>",
            "Detection Event:", filtered_movements_data()$det_type, "<br>",
            "Date:", as.character(filtered_movements_data()$Datetime))
          ) %>%
        
        ###polylines and points: obtained from GISdb from this study
        addAwesomeMarkers(data = antenna_sites@coords,
                          icon = Station_icons,
                          clusterOptions = markerClusterOptions(),
                          label = paste(antenna_sites@data$SiteLabel),
                          popup = paste(antenna_sites@data$SiteName, "<br>",
                                        "Channel Width:", antenna_sites@data$ChannelWid, "feet"),
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
        addLayersControl(overlayGroups = c("Detections", "Antennas", "Release Sites", "Stream Centerlines", "Stations (m)", "Mobile Reaches")) %>%
        hideGroup(c("Stream Centerlines", "Stations (m)", "Antennas", "Release Sites", "Mobile Reaches"))
      
      
    })
    
    #when map is clicked, go to that icon in the dataTable
    # pagination wasn't working bc ID being assigned was different than row number; 
    #so when a icon was clicked, it was selecting the right row, but the row number was different than Id so it was going to the wrong page
    #fixed by assigning a new Id column evyertime the data is filtered; might be a more efficient way to do this but idk/idc
    observeEvent(input$map1_marker_click, {
      #need to assign layer ID's in leafletrender to have an id associated with the click
      #clicking the map gives info in the form of a list, including the layer id assigned in leaflet
      clickId <- input$map1_marker_click$id
      #print(clickId)
      #print(input$movements1_state$length)
      #print(which(filtered_movements_data()$id == clickId))
      #saying get the rows in the data with the same id as clickId; clickId is the row number
      dataTableProxy("movements1") %>%
        selectRows(which(filtered_movements_data()$id == clickId)) %>%
        selectPage(which(input$movements1_rows_all == clickId) %/% input$movements1_state$length + 1)
    })
    

# Movements Animation Output ----------------------------------------------
    observeEvent(input$button9, {
      animationDatalist <- Animation_function(filtered_movements_data())
      set_defaults(map_service = "esri", map_type = "world_imagery")
      
      map_with_data <- ggplot() +
        basemap_gglayer(animationDatalist$coords1) +
        scale_fill_identity() +
        coord_sf() +
        theme_classic() +
        guides(size = FALSE, color = guide_legend(title = "Movement"))
      
      
      
      output$plot12 <- renderImage(
        {
          if (input$radio2 == "weeks"){
            map_with_data <- map_with_data + 
              geom_point(data = animationDatalist$data, aes(x = animationDatalist$data$X.1, y = animationDatalist$data$Y.1,
                                                            size = input$pointSize_Slider,
                                                            color = animationDatalist$data$movement_only, group = animationDatalist$data$weeks_since))+
              transition_time(weeks_since) +
              ggtitle(
                #paste("Date", m3$Date),
                paste(input$anim_Title, '{frame_time}'),
                subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist$data$Date) ))
            map_with_data
            anim_save("outfile.gif", animate(map_with_data, nframes = animationDatalist$num_weeks, fps = input$fps_Slider, height = 1200, width =1200)) # New
            
            
          } else if (input$radio2 == "days"){
            map_with_data <- map_with_data + 
              geom_point(data = animationDatalist$data, aes(x = animationDatalist$data$X.1, y = animationDatalist$data$Y.1,
                                                            size = 10,
                                                            color = animationDatalist$data$movement_only, group = animationDatalist$data$days_since))+
              transition_time(days_since) + 
              labs(title = "Days") +
              ggtitle(
                
                paste(input$anim_Title, '{frame_time}'),
                subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(animationDatalist$data$Date) ))
            map_with_data
            anim_save("WindyGapFishMovements.gif", animate(map_with_data, nframes = animationDatalist$num_days, fps = input$fps_Slider, height = 1200, width =1200)) # New
            
          }
          
          
          list(src = "WindyGapFishMovements.gif", contentType = "image/gif")
        },
        deleteFile = FALSE
      )
    })
    

# Movement Plots Output ----------------------------------------------------

    #daily
    output$plot1 <- renderPlotly({
      plot <- filtered_movements_data() %>%
        ggplot(aes(x = Date, fill = movement_only,
                   text = paste('Date: ', as.character(Date), '\n'))
        ) +
        geom_bar(stat = "count", position = "dodge") +
        theme_classic() +
        labs(title="Fish Movement by Day",
             x ="Date", y = "Count") +
        scale_fill_manual(values = c("Downstream Movement" = "red",
                                     "Upstream Movement" = "chartreuse3",
                                     "No Movement" = "black",
                                     "Initial Release" = "darkorange",
                                     "Changed Rivers" = "purple"))
      
      
      plotly1 <- ggplotly(p = plot)
      plotly1
    })    
    
    
    
    
    #seasonally
    seasonal_movts <- reactive({filtered_movements_data() %>%
      group_by(month(Date), day(Date), movement_only) %>%
      summarise(total_events = n())
    })
    
    output$plot6 <- renderPlotly({

      plot <- seasonal_movts() %>%
        mutate(merged = (parse_date_time(paste(`month(Date)`, `day(Date)`), "md"))) %>%
        ggplot(aes(x = merged, y = total_events, fill = movement_only)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_classic() +
        labs(title = "Seasonal Daily Movements", x = "Day", y = "Counts",
             caption = "Currently No download option for this data.") +
        scale_x_datetime(date_labels = "%b") +
        scale_fill_manual(values = c("Downstream Movement" = "red",
                                     "Upstream Movement" = "chartreuse3",
                                     "No Movement" = "black",
                                     "Initial Release" = "darkorange",
                                     "Changed Rivers" = "purple"))
      
      ggplotly(plot)
    })  
    
    # Total movements
    output$plot7 <- renderPlotly({
      plot <- filtered_movements_data() %>%
        filter(
          !dist_moved %in% c(0)) %>%
        ggplot(aes(x = dist_moved)) +
        geom_histogram(binwidth = 50) +
        theme_classic() +
        labs(title = "Each movement detected: ('No movements' excluded)", subtitle = "Groupings are 50 m")
      ggplotly(plot)
      
      plotly1 <- ggplotly(p = plot)
      plotly1
    })  
    
    #cumulative movement
    output$plot8 <- renderPlotly({
      plot <- filtered_movements_data() %>%
        ggplot(aes(x = sum_dist)) +
        geom_histogram(binwidth = 300) +
        theme_classic() +
        labs(title = "Cumulative movement", subtitle = "Groupings are 300 m")
      plotly1 <- ggplotly(p = plot)
      plotly1
      
    })  
    
    output$plot9 <- renderPlotly({
      plot <- filtered_movements_data() %>%
        ggplot(aes(x = hour(Datetime), fill = movement_only)) +
        geom_histogram(binwidth = 1) +
        theme_classic() +
        labs(title = "Detections by Hour") 
      plotly1 <- ggplotly(p = plot)
      plotly1
      
    })  
    
    output$text2 <- renderPrint({
      "'Fish Movement by Day' plot renders table data shown in the 'map and table' tab. 
      'Seasonal Daily Movements' graph data can be downloaded below."
    })

 
    

# MarkerTag Plot Output ---------------------------------------------------

    output$plot2 <- renderPlotly({
      plot2 <- filtered_markertag_data() %>%
        ggplot(aes(x = DTY, y = CleanARR, color = SCD, text = paste(TAG) )) +
        geom_point() +
        theme_classic() +
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank())
      
      
      plotly2 <- ggplotly(p = plot2)
      plotly2
    })    
    

# Release and Recap Data L/W Plot Output --------------------------------------------
    output$plot3 <- renderPlotly({
      plot3 <- Release %>%
        ggplot(aes(x = Length, y = Weight, color = Species)) +
        geom_point() + 
        theme_classic() +
        labs(title = "Release Data")
      
      plotly3 <- ggplotly(plot3) 
      plotly3
    })    
    
    output$plot4 <- renderPlotly({
      plot4 <- Recaptures %>%
        ggplot(aes(x = Length, y = Weight, color = Species)) +
        geom_point() + 
        theme_classic() +
        labs(title = "Recapture Data")
      
      plotly4 <- ggplotly(plot4) 
      plotly4
    })    
    
# Download Handlers -------------------------------------------------------

    
    output$download1 <- downloadHandler(
      filename = 
        function() {
          paste0("ReleaseEncounters_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(enc_hist_wide_filtered(), file)
        
        
      }
    ) #end of download1
    
    output$download2 <- downloadHandler(
      filename = 
        function() {
          paste0("allevents_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(all_events_data(), file)
        
        
      }
    ) #end of download2
    
    output$download3 <- downloadHandler(
      filename = 
        function() {
          paste0("StationaryClean_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(indiv_datasets_list()$stationarycleandata, file)
        
        
      }
    ) #end of download3
    
    output$download4 <- downloadHandler(
      filename = 
        function() {
          paste0("DailyStates_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(filtered_states_data()$filtered_states, file)
        
        
      }
    ) #end of download4
    
    output$download5 <- downloadHandler(
      filename = 
        function() {
          paste0("DatilyStates_Wide_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(filtered_states_data()$filtered_wide, file)
        
        
      }
    ) #end of download5
    
    output$download6 <- downloadHandler(
      filename = 
        function() {
          paste0("AllMovements_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(filtered_movements_data(), file)
        
        
      }
    ) #end of download6
    
    output$download7 <- downloadHandler(
      filename = 
        function() {
          paste0("MarkerTagsOnly_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(filtered_markertag_data(), file)
        
        
      }
    ) #end of download7
    
    output$download8 <- downloadHandler(
      filename = 
        function() {
          paste0("SeasonalDailyMovements_",most_recent_date,".csv")
        },
      content = function(file) {
        write_csv(seasonal_movts(), file)
      }
    ) #end of download8
}

# Run the application 
shinyApp(ui = ui, server = server)
