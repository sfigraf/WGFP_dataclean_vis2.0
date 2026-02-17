library(shiny)
library(shinycssloaders)
library(tidyverse) 
library(lubridate)
library(leaflet)
library(sf)
library(plotly) #for turning maps to plotly
library(DT)
library(shinyWidgets) # for pickerinput
library(shinythemes)
library(shinydashboard)
library(bslib)
library(keys) #for alloiwng enter keypress to render data on certain pages
#animation stuff
#library(mapedit)
library(gganimate)
library(basemaps)
#minicharts
library(leaflet.minicharts)
library(shinyjs)
library(shinyjqui)
library(shinymanager) #for password authentcation
library(odbc) #for making and storing passwords for logiuns
library(RSQLite)
#library(mapview)

# cntrl + shft + A to reformat chunks of code
# rsconnect::showLogs(appName="WGFP_dataclean_vis",streaming=TRUE) will show logs when trying to load app browser
# had "application failed to start" error and fixed both times with above command. both times because packages in local environment (tidyverse and lubridate) weren't called with library() command 

#suppresses grouping messages ie `summarise()` has grouped output by 'Datetime'. You can override using the `.groups` argument.
options(dplyr.summarise.inform = FALSE)
###
# Create local directory for database if it doesn't exist
local_db_dir <- file.path(getwd(), "app_data")
if (!dir.exists(local_db_dir)) {
  dir.create(local_db_dir)
  cat("📁 Created local data directory:", local_db_dir, "\n")
}

# Initialize SQLite database
db_path <- file.path(local_db_dir, "tracker_app.db")
init_database <- function() {
  con <- dbConnect(SQLite(), db_path)
  
  # Create tables
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_projects (
      username TEXT NOT NULL,
      project_name TEXT NOT NULL,
      added_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (username, project_name)
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS change_requests (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT NOT NULL,
      project_name TEXT NOT NULL,
      change_type TEXT NOT NULL,
      change_description TEXT,
      submitted_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      status TEXT DEFAULT 'pending',
      reviewed_by TEXT,
      reviewed_date TIMESTAMP,
      review_notes TEXT
    )
  ")
  ###USER CREDNETIALS DB
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_credentials (
      username TEXT PRIMARY KEY,
      password TEXT NOT NULL,
      is_admin INTEGER DEFAULT 0,
      last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create mock data table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS animal_events_normalized (
      tracker_animal_id TEXT,
      project_name TEXT,
      animal_id_local TEXT,
      serial_number INTEGER,
      capture_date DATE,
      mark_vendor TEXT,
      mark_type TEXT,
      sex TEXT,
      age_class TEXT,
      animal_still_monitored BOOLEAN,
      mort_date DATE,
      censor_date DATE,
      comments TEXT,
      upsert_timestamp TIMESTAMP,
      capt_last_updated_timestamp TIMESTAMP,
      mort_last_updated_timestamp TIMESTAMP,
      censor_last_updated_timestamp TIMESTAMP
    )
  ")
  
  # Check if mock data already exists
  # existing_data <- dbGetQuery(con, "SELECT COUNT(*) as count FROM animal_events_normalized")
  # 
  # if (existing_data$count == 0) {
  #   # Insert mock data
  #   mock_data <- create_mock_data()
  #   dbWriteTable(con, "animal_events_normalized", mock_data, append = TRUE)
  #   cat("📊 Created mock data with", nrow(mock_data), "records\n")
  # }
  
  dbDisconnect(con)
}
init_credentials <- function() {
  con <- dbConnect(SQLite(), db_path)
  # Check if credentials exist
  existing <- dbGetQuery(con, "SELECT username FROM user_credentials")
  if (nrow(existing) == 0) {
    # Insert default credentials
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('admin', 'admin123', 1)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('demo_bio', 'bio123', 0)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('test_user', 'test123', 0)")
    
    # real users
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('graffs', 'test123', 1)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('ericher', 'traviskelce', 0)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('efetherman', 'berdnerd', 0)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('mkondratieff', 'mergmaster', 0)")
    
    cat("✅ Initialized default credentials:\n")
    cat("   Admin user: admin / admin123\n")
    cat("   Demo user: demo_bio / bio123\n")
    cat("   Test user: test_user / test123\n")
  }
  dbDisconnect(con)
}

# Initialize database
init_database()
init_credentials()


## Credentials - load from database
load_credentials <- function() {
  con <- dbConnect(SQLite(), db_path)
  creds <- dbGetQuery(con, "SELECT username as user, password, is_admin FROM user_credentials")
  dbDisconnect(con)
  return(creds)
}

# Data Read Ins -----------------------------------------------------------
 
start_time <- Sys.time()
print("Reading in Static Files for app.....")
if(!exists("combinedData_df_list")){
  combinedData_df_list <- readRDS("data/flatFilesforApp/combinedData_df_list.rds")
}
if(!exists("indiv_datasets_list")){
  indiv_datasets_list <- readRDS("data/flatFilesforApp/indiv_datasets_list.rds")
}
if(!exists("Enc_release_data")){
  Enc_release_data <- readRDS("data/flatFilesforApp/Enc_release_data.rds")
}
if(!exists("encounterMARKStates")){
  encounterMARKStates <- readRDS("data/flatFilesforApp/encounterMARKStates.rds")
}
if(!exists("movements_list")){
  movements_list <- readRDS("data/flatFilesforApp/movements_list.rds")
}
if(!exists("unknown_tags")){
  unknown_tags <- readRDS("data/flatFilesforApp/unknown_tags.rds")
}

if(!exists("avianPredationList")){
  avianPredationList <- readRDS("data/flatFilesforApp/possibleAvianPredationDFs.rds")
}

if(!exists("wgfpMetadata")){
  wgfpMetadata <- readRDS("data/flatFilesforApp/wgfpMetadata.rds")
}

if(!exists("metaDataVariableNames")){
  metaDataVariableNames <- readRDS("data/flatFilesforApp/metaDataVariableNames.rds")
}

if(!exists("PTData")){
  PTData <- readRDS("data/flatFilesforApp/PTData.rds")
}

if(!exists("USGSData")){
  USGSData <- readRDS("data/flatFilesforApp/USGSData.rds")
}

if(!exists("SiteVisitData")){
  SiteVisitData <- readRDS("data/flatFilesforApp/SiteVisitData.rds")
}


# # Functions Read-in -------------------------------------------------------
 
# #functions
neededFunctions <- c("Animation_function.R", "calculateCrosstalkProportion.R", "getSequences.R", "renderDTFunction.R", 
                     "Wrangleminicharts_function.R", "updatePassword.R")

for (i in neededFunctions) {
  source(paste0("./functions/",i))
}
# 
for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}
# 
for (i in list.files("./miscR/")) {
  if (grepl(".R", i)) {
    source(paste0("./miscR/",i))
  }
}

end_time <- Sys.time()
print(paste("Static File Read-in took", round((end_time-start_time), 2)))


#colors
#rainbow trout color pallete used to assign to Sites:
# Confluence, Connectivity Downstream, Connectivity Side Channel, Connectivity Upstream, Hitching Post, 
# Kaibab Park, Red Barn, Windy Gap Auxiliary, Windy Gap Bypass Channel"
rainbow_trout_colors <- c("#8B8000", "#008080", "#FF69B4", "#FF4500", "#6A5ACD","#32CD32", "#20B2AA",
                          "#FF1C55", "#4682B4", "#556B2F", "#DC143C", "#B22222", "#7FFF00", "#8A2BE2", "#00CED1")
#currently "Changed Rivers", "Downstream Movement" "Initial Release", "No Movement", "Upstream Movement" (5/17/24)
#note: these are a little diffeerent than what we see in the map because the map/marker color optoins are very limited. 
movementColorsValues <- c("purple", "#eb0e2b", "#FF8C00", "gray", "#22bd74") #, "#66FF00"
# currently "BRK" "LOC" "MERG" "MTS" "RBT" "RXN" "TGM" (5/17/24)
speciesColors <- c("#FFD700", "#654321", "#4F7942", "#FF7F50", "#1E90FF", "#008080", "#DAA520", "#D2691E", "#9A5ECD") 

#maybe a better method for this, but pressure transducer site names are changed to the same names as Site Visit site names
#and that variable is what we use to assign colors to sites   
allSites <- metaDataVariableNames$allDetectionDistanceSiteNames
movementColors <- setNames(movementColorsValues, sort(unique(movements_list$Movements_df$movement_only)))
siteColors <- setNames(rainbow_trout_colors[0:length(allSites)], allSites)
speciesColors <- setNames(speciesColors[0:length(unique(indiv_datasets_list$releasedata$Species))], sort(unique(indiv_datasets_list$releasedata$Species)))
 
allColors <- c(movementColors, siteColors, speciesColors)

# Define UI for application that draws a histogram

ui <- fluidPage(

  navbarPage(title = "WGFP Data Exploration",
             id = "tabs", 
             theme = shinytheme("sandstone"), #end of navbar page arguments; what follow is all inside it
             
             tabPanel("About/How to Use",
                      includeHTML(paste0("www/", "WGFP_dataclean_vis_about.html"))
                      ), #end fo how to use TabPanel

# Individual Datasets UI ---------------------------------------------------

             
             tabPanel("Individual Datasets",
                      value = "IndividualDatasetsTab",
                      IndividualDatasets_UI("IndividualDatasetsTab1", combinedData_df_list, indiv_datasets_list$releasedata)
                    ),#end of Individual data tab panel
             
# Encounter Histories UI --------------------------------------------------

             tabPanel("Encounter Histories",
                      value = "EncounterHistories",
                      tabsetPanel(
                        tabPanel("Encounter Histories Summaries Wide",
                                 EncounterHistoriesSummariesWide_UI("EncounterHistoriesSummariesWideTab1", Enc_release_data)),
                        
                        tabPanel("All Encounter Histories",
                                 AllEncounters_UI("AllEncountersTab1", combinedData_df_list)), 
                        tabPanel("Sequences",
                                 value = "SequencesTab",
                                 Sequences_UI("SequencesTab1", metaDataVariableNames$AntennaSiteShortHandCodes)
                        )
                      )
                      ), #end of Encounter Histories Tab


# States UI -----------------------------------------------------

            tabPanel("MARK States",
                     value = "StatesTab",
                     States_UI("StatesTab1")
                    ),#end of States ui Tab

# Movements and Map UI Tab --------------------------------------------------------------

### note on map: if detectoins are close together, they'll be grouped and you can't do more resolution. But they can still be upstream/downstream movements if they're >= 10 m difference in station
#the filtering also automatically takes out NA values on movement with picker6; but all the NA movement onlys should be from fish where we have no release info for,
#and also from fish that have detections before their official "release" back in May
#if marker_color or icon_color is NA, it wont get mapped or displayed in data
#picker wasn't working becuase I had 2 different pickers named the same
            tabPanel("Daily Movements Map, Plot, and Data",
                     value = "MovementsTab",
                     movements_UI("MovementsTab1", movements_list$Movements_df)
            ),


# PT Data -----------------------------------------------------------------

          tabPanel("Pressure Transducer, USGS, and Detection Distance",
                   value = "PTtab",
                   PT_UI("PTtab1", PTData, movements_list$Movements_df, SiteVisitData$WGFP_SiteVisits_FieldData)
          ),
            

# QAQC UI tab -------------------------------------------------------------

          tabPanel("QAQC",
                   value = "QAQCTab",
                   QAQC_UI("QAQCTab1", combinedData_df_list)
                   ),  
          tabPanel("Account",
                   value = "accountTab",
                   accountManagement_UI("accountManagementTab")
          )

) #end of navbar page
) #end of fluidpage

# Wrap with authentication
ui <- secure_app(ui, choose_language = FALSE)


# Define server logic
# Warning: Error in validate_session_object: object 'session' not found solved by adding session to the part up here
server <- function(input, output, session) {
  
  # Authentication - load credentials dynamically
  res_auth <- secure_server(
    check_credentials = check_credentials(load_credentials())
  )
  
  # Get current user
  current_user <- reactive({
    if (!is.null(res_auth$user)) {
      return(res_auth$user)
    }
    return(NULL)
  })
  
  observe({
    #need user to continue with rest of the app
    user <- current_user()
    req(user)
    
      movements_Server("MovementsTab1", movements_list$Movements_df, allColors)
    
      IndividualDatasets_Server("IndividualDatasetsTab1", indiv_datasets_list, allColors)
    
      EncounterHistoriesSummariesWide_Server("EncounterHistoriesSummariesWideTab1", Enc_release_data)
      
      Sequences_Server("SequencesTab1", combinedData_df_list$All_Events, metaDataVariableNames$AntennaSiteShortHandCodes, metaDataVariableNames$MobileRunFrontendCodes, AvianPredation = indiv_datasets_list$avian_preddata)
      
      AllEncounters_Server("AllEncountersTab1", combinedData_df_list)
    
      States_Server("StatesTab1", encounterMARKStates$MARKEncounterHistories)
      
      PT_Server("PTtab1", PTData, movements_list$Movements_df, USGSData, SiteVisitData$WGFP_SiteVisits_FieldData, allColors)
   
      QAQC_Server("QAQCTab1", indiv_datasets_list$releasedata, indiv_datasets_list$recapdata, 
                  unknown_tags, movements_list$ghostTagsWithMovementAfterGhostDate, avianPredationList,
                  combinedData_df_list, wgfpMetadata, metaDataVariableNames, SiteVisitData$SiteVisitAndPTData, allColors)
      
      accountManagement_Server("accountManagementTab", user = user, db_path = db_path)
    
  })
  
}

shinyApp(ui = ui, server = server)
