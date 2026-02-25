# A simple module that captializes input text
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

options(dplyr.summarise.inform = FALSE)
###


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




enableBookmarking(store = "server")
# Main app code
ui <- function(request) {
  fluidPage(
    tabsetPanel(id = "tabs", 
                tabPanel("tab 1", 
                         capitalizerUI("tc")), 
                tabPanel("tab2", 
                         capitalizerUI("tl")),
                tabPanel("encUI",
                         EncounterHistoriesSummariesWide_UI("EncounterHistoriesSummariesWideTab1", Enc_release_data)
                         )
                #capitalizerUI("tc")
                ),
    bookmarkButton()
  )
}
server <- function(input, output, session) {
  observe({
    capitalizerServer("tc")
   capitalizerServer("tl")
   EncounterHistoriesSummariesWide_Server("EncounterHistoriesSummariesWideTab1", Enc_release_data)
   
  })
  
}
shinyApp(ui, server)
