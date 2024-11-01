avianPredationMod_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(3), 
             column(9, 
                    tabsetPanel(
                      tabPanel("Sequences"),
                      tabPanel("Encounter Summaries Wide"), 
                      tabPanel("States"), 
                      
                    )
                    )
             )
    
  
  )
}

avianPredationMod_Server <- function(id, avianPredationList) {
  moduleServer(
    id,
    function(input, output, session) {
      #withSpinner(DT::dataTableOutput(ns("ghostTags1")))
    }
  )
}