avianPredationMod_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(3, 
                    withSpinner(DT::dataTableOutput(ns("tagsFrequencyTable")))
                    ), 
             column(9, 
                    tabsetPanel(
                      tabPanel("Sequences", 
                               fluidRow(
                                 column(6, 
                                        withSpinner(DT::dataTableOutput(ns("downstreamSequences")))
                                        ), 
                                 column(6, 
                                        withSpinner(DT::dataTableOutput(ns("upstreamSequences"))) 
                               )
                               )
                               ),
                      tabPanel("Encounter Summaries Wide", 
                               withSpinner(DT::dataTableOutput(ns("largeMovementsWithoutChannel")))
                               ), 
                      tabPanel("States", 
                               fluidRow(
                                 column(6, 
                                        withSpinner(DT::dataTableOutput(ns("statesWeeklyActiveFish")))
                                 ), 
                                 column(6, 
                                        withSpinner(DT::dataTableOutput(ns("statesAllActiveFish"))) 
                                 )
                               )
                            )
                    )
                    )
             )
    
  
  )
}

avianPredationMod_Server <- function(id, avianPredationList) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$tagsFrequencyTable <- renderDT({
        
        datatable(
          avianPredationList$tagCountsNoPredation,
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          caption = "Date filter does not apply to this table",
          options = list(
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
            dom = 'lrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
      })
      
      output$downstreamSequences <- renderDT({
        
        datatable(
          avianPredationList$movingDownstream,
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          caption = "Date filter does not apply to this table",
          options = list(
            autowidth = TRUE,
            scrollX = TRUE, 
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
            dom = 'lrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
      })
      
      output$upstreamSequences <- renderDT({
        
        datatable(
          avianPredationList$movingUpstream,
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          caption = "Date filter does not apply to this table",
          options = list(
            autowidth = TRUE,
            scrollX = TRUE, 
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
            dom = 'lrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
      })
      
      output$largeMovementsWithoutChannel <- renderDT({
        
        datatable(
          avianPredationList$largeMovementsWithoutChannel,
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          caption = "Date filter does not apply to this table",
          options = list(
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
            dom = 'lrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
      })
      
      output$statesWeeklyActiveFish <- renderDT({
        
        datatable(
          avianPredationList$statesWeeklyActiveFish,
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          caption = "Date filter does not apply to this table",
          options = list(
            scrollX = TRUE, 
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
            dom = 'lrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
      })
      
      output$statesAllActiveFish <- renderDT({
        
        datatable(
          avianPredationList$statesAllActiveFish,
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          caption = "Date filter does not apply to this table",
          options = list(
            scrollX = TRUE, 
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
            dom = 'lrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
      })
    }
  )
}