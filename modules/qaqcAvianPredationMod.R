avianPredationMod_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(3, 
                    box(
                      title = "Frequency Table",
                      width = 12,
                    withSpinner(DT::dataTableOutput(ns("tagsFrequencyTable")))
                    )
                    ), 
             column(9, 
                    tabsetPanel(
                      tabPanel("Sequences", 
                               fluidRow(
                                 column(6, 
                                        box(
                                          title = "Downstream Sequences",
                                          width = 12,
                                        withSpinner(DT::dataTableOutput(ns("downstreamSequences")))
                                        )
                                        ), 
                                 column(6, 
                                        box(
                                          title = "Upstream Sequences",
                                          width = 12,
                                        withSpinner(DT::dataTableOutput(ns("upstreamSequences"))) 
                                        )
                               )
                               )
                               ),
                      tabPanel("Encounter Summaries Wide", 
                               box(
                                 title = "High Cumulative Movements",
                                 width = 12,
                               withSpinner(DT::dataTableOutput(ns("largeMovementsWithoutChannel")))
                               )
                               ), 
                      tabPanel("States", 
                               fluidRow(
                                 column(6, 
                                        box(
                                          title = "High Weekly Activity",
                                          width = 12,
                                        withSpinner(DT::dataTableOutput(ns("statesWeeklyActiveFish")))
                                        )
                                 ), 
                                 column(6, 
                                        box(
                                          title = "High Cumulative Activity",
                                          width = 12,
                                        withSpinner(DT::dataTableOutput(ns("statesAllActiveFish"))) 
                                        )
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
          caption = "The amount of times a tag has shown up in the selected potential avian predation DFs to the right.",
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
          caption = "Tags that first hit either CF, GD1, or RR1, then hit either HP, RB, WG1, or WG2 without any antennas in between.
           Sorting by the time between detections can potentially reveal predated tags.", 
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
          caption = "Tags that first hit either HP, RB, WG1, or WG2, then hit either CF, GD1, or RR1 without any antennas in between.
          Sorting by the time between detections can potentially reveal predated tags.", 
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
          caption = "Fish that have traveled >1000m. Sorting by channel usage can show tags that may have a unrealistic encounter history.",
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
          caption = "Tags with > 2 state changes in a week or > 4 weekly unique events. Very active fish on a weekly basis.",
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
          caption = "Tags >2 total state changes across its entire encounter history. Very active fish on an overall basis.",
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