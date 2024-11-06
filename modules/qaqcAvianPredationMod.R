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
                            ), 
                      tabPanel("Movements", 
                               fluidRow(
                                 column(6, 
                                        box(
                                          title = "Large Movements",
                                          width = 12,
                                          withSpinner(DT::dataTableOutput(ns("largeMovements")))
                                        )
                                 ), 
                                 column(6, 
                                        box(
                                          title = "Fast Movements",
                                          width = 12,
                                          withSpinner(DT::dataTableOutput(ns("fastMovements"))) 
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
      
      renderDTFunction(output, "tagsFrequencyTable", avianPredationList$tagCountsNoPredation, 
                       c("The amount of times a tag has shown up in the selected potential avian predation DFs to the right."))
      
      renderDTFunction(output, "downstreamSequences", avianPredationList$movingDownstream, 
                       c("Tags that first hit either CF, GD1, or RR1, then hit either HP, RB, WG1, or WG2 without any antennas in between.
           Sorting by the time between detections can potentially reveal predated tags."))
      # output$downstreamSequences <- renderDT({
      #   
      #   datatable(
      #     avianPredationList$movingDownstream,
      #     rownames = FALSE,
      #     selection = "single",
      #     filter = 'top',
      #     caption = "Tags that first hit either CF, GD1, or RR1, then hit either HP, RB, WG1, or WG2 without any antennas in between.
      #      Sorting by the time between detections can potentially reveal predated tags.", 
      #     options = list(
      #       autowidth = TRUE,
      #       scrollX = TRUE, 
      #       #statesave is restore table state on page reload
      #       stateSave = TRUE,
      #       pageLength = 10,
      #       info = TRUE,
      #       lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      #       dom = 'lrtip',
      #       #had to add 'lowercase L' letter to display the page length again
      #       language = list(emptyTable = "Enter inputs and press Render Table")
      #     )
      #   )
      # })
      
      renderDTFunction(output, "upstreamSequences", avianPredationList$movingUpstream, 
                       c("Tags that first hit either HP, RB, WG1, or WG2, then hit either CF, GD1, or RR1 without any antennas in between.
          Sorting by the time between detections can potentially reveal predated tags."))
      # output$upstreamSequences <- renderDT({
      #   
      #   datatable(
      #     avianPredationList$movingUpstream,
      #     rownames = FALSE,
      #     selection = "single",
      #     filter = 'top',
      #     caption = "Tags that first hit either HP, RB, WG1, or WG2, then hit either CF, GD1, or RR1 without any antennas in between.
      #     Sorting by the time between detections can potentially reveal predated tags.", 
      #     options = list(
      #       autowidth = TRUE,
      #       scrollX = TRUE, 
      #       #statesave is restore table state on page reload
      #       stateSave = TRUE,
      #       pageLength = 10,
      #       info = TRUE,
      #       lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      #       dom = 'lrtip',
      #       #had to add 'lowercase L' letter to display the page length again
      #       language = list(emptyTable = "Enter inputs and press Render Table")
      #     )
      #   )
      # })
      
      renderDTFunction(output, "largeMovementsWithoutChannel", avianPredationList$largeMovementsWithoutChannel, 
                       c("Fish that have traveled >1000m. Sorting by channel usage can show tags that may have a unrealistic encounter history."))
      
      # output$largeMovementsWithoutChannel <- renderDT({
      #   
      #   datatable(
      #     avianPredationList$largeMovementsWithoutChannel,
      #     rownames = FALSE,
      #     selection = "single",
      #     filter = 'top',
      #     caption = "Fish that have traveled >1000m. Sorting by channel usage can show tags that may have a unrealistic encounter history.",
      #     options = list(
      #       #statesave is restore table state on page reload
      #       stateSave = TRUE,
      #       pageLength = 10,
      #       info = TRUE,
      #       lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      #       dom = 'lrtip',
      #       #had to add 'lowercase L' letter to display the page length again
      #       language = list(emptyTable = "Enter inputs and press Render Table")
      #     )
      #   )
      # })
      
      renderDTFunction(output, "statesWeeklyActiveFish", avianPredationList$statesWeeklyActiveFish, 
                       c("Tags with > 2 state changes in a week or > 4 weekly unique events. Very active fish on a weekly basis."))
      
      # output$statesWeeklyActiveFish <- renderDT({
      #   
      #   datatable(
      #     avianPredationList$statesWeeklyActiveFish,
      #     rownames = FALSE,
      #     selection = "single",
      #     filter = 'top',
      #     caption = "Tags with > 2 state changes in a week or > 4 weekly unique events. Very active fish on a weekly basis.",
      #     options = list(
      #       scrollX = TRUE, 
      #       #statesave is restore table state on page reload
      #       stateSave = TRUE,
      #       pageLength = 10,
      #       info = TRUE,
      #       lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      #       dom = 'lrtip',
      #       #had to add 'lowercase L' letter to display the page length again
      #       language = list(emptyTable = "Enter inputs and press Render Table")
      #     )
      #   )
      # })
      
      renderDTFunction(output, "statesAllActiveFish", avianPredationList$statesAllActiveFish, 
                       c("Tags >2 total state changes across its entire encounter history. Very active fish on an overall basis."))
      
      # output$statesAllActiveFish <- renderDT({
      #   
      #   datatable(
      #     avianPredationList$statesAllActiveFish,
      #     rownames = FALSE,
      #     selection = "single",
      #     filter = 'top',
      #     caption = "Tags >2 total state changes across its entire encounter history. Very active fish on an overall basis.",
      #     options = list(
      #       scrollX = TRUE, 
      #       #statesave is restore table state on page reload
      #       stateSave = TRUE,
      #       pageLength = 10,
      #       info = TRUE,
      #       lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      #       dom = 'lrtip',
      #       #had to add 'lowercase L' letter to display the page length again
      #       language = list(emptyTable = "Enter inputs and press Render Table")
      #     )
      #   )
      # })
      
      
      renderDTFunction(output, "largeMovements", avianPredationList$longMovements, 
                       c("Tags that have moved > 3700m (fyi HP to CF is 3760m) on one movement."))
      # output$largeMovements <- renderDT({
      #   
      #   datatable(
      #     avianPredationList$longMovements,
      #     rownames = FALSE,
      #     selection = "single",
      #     filter = 'top',
      #     caption = "Tags that have moved > 3700m (fyi HP to CF is 3760m) on one movement.",
      #     options = list(
      #       scrollX = TRUE, 
      #       #statesave is restore table state on page reload
      #       stateSave = TRUE,
      #       pageLength = 10,
      #       info = TRUE,
      #       lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      #       dom = 'lrtip',
      #       #had to add 'lowercase L' letter to display the page length again
      #       language = list(emptyTable = "Enter inputs and press Render Table")
      #     )
      #   )
      # })
      
      
      renderDTFunction(output, "fastMovements", avianPredationList$fastMovements, 
                       c("Top 5% of individual movements by fish by speed (meters per second between detections) upstream or downstream."))
      # output$fastMovements <- renderDT({
      #   
      #   datatable(
      #     avianPredationList$fastMovements,
      #     rownames = FALSE,
      #     selection = "single",
      #     filter = 'top',
      #     caption = "Top 5% of individual movements by fish by speed (meters per second between detections) upstream or downstream.",
      #     options = list(
      #       scrollX = TRUE, 
      #       #statesave is restore table state on page reload
      #       stateSave = TRUE,
      #       pageLength = 10,
      #       info = TRUE,
      #       lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      #       dom = 'lrtip',
      #       #had to add 'lowercase L' letter to display the page length again
      #       language = list(emptyTable = "Enter inputs and press Render Table")
      #     )
      #   )
      # })
    }
  )
}