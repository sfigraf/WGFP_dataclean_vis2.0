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
                      ), 
                      tabPanel("QAQC Predated Tag Movements", 
                               fluidRow(
                                 column(12, 
                                        box(
                                          title = "Predated Tag Movements",
                                          width = 12,
                                          withSpinner(DT::dataTableOutput(ns("predTagMovements")))
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
                       c("The amount of times a tag has shown up in the selected potential avian predation DFs to the right. 
                         Shading: Green are tags previously checked and deemed not predated, yellows are maybes, red are predated tags that just haven't been 
                         added to the master list yet. No color are tags that haven't been checked. After checking a tag, record findings in 'Potential Avian predated Tags.csv'"))
      
      renderDTFunction(output, "downstreamSequences", avianPredationList$movingDownstream, 
                       c("Tags that first hit either CF, GD1, or RR1, then hit either HP, RB, WG1, or WG2 without any antennas in between.
           Sorting by the time between detections can potentially reveal predated tags."))

      
      renderDTFunction(output, "upstreamSequences", avianPredationList$movingUpstream, 
                       c("Tags that first hit either HP, RB, WG1, or WG2, then hit either CF, GD1, or RR1 without any antennas in between.
          Sorting by the time between detections can potentially reveal predated tags."))

      
      renderDTFunction(output, "largeMovementsWithoutChannel", avianPredationList$largeMovementsWithoutChannel, 
                       c("Fish that have traveled >1000m. Sorting by channel usage can show tags that may have a unrealistic encounter history."))
      
      
      renderDTFunction(output, "statesWeeklyActiveFish", avianPredationList$statesWeeklyActiveFish, 
                       c("Tags with > 2 state changes in a week or > 4 weekly unique events. Very active fish on a weekly basis."))

      
      renderDTFunction(output, "statesAllActiveFish", avianPredationList$statesAllActiveFish, 
                       c("Tags >2 total state changes across its entire encounter history. Very active fish on an overall basis."))
      
      
      renderDTFunction(output, "largeMovements", avianPredationList$longMovements, 
                       c("Tags that have moved > 3700m (fyi HP to CF is 3760m) on one movement."))

      
      renderDTFunction(output, "fastMovements", avianPredationList$fastMovements, 
                       c("Top 5% of individual movements by fish by speed (meters per second between detections) upstream or downstream."))
      
      renderDTFunction(output, "predTagMovements", avianPredationList$avianPredatedTagsMovementsMonthAfterPD, 
                       c("Movements of tags in predation list that have detections >1 month after predation date. 
                         Shaded green tags are already listed in ghost tag file."))

    }
  )
}