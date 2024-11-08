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
      
      # checkedTags <- avianPredationList$checkedTags$TAG
      # checkedTags <- paste("['", paste(checkedTags, collapse = "','"), "']", sep = "")
      # tag_opinion_map <- setNames(avianPredationList$checkedTags$`SG Opinion`, avianPredationList$checkedTags$TAG)
      # checkedTags <- paste0("{", 
      #                         paste(
      #                           sapply(names(tag_opinion_map), function(tag) {
      #                             paste0("'", tag, "': '", tag_opinion_map[[tag]], "'")
      #                           }), 
      #                           collapse = ", "), 
      #                         "}"
      #                       )
      # checkedTags <- avianPredationList$checkedTags
                           
     # output$table1 <- renderDT({
     #   
     #   # Create a named list of tags and opinions from datatable2
     #   
     #   
     #   # Create a JavaScript object to pass the opinion data
     #   
     #   
     #   # Create a datatable and apply conditional row highlighting
     #   datatable(datatable1, options = list(
     #     rowCallback = JS(
     #       "function(row, data) {",
     #       "  var tag = data[0];",  # 'TAG' is the first column (index 0)",
     #       "  var opinions = ", tag_opinion_js, ";",  # Insert the tag-opinion mapping from R to JS
     #       "  var opinion = opinions[tag];",  # Get the opinion for the current tag from table 2",
     #       "  if (opinion === 'No' || opinion === 'no') {",
     #       "    $('td', row).css('background-color', 'green');",  
     #       "  } else if (opinion === 'Yes' || opinion === 'yes') {",
     #       "    $('td', row).css('background-color', 'red');",  
     #       "  } else if (opinion !== undefined) {",
     #       "    $('td', row).css('background-color', 'yellow');",  
     #         "  }",
     #       "}",
     #       sep = "\n"
     #     )
     #   ))
     #   
     # })
      
      renderDTFunction(output, "tagsFrequencyTable", avianPredationList$tagCountsNoPredation, 
                       c("The amount of times a tag has shown up in the selected potential avian predation DFs to the right."))
      
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

    }
  )
}