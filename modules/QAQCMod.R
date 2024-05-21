QAQC_UI <- function(id, Marker_Tag_data, combinedData_df_list) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Marker Tags",
               tabsetPanel(
                 tabPanel("Stationary", 
                          MarkerTagQAQC_UI(ns("StationaryMarkerTags"), Marker_Tag_data)
               ), 
               tabPanel("Biomark",
                        MarkerTagQAQC_UI(ns("BiomarkMarkerTags"), Marker_Tag_data)
                        ), 
               tabPanel("Downtime Periods",
                        br(),
                        dataTableOutput(ns("markerTagDowntimeTable"))
               )
              )
               
      ), #end of marker tag tab
      tabPanel("Release and Recap Length/Weights",
               fluidRow(
                 column(
                   width = 8,
                   br(),
                   withSpinner(plotlyOutput(ns("plot3")))
                 ),#end of column
                 column(
                   br(),
                   width = 4,
                   withSpinner(plotlyOutput(ns("plot4")))
                   
                 )#end of column
               )#end of fluidrow
      ), #end of tabPanel
      tabPanel("Unknown Tags",
               br(),
               withSpinner(DT::dataTableOutput(ns("unknowntags1")))
      ),
      tabPanel("Ghost Tag Movements",
               br(),
               downloadData_UI(ns("downloadghostTags1")),
               withSpinner(DT::dataTableOutput(ns("ghostTags1")))
      ), 
      tabPanel("Crosstalk QAQC",
               qaqcCrosstalkMod_UI(ns("qaqcCrosstalk"), combinedData_df_list)
      ), 
      tabPanel("Detection Distance/Water Level",
               br(),
               withSpinner(DT::dataTableOutput(ns("DDWaterLevelTable")))
      )
    )#end of tabset Panel 
  )
}

QAQC_Server <- function(id, Marker_Tag_data, Release_05, Recaptures_05, unknown_tags, ghostTagsWithMovementAfterGhostDate, 
                        combinedData_df_list, wgfpMetadata, metaDataVariableNames, WGFP_SiteVisits_FieldDatawithPTData, allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      MarkerTagQAQC_Server("StationaryMarkerTags", Marker_Tag_data)
      MarkerTagQAQC_Server("BiomarkMarkerTags", Marker_Tag_data)
      
      output$markerTagDowntimeTable <- renderDT({
        
        datatable(wgfpMetadata$MarkerTagIssues,
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  caption = ("Manually discerned periods of time where the specified marker tag was not recording during consistent intervals"),
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        ) 
      })
      
      
      # Release and Recap Data L/W Plot Output --------------------------------------------
      output$plot3 <- renderPlotly({
        Release_05 %>%
          ggplot(aes(x = Length, y = Weight, color = Species)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Release Data") +
          scale_color_manual(values = allColors)
      })    
      
      output$plot4 <- renderPlotly({
        Recaptures_05 %>%
          ggplot(aes(x = Length, y = Weight, color = Species)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Recapture Data") +
          scale_color_manual(values = allColors)
        
      })
      
      
      output$unknowntags1 <- renderDT({
        datatable(unknown_tags,
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  caption = ("Tags that initially started  with 900_ but are not marker tags, test tags, or part of the release file."),
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        ) 
      })
      
      downloadData_Server("downloadghostTags1", ghostTagsWithMovementAfterGhostDate, "Post-GhostDateMovements")
      
      output$ghostTags1 <- renderDT({
        datatable(ghostTagsWithMovementAfterGhostDate,
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  caption = ("Ghost Tags To Double Check: each of these tags has moved > 0m since its assigned Ghost Date."),
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 25, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        ) 
      })

# Detection distance/water level ------------------------------------------

      output$DDWaterLevelTable <- renderDT({
        
        # brks <- quantile(WGFP_SiteVisits_FieldDatawithPTData, probs = seq(.05, .95, .05), na.rm = TRUE)
        # clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
        #   {paste0("rgb(255,", ., ",", ., ")")}
        columns_to_format <- grep("32mm", colnames(WGFP_SiteVisits_FieldDatawithPTData), value = TRUE)
        
        dt <- datatable(WGFP_SiteVisits_FieldDatawithPTData,
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  caption = ("Red is where Detection Distance is less than water level, green is where it's equal to or below water level"),
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 25, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        ) 
          for (col in columns_to_format) {
            dt <- dt %>%
              formatStyle(
                columns = col,
                backgroundColor = styleEqual(
                  WGFP_SiteVisits_FieldDatawithPTData[[col]] >= WGFP_SiteVisits_FieldDatawithPTData$Water_Level_NoIce_ft,
                  c("red")
                )
                # backgroundColor = styleInterval(
                #   cuts = c(-Inf, 0),
                #   values = c("red", "green")
                # )
              )
          }
          # formatStyle(
          #   columns = c("32mm RR (ft) DS Initial"), 
          #   backgroundColor = styleEqual(c(0, 1), c('red', 'green'))
          # )
        # # Apply custom JavaScript for conditional formatting
        # for (col in columns_to_format) {
        #   dt <- dt %>%
        #     formatStyle(
        #       columns = col,
        #       backgroundColor = styleEqual(
        #         rep(TRUE, nrow(WGFP_SiteVisits_FieldDatawithPTData)),
        #         rep("white", nrow(WGFP_SiteVisits_FieldDatawithPTData))
        #       ),
        #       customJS = JS(sprintf("
        #     function(data, type, row, meta) {
        #       var waterLevel = parseFloat(data[%d]);
        #       var detectionDistance = parseFloat(data[meta.col]);
        #       if (detectionDistance >= waterLevel) {
        #         $(meta.row).css('background-color', 'green');
        #       } else {
        #         $(meta.row).css('background-color', 'red');
        #       }
        #     }
        #   ", which(colnames(WGFP_SiteVisits_FieldDatawithPTData) == "Water_Level_NoIce_ft")))
        #     )
        # }
        dt
          # Loop over each "32mm" column and apply conditional formatting
          # lapply(grep("32mm", colnames(WGFP_SiteVisits_FieldDatawithPTData), value = TRUE), function(col) {
          #   formatStyle(
          #     columns = col,
          #     backgroundColor = styleEqual(
          #       WGFP_SiteVisits_FieldDatawithPTData[[col]] >= WGFP_SiteVisits_FieldDatawithPTData$Water_Level_NoIce_ft,
          #       c("red", "green")
          #     )
          #   )
          # }) %>% 
          # Reduce(function(x, y) x %>% y, .)
          # DT::formatStyle(
          #   columns = grep("32mm", colnames(WGFP_SiteVisits_FieldDatawithPTData), 
          #                  value = TRUE),
          #   target = "cell", 
          #   backgroundColor = styleInterval(
          #     WGFP_SiteVisits_FieldDatawithPTData$Water_Level_NoIce_ft ,  # a small offset to include exact matches
          #     c("red", "green")
          #   )
          # )
      })      
      ######
      qaqcCrosstalkMod_Server("qaqcCrosstalk", combinedData_df_list, metaDataVariableNames)
      
        
    }
  )
}
