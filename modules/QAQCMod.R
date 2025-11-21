QAQC_UI <- function(id, combinedData_df_list) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Marker Tags",
               tabsetPanel(
                 tabPanel("Stationary", 
                          MarkerTagQAQC_UI(ns("StationaryMarkerTags"), combinedData_df_list$All_Detections)
               ), 
               tabPanel("Biomark",
                        MarkerTagQAQC_UI(ns("BiomarkMarkerTags"), combinedData_df_list$All_Detections)
                        ), 
               tabPanel("Downtime Periods",
                        br(),
                        DTOutput(ns("markerTagDowntimeTable"))
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
               ), #end of fluidrow
               fluidRow(
                 withSpinner(plotlyOutput(ns("growthRatesPlot")))
               ), 
               fluidRow(
                 withSpinner(DT::DTOutput(ns("growthRatesSummarizedTable")))
               )
      ), #end of tabPanel
      tabPanel("Unknown Tags",
               br(),
               withSpinner(DT::DTOutput(ns("unknowntags1")))
      ),
      tabPanel("Ghost Tag Movements",
               br(),
               downloadData_UI(ns("downloadghostTags1")),
               withSpinner(DT::DTOutput(ns("ghostTags1")))
      ), 
      tabPanel("Avian Predation",
               br(),
               avianPredationMod_UI(ns("avianPredation"))
      ), 
      tabPanel("Crosstalk QAQC",
               qaqcCrosstalkMod_UI(ns("qaqcCrosstalk"), combinedData_df_list)
      ), 
      tabPanel("Detection Distance/Water Level",
               br(),
               withSpinner(DT::DTOutput(ns("DDWaterLevelTable")))
      )
    )#end of tabset Panel 
  )
}

QAQC_Server <- function(id, Release_05, Recaptures_05, unknown_tags, ghostTagsWithMovementAfterGhostDate, avianPredationList,
                        combinedData_df_list, wgfpMetadata, metaDataVariableNames, WGFP_SiteVisits_FieldDatawithPTData, allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      MarkerTagQAQC_Server("StationaryMarkerTags", combinedData_df_list$All_Detections)
      MarkerTagQAQC_Server("BiomarkMarkerTags", combinedData_df_list$All_Detections)
      
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
      
      output$growthRatesPlot <- renderPlotly({
        combinedData_df_list$QAQCtables$growthRates %>%
          ggplot(aes(x = `Length Growth Rate mm per Year`, y = `Weight Growth Rate g per Year`, color = Species, text = TagID)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Growth Rates") +
          scale_color_manual(values = allColors) 
      })
      
      output$growthRatesSummarizedTable <- renderDT({
        dataSummarized <- combinedData_df_list$QAQCtables$growthRates %>%
          mutate(Species = str_trim(Species)) %>%
          dplyr::group_by(Species) %>%
          dplyr::summarise(`Median Length Growth Rate (g per year)` = round(median(`Length Growth Rate mm per Year`, na.rm = TRUE), 2), 
                           `Median Weight Growth Rate (mm per year)` = round(median(`Weight Growth Rate g per Year`, na.rm = TRUE), 2), 
                           `Mean Length Growth Rate (g per year)` = round(mean(`Length Growth Rate mm per Year`, na.rm = TRUE), 2), 
                           `Mean Weight Growth Rate (mm per year)` = round(mean(`Weight Growth Rate g per Year`, na.rm = TRUE), 2), 
                           `Number of Observations` = n()
          )
                           
        datatable(dataSummarized,
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  caption = ("Data from each time a fish was recaptured"),
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip' #had to add 'lowercase L' letter to display the page length again
                    
                  )
        )
        
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
      

# Avian Predation ---------------------------------------------------------

      avianPredationMod_Server("avianPredation", avianPredationList)      

# Detection distance/water level ------------------------------------------

      output$DDWaterLevelTable <- renderDT({
        
        datatable(
          WGFP_SiteVisits_FieldDatawithPTData,
          class = 'nowrap display',
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          caption = ("Green cells show where 32mm detection distance is greater than or equal to water level, red is where 32mm detection distance is less than water level. Water level readings are within 13 hours of site visit."),
          options = list(
            stateSave = TRUE,
            pageLength = 25,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50, 100, 200), c("10", "25", "50", "100", "200")),
            dom = 'Blfrtip',
            language = list(emptyTable = "Enter inputs and press Render Table"),
            rowCallback = JS(
              'function(row, data, index) {',
              '  var api = this.api();',
              '  var waterLevelIndex = api.column(":contains(Water_Level_NoIce_ft)").index();',
              '  var waterLevel = parseFloat(data[waterLevelIndex]);',
              '  for (var i = 0; i < data.length; i++) {',
              '    if (/32mm/.test(api.column(i).header().textContent)) {',
              '      var cellValue = parseFloat(data[i]);',
              '      if (cellValue <= waterLevel) {',
              '        $("td:eq(" + i + ")", row).css("background-color", "#8B0000AA");',
              '      } else if (cellValue > waterLevel) {',
              '        $("td:eq(" + i + ")", row).css("background-color", "#228B22AA");',
              '      }',
              '    }',
              '  }',
              '}'
            )
          )
        ) 
      })      
      ######
      qaqcCrosstalkMod_Server("qaqcCrosstalk", combinedData_df_list, metaDataVariableNames)
      
        
    }
  )
}
