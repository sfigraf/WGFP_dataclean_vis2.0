QAQC_UI <- function(id, Marker_Tag_data) {
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
               withSpinner(DT::dataTableOutput(ns("ghostTags1")))
      )#end of tab panel#end of tab panel
    )#end of tabset Panel 
  )
}

QAQC_Server <- function(id, Marker_Tag_data, Release_05, Recaptures_05, unknown_tags, ghostTagsWithMovementAfterGhostDate) {
  moduleServer(
    id,
    function(input, output, session) {
      
      MarkerTagQAQC_Server("StationaryMarkerTags", Marker_Tag_data)
      MarkerTagQAQC_Server("BiomarkMarkerTags", Marker_Tag_data)
      
      
      
      # Release and Recap Data L/W Plot Output --------------------------------------------
      output$plot3 <- renderPlotly({
        Release_05 %>%
          ggplot(aes(x = Length, y = Weight, color = Species)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Release Data")
      })    
      
      output$plot4 <- renderPlotly({
        Recaptures_05 %>%
          ggplot(aes(x = Length, y = Weight, color = Species)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Recapture Data")
        
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
      
      output$ghostTags1 <- renderDT({
        datatable(ghostTagsWithMovementAfterGhostDate,
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  caption = ("Ghost Tags To Double Check: each of these tags has moved >= 0m since its assigned Ghost Date."),
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 25, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        ) 
      })
    }
  )
}
