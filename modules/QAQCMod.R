QAQC_UI <- function(id, Marker_Tag_data) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Marker Tags",
               tabsetPanel(
                 tabPanel("Stationary", 
                          MarkerTagQAQC_UI(ns("StationaryMarkerTags"), Marker_Tag_data)
               ), 
               tabPanel("Biomark"
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
      )#end of tab panel
    )#end of tabset Panel 
  )
}

QAQC_Server <- function(id, Marker_Tag_data, Release_05, Recaptures_05, unknown_tags) {
  moduleServer(
    id,
    function(input, output, session) {
      
      MarkerTagQAQC_Server("StationaryMarkerTags", Marker_Tag_data)
      
      
      # Release and Recap Data L/W Plot Output --------------------------------------------
      output$plot3 <- renderPlotly({
        plot3 <- Release_05 %>%
          ggplot(aes(x = Length, y = Weight, color = Species)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Release Data")
        
        ggplotly(plot3)
      })    
      
      output$plot4 <- renderPlotly({
        plot4 <- Recaptures_05 %>%
          ggplot(aes(x = Length, y = Weight, color = Species)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Recapture Data")
        
        ggplotly(plot4)
      })
      
      
      output$unknowntags1 <- renderDT({
        datatable(unknown_tags,
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  caption = ("Tags that initially started  with 900_ but are not in the release file."),
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        ) 
      })
    }
  )
}
