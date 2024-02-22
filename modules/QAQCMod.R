QAQC_UI <- function(id, Marker_Tag_data) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Marker Tags",
               sidebarLayout(
                 sidebarPanel(
                   pickerInput(ns("picker8"),
                               label = "Select Site Code",
                               choices = sort(unique(Marker_Tag_data$SCD)),
                               selected = unique(Marker_Tag_data$SCD),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 8
                   pickerInput(ns("picker9"),
                               label = "Select Marker Tag",
                               choices = sort(unique(Marker_Tag_data$TAG)),
                               selected = unique(Marker_Tag_data$TAG)[1],
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 9 
                   sliderInput(ns("slider3"),
                               "Date",
                               min = min(Marker_Tag_data$DTY -1),
                               max = max(Marker_Tag_data$DTY +1),  
                               value = c(max(Marker_Tag_data$DTY - 30),max(Marker_Tag_data$DTY +1)),
                               step = 1,
                               timeFormat = "%d %b %y",
                               #animate = animationOptions(interval = 500, loop = FALSE)
                   ),
                   actionButton(ns("button8"), label = "Render Marker Tag Plot")
                 ), #end of sidebar panel
                 mainPanel(
                   splitLayout(
                     
                     withSpinner(DT::dataTableOutput(ns("markertags1"))),
                     
                     withSpinner(plotlyOutput(ns("plot2")))
                     
                   ),
                  
                 )#end of mainpanel
               )#end of sidebar layout
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
      
      plotAndTableMarkerTagDataList <- eventReactive(input$button8,ignoreNULL = FALSE,{
        
        
        markerTagDataFiltered <- Marker_Tag_data %>%
          filter(SCD %in% c(input$picker8),
                 TAG %in% c(input$picker9))
        
        markerTagDataForPlot <- markerTagDataFiltered %>%
          filter(DTY >= input$slider3[1] & DTY <= input$slider3[2])
        
        summarizedMarkerTagDataForTable <- markerTagDataFiltered %>%
          dplyr::count(SCD, TAG, name = "totalDetectionsSinceProjectInception")
        
        plotAndTableMarkerTagDataList <- list("markerTagDataForPlot" = markerTagDataForPlot, 
                              "summarizedMarkerTagDataForTable" = summarizedMarkerTagDataForTable)
        return(plotAndTableMarkerTagDataList)
      }) 
      
      # MarkerTag Plot Output ---------------------------------------------------
      
      output$plot2 <- renderPlotly({
        plot2 <- plotAndTableMarkerTagDataList()$markerTagDataForPlot %>%
          ggplot(aes(x = DTY, y = ARR, color = SCD, text = paste(TAG) )) +
          geom_point() +
          labs(title = "Marker Tag Detection Times") +
          xlab("Date") +
          ylab("Time") +
          theme_classic() +
          theme(
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank())
        
        ggplotly(p = plot2)
        
      })    
      
      
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
      
      output$markertags1 <- renderDT({
        
        datatable(
          plotAndTableMarkerTagDataList()$markerTagDataForPlot,
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          caption = "Marker tags are inferred as tags that start with 0000000",
          options = list(
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50, 100, 200), c("10", "25", "50", "100", "200")),
            dom = 'Blfrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
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
