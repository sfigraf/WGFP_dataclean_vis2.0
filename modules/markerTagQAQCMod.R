##### Marker Tag Mod

MarkerTagQAQC_UI <- function(id, Marker_Tag_data) {
  ns <- NS(id)
  tagList(
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
                    value = c(max(Marker_Tag_data$DTY - 30), max(Marker_Tag_data$DTY +1)),
                    step = 1,
                    timeFormat = "%d %b %y",
                    #animate = animationOptions(interval = 500, loop = FALSE)
        ),
        actionButton(ns("button8"), label = "Render Marker Tag Plot")
      ), #end of sidebar panel
      mainPanel(
        br(),
        splitLayout(
          
          withSpinner(DT::dataTableOutput(ns("markerTagsPlotData"))),
          
          withSpinner(plotlyOutput(ns("plot2")))
          
        ),
        br(),
        shinydashboard::box(
          title = "Summarized Marker tag Data for Selected Tags and Sites",
          width = "600px",
          withSpinner(DT::dataTableOutput(ns("summarizedMarkerTagData")))
        )
      )#end of mainpanel
    )#end of sidebar layout
  
  )
}

MarkerTagQAQC_Server <- function(id, Marker_Tag_data) {
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
      
      
      output$markerTagsPlotData <- renderDT({
        
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
      
      output$summarizedMarkerTagData <- renderDT({
        
        datatable(
          plotAndTableMarkerTagDataList()$summarizedMarkerTagDataForTable,
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
            dom = 'Blrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
      })
    }
  )
}