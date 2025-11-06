##### Marker Tag Mod

MarkerTagQAQC_UI <- function(id, All_Detections) {
  ns <- NS(id)
  #deciding which marker tag data to show
  if(str_detect(id, "Stationary")){
    Marker_Tag_data <- All_Detections %>%
      dplyr::filter(str_detect(TAG, "^0000000"))
  } else{
    Marker_Tag_data <- All_Detections %>%
      dplyr::filter(str_detect(TAG, "^999"))
  }
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        pickerInput(ns("picker8"),
                    label = "Select Site Code",
                    choices = sort(unique(Marker_Tag_data$Site_Code)),
                    selected = unique(Marker_Tag_data$Site_Code),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE #this makes the "select/deselect all" option
                    )
        ), #end of picker 8
        pickerInput(ns("picker9"),
                    label = "Select Marker Tag",
                    choices = sort(unique(Marker_Tag_data$TAG)),
                    selected = sort(unique(Marker_Tag_data$TAG))[1],
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE #this makes the "select/deselect all" option
                    )
        ), #end of picker 9 
        dateRangeInput(ns("markerDateRangeInput"),
                    "Date",
                    start = max(Marker_Tag_data$Scan_Date - 30),
                    end = max(Marker_Tag_data$Scan_Date + 1), 
                    min = min(Marker_Tag_data$Scan_Date - 1)
        ),
        checkboxInput(ns("fishDetectionOverlay"), "Overlay Fish Detection Data"
        ),
        actionButton(ns("button8"), label = "Render Marker Tag Plot")
      ), #end of sidebar panel
      mainPanel(
        br(),
        withSpinner(plotlyOutput(ns("plot2"))),
          withSpinner(DT::dataTableOutput(ns("markerTagsPlotData"))),
        downloadData_UI(ns("downloadmarkerTagsPlotData")),
        br(),
        shinydashboard::box(
          title = "Summarized Marker tag Data for Selected Tags and Sites",
          width = "600px",
          withSpinner(DT::dataTableOutput(ns("summarizedMarkerTagData"))), 
          downloadData_UI(ns("downloadsummarizedMarkerTagData")),
        )
      )#end of mainpanel
    )#end of sidebar layout
  )
}

MarkerTagQAQC_Server <- function(id, All_Detections) {
  moduleServer(
    id,
    function(input, output, session) {
      
      plotAndTableMarkerTagDataList <- eventReactive(input$button8,ignoreNULL = FALSE,{
        #first filtering by site code and date range (tag is a bit trickier when throwing in fish data)

        allDetectionsFiltered <- All_Detections %>%
          filter(Site_Code %in% c(input$picker8),
                 Scan_Date >= input$markerDateRangeInput[1] & Scan_Date <= input$markerDateRangeInput[2]
          )
        if(input$fishDetectionOverlay){
          if(str_detect(id, "Stationary")){
            #if overlay is on and stationary selected, filter out biomark tags and make stationary marker tags ddisplay as "_marker tag
            #and have fish display as "_fish detection"
            allDetectionsFilteredForPlot <- allDetectionsFiltered %>%
              mutate(Site_Code = case_when(str_detect(TAG, "^0000000") ~ paste0(Site_Code, "_MarkerTag"), 
                                           TRUE ~ paste0(Site_Code, "_Fish Detection")
              )
              ) %>%
              dplyr::filter(!str_detect(TAG, "^999"))
            
            
          } else {
            allDetectionsFilteredForPlot <- allDetectionsFiltered %>%
              mutate(Site_Code = case_when(str_detect(TAG, "^999") ~ paste0(Site_Code, "_MarkerTag"), 
                                           TRUE ~ paste0(Site_Code, "_Fish Detection")
              )) %>%
              dplyr::filter(!str_detect(TAG, "^0000000"))
          }
          #now, separate fish and marker tags based on selection in Tag Picker
          fishOnly <- allDetectionsFilteredForPlot %>%
            filter(str_detect(Site_Code, "_Fish Detection"))
          
          markerTagDataForMerging <- allDetectionsFiltered %>%
            filter(TAG %in% c(input$picker9))
          #bind rows back together so that fish tags will display if the overlay is on and specific marker tag is selected
          
          markerTagDataForPlot <- rbind(fishOnly, markerTagDataForMerging)
          
        } else{
          #if the fish overlay is off, just filter by selected tag now
          markerTagDataForPlot <- allDetectionsFiltered %>%
            filter(TAG %in% c(input$picker9))
        }
        
        #this will include fishDetections (including unkown tags bc they haven't been fitlered out of this DF) but not a big deal
        summarizedMarkerTagDataForTable <- markerTagDataForPlot %>%
          dplyr::count(Site_Code, TAG, name = "totalDetectionsSinceProjectInception")
        
        plotAndTableMarkerTagDataList <- list("markerTagDataForPlot" = markerTagDataForPlot, 
                                              "summarizedMarkerTagDataForTable" = summarizedMarkerTagDataForTable)
        
        return(plotAndTableMarkerTagDataList)
      }) 
      
      # MarkerTag Plot Output ---------------------------------------------------

      output$plot2 <- renderPlotly({
        markerPlot <- plotAndTableMarkerTagDataList()$markerTagDataForPlot %>%
          ggplot(aes(x = Scan_Date, y = Scan_Time, color = Site_Code, text = paste(TAG) )) +
          geom_point() +
          labs(title = "Marker Tag Detection Times") +
          xlab("Date") +
          ylab("Time") +
          theme_classic() +
          theme(
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank()) +
          scale_color_brewer(palette="Dark2")
      })
      downloadData_Server("downloadmarkerTagsPlotData", plotAndTableMarkerTagDataList()$markerTagDataForPlot, "MarkerTagData")
      
      
      output$markerTagsPlotData <- renderDT({
        
        datatable(
          plotAndTableMarkerTagDataList()$markerTagDataForPlot,
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          caption = "Marker tags are inferred as tags that start with 0000000 for Stationary and 999 for Biomark",
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
      downloadData_Server("downloadsummarizedMarkerTagData", plotAndTableMarkerTagDataList()$summarizedMarkerTagDataForTable, "SummarizedMarkerTagData")
      
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
            dom = 'lrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        )
      })
    }
  )
}