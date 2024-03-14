qaqcCrosstalkMod_UI <- function(id, combinedData_df_list) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("crosstalkDateSlider"), "Date",
                    min = min(combinedData_df_list$All_Events$Date - 1),
                    max = max(combinedData_df_list$All_Events$Date + 1),  
                    value = c(max(combinedData_df_list$All_Events$Date - 30), max(combinedData_df_list$All_Events$Date + 1)),
                    step = 1,
                    timeFormat = "%d %b %y",
                    #animate = animationOptions(interval = 500, loop = FALSE)
        ),
        #end of slider8
        actionButton(ns("crosstalkRenderButton"), label = "Render Table"), 
        hr()
      ),
      mainPanel(
        br(),
        box(
          title = "Crosstalk Occurrance Percentage",
          withSpinner(DT::dataTableOutput(ns("crosstalkTable"))),
          footer = "May take a few seconds to load"
        )
      )
    )
  )
}

qaqcCrosstalkMod_Server <- function(id, combinedData_df_list, metaDataVariableNames) {
  moduleServer(
    id,
    function(input, output, session) {
      crosstalkData <- eventReactive(input$crosstalkRenderButton, ignoreNULL = FALSE,{
        
        crosstalkData <- combinedData_df_list$All_Events %>%
          filter(
            Date >= input$crosstalkDateSlider[1] & Date <= input$crosstalkDateSlider[2]
          )
        crosstalkDF <- data.frame(
          "AntennaCodes" = character(),
          "PercentageOfDetectionsWithSameTimestamp" = numeric(),
          stringsAsFactors = FALSE
        )
        
        for(codes in list(metaDataVariableNames$RedBarnFrontendCodes, metaDataVariableNames$HitchingPostFrontendCodes,
                          metaDataVariableNames$ConfluenceFrontendCodes, metaDataVariableNames$ConnectivityChannelDownstreamFrontendCodes,
                          metaDataVariableNames$ConnectivityChannelSideChannelFrontendCodes, metaDataVariableNames$ConnectivityChannelUpstreamFrontendCodes)){
          
          crosstalkDF <- crosstalkDF %>%
            add_row(
              AntennaCodes = paste(codes, collapse = ", "), 
              PercentageOfDetectionsWithSameTimestamp = calculateCrosstalkProportion(SelectedAllEvents = crosstalkData, antennaCodes = codes)
            )
        }
        
        return(crosstalkDF)
      })
      
      output$crosstalkTable <- renderDT({
        datatable(
          crosstalkData(),
          rownames = FALSE,
          selection = "single",
          caption = "% of FISH detections on each antenna with the exact same timestamp. 
          Detections in raw data may differ by milliseconds, but milliseconds are not used in the app data.", 
          options = list(
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            dom = 'tri',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        ) %>%
          formatPercentage(c("PercentageOfDetectionsWithSameTimestamp"), 2)
      })
    }
  )
}