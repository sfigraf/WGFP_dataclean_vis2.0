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
                    timeFormat = "%d %b %y"
        ),
        actionButton(ns("crosstalkRenderButton"), label = "Render Table"), 
        hr()
      ),
      mainPanel(
        br(),
        tabsetPanel(id = ns("tabset"), 
                    tabPanel(
                      "Summary Table",
                      box(
                        title = "Crosstalk Occurrence Percentage",
                        withSpinner(DT::dataTableOutput(ns("crosstalkTable"))),
                        footer = "May take a few seconds to load"
                      )
                    )
        )
      )
    )
  )
}

qaqcCrosstalkMod_Server <- function(id, combinedData_df_list, metaDataVariableNames) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      siteCodesList <- list(metaDataVariableNames$RedBarnFrontendCodes, metaDataVariableNames$HitchingPostFrontendCodes,
                            metaDataVariableNames$ConfluenceFrontendCodes, metaDataVariableNames$ConnectivityChannelDownstreamFrontendCodes,
                            metaDataVariableNames$ConnectivityChannelSideChannelFrontendCodes, metaDataVariableNames$ConnectivityChannelUpstreamFrontendCodes
      )
      
      #this is kind of a hacky way to get the tabs to render once and i kinda hate it
      tabsCreated <- reactiveVal(FALSE)
      
      observeEvent(!tabsCreated(), once = TRUE, {
        
        siteCodes <- unique(crosstalkData()$siteCodes)
        # Generate tab panels for each site code
        for (siteCode in siteCodes) {
          
          appendTab(inputId = "tabset", 
                    tabPanel(
                      title = siteCode, 
                      box(
                        title = paste("Crosstalk Occurrences Between ", input$crosstalkDateSlider[1],  "and", input$crosstalkDateSlider[2]),
                        withSpinner(dataTableOutput(ns(paste0("dataTable_", siteCode)))),
                        downloadData_UI(ns(paste0("downloaddataTable_", siteCode))),
                        footer = "May take a few seconds to load"
                      )
                    )
          )
        }
        tabsCreated(TRUE)
      })
      
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
        
        siteCodeVector <- c()
        
        crosstalkIndividualList <- list()
        
        for(codes in siteCodesList){
          
          crosstalkAnalyses <- calculateCrosstalkProportion(SelectedAllEvents = crosstalkData, antennaCodes = codes)
          
          crosstalkDF <- crosstalkDF %>%
            add_row(
              AntennaCodes = paste(codes, collapse = ", "), 
              PercentageOfDetectionsWithSameTimestamp = crosstalkAnalyses$proportionOccurance
            )
          
          siteCode = unique(gsub("\\d", "", codes))
          siteCodeVector <- append(siteCodeVector, siteCode)
          
          crosstalkIndividualList[[siteCode]] <- crosstalkAnalyses$siteCodeCrosstalk
          
        }
        
        return(
          list("summaryTable" = crosstalkDF,
               "siteCodes" = siteCodeVector, 
               "crosstalkIndividualList" = crosstalkIndividualList
          )
        )
      })
      
      output$crosstalkTable <- renderDT({
        datatable(
          crosstalkData()$summaryTable,
          extensions = c("Buttons"),
          rownames = FALSE,
          selection = "single",
          caption = "% of FISH detections on each antenna with the exact same timestamp.
          Detections in raw data may differ by milliseconds, but milliseconds are not used in the Stationary app data.",
          options = list(
            #statesave is restore table state on page reload
            stateSave = TRUE,
            pageLength = 100,
            info = TRUE,
            dom = 'Btri',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "Enter inputs and press Render Table")
          )
        ) %>%
          formatPercentage(c("PercentageOfDetectionsWithSameTimestamp"), 2)
      })
      
      observe({
        
        siteCodes <- unique(crosstalkData()$siteCodes)
        
        for (siteCode in siteCodes) {
          local({
            #in order to get the for-loop to run, have to create a local instance of "i" everytime
            localSiteCodeInstance <- siteCode
            downloadData_Server(paste0("downloaddataTable_", localSiteCodeInstance), crosstalkData()[["crosstalkIndividualList"]][[localSiteCodeInstance]], paste0("crosstalkOccurences_", localSiteCodeInstance))
            
            output[[paste0("dataTable_", localSiteCodeInstance)]] <- renderDataTable({
              
              datatable(
                crosstalkData()[["crosstalkIndividualList"]][[localSiteCodeInstance]], 
                rownames = FALSE,
                selection = "single", 
                options = list(
                  #statesave is restore table state on page reload
                  stateSave = TRUE,
                  pageLength = 10,
                  info = TRUE,
                  dom = 'ltri'
                  #had to add 'lowercase L' letter to display the page length again
                )
              )
            })
          })
        }
      })
    }
  )
}