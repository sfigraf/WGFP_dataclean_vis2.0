# individual datasets mod
IndividualDatasets_UI <- function(id, df_list, Release_05) {
  ns <- NS(id)
  tagList(
    
        tabsetPanel(
        tabPanel("Stationary Clean",
                 br(), 
                 downloadData_UI(ns("downloadstationary1")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("stationary1")))),
        tabPanel("Biomark",
                 br(), 
                 downloadData_UI(ns("downloadbiomark1")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("biomark1")))),
        tabPanel("Mobile",
                 br(), 
                 downloadData_UI(ns("downloadmobile1")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("mobile1")))),
        tabPanel("Recaptures",
                 br(), 
                 downloadData_UI(ns("downloadrecaps1")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("recaps1")))),
        tabPanel("Release",
                 br(), 
                 downloadData_UI(ns("downloadrelease1")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("release1"))),
                 sliderInput(ns("slider11"), "Length Binwidth",
                             min = 1, 
                             max = max(Release_05$Length, na.rm = TRUE),
                             value = 20),
                 withSpinner(plotlyOutput(ns("plot10"))),
                 hr(),
                 sliderInput(ns("slider12"), "Weight Binwidth",
                             min = 1, 
                             max = max(Release_05$Weight, na.rm = TRUE), 
                             value = 100),
                 withSpinner(plotlyOutput(ns("plot11"))),
                 hr()),
        tabPanel("Ghost Tags",
                 br(), 
                 downloadData_UI(ns("downloadghost1")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("ghost1")))),
        tabPanel("Aviation Predation Tags",
                 br(), 
                 downloadData_UI(ns("downloadav_pred1")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("av_pred1")))), 
        tabPanel("Pressure Transducers",
                 br(), 
                 downloadData_UI(ns("downloadPTdata")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("PTdata")))), 
        tabPanel("USGS at Hitching Post 15 Min",
                 br(), 
                 downloadData_UI(ns("downloadUSGSdata")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("USGSdata")))), 
        tabPanel("Combined Site Visit Data",
                 br(), 
                 downloadData_UI(ns("downloadSiteVisitData")),
                 br(), 
                 withSpinner(DT::DTOutput(ns("SiteVisitData"))))
      
    )#end of individual datasets Mainpanel)
  
  )
}

IndividualDatasets_Server <- function(id, indiv_datasets_list, allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      downloadData_Server("downloadstationary1", indiv_datasets_list$stationarycleandata, "StationaryData")
      downloadData_Server("downloadbiomark1", indiv_datasets_list$biomarkdata, "BiomarkData")
      downloadData_Server("downloadmobile1", indiv_datasets_list$mobiledata, "MobileData")
      downloadData_Server("downloadrecaps1", indiv_datasets_list$recapdata, "RecaptureData")
      downloadData_Server("downloadrelease1", indiv_datasets_list$releasedata, "ReleaseData")
      downloadData_Server("downloadghost1", indiv_datasets_list$ghostdata, "GhostTagData")
      downloadData_Server("downloadav_pred1", indiv_datasets_list$avian_preddata, "AvianPredationData")
      downloadData_Server("downloadPTdata", indiv_datasets_list$PTDataRawCombined, "PTDataCombined")
      downloadData_Server("downloadUSGSdata", indiv_datasets_list$USGSData15Min, "USGS15MinData")
      downloadData_Server("downloadSiteVisitData", indiv_datasets_list$SiteVisitDataCombined, "SiteVisitDataCombined")
      
      output$stationary1 <- DT::renderDT(
        
        
        indiv_datasets_list$stationarycleandata,
        rownames = FALSE,
        extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
        
      )
      
      
      output$biomark1 <- renderDT(
        
        indiv_datasets_list$biomarkdata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      
      output$mobile1 <- renderDT(
        
        indiv_datasets_list$mobiledata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      
      
      output$recaps1 <- renderDT(
        
        indiv_datasets_list$recapdata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      output$release1 <- renderDT(
        
        indiv_datasets_list$releasedata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      output$plot10 <- renderPlotly({
        indiv_datasets_list$releasedata %>%
          ggplot(aes(x = Length, fill = Species) ) +
          geom_histogram(binwidth = input$slider11)+
          theme_classic() +
          labs(title = "Released Fish by Length", caption = "Binwidth = 20mm") +
          scale_fill_manual(values = allColors)
      })
      
      output$plot11 <- renderPlotly({
        indiv_datasets_list$releasedata %>%
          ggplot(aes(x = Weight, fill = Species) ) +
          geom_histogram(binwidth = input$slider12)+
          theme_classic() +
          labs(title = "Released Fish by Weight", caption = "Binwidth = 100g") +
          scale_fill_manual(values = allColors)
      })
      
      output$ghost1 <- renderDT(
        
        indiv_datasets_list$ghostdata,
        rownames = FALSE,
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      output$av_pred1 <- renderDT(
        
        indiv_datasets_list$avian_preddata,
        rownames = FALSE,
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      output$PTdata <- renderDT(
        
        indiv_datasets_list$PTDataRawCombined,
        rownames = FALSE,
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
        )
      )
      
      output$USGSdata <- renderDT(
        
        indiv_datasets_list$USGSData15Min,
        rownames = FALSE,
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
        )
      )
      
      output$SiteVisitData <- renderDT(
        
        indiv_datasets_list$SiteVisitDataCombined,
        rownames = FALSE,
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
        )
      )
      
    }
  )
}
