# individual datasets mod
IndividualDatasets_UI <- function(id, df_list, Release_05) {
  ns <- NS(id)
  tagList(
    
        tabsetPanel(
        tabPanel("Stationary Clean",
                 br(), 
                 withSpinner(DT::dataTableOutput(ns("stationary1")))),
        tabPanel("Biomark",
                 br(), 
                 withSpinner(DT::dataTableOutput(ns("biomark1")))),
        tabPanel("Mobile",
                 br(), 
                 withSpinner(DT::dataTableOutput(ns("mobile1")))),
        tabPanel("Recaptures",
                 br(), 
                 withSpinner(DT::dataTableOutput(ns("recaps1")))),
        tabPanel("Release",
                 br(), 
                 withSpinner(DT::dataTableOutput(ns("release1"))),
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
                 withSpinner(DT::dataTableOutput(ns("ghost1")))),
        tabPanel("Aviation Predation Tags",
                 br(), 
                 withSpinner(DT::dataTableOutput(ns("av_pred1"))))
      
    )#end of individual datasets Mainpanel)
  
  )
}

IndividualDatasets_Server <- function(id, indiv_datasets_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$stationary1 <- DT::renderDataTable(
        
        
        indiv_datasets_list$stationarycleandata,
        rownames = FALSE,
        extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
        
      )
      
      
      output$biomark1 <- renderDataTable(
        
        indiv_datasets_list$biomarkdata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      
      output$mobile1 <- renderDataTable(
        
        indiv_datasets_list$mobiledata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      output$recaps1 <- renderDataTable(
        
        indiv_datasets_list$recapdata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      output$release1 <- renderDataTable(
        
        indiv_datasets_list$releasedata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      output$plot10 <- renderPlotly({
        indiv_datasets_list$releasedata %>%
          ggplot(aes(x = Length, fill = Species) ) +
          geom_histogram(binwidth = input$slider11)+
          theme_classic() +
          labs(title = "Released Fish by Length", caption = "Binwidth = 20mm")
      })
      
      output$plot11 <- renderPlotly({
        indiv_datasets_list$releasedata %>%
          ggplot(aes(x = Weight, fill = Species) ) +
          geom_histogram(binwidth = input$slider12)+
          theme_classic() +
          labs(title = "Released Fish by Weight", caption = "Binwidth = 100g")
      })
      
      output$ghost1 <- renderDataTable(
        
        indiv_datasets_list$ghostdata,
        rownames = FALSE,
        caption = ("Date filter applies to Ghost Date"),
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
      output$av_pred1 <- renderDataTable(
        
        indiv_datasets_list$avian_preddata,
        rownames = FALSE,
        caption = ("Date filter applies to Predation Date"),
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
      )
      
    }
  )
}