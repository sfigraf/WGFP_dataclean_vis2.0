growthRates_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      withSpinner(plotlyOutput(ns("growthRatesPlot")))
    ), 
    fluidRow(
      withSpinner(DT::DTOutput(ns("growthRatesSummarizedTable")))
    )
  
  )
}

growthRates_Server <- function(id, indiv_datasets_list = indiv_datasets_list, allColors = allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ###get growth rates for QAQC tab
      growthRates <- reactive({
        # print("gets here")
        # req(indiv_datasets_list$releasedata)
        # req(indiv_datasets_list$recapdata)
        return(getGrowthRates(Release = indiv_datasets_list$releasedata, Recaptures = indiv_datasets_list$recapdata))
        #print("gorth rates calculated")
      })
      
      
      output$growthRatesPlot <- renderPlotly({
        
        growthRates() %>%
          ggplot(aes(x = `Length Growth Rate mm per Year`, y = `Weight Growth Rate g per Year`, color = Species, text = TagID)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Growth Rates") +
          scale_color_manual(values = allColors) 
      })
      
      output$growthRatesSummarizedTable <- renderDT({
        dataSummarized <- growthRates() %>%
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
      
    }
  )
}