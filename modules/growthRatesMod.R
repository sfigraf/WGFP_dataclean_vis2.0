growthRates_UI <- function(id) {
  ns <- NS(id)
  tagList(

      sidebarLayout(
        sidebarPanel(
          h4("Filter Data"),
          # Slider for Minimum Time Before Recapture (Days)
          sliderInput(ns("min_time_at_large"),
                      label = "Minimum time before recapture (Days):",
                      min = 0, 
                      max = 730, # Arbitrary max (e.g., 2 years) - adjust as needed
                      value = 90, # Default to ~3 months
                      step = 1), 
          
          h4("Summarize Output"),
          checkboxGroupInput(ns("group_vars"), 
                             label = "Group Summary By (leave blank for raw data):",
                             choices = c("Species" = "Species"#,       # Update "Species" to match your actual col name
                                         #"Age Class" = "AgeClass"
                                         )
                             ),
          
          actionButton(ns("renderData"), label = "Calculate Growth Rates")
       # )
          # pickerInput(ns("picker8"),
          #             label = "Select Site Code",
          #             choices = sort(unique(Marker_Tag_data$Site_Code)),
          #             selected = unique(Marker_Tag_data$Site_Code),
          #             multiple = TRUE,
          #             options = list(
          #               `actions-box` = TRUE #this makes the "select/deselect all" option
          #             )
          # ), #end of picker 8
        ), 
        mainPanel(
          fluidRow(
            withSpinner(plotlyOutput(ns("growthRatesPlot")))
          ), 
          fluidRow(
            withSpinner(DT::DTOutput(ns("growthRatesSummarizedTable")))
          )
        )
      )
  )
}

growthRates_Server <- function(id, indiv_datasets_list = indiv_datasets_list, allColors = allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ###get growth rates for QAQC tab
      growthRates <- eventReactive(input$renderData, {
        
        Release = indiv_datasets_list$releasedata
        Recaptures = indiv_datasets_list$recapdata
        
        RecapturesforBind <- alignColumns(Recaptures, names(Release), Release) %>%
          left_join(Recaptures[,c("TagID", "Length", "Weight", "RecaptureSite")], by = c("TagID", "Length", "Weight"))
        
        ReleaseforBind <- alignColumns(Release, names(RecapturesforBind), RecapturesforBind)
        
        ReleaseRecaps <- bind_rows(ReleaseforBind, RecapturesforBind)
        
        DFforGrowthRates <- ReleaseRecaps %>%
          mutate(Date = lubridate::ymd(Date)) %>%
          group_by(TagID) %>%
          arrange(Date, .by_group = TRUE) %>%
          #use 52.25 weeks to account for leap years
          mutate(yearsSince = as.numeric(difftime(Date, lag(Date), units = "weeks"))/52.25, 
                 daysSince = as.numeric(difftime(Date, lag(Date), units = "days")), 
                 previousLength = lag(Length), 
                 previousWeight = lag(Weight)
          )
        #getGrowthRates(Release = indiv_datasets_list$releasedata, Recaptures = indiv_datasets_list$recapdata)
        GrowthRatesDF <- DFforGrowthRates %>%
          filter(daysSince > input$min_time_at_large) %>%
          mutate(
            `Length Growth Rate mm per Year`= round((Length - previousLength)/yearsSince, 2), 
            `Weight Growth Rate g per Year`= round((Weight - previousWeight)/yearsSince, 2)
          )
        
        # NEW: Dynamic Grouping and Summarization
        # Check if the user selected any grouping variables
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          
          # Summarize data by the chosen columns
          SummaryDF <- GrowthRatesDF %>%
            group_by(across(all_of(input$group_vars))) %>%
            summarize(
              `Mean Length Growth (mm/yr)` = round(mean(`Length Growth Rate mm per Year`, na.rm = TRUE), 2),
              `Mean Weight Growth (g/yr)` = round(mean(`Weight Growth Rate g per Year`, na.rm = TRUE), 2),
              `Sample Size (n)` = n(),
              .groups = "drop" # Drops grouping structure after summarizing
            )
          
          return(SummaryDF)
          
        } else {
          # If no grouping is selected, return the raw calculated dataset
          return(GrowthRatesDF)
        }

        #return(GrowthRatesDF)
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