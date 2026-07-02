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
                             choices = c("Species" = "Species",       # Update "Species" to match your actual col name
                                         "Age Class" = "AgeClass"
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
          ), 
          fluidRow(
            column(2, 
                   downloadData_UI(ns("downloadRawData"), labelText = "Save Raw Data")
                   ),
            column(2, 
                   downloadData_UI(ns("downloadSummarizedData"), labelText = "Save Summarized Data")
                   )
          ), 
          
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
          mutate(Date = lubridate::ymd(Date), 
                 AgeClass = case_when(
                   !Species %in% c("RBT", "BRK", "LOC") ~ "Unknown",
                   Length <= 150 ~ "0-1 Years",
                   Length > 150 & Length <= 250 ~ "1-2 Years",
                   Length > 250 & Length <= 350 ~ "2-3 Years",
                   Length > 350 ~ "3+ Years",
                   TRUE ~ "Unknown" # Catch-all for NA or missing lengths
                 )
          ) %>%
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
        # if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
        #   
        #   dataSummarized <- GrowthRatesDF %>%
        #     mutate(Species = str_trim(Species)) %>%
        #     dplyr::group_by(across(all_of(input$group_vars))) %>%
        #     dplyr::summarise(`Median Length Growth Rate (g per year)` = round(median(`Length Growth Rate mm per Year`, na.rm = TRUE), 2),
        #                      `Median Weight Growth Rate (mm per year)` = round(median(`Weight Growth Rate g per Year`, na.rm = TRUE), 2),
        #                      `Mean Length Growth Rate (g per year)` = round(mean(`Length Growth Rate mm per Year`, na.rm = TRUE), 2),
        #                      `Mean Weight Growth Rate (mm per year)` = round(mean(`Weight Growth Rate g per Year`, na.rm = TRUE), 2),
        #                      `Sample Size (n)` = n(), 
        #                      .groups = "drop"
        #     )
          
          # Summarize data by the chosen columns
          # SummaryDF <- GrowthRatesDF %>%
          #   group_by(across(all_of(input$group_vars))) %>%
          #   summarize(
          #     `Mean Length Growth (mm/yr)` = round(mean(`Length Growth Rate mm per Year`, na.rm = TRUE), 2),
          #     `Mean Weight Growth (g/yr)` = round(mean(`Weight Growth Rate g per Year`, na.rm = TRUE), 2),
          #     `Sample Size (n)` = n(),
          #     .groups = "drop" # Drops grouping structure after summarizing
          #   )
          
          #return(dataSummarized)
          
        # else {
          # If no grouping is selected, return the raw calculated dataset
          return(GrowthRatesDF)
        #}

        #return(GrowthRatesDF)
      })
      
      display_data <- reactive({
        # Start with the raw calculated data
        df <- growthRates() 
        
        # Check if grouping is requested
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          
          dataSummarized <- df %>%
            mutate(Species = str_trim(Species)) %>%
            dplyr::group_by(across(all_of(input$group_vars))) %>%
            dplyr::summarise(`Median Length Growth Rate (g per year)` = round(median(`Length Growth Rate mm per Year`, na.rm = TRUE), 2),
                             `Median Weight Growth Rate (mm per year)` = round(median(`Weight Growth Rate g per Year`, na.rm = TRUE), 2),
                             `Mean Length Growth Rate (g per year)` = round(mean(`Length Growth Rate mm per Year`, na.rm = TRUE), 2),
                             `Mean Weight Growth Rate (mm per year)` = round(mean(`Weight Growth Rate g per Year`, na.rm = TRUE), 2),
                             `Sample Size (n)` = n(), 
                             .groups = "drop"
            )
          
          return(dataSummarized)
        } else {
          return(df)
        }
      })
      
      
      output$growthRatesPlot <- renderPlotly({
        
        # growthRates() %>%
        #   ggplot(aes(x = `Length Growth Rate mm per Year`, y = `Weight Growth Rate g per Year`, color = Species, text = TagID)) +
        #   geom_point() +
        #   theme_classic() +
        #   labs(title = "Growth Rates") +
        #   scale_color_manual(values = allColors)
        
        df <- growthRates()
        req(df)
        
        # 1. Handle dynamic coloring based on the checkbox input
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          # Combine selected columns into a single 'ColorGroup' column
          df <- df %>% 
            unite("ColorGroup", all_of(input$group_vars), sep = " - ", remove = FALSE)
        } else {
          # Fallback if no checkboxes are selected (defaults to Species)
          df$ColorGroup <- df$Species 
        }
        
        # 2. Build the ggplot using the new ColorGroup column
        p <- df %>%
          ggplot(aes(x = `Length Growth Rate mm per Year`, 
                     y = `Weight Growth Rate g per Year`, 
                     color = ColorGroup, # Dynamically points to our new column
                     text = TagID)) +
          geom_point() +
          theme_classic() +
          labs(
            title = "Growth Rates",
            color = "Group" # Renames the legend title nicely
          )
        
        # 3. Handle custom colors (See warning below)
        # If you only group by Species, your allColors vector will work.
        # If you group by multiple variables, we need to let ggplot pick the colors.
        if (is.null(input$group_vars) || identical(input$group_vars, "Species")) {
          p <- p + scale_color_manual(values = allColors)
        }
        
        # Convert to Plotly
        ggplotly(p, tooltip = c("text", "color", "x", "y"))
        
      })
      
      output$growthRatesSummarizedTable <- renderDT({
        # dataSummarized <- growthRates() %>%
        #   mutate(Species = str_trim(Species)) %>%
        #   dplyr::group_by(Species) %>%
        #   dplyr::summarise(`Median Length Growth Rate (g per year)` = round(median(`Length Growth Rate mm per Year`, na.rm = TRUE), 2),
        #                    `Median Weight Growth Rate (mm per year)` = round(median(`Weight Growth Rate g per Year`, na.rm = TRUE), 2),
        #                    `Mean Length Growth Rate (g per year)` = round(mean(`Length Growth Rate mm per Year`, na.rm = TRUE), 2),
        #                    `Mean Weight Growth Rate (mm per year)` = round(mean(`Weight Growth Rate g per Year`, na.rm = TRUE), 2),
        #                    `Number of Observations` = n()
        #   )

        datatable(display_data(),
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
      
      downloadData_Server("downloadRawData", growthRates(), "growthRatesAll")
      
      downloadData_Server("downloadSummarizedData", display_data(), "growthRatesSummarized")
      
      
    }
  )
}