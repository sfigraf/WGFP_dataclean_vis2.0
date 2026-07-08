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
                      max = 730, # Arbitrary max 
                      value = 90, # Default to ~3 months
                      step = 1), 
          
          h4("Summarize Output"),
          checkboxGroupInput(ns("group_vars"), 
                             label = "Group Summary By (leave blank for raw data):",
                             choices = c("Species" = "Species",       
                                         #previousAgeClass and previousRiver is used because if a LOC is released at 193 mm and recapped at 330, its growth rate should go in the 2 year age class rate instead of 3 year
                                         "Age Class" = "previousAgeClass", 
                                         "River" = "previousRiver"
                                         )
                             )
          
          #actionButton(ns("renderData"), label = "Calculate Growth Rates")
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

growthRates_Server <- function(id, DFforGrowthRates, allColors = allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ###get growth rates for QAQC tab
      growthRates <- reactive({ #input$renderData, 
        
        # ReleasepostScript = indiv_datasets_list$releasedata
        # RecapturespostScreipt = indiv_datasets_list$recapdata
        # 
        # RecapturesforBind <- alignColumns(Recaptures, names(Release), Release) %>%
        #   left_join(Recaptures[,c("TagID", "Length", "Weight", "RecaptureSite")], by = c("TagID", "Length", "Weight"))
        # 
        # ReleaseforBind <- alignColumns(Release, names(RecapturesforBind), RecapturesforBind)
        # 
        # ReleaseRecaps <- bind_rows(ReleaseforBind, RecapturesforBind)
        # 
        # DFforGrowthRates <- ReleaseRecaps %>%
        #   mutate(Date = lubridate::ymd(Date), 
        #          AgeClass = case_when(
        #            #age classes based on examination of age frequency graph and looking at eaks and vallyes and talking with eric fetherman
        #            !Species %in% c("RBT", "LOC") ~ "Unknown",
        #            
        #            #LOC
        #            Species == "LOC" & Length <= 150 ~ "0-1 Years",
        #            Species == "LOC" & Length > 150 & Length <= 230 ~ "2 Years",
        #            Species == "LOC" & Length > 230 & Length <= 360 ~ "3 Years",
        #            Species == "LOC" & Length > 360 ~ "3+ Years",
        #            
        #            ##RBT
        #            Species == "RBT" & Length <= 140 ~ "0-1 Years",
        #            Species == "RBT" & Length > 140 & Length <= 330 ~ "2 Years",
        #            Species == "RBT" & Length > 330 & Length <= 430 ~ "3 Years",
        #            Species == "RBT" & Length > 430 ~ "3+ Years",
        #            TRUE ~ "Unknown" # Catch-all for NA or missing lengths
        #          )
        #   ) %>%
        #   group_by(TagID) %>%
        #   arrange(Date, .by_group = TRUE) %>%
        #   #use 52.25 weeks to account for leap years
        #   mutate(daysSince = as.numeric(difftime(Date, lag(Date), units = "days")), 
        #          yearsSince = daysSince/365.25,
        #          previousLength = lag(Length), 
        #          previousWeight = lag(Weight),
        #          previousYear = lag(year(Date)),
        #          previousAgeClass = lag(AgeClass), 
        #          previousRiver = lag(River)
        #   )
        #getGrowthRates(Release = indiv_datasets_list$releasedata, Recaptures = indiv_datasets_list$recapdata)
        GrowthRatesDF <- DFforGrowthRates %>%
          filter(daysSince > input$min_time_at_large) #%>%
          # mutate(
          #   `Length Growth Rate mm per Year`= round((Length - previousLength)/yearsSince, 2), 
          #   `Weight Growth Rate g per Year`= round((Weight - previousWeight)/yearsSince, 2)
          # )
        
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
            unite("Group", all_of(input$group_vars), sep = " - ", remove = FALSE)
        } else {
          # Fallback if no checkboxes are selected (defaults to Species)
          df$Group <- df$Species 
        }
        
        # 2. Build the ggplot using the new ColorGroup column
        p <- df %>%
          ggplot(aes(x = `Length Growth Rate mm per Year`, 
                     y = `Weight Growth Rate g per Year`, 
                     color = Group, # Dynamically points to our new column
                     text = paste0("Tag: ", TagID,
                                   "<br>Previous Length: ", previousLength, " (", previousYear, ")", 
                                   "<br>Current Length: ", Length, " (", year(Date), ")"
                     )
          )
          ) +
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