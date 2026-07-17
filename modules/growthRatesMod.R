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
          )
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
        GrowthRatesDF <- DFforGrowthRates %>%
          filter(daysSince > input$min_time_at_large) 
          return(GrowthRatesDF)
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

        df <- growthRates()
        req(df)
        
        # Handle dynamic coloring based on the checkbox input
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          # Combine selected columns into a single 'Group' column
          df <- df %>% 
            unite("Group", all_of(input$group_vars), sep = " - ", remove = FALSE)
        } else {
          # if no checkboxes are selected defaults to Species
          df$Group <- df$Species 
        }
        
        #Build the ggplot using the new ColorGroup column
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
        
        # colors
        # If only group by Species, allColors vector will work.
        # If group by multiple variables, we need to let ggplot pick the colors.
        if (is.null(input$group_vars) || identical(input$group_vars, "Species")) {
          p <- p + scale_color_manual(values = allColors)
        }
        
        # Convert to Plotly
        ggplotly(p, tooltip = c("text", "color", "x", "y"))
        
      })
      
      output$growthRatesSummarizedTable <- renderDT({

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