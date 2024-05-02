movementsFiltered_UI <- function(id, Movements_df) {
  ns <- NS(id)
  tagList(
    textInput(ns("textinput3"), label = "Filter by TAG"),
    pickerInput(ns("picker6"),
                label = "Select Movement Type:",
                choices = sort(unique(Movements_df$movement_only)),
                selected = unique(Movements_df$movement_only),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE #this makes the "select/deselect all" option
                )
    ), #end of picker 6 input
    
    pickerInput(ns("picker7"),
                label = "Select Detection Type",
                choices = sort(unique(Movements_df$det_type)),
                selected = unique(Movements_df$det_type),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE #this makes the "select/deselect all" option
                )
    ), #end of picker 7 
    pickerInput(ns("picker10"),
                label = "Select Species Type",
                choices = sort(unique(Movements_df$Species)),
                selected = unique(Movements_df$Species),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE #this makes the "select/deselect all" option
                )
    ), #end of picker 10 
    
    sliderInput(ns("slider10"), "Fish Release Length",
                min = min(Movements_df$Release_Length, na.rm = TRUE),
                max = max(Movements_df$Release_Length, na.rm = TRUE),  
                value = c(min(Movements_df$Release_Length, na.rm = TRUE),max(Movements_df$Release_Length, na.rm = TRUE)),
                step = 1,
                
    ), #end of slider8
    
    
    sliderInput(ns("slider2"), "Date",
                min = min(Movements_df$Date -1),
                max = max(Movements_df$Date +1),  
                value = c(min(Movements_df$Date -1),max(Movements_df$Date +1)),
                step = 1,
                timeFormat = "%d %b %y",
                #animate = animationOptions(interval = 500, loop = FALSE)
    ),
    
    sliderInput(ns("slider9"), "Total distance travelled (m)",
                min = min(Movements_df$sum_dist, na.rm = TRUE),
                max = max(Movements_df$sum_dist, na.rm = TRUE),  
                value = c(min(Movements_df$sum_dist, na.rm = TRUE),max(Movements_df$sum_dist, na.rm = TRUE)),
                step = 1,
                
    ), #end of slider8
    checkboxInput(ns("dischargeDataFilter"), "Display USGS Discharge Filter"),
    uiOutput(ns("DischargeFilterUI")), 
    
    checkboxInput(ns("tempDataFilter"), "Display USGS Water Temp Filter"),
    uiOutput(ns("TempFilterUI")),
    
    actionButton(ns("button7"), label = "Render"), 
    h6("Note: entries with NA values in any of the filter fields are excluded from the results")
  
  )
}

movementsFiltered_Server <- function(id, Movements_df) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      #USGSDischargeDaily, WtempF, 
      output$DischargeFilterUI <- renderUI({
        req(input$dischargeDataFilter)
        tagList(
          h6("Note: Daily Discharge is measured from USGS Gauge at Hitching Post"),
          sliderInput(ns("sliderDischarge"), "USGS Discharge (CFS)",
                      min = min(Movements_df$USGSDischargeDaily, na.rm = TRUE),
                      max = max(Movements_df$USGSDischargeDaily, na.rm = TRUE),
                      value = c(min(Movements_df$USGSDischargeDaily, na.rm = TRUE), max(Movements_df$USGSDischargeDaily, na.rm = TRUE)),
                      step = 1,
          )
        )
      })
      
      output$TempFilterUI <- renderUI({
        req(input$tempDataFilter)
        tagList(
          h6("Note: Daily Water Temp is measured from USGS Gauge at Hitching Post"),
          sliderInput(ns("sliderTemp"), "USGS Discharge (CFS)",
                      min = min(Movements_df$WtempF, na.rm = TRUE),
                      max = max(Movements_df$WtempF, na.rm = TRUE),
                      value = c(min(Movements_df$WtempF, na.rm = TRUE), max(Movements_df$WtempF, na.rm = TRUE)),
                      step = 1,
          )
        )
      })
      
      filtered_movements_data <- eventReactive(input$button7, ignoreNULL = FALSE,{
        
        Movements_df1 <- Movements_df
        
        if(input$dischargeDataFilter){
          Movements_df1 <- Movements_df1 %>%
            filter(
              USGSDischargeDaily >= input$sliderDischarge[1] & USGSDischargeDaily <= input$sliderDischarge[2]
            )
        }
        if(input$tempDataFilter){
          Movements_df1 <- Movements_df1 %>%
            filter(
              WtempF >= input$sliderTemp[1] & WtempF <= input$sliderTemp[2]
            )
        }
        
        if(input$textinput3 != ''){
          movements_data1 <- Movements_df1 %>%
            filter(TAG %in% trimws(input$textinput3),
                   Release_Length >= input$slider10[1] & Release_Length <= input$slider10[2],
                   Date >= input$slider2[1] & Date <= input$slider2[2],
                   movement_only %in% c(input$picker6),
                   det_type %in% c(input$picker7),
                   Species %in% c(input$picker10),
                   sum_dist >= input$slider9[1] & sum_dist <= input$slider9[2],
                   
            ) %>%
            arrange(Datetime)
          #this id column is used for the map and datatable proxy and needs to be redone each time a filter is applied
          movements_data1$id <- seq.int(nrow(movements_data1))
          
        } else {
          movements_data1 <- Movements_df1  %>% 
            filter(
              Release_Length >= input$slider10[1] & Release_Length <= input$slider10[2],
              Date >= input$slider2[1] & Date <= input$slider2[2],
              movement_only %in% c(input$picker6),
              det_type %in% c(input$picker7),
              Species %in% c(input$picker10),
              sum_dist >= input$slider9[1] & sum_dist <= input$slider9[2]
              
            ) %>%
            arrange(Datetime)
          
          movements_data1$id <- seq.int(nrow(movements_data1))
          
        }
        return(movements_data1)
      }) 
      
      return(filtered_movements_data)
    }
  )
}