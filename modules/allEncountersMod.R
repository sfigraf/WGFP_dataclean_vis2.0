#define hotkeys
hotkeys <- c("enter")

AllEncounters_UI <- function(id, combinedData_df_list) {
  ns <- NS(id)
  
  tagList(
    #makes it so we can use hot keys as a reactive
    keys::useKeys(),
    keys::keysInput(ns("keys"), hotkeys),
    sidebarLayout(
                   sidebarPanel(
                     textInput(ns("textinput1"), "Filter by Tag"),
                     dateRangeInput(ns("drangeinput2"), "Select a Date Range:",
                                    start = "2020-08-01",
                                    end = max(combinedData_df_list$All_Events$Date) + 1), #end of date range input
                     sliderInput(ns("slider1"), "Hour of Day",
                                 min = min(lubridate::hour(combinedData_df_list$All_Events$Datetime), na.rm = TRUE),
                                 max = max(lubridate::hour(combinedData_df_list$All_Events$Datetime), na.rm = TRUE),
                                 value = c(min(lubridate::hour(combinedData_df_list$All_Events$Datetime), na.rm = TRUE),max(lubridate::hour(combinedData_df_list$All_Events$Datetime), na.rm = TRUE)),
                                 step = 1
                     ),
                     pickerInput(ns("picker1"),
                                 label = "Select Event",
                                 choices = sort(unique(combinedData_df_list$All_Events$Event)),
                                 selected = unique(combinedData_df_list$All_Events$Event),
                                 multiple = TRUE,
                                 options = list(
                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                 ),
                     ), #end of picker input
                     pickerInput(ns("picker2"),
                                 label = "Select Fish Species:",
                                 choices = sort(unique(combinedData_df_list$All_Events$Species)),
                                 selected = unique(combinedData_df_list$All_Events$Species),
                                 multiple = TRUE,
                                 options = list(
                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                 ),
                     ), #end of picker 2 input
                     sliderInput(ns("slider6"), "Fish Release Length (mm)",
                                 min = min(combinedData_df_list$All_Events$Release_Length, na.rm = TRUE),
                                 max = max(combinedData_df_list$All_Events$Release_Length, na.rm = TRUE),
                                 value = c(min(combinedData_df_list$All_Events$Release_Length, na.rm = TRUE),max(combinedData_df_list$All_Events$Release_Length, na.rm = TRUE)),
                                 step = 1,
                     ),
                     sliderInput(ns("slider7"), "Fish Release Weight (grams)",
                                 min = min(combinedData_df_list$All_Events$Release_Weight, na.rm = TRUE),
                                 max = max(combinedData_df_list$All_Events$Release_Weight, na.rm = TRUE),
                                 value = c(min(combinedData_df_list$All_Events$Release_Weight, na.rm = TRUE),max(combinedData_df_list$All_Events$Release_Weight, na.rm = TRUE)),
                                 step = 1,
                     ),
                     dateRangeInput(ns("drangeinput3"), "Release Date Range:",
                                    start = min(combinedData_df_list$All_Events$Release_Date, na.rm = TRUE) - 1,
                                    end = max(combinedData_df_list$All_Events$Release_Date, na.rm = TRUE) + 1), #end of date range input
                     pickerInput(ns("picker3"),
                                 label = "Select Release Site:",
                                 choices = sort(unique(combinedData_df_list$All_Events$ReleaseSite)),
                                 selected = unique(combinedData_df_list$All_Events$ReleaseSite),
                                 multiple = TRUE,
                                 options = list(
                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                 )
                     ), #end of picker 3 input
                     
                     checkboxInput(ns("dischargeDataFilter"), "Display USGS Discharge Filter"),
                     uiOutput(ns("DischargeFilterUI")), 
                     
                     
                     
                     checkboxInput(ns("PTFilters"), "Display Environmental Data Filters"),
                     uiOutput(ns("PTFiltersUI")), 

                     checkboxInput(ns("checkbox1"), "Remove Duplicate Days, TAGs, Events and UTMs"),
                     checkboxInput(ns("checkbox2"), "Remove Duplicate TAGs: doesn't work with TAG filter"), #deliberate decision not to add another if statement to have it actually work because it doesn't make sense you would use both at the same time
                     actionButton(ns("button2"), "Reset Filters"),
                     h6("Note: If there are any NA entries in any of the applicable filters (especially the numeric ones), those rows are excluded from the table."),
                     hr(),

                     actionButton(ns("button3"), label = "Render Table", width = "100%")
                   ),#end of all events and plot sidebar panel
                   mainPanel(
                     tabsetPanel(tabPanel("All Events",
                                          br(),
                                          downloadData_UI(ns("downloadallevents1")), 
                       
                       withSpinner(DT::dataTableOutput(ns("allevents1"))),
                      ),
                      tabPanel("Plot",
                               br(),
                               box(title = "Raw Detections Time Series", 
                                   width = 10, 
                                   withSpinner(plotlyOutput(ns("plot5")))
                                   ),
                               box(title = "Raw Detection Frequencies by Event", 
                                   width = 10, 
                                   withSpinner(DT::dataTableOutput(ns("alleventsfrequencies1")))
                                   )
                               
                               ),
                     )#end of tabset panel
                   )#end of all events and plot mainpanel
                 )# end of all events and plot sidebarLayout
  )
}

AllEncounters_Server <- function(id, combinedData_df_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$DischargeFilterUI <- renderUI({
        req(input$dischargeDataFilter)
        tagList(
          h6("Note: Discharge is measured from USGS Gauge at Hitching Post and is associated with detections if measurement was within 13 hours of detection"),
          sliderInput(ns("sliderDischarge"), "USGS Discharge (CFS)",
                      min = min(combinedData_df_list$All_Events$USGSDischarge, na.rm = TRUE),
                      max = max(combinedData_df_list$All_Events$USGSDischarge, na.rm = TRUE),
                      value = c(min(combinedData_df_list$All_Events$USGSDischarge, na.rm = TRUE),max(combinedData_df_list$All_Events$USGSDischarge, na.rm = TRUE)),
                      step = 1,
          )
        )
      })
      
      output$PTFiltersUI <- renderUI({
        req(input$PTFilters)
        
        tagList(
          h6("For detections at specific Stationary Site, PT data is associated if detection was within 1 hour of measurement."),
         
          sliderInput(ns("sliderWaterPressure"), "Water Pressure (PSI)",
                      min = min(combinedData_df_list$All_Events$Water_Pres_psi, na.rm = TRUE),
                      max = max(combinedData_df_list$All_Events$Water_Pres_psi, na.rm = TRUE),
                      value = c(min(combinedData_df_list$All_Events$Water_Pres_psi, na.rm = TRUE),max(combinedData_df_list$All_Events$Water_Pres_psi, na.rm = TRUE)),
                      
          ),
          
          sliderInput(ns("sliderWaterTemp"), "Water Temp (F)",
                      min = min(combinedData_df_list$All_Events$Water_Temp_F, na.rm = TRUE),
                      max = max(combinedData_df_list$All_Events$Water_Temp_F, na.rm = TRUE),
                      value = c(min(combinedData_df_list$All_Events$Water_Temp_F, na.rm = TRUE),max(combinedData_df_list$All_Events$Water_Temp_F, na.rm = TRUE)),
                      step = 1,
          ),
          
          sliderInput(ns("sliderBaromPres"), "Barometric Pressure (PSI)",
                      min = min(combinedData_df_list$All_Events$Barom_Pres_psi, na.rm = TRUE),
                      max = max(combinedData_df_list$All_Events$Barom_Pres_psi, na.rm = TRUE),
                      value = c(min(combinedData_df_list$All_Events$Barom_Pres_psi, na.rm = TRUE),max(combinedData_df_list$All_Events$Barom_Pres_psi, na.rm = TRUE)),
                      
          ),
          
          sliderInput(ns("sliderAirTemp"), "Air Temp (F)",
                      min = min(combinedData_df_list$All_Events$Air_Temp_F, na.rm = TRUE),
                      max = max(combinedData_df_list$All_Events$Air_Temp_F, na.rm = TRUE),
                      value = c(min(combinedData_df_list$All_Events$Air_Temp_F, na.rm = TRUE), max(combinedData_df_list$All_Events$Air_Temp_F, na.rm = TRUE)),
                      step = 1
          ),
          
          sliderInput(ns("sliderWaterLevel"), "Water Level (ft)",
                      min = min(combinedData_df_list$All_Events$Water_Level_NoIce_ft, na.rm = TRUE),
                      max = max(combinedData_df_list$All_Events$Water_Level_NoIce_ft, na.rm = TRUE),
                      value = c(min(combinedData_df_list$All_Events$Water_Level_NoIce_ft, na.rm = TRUE), max(combinedData_df_list$All_Events$Water_Level_NoIce_ft, na.rm = TRUE)),
                      
          ),
          h6("This is Water_Level_NoIce_ft. Readings of 0 are Ice-on."),
          
          checkboxGroupInput(ns("radioFilterIce"), label = "Filter Ice", 
                       choices = unique(combinedData_df_list$All_Events$Filter_Ice), 
                       selected = unique(combinedData_df_list$All_Events$Filter_Ice), 
                       inline = TRUE
                       ),
          
          pickerInput(ns("environmentalSite"),
                      label = "Select PT Logger Site",
                      choices = sort(unique(combinedData_df_list$All_Events$Site)),
                      selected = unique(combinedData_df_list$All_Events$Site),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE #this makes the "select/deselect all" option
                      )
          )
        )
        
      
      })
      
      observeEvent(input$button2, {
        
        updateTextInput(session, "textinput1",
                        value = "")
        
        updateDateRangeInput(session, "drangeinput2",
                             start = "2020-08-01",
                             end = max(combinedData_df_list$All_Events$Date) + 1)
        
        updateDateRangeInput(session, "drangeinput3",
                             start = min(combinedData_df_list$All_Events$Release_Date, na.rm = TRUE) - 1,
                             end = max(combinedData_df_list$All_Events$Release_Date, na.rm = TRUE) + 1
        )
        
        updatePickerInput(session, "picker1",
                          selected = unique(combinedData_df_list$All_Events$Event)
        )
        
        updatePickerInput(session, "picker2",
                          selected = unique(combinedData_df_list$All_Events$Species)
        )
        
        updatePickerInput(session, "picker3",
                          selected = unique(combinedData_df_list$All_Events$ReleaseSite)
        )
        
        updateCheckboxInput(session, "checkbox1",
                            value = NULL)
        
        updateCheckboxInput(session, "checkbox2",
                            value = NULL)
        
        updateSliderInput(session, "slider1",
                          value = c(min(lubridate::hour(combinedData_df_list$All_Events$Datetime)),max(lubridate::hour(combinedData_df_list$All_Events$Datetime)))
        )
        
        updateSliderInput(session, "slider6",
                          value = c(
                            min(combinedData_df_list$All_Events$Release_Length, na.rm = TRUE), 
                            max(combinedData_df_list$All_Events$Release_Length, na.rm = TRUE)
                          )
        )
        
        updateSliderInput(session, "slider7",
                          value = c(
                            min(combinedData_df_list$All_Events$Release_Weight, na.rm = TRUE), 
                            max(combinedData_df_list$All_Events$Release_Weight, na.rm = TRUE)
                          )
        )
        
        updateSliderInput(session, "sliderDischarge",
                          value = c(min(combinedData_df_list$All_Events$USGSDischarge, na.rm = TRUE),max(combinedData_df_list$All_Events$USGSDischarge, na.rm = TRUE)),
                          
        )
        updateSliderInput(session,"sliderWaterPressure",
                          value = c(min(combinedData_df_list$All_Events$Water_Pres_psi, na.rm = TRUE),max(combinedData_df_list$All_Events$Water_Pres_psi, na.rm = TRUE)),
                          
        )
        
        updateSliderInput(session,"sliderWaterTemp",
                          value = c(min(combinedData_df_list$All_Events$Water_Temp_F, na.rm = TRUE),max(combinedData_df_list$All_Events$Water_Temp_F, na.rm = TRUE)),
                          
        )
        
        updateSliderInput(session, "sliderBaromPres",
                          value = c(min(combinedData_df_list$All_Events$Barom_Pres_psi, na.rm = TRUE),max(combinedData_df_list$All_Events$Barom_Pres_psi, na.rm = TRUE)),
                          
        )
        
        updateSliderInput(session, "sliderAirTemp",
                          value = c(min(combinedData_df_list$All_Events$Air_Temp_F, na.rm = TRUE), max(combinedData_df_list$All_Events$Air_Temp_F, na.rm = TRUE)),
                          
        )
        
        updateSliderInput(session,"sliderWaterLevel",
                          value = c(min(combinedData_df_list$All_Events$Water_Level_NoIce_ft, na.rm = TRUE), max(combinedData_df_list$All_Events$Water_Level_NoIce_ft, na.rm = TRUE)),
                          
        )
        
        updateCheckboxGroupInput(session, "radioFilterIce",
                                 selected = unique(combinedData_df_list$All_Events$Filter_Ice)
        )
        
        updatePickerInput(session, "environmentalSite",
                    selected = unique(combinedData_df_list$All_Events$Site)
        )
        
      }) #end of reset
      
#ALL Events and Plot Reactive --------------------------------------------

        # enc_release_data wasn't registering bc i used reactive() instead of reactive ({}).
        # i guess reactive ({}) makes it so you can make multiple expressions within a reactive context whereas reactive() can only do 1
        all_events_data <- eventReactive(list(input$button3, input$keys), ignoreNULL = FALSE,{
          
          All_Events <- combinedData_df_list$All_Events
          if(input$dischargeDataFilter){
            validate(need(isTruthy(input$sliderDischarge), "Needs USGS filter slider to load before clicking 'Render Table'. Re-click 'Render Table' with slider present and wait 15 seconds."))
            All_Events <- All_Events %>%
              filter(
                USGSDischarge >= input$sliderDischarge[1] & USGSDischarge <= input$sliderDischarge[2]
              )
          }
          
          if(input$PTFilters){
            validate(need(isTruthy(input$sliderWaterPressure), "Needs Environmental filters to load before clicking 'Render Table'. Re-click 'Render Table' with slider present and wait 15 seconds."))
            
            All_Events <- All_Events %>%
              filter(
                Water_Pres_psi >= input$sliderWaterPressure[1] & Water_Pres_psi <= input$sliderWaterPressure[2],
                Water_Temp_F >= input$sliderWaterTemp[1] & Water_Temp_F <= input$sliderWaterTemp[2],
                Barom_Pres_psi >= input$sliderBaromPres[1] & Barom_Pres_psi <= input$sliderBaromPres[2],
                Air_Temp_F >= input$sliderAirTemp[1] & Air_Temp_F <= input$sliderAirTemp[2],
                Water_Level_NoIce_ft >= input$sliderWaterLevel[1] & Water_Level_NoIce_ft <= input$sliderWaterLevel[2],
                (!is.null(input$radioFilterIce) & Filter_Ice %in% input$radioFilterIce),
                (Site %in% input$environmentalSite))
              
          }
            
          
          # if the Tag filter is used or not
          if(input$textinput1 !=''){
            #all events
            all_events_filtered <- All_Events  %>%
              filter(
                
                TAG %in% c(input$textinput1),
                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                lubridate::hour(Datetime) >= input$slider1[1] & lubridate::hour(Datetime) <= input$slider1[2],
                Event %in% input$picker1,
                Species %in% input$picker2,
                Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
                Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
                ReleaseSite %in% input$picker3,
                Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2],
                
              ) %>%
              arrange(Datetime)


          } else {
            all_events_filtered <- All_Events  %>%
              filter(

                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                lubridate::hour(Datetime) >= input$slider1[1] & lubridate::hour(Datetime) <= input$slider1[2],
                Event %in% input$picker1,
                Species %in% input$picker2,
                Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
                Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
                ReleaseSite %in% input$picker3,
                Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2]
              )%>%
              arrange(Datetime)

          }

          # error below solved because I wasn't using the correct variable names for each dataset
          # x `Site_Code` not found in `.data`.
          # x `Scan_Date` not found in `.data`
          ### Filtering for TAG, SIte Code, and Day

          #if there is a tag input along with the first box checked
          if (input$checkbox1 == TRUE & input$checkbox2 == FALSE & input$textinput1 !='') {

            all_events_filtered <- All_Events %>%

              filter(
                TAG == input$textinput1,
                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                lubridate::hour(Datetime) >= input$slider1[1] & lubridate::hour(Datetime) <= input$slider1[2],

                Event %in% input$picker1,
                Species %in% input$picker2,
                ReleaseSite %in% input$picker3,
                Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
                Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
                Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2]
              ) %>%
              #this part is for making sure the sequence of events will make sense sequentially: tells where a fish started and ended the day and keeps other unique entries in between
              group_by(Date) %>%
              mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                            Datetime == max(Datetime) ~ "Last_of_day",
                                            Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
              ) %>%
              ungroup() %>%
              #need to include UTM_X and UTM_Y so that you can get multiple daily detections of mobile antennas in different locations
              distinct(TAG, Event, Date, first_last, UTM_X, UTM_Y, .keep_all = TRUE) %>%
              arrange(Datetime) %>%
              select(-first_last)


          }
          #if there isn't a tag input along with first box checked
          if (input$checkbox1 == TRUE & input$checkbox2 == FALSE & input$textinput1 =='') {

            all_events_filtered <- All_Events %>%

              filter(
                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                lubridate::hour(Datetime) >= input$slider1[1] & lubridate::hour(Datetime) <= input$slider1[2],

                Event %in% input$picker1,
                Species %in% input$picker2,
                Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
                Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
                Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2],
                ReleaseSite %in% input$picker3) %>%
              #this part is for making sure the sequence of events will make sense
              # if there's no tag input then have to group_by TAG as well
              group_by(Date, TAG) %>%
              mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                            Datetime == max(Datetime) ~ "Last_of_day",
                                            Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
              ) %>%
              ungroup() %>%
              distinct(TAG, Event, Date, first_last,  UTM_X, UTM_Y, .keep_all = TRUE) %>%
              arrange(Datetime) %>%
              select(-first_last)

          }

          if (input$checkbox2 == TRUE) {

            all_events_filtered <- All_Events %>%
              filter(
                
                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                lubridate::hour(Datetime) >= input$slider1[1] & lubridate::hour(Datetime) <= input$slider1[2],

                Event %in% input$picker1,
                Species %in% input$picker2,
                Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
                Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
                Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2],
                ReleaseSite %in% input$picker3) %>%
              arrange(Datetime) %>%
              #need to have distinct() at the end of the expression

              distinct(TAG, .keep_all = TRUE)

          }
          #### filter dummy row
          all_events_filtered <- all_events_filtered %>%
            filter(!TAG %in% c("230000999999"))


          return(all_events_filtered)
        }) #end of ENC data list eventReactive
        
        downloadData_Server("downloadallevents1", all_events_data(), "AllEventsData")
        
        output$allevents1 <- renderDataTable({
              datatable(all_events_data(),
                        rownames = FALSE,
                        extensions = c('Buttons'),
                        #for slider filter instead of text input
                        filter = 'top',
                        options = list(
                          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                          dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                          language = list(emptyTable = "Enter inputs and press Render Table")
                        ) #end of options list
              ) %>%
                formatStyle(
                  columns = 1:ncol(all_events_data())
                  
                )

            })

            # Enc Hist Plot Render ----------------------------------------------------

            output$plot5 <- renderPlotly({
              plot <- all_events_data() %>%
                ggplot(aes(x= Date, fill = Event,
                           text = paste('Date: ', as.character(Date), '\n')
                )) +
                geom_bar(stat = "count", position = "dodge") +
                theme_classic() 

              ggplotly(p = plot)

            })
        
        output$alleventsfrequencies1 <- renderDataTable({
          frequenciesSummarized <- all_events_data() %>%
            filter(!TAG %in% c(230000999999)) %>%
            count(Event, name = "Raw Detections")
          
          datatable(frequenciesSummarized,
                    rownames = FALSE,
                    extensions = c('Buttons'),
                    #for slider filter instead of text input
                    filter = 'top',
                    options = list(
                      pageLength = 10, info = TRUE, lengthMenu = list(c(10,25), c("10", "25")),
                      dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                      language = list(emptyTable = "Enter inputs and press Render Table")
                    ) #end of options list
          )
          
        })
        
        

      
    }
  )
}
