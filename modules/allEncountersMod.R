AllEncounters_UI <- function(id, df_list) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
                   sidebarPanel(
                     textInput(ns("textinput1"), "Filter by Tag"),
                     dateRangeInput(ns("drangeinput2"), "Select a Date Range:",
                                    start = "2020-08-01",
                                    end = max(df_list$All_Events$Date) + 1), #end of date range input
                     sliderInput(ns("slider1"), "Hour of Day",
                                 min = min(hour(df_list$All_Events$Datetime)),
                                 max = max(hour(df_list$All_Events$Datetime)),
                                 value = c(min(hour(df_list$All_Events$Datetime)),max(hour(df_list$All_Events$Datetime))),
                                 step = 1,
                                 #timeFormat = "%T",
                                 #animate = animationOptions(interval = 500, loop = FALSE)
                     ),
                     pickerInput(ns("picker1"),
                                 label = "Select Event",
                                 choices = unique(df_list$All_Events$Event),
                                 selected = unique(df_list$All_Events$Event),
                                 multiple = TRUE,
                                 options = list(
                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                 ),
                     ), #end of picker input
                     pickerInput(ns("picker2"),
                                 label = "Select Fish Species:",
                                 choices = sort(unique(df_list$All_Events$Species)),
                                 selected = unique(df_list$All_Events$Species),
                                 multiple = TRUE,
                                 options = list(
                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                 ),
                     ), #end of picker 2 input
                     sliderInput(ns("slider6"), "Fish Release Length (mm)",
                                 min = min(df_list$All_Events$Release_Length, na.rm = TRUE),
                                 max = max(df_list$All_Events$Release_Length, na.rm = TRUE),
                                 value = c(min(df_list$All_Events$Release_Length, na.rm = TRUE),max(df_list$All_Events$Release_Length, na.rm = TRUE)),
                                 step = 1,
                     ),
                     sliderInput(ns("slider7"), "Fish Release Weight (grams)",
                                 min = min(df_list$All_Events$Release_Weight, na.rm = TRUE),
                                 max = max(df_list$All_Events$Release_Weight, na.rm = TRUE),
                                 value = c(min(df_list$All_Events$Release_Weight, na.rm = TRUE),max(df_list$All_Events$Release_Weight, na.rm = TRUE)),
                                 step = 1,
                     ),
                     dateRangeInput(ns("drangeinput3"), "Release Date Range:",
                                    start = min(df_list$All_Events$Release_Date, na.rm = TRUE) - 1,
                                    end = max(df_list$All_Events$Release_Date, na.rm = TRUE) + 1), #end of date range input
                     pickerInput(ns("picker3"),
                                 label = "Select Release Site:",
                                 choices = sort(unique(df_list$All_Events$ReleaseSite)),
                                 selected = unique(df_list$All_Events$ReleaseSite),
                                 multiple = TRUE,
                                 options = list(
                                   `actions-box` = TRUE #this makes the "select/deselect all" option
                                 )
                     ), #end of picker 3 input

                     checkboxInput(ns("checkbox1"), "Remove Duplicate Days, TAGs, Events and UTMs"),
                     checkboxInput(ns("checkbox2"), "Remove Duplicate TAGs: doesn't work with TAG filter"), #deliberate decision not to add another if statement to have it actually work because it doesn't make sense you would use both at the same time
                     actionButton(ns("button2"), "Reset Filters"),
                     hr(),
                     #submit button is limited in scope, doesn't even have a input ID , but works for controlling literally all inputs
                     #submitButton("Update inputs", icon("sync"))

                     actionButton(ns("button3"), label = "Render Table", width = "100%")
                   ),#end of all events and plot sidebar panel
                   mainPanel(
                     tabsetPanel(tabPanel("All Events",
                                          br(),
                       
                       withSpinner(DT::dataTableOutput(ns("allevents1"))),
                      ),
                      tabPanel("Plot",
                               br(),
                               plotlyOutput(ns("plot5"))
                               )
                     )#end of tabset panel
                   )#end of all events and plot mainpanel
                 ),# end of all events and plot sidebarLayout
  )
}

AllEncounters_Server <- function(id, df_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$button2, {

        updateTextInput(session, "textinput1",
                        value = "")

        updateDateRangeInput(session, "drangeinput2",
                             start = "2020-08-01",
                             end = max(df_list$All_Events$Date) + 1)

        updatePickerInput(session, "picker1",
                          selected = unique(df_list$All_Events$Event)
        )

        updatePickerInput(session, "picker2",
                          selected = unique(df_list$All_Events$Species)
        )

        updatePickerInput(session, "picker3",
                          selected = unique(df_list$All_Events$ReleaseSite)
        )

        updateCheckboxInput(session, "checkbox1",
                            value = NULL)

        updateCheckboxInput(session, "checkbox2",
                            value = NULL)

        updateSliderInput(session, "slider1",
                          value = c(min(hour(All_events$Datetime)),max(hour(All_events$Datetime)))
        )

      }) #end of reset
      
#ALL Events and Plot Reactive --------------------------------------------



        # enc_release_data wasn't registering bc i used reactive() instead of reactive ({}).
        # i guess reactive ({}) makes it so you can make multiple expressions within a reactive context whereas reactive() can only do 1
        all_events_data <- eventReactive(input$button3,{
          # if the Tag filter is used or not
          if(input$textinput1 !=''){
            #all events
            all_events_filtered <- df_list$All_Events  %>%
              filter(

                TAG %in% c(input$textinput1),
                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],
                Event %in% input$picker1,
                Species %in% input$picker2,
                Release_Length >= input$slider6[1] & Release_Length <= input$slider6[2],
                Release_Weight >= input$slider7[1] & Release_Weight <= input$slider7[2],
                ReleaseSite %in% input$picker3,
                Release_Date >= input$drangeinput3[1] & Release_Date <= input$drangeinput3[2]
              ) %>%
              arrange(Datetime)


          } else {
            all_events_filtered <- df_list$All_Events  %>%
              filter(

                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],
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

            all_events_filtered <- df_list$All_Events %>%

              filter(
                TAG == input$textinput1,
                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],

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

            all_events_filtered <- df_list$All_Events %>%

              filter(
                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],

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



            all_events_filtered <- df_list$All_Events %>%
              filter(
                
                Date >= input$drangeinput2[1] & Date <= input$drangeinput2[2],
                hour(Datetime) >= input$slider1[1] & hour(Datetime) <= input$slider1[2],

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
        
        output$allevents1 <- renderDataTable({
              datatable(all_events_data(),
                        rownames = FALSE,
                        extensions = c('Buttons'),
                        #for slider filter instead of text input
                        filter = 'top',
                        options = list(
                          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
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
                theme_classic() +
                labs(title = "Raw Detections Frequency")


              plotly5 <- ggplotly(p = plot)
              plotly5

            })

      
    }
  )
}