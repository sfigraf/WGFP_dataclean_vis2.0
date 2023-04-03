States_UI <- function(id,states_data_list) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(      
        textInput(ns("textinput2"), label = "Filter by TAG"),
        pickerInput(ns("picker4"),
                    label = "Select number of weekly unique events:",
                    choices = sort(unique(states_data_list$All_States$weekly_unique_events)), #will need to be updated later on for uniqueness
                    selected = unique(states_data_list$All_States$weekly_unique_events),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE #this makes the "select/deselect all" option
                    )
        ), #end of picker 4 input 
        pickerInput(ns("picker5"),
                    label = "States:",
                    choices = sort(unique(states_data_list$All_States$State)), #will need to be updated later on for uniqueness
                    selected = unique(states_data_list$All_States$State),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE #this makes the "select/deselect all" option
                    )
        ), #end of picker 5 input 
        actionButton(ns("button5"), label = "Render Table/Data", width = "100%")
      ),#end of sidebar panel
      mainPanel(tabsetPanel(
        tabPanel("States Dataframe",
                 
                 br(),
                 withSpinner(DT::dataTableOutput(ns("states1")))),
        tabPanel("States and Weeks Wide",
                  br(),
                  withSpinner(DT::dataTableOutput(ns("states2")))
                          
                 )
                 
      )#end of tabsetPanel
      )#end of mainPanel
    )#end of sidebarLayout including sidebarPanel and Mainpanel
  
  )
}

States_Server <- function(id, states_data_list, weeks) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      observeEvent(input$button5,{
        updatePickerInput(session, "picker4",
                          choices = sort(unique(filtered_states_data()$All_States$weekly_unique_events)),
                          selected = unique(filtered_states_data()$All_States$weekly_unique_events)
        )
        
        updatePickerInput(session, "picker5",
                          choices = sort(unique(filtered_states_data()$All_States$State)),
                          selected = unique(filtered_states_data()$All_States$State)
        )
        
      })
      
      filtered_states_data <- eventReactive(input$button5, ignoreNULL = FALSE,{
        
        if(input$textinput2 != ''){ 
          states_data1 <- states_data_list$All_States %>%
            filter(TAG %in% c(input$textinput2),
                   weekly_unique_events %in% input$picker4,
                   State %in% input$picker5
            )%>%
            arrange(Date)
        } else { 
          states_data1 <- states_data_list$All_States %>%
            filter(
              weekly_unique_events %in% input$picker4,
              State %in% input$picker5
            )%>%
            arrange(Date)
        }
        

        #weeks <- data.frame(weeks_since = 1:max(states_data1$weeks_since))
        weeks_and_states <- full_join(weeks, states_data1, by = "weeks_since")
        
        weeks_and_states_wide <- pivot_wider(weeks_and_states, id_cols = TAG, names_from = weeks_since, values_from = State)
        #need to change this part if we decide to start at week 1 instead of 0
        weeks_and_states_wide <- weeks_and_states_wide %>%
          select(TAG, `0`, 2:ncol(weeks_and_states_wide))
        
        #putting all together in a list
        
        states_data_list <- list("filtered_states" = states_data1, "filtered_wide" =  weeks_and_states_wide)
        
        return(states_data_list)
      }) 
      
      output$states1 <- renderDT({
        
        
        datatable(filtered_states_data()$filtered_states, #initial_states_data_list()$All_States
                  rownames = FALSE,
                  extensions = c("Buttons"),
                  
                  #for slider filter instead of text input
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                    
                  )
        ) %>%
          formatStyle(
            columns = c(1:ncol(filtered_states_data()$filtered_states))
            
          )
      })
      
      
      output$states2 <- renderDT({
        
        
        datatable(filtered_states_data()$filtered_wide,
                  rownames = FALSE,
                  extensions = c("Buttons"),
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                    
                    #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
                  )
        ) 
        
        
      })
      
    
      
      
      
    }
  )
}