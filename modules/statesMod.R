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
                 # hr(),
                 # downloadButton(outputId = "download4", label = "Save this data as CSV"),
                 # hr(),
                 br(),
                 withSpinner(DT::dataTableOutput(ns("states1")))),
        tabPanel("States and Weeks Wide",
                  br(),
                  verbatimTextOutput(ns("text1")),
                  br(),
                  withSpinner(DT::dataTableOutput(ns("states2")))
                          
                 )
                 # hr(),
                 # downloadButton(outputId = "download5", label = "Save this data as CSV"),
                 # hr(),
                 
        # tabPanel("Unknown States",
        #          hr(),
        #          withSpinner(DT::dataTableOutput(ns("unknownstates1"))),
        #) #end of tabpanel
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
      
      filtered_states_data <- eventReactive(input$button5,{
        
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
      
      output$text1 <- renderPrint({
        "Sidebar filters also work on Wide Data"
      })
      
      output$states2 <- renderDT({
        #input$button5
        
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
      
      # output$unknownstates1 <- renderDT({   
      #   
      #   datatable(states_data_list$Flagged_movements,
      #             rownames = FALSE,
      #             caption = "2022-12-16: 'unknown'states df needs some work so don't pay too much atantion to this. -SG. this hopefully should be pretty small...filled with tags with detections before official 'Release' such as in in May 2021 and tags without release info.",
      #             filter = 'top',
      #             options = list(
      #               pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
      #               dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
      #               language = list(emptyTable = "Enter inputs and press Render Table")
      #               
      #               #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
      #             )
      #   ) 
      #   
      # })
      
      
      
    }
  )
}