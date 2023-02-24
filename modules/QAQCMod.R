QAQC_UI <- function(id, df_list) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Marker Tags",
               sidebarLayout(
                 sidebarPanel(
                   pickerInput(ns("picker8"),
                               label = "Select Site Code",
                               choices = sort(unique(df_list$Marker_Tag_data$SCD)),
                               selected = unique(df_list$Marker_Tag_data$SCD),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 8
                   pickerInput(ns("picker9"),
                               label = "Select Marker Tag",
                               choices = sort(unique(df_list$Marker_Tag_data$TAG)),
                               selected = unique(df_list$Marker_Tag_data$TAG)[1],
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 9 
                   sliderInput(ns("slider3"),
                               "Date",
                               min = min(df_list$Marker_Tag_data$DTY -1),
                               max = max(df_list$Marker_Tag_data$DTY +1),  
                               value = c(min(df_list$Marker_Tag_data$DTY -1),max(df_list$Marker_Tag_data$DTY +1)),
                               step = 1,
                               timeFormat = "%d %b %y",
                               #animate = animationOptions(interval = 500, loop = FALSE)
                   ),
                   actionButton(ns("button8"), label = "Render Marker Tag Plot")
                 ), #end of sidebar panel
                 mainPanel(
                   splitLayout(
                     
                     withSpinner(DT::dataTableOutput(ns("markertags1"))),
                     
                     withSpinner(plotlyOutput(ns("plot2")))
                     
                   ),
                   # hr(),
                   # downloadButton(outputId = "download7", label = "Save this data as CSV"),
                   # hr(),
                 )#end of mainpanel
               )#end of sidebar layout
      ), #end of marker tag tab
      tabPanel("Release and Recap Length/Weights",
               fluidRow(
                 column(
                   width = 8,
                   br(),
                   withSpinner(plotlyOutput(ns("plot3")))
                 ),#end of column
                 column(
                   br(),
                   width = 4,
                   withSpinner(plotlyOutput(ns("plot4")))
                   
                 )#end of column
               )#end of fluidrow
      ), #end of tabPanel
      tabPanel("Unknown Tags",
               br(),
               withSpinner(DT::dataTableOutput(ns("unknowntags1")))
      )#end of tab panel
    )#end of tabset Panel 
  )
}

QAQC_Server <- function(id, df_list, Release_05, Recaptures_05) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filtered_markertag_data <- eventReactive(input$button8,{
        
        markertag_data1 <- df_list$Marker_Tag_data %>%
          filter(SCD %in% c(input$picker8),
                 TAG %in% c(input$picker9),
                 DTY >= input$slider3[1] & DTY <= input$slider3[2]
          )
        return(markertag_data1)
      }) 
      
      # MarkerTag Plot Output ---------------------------------------------------
      
      output$plot2 <- renderPlotly({
        plot2 <- filtered_markertag_data() %>%
          ggplot(aes(x = DTY, y = CleanARR, color = SCD, text = paste(TAG) )) +
          geom_point() +
          theme_classic() +
          theme(
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank())
        
        
        plotly2 <- ggplotly(p = plot2)
        plotly2
      })    
      
      
      # Release and Recap Data L/W Plot Output --------------------------------------------
      output$plot3 <- renderPlotly({
        plot3 <- Release_05 %>%
          ggplot(aes(x = Length, y = Weight, color = Species)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Release Data")
        
        plotly3 <- ggplotly(plot3) 
        plotly3
      })    
      
      output$plot4 <- renderPlotly({
        plot4 <- Recaptures_05 %>%
          ggplot(aes(x = Length, y = Weight, color = Species)) +
          geom_point() + 
          theme_classic() +
          labs(title = "Recapture Data")
        
        plotly4 <- ggplotly(plot4) 
        plotly4
      })
      
      output$markertags1 <- renderDT({
        datatable(filtered_markertag_data(),
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        ) 
      })
      
      output$unknowntags1 <- renderDT({
        datatable(unknown_tags,
                  rownames = FALSE,
                  selection = "single",
                  filter = 'top',
                  caption = ("Tags that initially started  with 900_ but are not in the release file."),
                  options = list(
                    #statesave is restore table state on page reload
                    stateSave =TRUE,
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        ) 
      })
    }
  )
}