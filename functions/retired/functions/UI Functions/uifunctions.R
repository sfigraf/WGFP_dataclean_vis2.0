Ind_data_ui_tab <- function(tabid, tableid, dateid, buttonid, downloadid, end_date) {
  tabPanel(tabid,
           sidebarLayout(
             sidebarPanel(
               dateRangeInput(dateid, "Select a Date Range:",
                              #was accidnetly omitting events from the allevents tab bc the earliest release date is 2020-09-01
                              #earliest detection was 2020-09-03, which was what it was set at before
                              start = "2020-08-01", 
                              end = end_date), #end of date range input
               
               actionButton(buttonid, label = "Render Table")
             ),
             mainPanel(
               hr(),
               downloadButton(outputId = downloadid, label = "Save this data as CSV"),
               hr(),
               withSpinner(DT::dataTableOutput(tableid))
             ) #end of mainPanel
                         ) #end of sidebar layout
           ) #end of tabPanel
  
}


Ind_data_reactive <- function(buttonid, dataset, dateid) {
  filtered_data <- eventReactive(input$buttonid,{
    data_filtered <- dataset %>%
      filter(Scan.Date >= input$dateid[1] & Scan.Date <= input$dateid[2])
  })
}

Ind_data_table_render <- function(filtered_data) {
  
  renderDataTable(
    
    filtered_data,
    rownames = FALSE,
    #extensions = c('Buttons'),
    #for slider filter instead of text input
    filter = 'top',
    options = list(
      pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
      dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
      language = list(emptyTable = "Enter inputs and press Render Table")
    )
  )
  
}
