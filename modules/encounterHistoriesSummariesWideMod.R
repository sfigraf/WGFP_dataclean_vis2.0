#library(lubridate)
## need to 
EncounterHistoriesSummariesWide_UI <- function(id, Enc_release_data) {
  ns <- NS(id)
  tagList(
     sidebarLayout(
                 sidebarPanel(
                   textInput(ns("textinput4"), "Filter by Tag"),
                   pickerInput(inputId = ns("picker11"),
                               label = "Select Fish Species:",
                               choices = sort(unique(Enc_release_data$Species)),
                               selected = unique(Enc_release_data$Species),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               ),
                   ), #end of picker 11 input
                   sliderInput(ns("slider4"), "Fish Length (mm)",
                               min = min(Enc_release_data$Length, na.rm = TRUE),
                               max = max(Enc_release_data$Length, na.rm = TRUE),
                               value = c(min(Enc_release_data$Length, na.rm = TRUE),max(Enc_release_data$Length, na.rm = TRUE)),
                               step = 1,
                               #timeFormat = "%T",
                               #animate = animationOptions(interval = 500, loop = FALSE)
                   ),
                   sliderInput(ns("slider5"), "Fish Weight (grams)",
                               min = min(Enc_release_data$Weight, na.rm = TRUE),
                               max = max(Enc_release_data$Weight, na.rm = TRUE),
                               value = c(min(Enc_release_data$Weight, na.rm = TRUE),max(Enc_release_data$Weight, na.rm = TRUE)),
                               step = 1,
                               #timeFormat = "%T",
                               #animate = animationOptions(interval = 500, loop = FALSE)
                   ),
                   pickerInput(inputId = ns("picker12"),
                               label = "Select Release Site:",
                               choices = sort(unique(Enc_release_data$ReleaseSite)),
                               selected = unique(Enc_release_data$ReleaseSite),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 12 input
                   pickerInput(inputId = ns("picker14"),
                               label = "Total Number of Unique Events/Encounters:",
                               choices = sort(unique(Enc_release_data$TotalEncounters)),
                               selected = unique(Enc_release_data$TotalEncounters),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 14 input
                   sliderInput(ns("slider8"), "Total distance travelled (m)",
                               min = min(Enc_release_data$sum_dist, na.rm = TRUE),
                               max = max(Enc_release_data$sum_dist, na.rm = TRUE),
                               value = c(min(Enc_release_data$sum_dist, na.rm = TRUE),max(Enc_release_data$sum_dist, na.rm = TRUE)),
                               step = 1,
                   ), #end of slider8
                   pickerInput(inputId = ns("picker13"),
                               label = "Above/Below/Through the Dam:",
                               choices = sort(unique(Enc_release_data$through_dam)),
                               selected = unique(Enc_release_data$through_dam),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 13 input
                   actionButton(ns("button6"), label = "Render Table/Data", width = "100%")
                 ), #end of sidebar panel for enc_release wide_summary
                 mainPanel(hr(),
                           
                           withSpinner(DT::dataTableOutput(ns("enc_release1"))),
                           )#end of mainpanel for enc_hist_wide
               ),#end of enc_hist_wide sidebar_layout
   
  )
}

EncounterHistoriesSummariesWide_Server <- function(id, Enc_release_data) {
  moduleServer(
    id,
    function(input, output, session) {
      enc_hist_wide_filtered <- eventReactive(input$button6,{
            ##gona have to change a lot of outputs later based on what this is named


            # if the Tag filter is used or not
            if(input$textinput4 !=''){

              Enc_release_data_filtered <- Enc_release_data %>%
                filter(
                  TAG %in% c(input$textinput4),
                  Species %in% input$picker11,
                  ReleaseSite %in% input$picker12,
                  Length >= input$slider4[1] & Length <= input$slider4[2],
                  Weight >= input$slider5[1] & Weight <= input$slider5[2],
                  sum_dist >= input$slider8[1] & sum_dist <= input$slider8[2],
                  through_dam %in% input$picker13,
                  TotalEncounters %in% input$picker14
                )

            } else {

              Enc_release_data_filtered <- Enc_release_data %>%
                filter(
                  Species %in% input$picker11,
                  ReleaseSite %in% input$picker12,
                  Length >= input$slider4[1] & Length <= input$slider4[2],
                  Weight >= input$slider5[1] & Weight <= input$slider5[2],
                  sum_dist >= input$slider8[1] & sum_dist <= input$slider8[2],
                  through_dam %in% input$picker13,
                  TotalEncounters %in% input$picker14
                )
            }

            return(Enc_release_data_filtered)
          }) #end ns(of ENC data list eventReactive
      
      output$enc_release1 <- renderDataTable({
        
        datatable(enc_hist_wide_filtered(),
        rownames = FALSE,
        extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
          language = list(emptyTable = "Enter inputs and press Render Table")
        )
        )
      })
      
    }
    
  )
}