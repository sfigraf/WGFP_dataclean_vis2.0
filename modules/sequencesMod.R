Sequences_UI <- function(id, antennaChoices) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   pickerInput(ns("antennas1"),
                               label = "Select Downstream Antenna(s) in Sequence:",
                               choices = antennaChoices,
                               selected = antennaChoices[1],
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE 
                               )
                   ), 
                   pickerInput(ns("antennas3_0"),
                               label = "Select Upstream Antenna(s) in Sequence:",
                               choices = antennaChoices,
                               selected = antennaChoices[3],
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), 
                   div(id = ns("container")),
                   div(class = "btn-group", 
                       actionButton(ns("add"), label = "Add Site"),
                       actionButton(ns("dropAntennaButton"), label = "Drop Site")
                   ),
                   br(), 
                   uiOutput(ns("dateSliderUI")),
                   actionButton(ns("renderbutton"), label = "Render"),
                   
      ), 
      mainPanel(width = 10,
                br(),
                uiOutput(ns("sequencestableUI"))
      )
    )
    
  )
}

Sequences_Server <- function(id, All_Events, antennaChoices, mobileCodes) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      boxTitle <- reactiveVal("")
      counter <- reactiveVal(0)
      
      All_Events <- All_Events %>%
        dplyr::filter(!TAG %in% c("230000999999"))
      
      observeEvent(input$add, {
        counter(counter() + 1)
        insertUI(
          selector = paste0("#", ns("container")),
          where = "beforeEnd",
          ui = div(id = ns(paste0("div", counter())),
                   pickerInput(ns(paste0("antennas3_", counter())),
                               label = "Select Next Antenna(s) in Sequence:",
                               choices = antennaChoices,
                               selected = character(0),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               ))
          )
        )
      })
      
      observeEvent(input$dropAntennaButton, {
        if (counter() > 0) {
          removeUI(
            selector = paste0("#", ns(paste0("div", counter())))
          )
          counter(counter() - 1)
        }
      })
      
      filteredData <- eventReactive(input$renderbutton, {
        upstream_antenna <- input[[paste0("antennas3_", counter())]]
        validate(
          need(length(input$antennas1) > 0, "Please select at least one antenna from Downstream Antennas."),
          need(length(input$antennas3_0) > 0, "Please select at least one antenna from Upstream Antennas."), 
          need(input$antennas3_0 != input$antennas1,"First and last antennas in sequence can't be identical without middle antennas."), 
          need(!input$antennas1 %in% input$antennas3_0,"Last antenna in sequence can't contain first antenna in sequence"), 
          need(length(upstream_antenna) > 0, "Last Antenna in Sequence can't be blank")
        )
        #gets list of all antennas in the middle
        middle_antennas <- if (counter() > 0) {
          lapply(0:(counter() - 1), function(i) {
            input[[paste0("antennas3_", i)]]
          }) #%>% unlist()
        } else {
          character(0)
        }
        middleAnts <<- middle_antennas
        print(paste("middle antennas", middle_antennas))
        
        print(paste("upstream", upstream_antenna))
        
        
        newTitle <- paste("Instances of Detections Between", 
                          paste(input$antennas1, collapse = ", "), 
                          "and", 
                          paste(upstream_antenna, collapse = ", "), 
                          "with middle antennas", 
                          paste(middle_antennas, collapse = ", "))
        
        boxTitle(newTitle)
        
        dateFilteredData <- All_Events %>%
          dplyr::filter(Date >= input$dateSlider[1] & Date <= input$dateSlider[2])
        
        data <- summarizedDf(dateFilteredData, input$antennas1, middle_antennas, upstream_antenna, mobileCodes)
        
        dataMovements <- data %>%
          dplyr::filter(MovementDirection %in% input$UpstreamDownstreamFilter)
        
        return(dataMovements)
      })
      
      output$sequencestableUI <- renderUI({
        tagList(
          box(title = boxTitle(), 
              withSpinner(DTOutput(ns("sequencesTable")))
          )
        )
      })
      
      output$dateSliderUI <- renderUI({
        tagList(
          sliderInput(ns("dateSlider"), "Date",
                      min = min(All_Events$Date - 1),
                      max = max(All_Events$Date + 1),  
                      value = c(min(All_Events$Date - 1), max(All_Events$Date + 1)),
                      step = 1,
                      timeFormat = "%d %b %y"
          ), 
          pickerInput(ns("UpstreamDownstreamFilter"), 
                      "Movement Type", 
                      choices = c("Upstream", "Downstream"), 
                      selected = c("Upstream", "Downstream"), 
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE 
                      )
          )
        )
      })
      
      output$sequencesTable <- renderDT({
        datatable(
          filteredData(),
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          options = list(
            stateSave = TRUE,
            pageLength = 10,
            info = TRUE,
            lengthMenu = list(c(10, 25, 50, 100, 200), c("10", "25", "50", "100", "200")),
            dom = 'lfrtip',
            language = list(emptyTable = "No Instances of Detections Between Selected Antenna Sites")
          )
        )
      })
    }
  )
}