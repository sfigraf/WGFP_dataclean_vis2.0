Sequences_UI <- function(id, antennaChoices) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    sidebarLayout(
      
      sidebarPanel(width = 2,
                   pickerInput(ns("antennas1"),
                               label = "Select First Antenna(s) in Sequence:",
                               choices = antennaChoices,
                               selected = antennaChoices[1],
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE 
                               )
                   ), 
                   pickerInput(ns("antennas3_0"),
                               label = "Select Last Antenna(s) in Sequence:",
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
                   checkboxInput(ns("avianPrediationExclude"), "Exclude Avian Predation Tags"),
                   actionButton(ns("renderbutton"), label = "Render"),
                   
      ), 
      mainPanel(width = 10,
                br(),
                downloadData_UI(ns("downloadsequenceData")),
                uiOutput(ns("sequencestableUI"))
      )
    )
    
  )
}

Sequences_Server <- function(id, All_Events, antennaChoices, mobileCodes, AvianPredation) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      #keeps track of title for data
      boxTitle <- reactiveVal("")
      #keeps track of making the pickerInputs
      counter <- reactiveVal(0)
      #keeps track of IDs of pickerInputs that change so that we can change the labels on them
      previousIDs <- reactiveVal(c())
      
      #if the add button is pressed, make a new pickerInput and add 1 to the counter for the next ID
      observeEvent(input$add, {
        counter(counter() + 1)
        
        newID <- paste0("antennas3_", counter())
        previousIDs(c("antennas3_0", previousIDs(), newID))
        #changing all "middle antennas" labels to "next antenna in sequence"

        lapply(previousIDs(), function(id) {
          shinyjs::runjs(sprintf('$("#%s-label").text("Select Next Antenna(s) in Sequence:");', ns(id)))
        })
        
        insertUI(
          selector = paste0("#", ns("container")),
          where = "beforeEnd",
          ui = div(id = ns(paste0("div", counter())),
                   pickerInput(ns(newID),
                               label = "Select Last Antenna(s) in Sequence:",
                               choices = antennaChoices,
                               selected = character(0),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               ))
          )
        )
      })
      
      #if we drop a sitem remove the UI of that pickerInput, decrease counter number and change the label of "last antenna"
      observeEvent(input$dropAntennaButton, {
        if (counter() > 0) {
          removeUI(
            selector = paste0("#", ns(paste0("div", counter())))
          )
          counter(counter() - 1)
        }
        #changing most recently made pickerInput to "last antenna"
        idToChange <- paste0("antennas3_", counter())
          shinyjs::runjs(sprintf('$("#%s-label").text("Select Last Antenna(s) in Sequence:");', ns(idToChange)))

      })
      
      
      #on render button press,
      sequencesData <- eventReactive(input$renderbutton, { #ignoreNULL = FALSE,
        #get which pickerInput is last antenna(s)
        lastAntenna <- input[[paste0("antennas3_", counter())]]
        #get the inputs of middle antenna(s) in a list
        middle_antennas <- if (counter() > 0) {
          lapply(0:(counter() - 1), function(i) {
            input[[paste0("antennas3_", i)]]
          }) 
        } else {
          character(0)
        }
        #check for conditions which the code shouldn't run on and send message to user
        validate(
          need(length(input$antennas1) > 0, "Please select at least one antenna from First Antennas."),
          need(length(lastAntenna) > 0, "Please select at least one antenna from Last Antennas."), 
          need(all(input$antennas3_0 != input$antennas1),"First and last antennas in sequence can't be identical without middle antennas."), 
          need(!input$antennas1 %in% input$antennas3_0,"Last antenna in sequence can't contain first antenna in sequence"), 
          need(!any(unlist(middle_antennas) %in% lastAntenna), "Cannot have last antenna in any middle antenna selection")
        )
        
        #get correct box title based on inputs and change boxTitle to that title
        newTitle <- paste("Instances of Detections Between", 
                          paste(input$antennas1, collapse = ", "), 
                          "and", 
                          paste(lastAntenna, collapse = ", "), 
                          "with middle antennas", 
                          paste(middle_antennas, collapse = ", "))
        
        boxTitle(newTitle)
        #filter by date input
        dateFilteredData <- All_Events %>%
          dplyr::filter(Date >= input$dateSlider[1] & Date <= input$dateSlider[2])
        
        if(input$avianPrediationExclude){
          dateFilteredData <- dateFilteredData %>%
            dplyr::filter(!TAG %in% unique(AvianPredation$TagID))
        }
        
        #get Sequences
        sequences <- getSequences(dateFilteredData, input$antennas1, middle_antennas, lastAntenna, mobileCodes)
        # sequenceswReleaseData <- sequences %>%
        #   left_join()
        
        return(sequences)
      })
      
      #be able to download resultant data
      downloadData_Server("downloadsequenceData", sequencesData(), paste0("Sequences", paste(input$antennas1, collapse = "_"), "To", input[[paste0("antennas3_", counter())]]))
      
      #make table with reactive title abse don inputs
      output$sequencestableUI <- renderUI({
        tagList(
          box(title = boxTitle(), 
              withSpinner(DTOutput(ns("sequencesTable")))
          )
        )
      })
      
      #make dateSlider
      output$dateSliderUI <- renderUI({
        tagList(
          sliderInput(ns("dateSlider"), "Date",
                      min = min(All_Events$Date - 1),
                      max = max(All_Events$Date + 1),  
                      value = c(min(All_Events$Date - 1), max(All_Events$Date + 1)),
                      step = 1,
                      timeFormat = "%d %b %y"
          )
        )
      })
      
      #output table
      output$sequencesTable <- renderDT({
        datatable(
          sequencesData(),
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          options = list(
            #stateSave = TRUE,
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
