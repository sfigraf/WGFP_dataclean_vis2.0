## movements module
counterButton <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(actionButton(ns("button"), label = label),
                   actionButton(ns("button1"), label = label),
                    sliderInput(ns("pointSize_Slider111"), "Select Size of Point", 
                                               min = 1,
                                               max = 2000,
                                               value = 1000),
                                 ),
      mainPanel(
        verbatimTextOutput(ns("out")),
        plotlyOutput(ns("plot999"))
      )
    )
    
    
  )
}

counterServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      count <- reactiveVal(0)
      observeEvent(input$button, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      count
      
      observe({
        
        
        
        output$plot999 <- renderPlotly({
          
          data %>%
            ggplot(aes(x = Length, y = Weight, color = Species)) +
            geom_point() +
            theme_classic() +
            labs(title = "Release1 Data")
        })
      })
      
      
    }
  )
}

# test_UI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     # tagList(
#       actionButton(ns("button"), label = label),
#       verbatimTextOutput(ns("out"))
#     # )
#    
#              # sidebarLayout(
#              #   sidebarPanel(
#              #     sliderInput(ns("pointSize_Slider111"), "Select Size of Point", 
#              #                 min = 1, 
#              #                 max = 300, 
#              #                 value = 10),
#              #   ), 
#              #   mainPanel(
#              #     plotlyOutput(ns("plot999"))
#              #   )
#              # )
#              
#   
#   )
# }
# 
# test_Server <- function(id, data) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#       
#       count <- reactiveVal(0)
#       observeEvent(input$button, {
#         count(count() + 1)
#       })
#       output$out <- renderText({
#         count()
#       })
#       count
#     }
#   )
#       
#       # observe({
#       #   Release1 <- data 
#       #   # %>%
#       #   #   filter(Weight < input$pointSize_Slider111)
#       #   
#       #   output$plot999 <- renderPlotly(
#       #     plot3 <- Release1 %>%
#       #       ggplot(aes(x = Length, y = Weight, color = Species)) +
#       #       geom_point() + 
#       #       theme_classic() +
#       #       labs(title = "Release1 Data"),
#       #     
#       #     ggplotly(plot3)
#       #   )
#       # })
#       
#       
#     }
#   )
# }