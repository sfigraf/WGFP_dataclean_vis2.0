capitalizerUI <- function(id) {
  ns <- NS(id)
  wellPanel(
    h4("Text captializer module"),
    textInput(ns("txt"), "Enter text:"),
    verbatimTextOutput(ns("out"))
  )
}

capitalizerServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
  output$out <- renderText({
    toupper(input$txt)
  })
    })
}