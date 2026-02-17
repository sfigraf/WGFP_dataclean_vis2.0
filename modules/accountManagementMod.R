#account management
accountManagement_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             h4("👤 User Info:"),
             verbatimTextOutput(ns("user_info"))
      ),
      column(6,
             h4("🔐 Change Password:"),
             passwordInput(ns("new_password"), "New Password:", placeholder = "Enter new password"),
             passwordInput(ns("confirm_password"), "Confirm Password:", placeholder = "Confirm new password"),
             actionButton(ns("change_password"), "Update Password", icon = icon("key"), class = "btn-warning"),
             br(),
             br(),
             uiOutput(ns("password_feedback"))
      )
    )
  
  )
}

accountManagement_Server <- function(id, user, db_path) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # User info
      output$user_info <- renderText({
        
        if (!is.null(user)) {
          paste(
            "Username:", user,
            "", #if(values$is_admin) "(Admin)" else ""
            "\nTime:", Sys.time(),
            "\nMode: LOCAL DEMO"
          )
        } else {
          "Not logged in"
        }
      })
      
      # Password change functionality
      output$password_feedback <- renderUI({
        input$new_password
        input$confirm_password
        return(NULL)
      })
      
      observeEvent(input$change_password, {
        req(user)
        
        # Validate passwords
        if (input$new_password == "") {
          output$password_feedback <- renderUI({
            p("Please enter a new password", style = "color: red;")
          })
          return()
        }
        
        if (nchar(input$new_password) < 6) {
          output$password_feedback <- renderUI({
            p("Password must be at least 6 characters", style = "color: red;")
          })
          return()
        }
        
        if (input$new_password != input$confirm_password) {
          output$password_feedback <- renderUI({
            p("Passwords don't match!", style = "color: red;")
          })
          return()
        }
        
        # Update password
        success <- updatePassword(user, input$new_password, db_path)
        
        if (success) {
          output$password_feedback <- renderUI({
            div(
              p("✅ Password updated successfully!", style = "color: green; font-weight: bold;"),
              p("You'll need to use your new password next time you log in.", style = "color: #666;")
            )
          })
          
          # Clear the input fields
          updateTextInput(session, "new_password", value = "")
          updateTextInput(session, "confirm_password", value = "")
          
          showNotification("Password changed successfully! 🔐", type = "message", duration = 5)
        } else {
          output$password_feedback <- renderUI({
            p("❌ Failed to update password. Please try again.", style = "color: red;")
          })
        }
      })
      
      
    }
  )
}