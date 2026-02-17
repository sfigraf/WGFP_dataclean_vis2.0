# Function to update password
updatePassword <- function(username, new_password, db_path) {
  con <- dbConnect(SQLite(), db_path)
  tryCatch({
    dbExecute(con, 
              "UPDATE user_credentials SET password = ?, last_updated = CURRENT_TIMESTAMP WHERE username = ?",
              params = list(new_password, username)
    )
    cat("🔐 Password updated for user", username, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Error updating password:", e$message, "\n")
    return(FALSE)
  }, finally = {
    dbDisconnect(con)
  })
}