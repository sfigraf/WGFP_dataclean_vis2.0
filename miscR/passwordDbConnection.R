#db connection/initialization script for password protection
# Create local directory for database if it doesn't exist
local_db_dir <- file.path(getwd(), "app_data")
if (!dir.exists(local_db_dir)) {
  dir.create(local_db_dir)
  cat("📁 Created local data directory:", local_db_dir, "\n")
}

# Initialize SQLite database
db_path <- file.path(local_db_dir, "tracker_app.db")
init_database <- function() {
  con <- dbConnect(SQLite(), db_path)
  
  # Create tables
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_projects (
      username TEXT NOT NULL,
      project_name TEXT NOT NULL,
      added_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (username, project_name)
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS change_requests (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT NOT NULL,
      project_name TEXT NOT NULL,
      change_type TEXT NOT NULL,
      change_description TEXT,
      submitted_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      status TEXT DEFAULT 'pending',
      reviewed_by TEXT,
      reviewed_date TIMESTAMP,
      review_notes TEXT
    )
  ")
  ###USER CREDNETIALS DB
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_credentials (
      username TEXT PRIMARY KEY,
      password TEXT NOT NULL,
      is_admin INTEGER DEFAULT 0,
      last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbDisconnect(con)
}
init_credentials <- function() {
  con <- dbConnect(SQLite(), db_path)
  # Check if credentials exist
  existing <- dbGetQuery(con, "SELECT username FROM user_credentials")
  if (nrow(existing) == 0) {
    # Insert default credentials
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('admin', 'admin123', 1)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('demo_bio', 'bio123', 0)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('test_user', 'test123', 0)")
    
    # real users
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('graffs', 'test123', 1)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('ericher', 'traviskelce', 0)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('efetherman', 'berdnerd', 0)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('mkondratieff', 'mergmaster', 0)")
    
    cat("✅ Initialized default credentials:\n")
    cat("   Admin user: admin / admin123\n")
    cat("   Demo user: demo_bio / bio123\n")
    cat("   Test user: test_user / test123\n")
  }
  dbDisconnect(con)
}

# Initialize database
init_database()
init_credentials()


## Credentials - load from database
load_credentials <- function() {
  con <- dbConnect(SQLite(), db_path)
  creds <- dbGetQuery(con, "SELECT username as user, password, is_admin FROM user_credentials")
  dbDisconnect(con)
  return(creds)
}