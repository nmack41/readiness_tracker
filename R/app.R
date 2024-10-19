# libraries ----
  library(shiny)
  library(httr)
  library(jsonlite)
  library(shinylogs)
  library(dotenv)
  library(shinythemes)
  library(shinycssloaders)

# Source UI and Server
  source("ui.R")
  source("server.R")

# Load environment variables
  load_dot_env()

  SUPABASE_URL <- Sys.getenv("SUPABASE_URL")
  SUPABASE_API_KEY <- Sys.getenv("SUPABASE_API_KEY")
  TABLE_NAME <- Sys.getenv("TABLE_NAME")




log_message("Server function setup completed")

# Run the app
shinyApp(ui = ui, server = server)

myapp()