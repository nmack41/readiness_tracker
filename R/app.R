# libraries ----
  library(shiny)
  library(httr)
  library(jsonlite)
  library(shinylogs)
  library(dotenv)
  library(shinythemes)
  library(shinycssloaders)

# Load environment variables
  load_dot_env()

  SUPABASE_URL <- Sys.getenv("SUPABASE_URL")
  SUPABASE_API_KEY <- Sys.getenv("SUPABASE_API_KEY")
  TABLE_NAME <- Sys.getenv("TABLE_NAME")

# Logging function
log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
  message <- paste(timestamp, "-", msg)
  cat(message, "\n", file = "app_log.txt", append = TRUE)
  print(message)  # This will also print the message to the console
}

# Modularized input UI
input_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("weight"), "Weight (kg):", value = 70, min = 0, max = 1000),
    numericInput(ns("sleep"), "Sleep Score:", value = 50, min = 0, max = 100),
    sliderInput(ns("hunger"), "Hunger Level:", min = 0, max = 10, value = 5),
    actionButton(ns("submit"), "Submit", class = "btn-primary")
  )
}

# Modularized input server logic
input_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        weight = input$weight,
        sleep = input$sleep,
        hunger = input$hunger,
        submit = input$submit
      )
    })
  })
}

# Function to insert data into Supabase
insert_data <- function(date, weight, sleep, hunger) {
  log_message("Entering insert_data function")
  log_message(paste("Inputs - Date:", date, "Weight:", weight, "Sleep:", sleep, "Hunger:", hunger))
  
  tryCatch({
    url <- paste0(SUPABASE_URL, "/rest/v1/", TABLE_NAME)
    log_message(paste("URL:", url))
    
    headers <- add_headers(
      `apikey` = SUPABASE_API_KEY,
      `Authorization` = paste("Bearer", SUPABASE_API_KEY),
      `Content-Type` = "application/json"
    )
    log_message("Headers created")
    
    body <- toJSON(list(
      Date = as.character(date),
      weight = as.numeric(weight),
      sleep = as.numeric(sleep),
      hunger = as.numeric(hunger)
    ), auto_unbox = TRUE)
    log_message(paste("Body created:", body))
    
    log_message("Sending POST request")
    res <- POST(url, headers, body = body)
    log_message(paste("POST request status:", status_code(res)))
    
    if (status_code(res) == 201) {
      log_message("Data inserted successfully")
      return(list(success = TRUE, message = "Data successfully submitted!"))
    } else {
      log_message(paste("Server error:", content(res, "text")))
      return(list(success = FALSE, message = paste("Server error:", content(res, "text"))))
    }
  }, error = function(e) {
    log_message(paste("Error in insert_data:", e$message))
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

# Function to validate inputs
validate_inputs <- function(weight, sleep, hunger) {
  errors <- c()
  
  if (weight <= 0 || weight > 1000) {
    errors <- c(errors, "Weight must be between 0 and 1000 kg.")
  }
  
  if (sleep < 0 || sleep > 100) {
    errors <- c(errors, "Sleep score must be between 0 and 100.")
  }
  
  if (hunger < 0 || hunger > 10) {
    errors <- c(errors, "Hunger level must be between 0 and 10.")
  }
  
  if (length(errors) > 0) {
    return(list(valid = FALSE, errors = paste(errors, collapse = "\n")))
  } else {
    return(list(valid = TRUE))
  }
}

# Function to retrieve data from Supabase
get_data <- function() {
  url <- paste0(SUPABASE_URL, "/rest/v1/", TABLE_NAME)
  headers <- add_headers(
    `apikey` = SUPABASE_API_KEY,
    `Authorization` = paste("Bearer", SUPABASE_API_KEY),
    `Content-Type` = "application/json"
  )
  
  # Get data from the last 30 days
  thirty_days_ago <- as.character(Sys.Date() - 30)
  query <- paste0("Date=gte.", thirty_days_ago)
  
  res <- GET(url, headers, query = list(select = "*", order = "Date.desc", `Date` = paste0("gte.", thirty_days_ago)))
  data <- fromJSON(content(res, "text"))
  return(data)
}

myapp <- function() {
# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  use_tracking(),
  titlePanel("Super Awesome Daily Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      input_module_ui("inputs")
    ),
    
    mainPanel(
      shinycssloaders::withSpinner(plotOutput("lineChart"))
    )
  )
)

server <- function(input, output, session) {
  
  log_message("Server function started")
  
  # Call the input module
  input_data <- input_module_server("inputs")
  log_message("Input module server called")
  
  # Observe the submit button click to insert data
  observeEvent(input_data()$submit, {
    log_message("Submit button clicked")
    tryCatch({
      log_message("Entering tryCatch block")
      req(input_data())
      log_message("input_data() is available")
      
      weight <- input_data()$weight
      sleep <- input_data()$sleep
      hunger <- input_data()$hunger
      log_message(paste("Inputs received - Weight:", weight, "Sleep:", sleep, "Hunger:", hunger))
      
      # Log input types
      log_message(paste("Input types - Weight:", class(weight), "Sleep:", class(sleep), "Hunger:", class(hunger)))
      
      # Ensure all inputs are numeric
      if (!is.numeric(weight) || !is.numeric(sleep) || !is.numeric(hunger)) {
        log_message("Non-numeric input detected")
        showNotification("All inputs must be numeric", type = "error")
        return()
      }
      
      log_message("Calling validate_inputs")
      validation <- validate_inputs(weight, sleep, hunger)
      log_message(paste("Validation result:", ifelse(isTRUE(validation$valid), "valid", "invalid")))
      
      if (isFALSE(validation$valid)) {
        showNotification(validation$errors, type = "error")
        log_message(paste("Validation failed:", validation$errors))
      } else {
        log_message("Calling insert_data")
        result <- insert_data(Sys.time(), weight, sleep, hunger)
        log_message(paste("Insert result:", ifelse(isTRUE(result$success), "success", "failure")))
        
        if (isTRUE(result$success)) {
          showNotification(result$message, type = "message")
          log_message("Data inserted successfully")
        } else {
          showNotification(result$message, type = "error")
          log_message(paste("Data insertion failed:", result$message))
        }
      }
    }, error = function(e) {
      log_message(paste("Error in observeEvent:", e$message))
      showNotification(paste("An error occurred:", e$message), type = "error")
    })
  })
  
  # Render plot from data retrieved from Supabase
  output$lineChart <- renderPlot({
    log_message("Entering renderPlot")
    tryCatch({
      log_message("Calling get_data")
      df <- get_data()
      log_message(paste("Data retrieved, row count:", ifelse(is.null(df), "NULL", nrow(df))))
      log_message(paste("Column names:", paste(colnames(df), collapse = ", ")))
      
      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
        log_message("Data frame is not null and has rows")
        required_columns <- c("Date", "weight", "sleep", "hunger")
        if (all(tolower(required_columns) %in% tolower(colnames(df)))) {
          log_message("All required columns are present")
          df$Date <- as.POSIXct(df$Date)
          log_message("Date column converted to POSIXct")
          
          plot(df$Date, df$weight, type = "l", col = "blue",
               ylim = range(df[c("weight", "sleep", "hunger")], na.rm = TRUE), 
               ylab = "Values", xlab = "Date", main = "Daily Tracker Data")
          lines(df$Date, df$sleep, type = "l", col = "green")
          lines(df$Date, df$hunger, type = "l", col = "red")
          
          legend("topright", legend = c("Weight", "Sleep", "Hunger"), 
                 col = c("blue", "green", "red"), lty = 1)
          log_message("Plot rendered successfully")
        } else {
          log_message("Missing required columns")
          plot(1, type = "n", xlab = "", ylab = "", axes = FALSE, main = "Data Error")
          text(1, 1, "Data is missing required columns", cex = 1.5)
        }
      } else {
        log_message("No valid data for plotting")
        plot(1, type = "n", xlab = "", ylab = "", axes = FALSE, main = "No Data")
        text(1, 1, "No data available to plot", cex = 1.5)
      }
    }, error = function(e) {
      log_message(paste("Error in renderPlot:", e$message))
      plot(1, type = "n", xlab = "", ylab = "", axes = FALSE, main = "Error")
      text(1, 1, paste("Error plotting data:", e$message), cex = 1.2)
    })
  })
  
  log_message("Server function setup completed")
}

# Run the app
shinyApp(ui = ui, server = server)
}

myapp()