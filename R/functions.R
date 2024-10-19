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

# Logging function
log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
  message <- paste(timestamp, "-", msg)
  cat(message, "\n", file = "app_log.txt", append = TRUE)
  print(message)  # This will also print the message to the console
}
