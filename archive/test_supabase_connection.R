library(httr)
library(jsonlite)
library(dotenv)

# Load environment variables
load_dot_env()


log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
  message <- paste(timestamp, "-", msg)
  cat(message, "", file = "app_log.txt", append = TRUE)
  print(message)  # This will also print the message to the console
}

get_data <- function() {
  log_message("Entering get_data function")
  
  # Retrieve the environment variables
  supabase_url <- Sys.getenv("SUPABASE_URL")
  table_name <- Sys.getenv("TABLE_NAME")
  api_key <- Sys.getenv("SUPABASE_API_KEY")
  
  tryCatch({
    url <- paste0(supabase_url, "/rest/v1/", table_name)
    log_message(paste("URL:", url))
    
    headers <- add_headers(
      `apikey` = api_key,
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    )
    log_message("Headers created")
    
    thirty_days_ago <- format(Sys.Date() - 30, "%Y-%m-%d")
    log_message(paste("Filtering rows created_at >= ", thirty_days_ago))    
    res <- GET(url, headers, query = list(
      select = "*", 
      order = "created_at.desc",
      created_at = paste0("gte.", thirty_days_ago)
    ))
    log_message(paste("GET request status:", status_code(res)))
    
    if (status_code(res) == 200) {
      data <- fromJSON(content(res, "text"))
      log_message(paste("Data retrieved successfully. Row count:", nrow(data)))
      
      return(data)
    } else {
      log_message(paste("Error retrieving data:", content(res, "text")))
      return(NULL)
    }
  }, error = function(e) {
    log_message(paste("Error in get_data:", e$message))
    return(NULL)
  })
}


# Print environment variables (with API key length only for security)
cat("Environment Variables:")
cat(paste0(
  "SUPABASE_URL: ", Sys.getenv("SUPABASE_URL"),
  "TABLE_NAME: ", Sys.getenv("TABLE_NAME"),
  "API_KEY length: ", nchar(Sys.getenv("SUPABASE_API_KEY")), " characters"
))

# Test connection
cat("Testing Supabase Connection:")

connection_result <- {
  # Retrieve environment variables
  supabase_url <- Sys.getenv("SUPABASE_URL")
  api_key <- Sys.getenv("SUPABASE_API_KEY")
  
  # Check if the variables are set
  if (supabase_url == "" || api_key == "") {
    cat("Error: Missing environment variables SUPABASE_URL or SUPABASE_API_KEY.")
    FALSE
  } else {
    # Define the endpoint
    endpoint <- paste0(supabase_url, "/rest/v1/")
    
    # Make a simple GET request to test the connection
    response <- tryCatch(
      {
        httr::GET(endpoint, add_headers(
          Authorization = paste("Bearer", api_key),
          apikey = api_key
        ))
      },
      error = function(e) {
        cat("Error connecting to Supabase:", e$message, "")
        NULL
      }
    )
    
    # Check if the request was successful
    if (is.null(response) || httr::status_code(response) != 200) {
      cat("Supabase connection failed with status:", httr::status_code(response), "")
      FALSE
    } else {
      cat("Supabase connection successful!")
      TRUE
    }
  }
}

# Try to get data
cat("Testing Data Retrieval:")
data <- get_data()

# Print results
cat("Results:")
cat(paste("Connection test:", if(connection_result) "SUCCESS" else "FAILED", ""))
cat(paste("Data retrieval:", if(!is.null(data)) "SUCCESS" else "FAILED", ""))

if (!is.null(data)) {
  cat("Data Preview:")
  print(head(data))
  cat(paste("Total rows:", nrow(data), ""))
  cat(paste("Columns:", paste(names(data), collapse=", "), ""))
}


# Try to get data
cat("Testing Data Retrieval:")
data <- get_data()

# Print results
cat("Results:")
cat(paste("Connection test:", if(connection_result) "SUCCESS" else "FAILED"))
cat(paste("Data retrieval:", if(!is.null(data)) "SUCCESS" else "FAILED"))

if (!is.null(data)) {
  cat("Data Preview:")
  print(head(data))
  cat(paste("Total rows:", nrow(data)))
  cat(paste("Columns:", paste(names(data), collapse=", ")))
}
