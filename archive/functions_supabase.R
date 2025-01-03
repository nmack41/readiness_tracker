# Load necessary libraries
library(dotenv)
library(httr)
library(jsonlite) 
# library(DBI)
# library(Rpostgres)

# Load environment variables
load_dot_env()

# Load the following from .env file
SUPABASE_API_KEY <- Sys.getenv("SUPABASE_API_KEY")
SUPABASE_URL <- Sys.getenv("SUPABASE_URL")
user_id <- user_id


# Helper function to validate email
validate_email <- function(email) {
  grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email)
}

# Function to sign up a new user
sign_up <- function(email, password) {
  if (!validate_email(email)) {
    stop("Invalid email format. Please provide a valid email address.")
  }
  
  response <- POST(
    url = paste0(SUPABASE_URL, "/auth/v1/signup"),
    add_headers(
      apikey = SUPABASE_API_KEY,
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(
      email = email,
      password = password
    ), auto_unbox = TRUE)
  )
  
  parsed_content <- content(response, "parsed")
  
  if (response$status_code == 200) {
    message("Sign-up successful!")
    return(parsed_content)  # Return session data including access_token
  } else {
    stop("Sign-up failed: ", parsed_content$message)
  }
}



# Function to log in an existing user
log_in <- function(email, password) {
  response <- POST(
    url = paste0(SUPABASE_URL, "/auth/v1/token?grant_type=password"),
    add_headers(
      apikey = SUPABASE_API_KEY,
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(
      email = email,
      password = password
    ), auto_unbox = TRUE)
  )
  
  parsed_content <- content(response, "parsed")
  
  if (response$status_code == 200) {
    message("Log-in successful!")
    
    access_token <- parsed_content$access_token
    user_id <- parsed_content$user$id
    
    # Return both as a list
    return(list(access_token = access_token, id = user_id))
  } else {
    stop("Log-in failed: ", parsed_content$message)
  }
}

login_data <- log_in("nick@nickmackowski.com", "StrongPassword123")

access_token <- login_data$access_token
user_id <- login_data$id

# Function to fetch user details
get_user_data <- function(access_token, user_id) {
  endpoint_url <- paste0(SUPABASE_URL, "/rest/v1/training_metrics?user_id=eq.", user_id)
  
  response <- GET(
    url = endpoint_url,
    add_headers(
      Authorization = paste("Bearer", access_token),
      apikey = SUPABASE_API_KEY,
      `Content-Type` = "application/json"
    )
  )
  parsed_content <- content(response, "parsed", simplifyVector = TRUE)
  if (response$status_code == 200) {
    return(parsed_content)
  } else {
    error_message <- if (!is.null(parsed_content$message)) {
      parsed_content$message
    } else {
      "Unknown error"
    }
    stop("Failed to fetch training metrics: ", error_message)
  }
}

user_data <- get_user_data(access_token, user_id)
print(user_data)



# Function to update user metadata
# update_user_data <- function(access_token, metadata) {
#   response <- PUT(
#     endpoint_url <- paste0(SUPABASE_URL, "/auth/v1/user"),
#     add_headers(
#       url = endpoint_url,
#       Authorization = paste("Bearer", access_token),
#       `Content-Type` = "application/json"
#     ),
#     body = toJSON(list(metadata = metadata), auto_unbox = TRUE)
#   )
#   
#   parsed_content <- content(response, "parsed")
#   
#   if (response$status_code == 200) {
#     message("User data updated successfully!")
#     return(parsed_content)
#   } else {
#     stop("Failed to update user data: ", parsed_content$message)
#   }
# }
