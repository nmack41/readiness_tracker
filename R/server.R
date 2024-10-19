source('functions.R')

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
      
      # Convert inputs to numeric
      weight <- as.numeric(input_data()$weight)
      sleep <- as.numeric(input_data()$sleep)
      hunger <- as.numeric(input_data()$hunger)
      
      log_message(paste("Inputs received - Weight:", weight, "Sleep:", sleep, "Hunger:", hunger))
      
      # Log input types
      log_message(paste("Input types - Weight:", class(weight), "Sleep:", class(sleep), "Hunger:", class(hunger)))
      
      # Ensure all inputs are numeric and not NA
      if (any(is.na(c(weight, sleep, hunger)))) {
        log_message("Non-numeric or missing input detected")
        showNotification("All inputs must be numeric and non-missing", type = "error")
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
      showNotification("An unexpected error occurred. Please try again.", type = "error")
    })
  })
  
  # Render plot from data retrieved from Supabase
  output$lineChart <- renderPlot({
    log_message("Entering renderPlot")
    tryCatch({
      log_message("Calling get_data")
      df <- get_data()
      log_message(paste("Data retrieved, row count:", ifelse(is.null(df), "NULL", nrow(df))))
      
      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
        log_message("Data frame is not null and has rows")
        required_columns <- c("Date", "weight", "sleep", "hunger")
        if (all(tolower(required_columns) %in% tolower(colnames(df)))) {
          log_message("All required columns are present")
          
          # Ensure correct column names
          df <- df %>%
            rename_with(tolower) %>%  # Convert all column names to lowercase
            rename(date = date, weight = weight, sleep = sleep, hunger = hunger)
          
          df$Date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")  # Adjust format as needed
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
      text(1, 1, "Error plotting data", cex = 1.2)
    })
  })
}
