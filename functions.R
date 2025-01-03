# Load necessary libraries
library(dotenv)
library(httr)
library(jsonlite) 

# Load environment variables
load_dot_env()

# If tracking.csv does not exist then create the file. Ensure the file has date, weight, sleep, and motivation_to_train as the column headers
initialize_tracking_file <- function() {
    file_path <- "tracking.csv"
    
    if (!file.exists(file_path)) {
        tracking_df <- data.frame(
            date = character(),
            weight = numeric(),
            sleep = numeric(),
            motivation_to_train = numeric(),
            stringsAsFactors = FALSE
        )
        write.csv(tracking_df, file = file_path, row.names = FALSE)
    }
}

# Take variables date, weight, sleep, and motivation and add that entry to the csv
add_entry <- function(date, weight, sleep, motivation) {
    new_entry <- data.frame(
        date = as.character(date),
        weight = as.numeric(weight),
        sleep = as.numeric(sleep),
        motivation_to_train = as.numeric(motivation),
        stringsAsFactors = FALSE
    )
    
    write.table(new_entry, file = "tracking.csv", 
                sep = ",", append = TRUE, 
                row.names = FALSE,
                col.names = !file.exists("tracking.csv"))
}
