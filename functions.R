# Load necessary libraries
library(dotenv)
library(readr)

# Load environment variables
load_dot_env()

# Function to initialize the tracking.csv file
create_tracking_file <- function() {
    file_path <- "tracking.csv"
    
    if (!file.exists(file_path)) {
        tracking_df <- data.frame(
            date = as.Date(character()),
            weight = numeric(),
            sleep = numeric(),
            motivation_to_train = numeric(),
            stringsAsFactors = FALSE
        )
        write_csv(tracking_df, file_path)
    }
}


# Function to add a new entry to the CSV file
add_entry <- function(date, weight, sleep, motivation) {
    new_entry <- data.frame(
        date = as.character(date),
        weight = as.numeric(weight),
        sleep = as.numeric(sleep),
        motivation_to_train = as.numeric(motivation)
    )
    
    write_csv(new_entry, "tracking.csv", append = TRUE)
}
