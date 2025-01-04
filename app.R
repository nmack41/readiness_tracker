library(shiny)
library(ggplot2)
library(readr)
library(DT)
library(dotenv)
library(shinythemes)

source('functions.R')

# Load environment variables
load_dot_env()

# UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),  # Add a theme
  tabsetPanel(
    tabPanel("Home",
             titlePanel("Readiness Tracker"),
             sidebarLayout(
               sidebarPanel(
                 h3(textOutput("current_date")),
                 numericInput("weight", "Weight (lbs)", min = 50, max = 500, value = 200),
                 numericInput("sleep", "Sleep (Hours)", min = 0, max = 5, value = 3),
                 numericInput("motivation_to_train", "Motivation to Train (0-5)", min = 0, max = 5, value = 3),
                 actionButton("add_entry", "Add Entry")
               ),
               mainPanel(
                 textOutput("weight_output"),
                 plotOutput("sample_data_plot")
               )
             )
    ),
    tabPanel("Data",
             h3("Logged Data"),
             DT::dataTableOutput("tracking_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Display current date
  output$current_date <- renderText(format(Sys.time(), "%m-%d-%Y"))
  
  # Initialize the CSV file if it doesn't exist
  create_tracking_file()
  
  # Reactive data reader to watch for changes in the CSV
  tracking_data <- reactiveFileReader(1000, session, "tracking.csv", read_csv)
  
  # Add new entry to CSV
  observeEvent(input$add_entry, {
    add_entry(Sys.Date(), input$weight, input$sleep, input$motivation_to_train)
  })
  
  # Plot data dynamically from the CSV
  output$sample_data_plot <- renderPlot({
    data <- tracking_data()
    ggplot(data, aes(x = date, y = weight)) +
      geom_line() +
      geom_point() +
      labs(title = "Weight Over Time", x = "Date", y = "Weight (lbs)")
  })
  
  # Display the CSV data in a table
  output$sample_data_table <- DT::renderDataTable({
    tracking_data()
  })
  
  # Display the interactive DT table on the Data tab
  output$tracking_table <- DT::renderDataTable({
    datatable(
      tracking_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
