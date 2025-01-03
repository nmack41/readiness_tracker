library(shiny)
library(ggplot2)
library(dotenv)

source('functions.R')

# Load environment variables
load_dot_env()

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Home",
             titlePanel("Readiness Tracker"),
             sidebarLayout(
               sidebarPanel(
                 h3(textOutput("current_date")),
                 numericInput(inputId = "weight", label = "Weight", min = 50, max = 500, value = 200),
                 numericInput(inputId = "sleep", label = "Sleep (0-5)", min = 0, max = 5, value = 3),
                 numericInput(inputId = "motivation_to_train", label = "Motivation to Train (0-5)", min = 0, max = 5, value = 3)
               ),
               mainPanel(
                 textOutput("weight_output"), # Changed to avoid duplicate ID
                 plotOutput('sample_data_plot'),
                 tableOutput('sample_data_table')
               )
             )
    ),
    tabPanel("Data",
             h3("Content for Tab 2")
    )
  )
)

server <- function(input, output, session) {
  
  current_datetime <- Sys.time()
  output$current_date <- renderText(format(current_datetime, "%m-%d-%Y"))
  
  ### sample data for graph
  data("Titanic")
  sample_data <- as.data.frame(Titanic)
  ###
  
  
  output$weight <- renderText(input$weight)
  
  output$sample_data_plot <- renderPlot({
    ggplot(sample_data, aes(x = Class, y = Freq, fill = Age)) + geom_col()})
  
  output$sample_data_table <- renderTable(summary(sample_data))
  
}


shinyApp(ui, server)
