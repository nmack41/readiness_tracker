source('functions.R')

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