library(shiny)
library(dplyr)
library(plotly)
library(readr)

# Load data from CSV files
unemployment_data <- read.csv("unemployment_data.csv")
allex_energy_data <- read.csv("allex_energy_data.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Data Visualization"),
  tabsetPanel(
    tabPanel("Gender Employment Gap", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("unemp_feature", "Select Feature Name:", choices = unique(unemployment_data$FeatureName)),
                 selectInput("unemp_age", "Select Age:", choices = unique(unemployment_data$Age))
               ),
               mainPanel(
                 plotlyOutput("unemp_plot")
               )
             )
    ),
    tabPanel("Energy Data",
             sidebarLayout(
               sidebarPanel(
                 selectInput("energy_feature", "Select Feature Name:", choices = unique(allex_energy_data$FeatureName)),
                 selectInput("energy_type", "Select Energy Type:", choices = unique(allex_energy_data$Energy.Type))
               ),
               mainPanel(
                 plotlyOutput("energy_plot")
               )
             )
    )
  )
)

# Define server
server <- function(input, output) {
  # Reactive filtering based on user selections (Gender Employment Gap)
  filtered_unemp_data <- reactive({
    unemployment_data %>%
      filter(FeatureName == input$unemp_feature, Age == input$unemp_age)
  })
  
  # Render the plot (Gender Employment Gap)
  output$unemp_plot <- renderPlotly({
    plot_ly(data = filtered_unemp_data(), x = ~DateCode, y = ~Value, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Date Code"), yaxis = list(title = "Value"))
  })
  
  # Reactive filtering based on user selections (Energy Data)
  filtered_energy_data <- reactive({
    allex_energy_data %>%
      filter(FeatureName == input$energy_feature, Energy.Type == input$energy_type)
  })
  
  # Render the plot (Energy Data)
  output$energy_plot <- renderPlotly({
    plot_ly(data = filtered_energy_data(), x = ~DateCode, y = ~Value, color = ~Energy.Type, type = 'scatter', mode = 'markers') %>%
      layout(xaxis = list(title = "Date Code"), yaxis = list(title = "Value"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
