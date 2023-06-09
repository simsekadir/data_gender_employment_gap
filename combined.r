library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)

# Load data from CSV files
unemployment_data <- read.csv("unemployment_data.csv")
allex_energy_data <- read.csv("allex_energy_data.csv")

# Sort the Council Areas and Years
sorted_areas <- sort(unique(allex_energy_data$FeatureName))
sorted_years <- sort(unique(allex_energy_data$DateCode))

# Define the UI
ui <- navbarPage(
  titlePanel("Data Analysis"),
  tabPanel(
    "Unemployment",
    sidebarLayout(
      sidebarPanel(
        # Select Council Area
        selectInput("areaInput", "Select Council Area:",
                    choices = unique(unemployment_data$FeatureName),
                    selected = unique(unemployment_data$FeatureName)[1]),
        
        # Select Year
        selectInput("yearInput", "Select Year:",
                    choices = unique(unemployment_data$DateCode),
                    selected = unique(unemployment_data$DateCode)[1]),
        
        # Select graph type
        selectInput("graphTypeInput", "Select Graph Type:",
                    choices = c("Histogram", "Pie Chart", "Scatter Plot"),
                    selected = "Histogram")
      ),
      
      mainPanel(
        # Display the graph
        plotOutput("graphUnemployment")
      )
    )
  ),
  tabPanel(
    "Energy Consumption",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          # Select Council Area
          selectInput("council_area", "Council Area:", choices = sorted_areas),
          
          # Select Year
          selectInput("year", "Year:", choices = sorted_years),
          
          # Select Graph Type
          selectInput("graph_type", "Graph Type:", choices = c("Histogram", "Pie Chart")),
          
          # Submit button
          actionButton("submit", "Submit")
        ),
        
        mainPanel(
          # Output graph
          plotOutput("graphEnergy")
        )
      )
    )
  ),
  tabPanel(
    "Energy and Unemployment Correlation",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          # Council Area selection
          selectInput("council_area_corr", "Select Council Area:",
                      choices = unique(allex_energy_data$FeatureName)),
          
          # Year selection
          
        ),
        
        mainPanel(
          plotOutput("correlation_plot")
        )
      )
    )
  ),
  tabPanel(
    "Unemployment and Energy Analysis",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          # Council Area selection
          selectInput("council_area_analysis", "Select Council Area:",
                      choices = sort(unique(allex_energy_data$FeatureName))),
          
          # Year selection
          selectInput("year_analysis", "Select Year:",
                      choices = sort(unique(allex_energy_data$DateCode)))
        ),
        
        mainPanel(
          # Plots
          plotOutput("unemployment_plot"),
          plotOutput("energy_plot"),
          
          # Summary Analysis
          h4("Unemployment Summary"),
          verbatimTextOutput("unemployment_stats"),
          h4("Energy Consumption Summary"),
          verbatimTextOutput("energy_stats")
        )
      )
    )
  ),
  tabPanel(
    "Gender Employment Gap", 
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
  tabPanel(
    "Energy Data",
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

# Define the server logic
server <- function(input, output) {
  # Filter the unemployment data based on user inputs
  filtered_unemployment_data <- reactive({
    unemployment_data %>%
      filter(FeatureName == input$areaInput, DateCode == input$yearInput)
  })
  
  # Create the unemployment graph based on the selected type
  output$graphUnemployment <- renderPlot({
    if (input$graphTypeInput == "Histogram") {
      ggplot(filtered_unemployment_data(), aes(x = Age, y = Value)) +
        geom_bar(stat = "identity", fill = "red") +
        labs(x = "Age", y = "Unemployment Gap", title = "Unemployment Gap by Age (Histogram)") +
        theme_minimal()
    } else if (input$graphTypeInput == "Pie Chart") {
      ggplot(filtered_unemployment_data(), aes(x = "", y = Value, fill = Age)) +
        geom_bar(stat = "identity") +
        coord_polar("y") +
        labs(x = NULL, y = NULL, fill = "Age") +
        ggtitle("Unemployment Gap by Age (Pie Chart)") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3")
    } else if (input$graphTypeInput == "Scatter Plot") {
      ggplot(filtered_unemployment_data(), aes(x = Age, y = Value)) +
        geom_point(color = "steelblue") +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(x = "Age", y = "Unemployment Gap", title = "Unemployment Gap by Age (Scatter Plot)") +
        theme_minimal()
    }
  })
  
  # Filter the energy consumption data based on user selections
  filtered_energy_data <- reactive({
    subset(allex_energy_data, FeatureName == input$council_area & DateCode == input$year)
  })
  
  # Render the energy consumption graph
  output$graphEnergy <- renderPlot({
    graph_title <- paste("Energy Consumption by Energy Type", input$year)
    
    if (input$graph_type == "Histogram") {
      ggplot(filtered_energy_data(), aes(x = Energy.Type, y = Value)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Energy Type", y = "Energy Consumption") +
        ggtitle(graph_title) +
        theme_minimal()
      
    } else if (input$graph_type == "Pie Chart") {
      ggplot(filtered_energy_data(), aes(x = "", y = Value, fill = Energy.Type)) +
        geom_bar(stat = "identity") +
        coord_polar("y") +
        labs(x = NULL, y = NULL, fill = "Energy Type") +
        ggtitle(graph_title) +
        theme_minimal()
    }
  })
  
  # Filter data based on user inputs and calculate correlation
  selected_data <- reactive({
    filtered_energy <- allex_energy_data %>%
      filter(FeatureName == input$council_area_corr) %>%
      group_by(DateCode) %>%
      summarise(mean_energy = mean(Value))
    
    filtered_unemployment <- unemployment_data %>%
      filter(FeatureName == input$council_area_corr) %>%
      group_by(DateCode) %>%
      summarise(mean_unemployment = mean(Value))
    
    correlation <- cor(filtered_unemployment$mean_unemployment, filtered_energy$mean_energy)
    
    list(mean_unemployment = filtered_unemployment, mean_energy = filtered_energy, correlation = correlation)
  })
  
  # Render the correlation plot
  output$correlation_plot <- renderPlot({
    data <- selected_data()
    
    plot(data$mean_unemployment$mean_unemployment, data$mean_energy$mean_energy,
         xlab = "Mean Unemployment Gap", ylab = "Mean Energy Consumed",
         main = "Correlation Plot")
  })
  
  # Filter the data based on user selections for analysis
  filtered_unemployment_analysis <- reactive({
    filter(unemployment_data, FeatureName == input$council_area_analysis, DateCode == input$year_analysis)
  })
  
  filtered_energy_analysis <- reactive({
    filter(allex_energy_data, FeatureName == input$council_area_analysis, DateCode == input$year_analysis)
  })
  
  # Perform statistical analysis on unemployment data
  output$unemployment_stats <- renderPrint({
    summary(filtered_unemployment_analysis()$Value)
  })
  
  # Perform statistical analysis on energy consumption data
  output$energy_stats <- renderPrint({
    summary(filtered_energy_analysis()$Value)
  })
  
  # Create a plot for unemployment gaps
  output$unemployment_plot <- renderPlot({
    ggplot(filtered_unemployment_analysis(), aes(x = Age, y = Value)) +
      geom_bar(stat = "identity", fill = "red") +
      labs(x = "Age", y = "Unemployment Gap", title = "Unemployment Gap by Age")
  })
  
  # Create a plot for energy consumption
  output$energy_plot <- renderPlot({
    ggplot(filtered_energy_analysis(), aes(x = Energy.Type, y = Value)) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(x = "Energy Type", y = "Energy Consumption", title = "Energy Consumption by Type")
  })
  
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
