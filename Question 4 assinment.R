# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
# Load the dataset
survey_data <-read.csv(file.choose(),header = T)

# Define UI
ui <- fluidPage(
  titlePanel("Eduvos IT Graduates Survey Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Use selectizeInput for programming languages
      selectizeInput(
        inputId = "language",
        label = "Select Programming Language:",
        choices = NULL,  # Choices will be set server-side
        options = list(placeholder = 'Type to search...')
      ),
      # Use selectizeInput for databases (server-side)
      selectizeInput(
        inputId = "database",
        label = "Select Database:",
        choices = NULL,  # Choices will be set server-side
        options = list(placeholder = 'Type to search...')
      ),
      sliderInput("experience", "Years of Professional Experience:", min = 0, max = 50, value = c(0, 10))  # Adjusted max value
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Programming Languages", plotlyOutput("language_plot")),
        tabPanel("Databases", plotlyOutput("database_plot")),
        tabPanel("Web Frameworks", plotlyOutput("framework_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Update selectizeInput choices for programming languages
  updateSelectizeInput(
    session,
    inputId = "language",
    choices = unique(survey_data$ProgLang),  # Correct column name
    server = TRUE  # Enable server-side processing
  )
  
  # Update selectizeInput choices for databases
  updateSelectizeInput(
    session,
    inputId = "database",
    choices = unique(survey_data$Databases),  # Correct column name
    server = TRUE  # Enable server-side processing
  )
  
  output$language_plot <- renderPlotly({
    req(input$language)  # Ensure a language is selected
    filtered_data <- survey_data %>%
      filter(ProgLang == input$language,  # Correct column name
             YearsCodePro >= input$experience[1],  # Correct column name
             YearsCodePro <= input$experience[2])
    ggplot(filtered_data, aes(x = YearsCodePro, y = ..count..)) +  # Correct column name
      geom_bar(stat = "count", fill = "steelblue") +  # Use stat = "count" for frequency
      labs(title = "Programming Language Usage", x = "Years of Professional Experience", y = "Count")
  })
  
  output$database_plot <- renderPlotly({
    req(input$database)  # Ensure a database is selected
    filtered_data <- survey_data %>%
      filter(Databases == input$database,  # Correct column name
             YearsCodePro >= input$experience[1],
             YearsCodePro <= input$experience[2])
    ggplot(filtered_data, aes(x = YearsCodePro, y = ..count..)) +  # Correct column name
      geom_bar(stat = "count", fill = "orange") +  # Use stat = "count" for frequency
      labs(title = "Database Usage", x = "Years of Professional Experience", y = "Count")
  })
  
  output$framework_plot <- renderPlotly({
    filtered_data <- survey_data %>%
      filter(YearsCodePro >= input$experience[1],  # Correct column name
             YearsCodePro <= input$experience[2])
    ggplot(filtered_data, aes(x = WebFramework, y = ..count..)) +  # Correct column name
      geom_bar(stat = "count", fill = "purple") +  # Use stat = "count" for frequency
      labs(title = "Web Framework Usage", x = "Web Framework", y = "Count")
  })
}

# Run the application
shinyApp(ui = ui, server = server)