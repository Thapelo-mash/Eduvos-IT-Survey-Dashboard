# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Load the dataset
survey_data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# Drop missing values
survey_data <- na.omit(survey_data)

# Define UI
ui <- fluidPage(
  titlePanel("Eduvos IT Graduates Survey Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "language",
        label = "Select Programming Language:",
        choices = NULL,
        options = list(placeholder = 'Type to search...')
      ),
      selectizeInput(
        inputId = "database",
        label = "Select Database:",
        choices = NULL,
        options = list(placeholder = 'Type to search...')
      ),
      selectizeInput(
        inputId = "framework",
        label = "Select Web Framework:",
        choices = NULL,
        options = list(placeholder = 'Type to search...')
      ),
      sliderInput("experience", "Years of Professional Experience:", min = 0, max = 50, value = c(0, 10))
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
  observe({
    updateSelectizeInput(
      session,
      inputId = "language",
      choices = unique(na.omit(survey_data$ProgLang)),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "database",
      choices = unique(na.omit(survey_data$Databases)),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "framework",
      choices = unique(na.omit(survey_data$WebFramework)),
      server = TRUE
    )
  })
  
  output$language_plot <- renderPlotly({
    req(input$language)
    filtered_data <- survey_data %>%
      filter(ProgLang == input$language,
             YearsCodePro >= input$experience[1],
             YearsCodePro <= input$experience[2])
    
    validate(
      need(nrow(filtered_data) > 0, "No data available for the selected filters.")
    )
    
    ggplotly(
      ggplot(filtered_data, aes(x = YearsCodePro)) +
        geom_bar(fill = "steelblue") +
        labs(title = "Programming Language Usage", x = "Years of Professional Experience", y = "Count")
    )
  })
  
  output$database_plot <- renderPlotly({
    req(input$database)
    filtered_data <- survey_data %>%
      filter(Databases == input$database,
             YearsCodePro >= input$experience[1],
             YearsCodePro <= input$experience[2])
    
    validate(
      need(nrow(filtered_data) > 0, "No data available for the selected filters.")
    )
    
    ggplotly(
      ggplot(filtered_data, aes(x = YearsCodePro)) +
        geom_bar(fill = "orange") +
        labs(title = "Database Usage", x = "Years of Professional Experience", y = "Count")
    )
  })
  
  output$framework_plot <- renderPlotly({
    req(input$framework)
    filtered_data <- survey_data %>%
      filter(WebFramework == input$framework,
             YearsCodePro >= input$experience[1],
             YearsCodePro <= input$experience[2])
    
    validate(
      need(nrow(filtered_data) > 0, "No data available for the selected filters.")
    )
    
    ggplotly(
      ggplot(filtered_data, aes(x = WebFramework)) +
        geom_bar(fill = "purple") +
        labs(title = "Web Framework Usage", x = "Web Framework", y = "Count")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Deploy the app
library(rsconnect)
rsconnect::setAccountInfo(
  name = 'mashabelashabza',
  token = '190DC201817F596DA2590047CE82EBF0',
  secret = 'nuG5vNLBLBi65it+2zpnq0viNpXqVQ0N1rzWUduV'
)

setwd("C:/Users/USER/Documents")
print(getwd())
dir.create("C:/Users/USER/Documents/ShinyApp", showWarnings = FALSE)

if (file.exists("C:/Users/USER/Documents/app.R")) {
  file.copy("C:/Users/USER/Documents/app.R", "C:/Users/USER/Documents/ShinyApp/")
} else {
  stop("app.R does not exist in the specified directory.")
}



rsconnect::deployApp("C:/Users/USER/Documents/ShinyApp")
