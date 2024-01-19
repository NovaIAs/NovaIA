```R
# Load necessary libraries
library(tidyverse)
library(shiny)
library(plotly)
library(lubridate)
library(scales)
library(ggplot2)
library(shinyWidgets)
library(RColorBrewer)
library(highcharter)
library(janitor)

# Define the user interface
ui <- fluidPage(
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      sliderInput("year",
                  "Select a year:",
                  min = 2010,
                  max = 2019,
                  value = 2015),
      
      # Sidebar with a dropdown input
      selectInput("country",
                  "Select a country:",
                  choices = c("United States", "Canada", "United Kingdom", "Australia")),
      
      # Sidebar with a checkbox input
      checkboxInput("show_plot",
                   "Show plot:",
                   value = TRUE)
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: Plot
      plotOutput("plot"),
      
      # Output: Table
      tableOutput("table")
    )
  )
)

# Define the server logic required to draw plot output based on user input
server <- function(input, output) {
  
  # Reactive expression to compute the filtered data based on user input
  data_subset <- reactive({
    # Filter the data based on the selected year and country
    subset(data, year == input$year & country == input$country)
  })
  
  # Output: Plot
  output$plot <- renderPlotly({
    # Plot the subset data
    ggplot(data_subset(), aes(x = month, y = value)) +
      geom_line() +
      labs(title = paste("Line Plot for", input$country, "in", input$year),
           x = "Month",
           y = "Value")
  })
  
  # Output: Table
  output$table <- renderTable({
    # Create a table with the subset data
    data_subset()
  })
}

# Create Shiny app with the user interface and server logic
shinyApp(ui, server)
```

**Explanation:**

1. **Libraries**: The code begins by loading several R libraries that provide various functionalities for data manipulation, visualization, and interactivity.

2. **User Interface (UI)**: The `ui` function defines the user interface of the Shiny app. It consists of a sidebar layout with two input widgets: a slider for selecting a year and a dropdown for selecting a country. Additionally, a checkbox is included to toggle the display of a plot. The main panel contains two output elements: a plot and a table.

3. **Server Logic**: The `server` function defines the logic for the app's behavior. It utilizes reactive programming to update the outputs dynamically based on user input.

4. **Reactive Expression**: Inside the server function, a reactive expression (`data_subset`) is created to filter the `data` based on the selected year and country. This ensures that the plot and table are always up-to-date with the user's selections.

5. **Plot Output**: The `renderPlotly` function is used to create an interactive plotly plot based on the filtered data. The plot displays the relationship between month and value for the selected country and year.

6. **Table Output**: The `renderTable` function is used to create a table that displays the subset data in a tabular format.

7. **Shiny App Creation**: Finally, the `shinyApp` function combines the user interface and server logic to create a complete Shiny app. This app can be run locally using the `runApp()` function or deployed online for wider accessibility.