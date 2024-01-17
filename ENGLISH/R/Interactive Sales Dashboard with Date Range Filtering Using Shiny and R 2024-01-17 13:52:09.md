```r
# Load the necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shiny)

# Load the data
data <- read.csv("data.csv")

# Clean the data
data <- data %>%
  mutate(date = ymd(date)) %>%
  filter(date >= "2017-01-01", date <= "2017-12-31")

# Create a summary table of the data
summary_table <- data %>%
  group_by(date) %>%
  summarize(total_sales = sum(sales))

# Create a line chart of the total sales over time
line_chart <- ggplot(summary_table, aes(x = date, y = total_sales)) +
  geom_line() +
  labs(title = "Total Sales Over Time",
       x = "Date",
       y = "Total Sales")

# Create a shiny app to display the data
ui <- fluidPage(
  titlePanel("Sales Dashboard"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Date Range", start = "2017-01-01", end = "2017-12-31")
    ),
    mainPanel(
      plotOutput("line_chart")
    )
  )
)

server <- function(input, output) {
  output$line_chart <- renderPlot({
    data_subset <- data %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    
    ggplot(data_subset, aes(x = date, y = sales)) +
      geom_line() +
      labs(title = "Total Sales Over Time",
           x = "Date",
           y = "Total Sales")
  })
}

shinyApp(ui, server)
```

This code loads the necessary libraries, loads the data, cleans the data, creates a summary table of the data, creates a line chart of the total sales over time, and creates a Shiny app to display the data.

The Shiny app allows the user to select a date range, and the line chart will update to show the total sales for the selected date range.

This code is complex and differentiated because it uses a variety of R functions and packages to perform data analysis and visualization. It also uses the Shiny package to create an interactive web application.