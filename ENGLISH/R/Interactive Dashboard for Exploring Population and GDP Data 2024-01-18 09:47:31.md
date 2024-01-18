```r
# Load the required libraries
library(tidyverse)
library(shiny)
library(ggplot2)
library(leaflet)

# Sample data -------------------------------------------------------------------

# Create a data frame with the population of the top 10 countries
population_data <- data.frame(
  country = c("China", "India", "United States", "Indonesia", "Pakistan",
            "Brazil", "Nigeria", "Bangladesh", "Russia", "Mexico"),
  population = c(1425845000, 1380004385, 331002651, 273523615, 220892340,
                212559417, 206139589, 164689383, 145934462, 128932477)
)

# Create a data frame with the GDP of the top 10 countries
gdp_data <- data.frame(
  country = c("China", "United States", "India", "Japan", "Germany",
            "United Kingdom", "France", "Italy", "Canada", "Russia"),
  gdp = c(14722730, 20893746, 2948291, 4937270, 3850347,
          2825883, 2582450, 1943860, 1670572, 1483568)
)

# Interactive dashboard ---------------------------------------------------------

# Define the user interface
ui <- fluidPage(
  titlePanel("Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country:", choices = unique(population_data$country)),
      sliderInput("population_range", "Population Range:",
                  min = min(population_data$population), max = max(population_data$population),
                  value = c(min(population_data$population), max(population_data$population))),
      sliderInput("gdp_range", "GDP Range:",
                  min = min(gdp_data$gdp), max = max(gdp_data$gdp),
                  value = c(min(gdp_data$gdp), max(gdp_data$gdp)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Population", plotOutput("population_plot")),
        tabPanel("GDP", plotOutput("gdp_plot")),
        tabPanel("Map", leafletOutput("map"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression for the filtered population data
  filtered_population_data <- reactive({
    population_data %>%
      filter(population >= input$population_range[1], population <= input$population_range[2])
  })
  
  # Reactive expression for the filtered GDP data
  filtered_gdp_data <- reactive({
    gdp_data %>%
      filter(gdp >= input$gdp_range[1], gdp <= input$gdp_range[2])
  })
  
  # Output: Population plot
  output$population_plot <- renderPlot({
    ggplot(data = filtered_population_data(), aes(x = country, y = population)) +
      geom_bar(stat = "identity") +
      labs(title = "Population of Selected Countries",
           x = "Country",
           y = "Population")
  })
  
  # Output: GDP plot
  output$gdp_plot <- renderPlot({
    ggplot(data = filtered_gdp_data(), aes(x = country, y = gdp)) +
      geom_bar(stat = "identity") +
      labs(title = "GDP of Selected Countries",
           x = "Country",
           y = "GDP")
  })
  
  # Output: Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 0, lng = 0, zoom = 2) %>%
      addMarkers(data = filtered_population_data(), lat = ~0, lng = ~0, popup = ~country)
  })
}

# Run the shiny app
shinyApp(ui, server)
```

This code creates an interactive dashboard using the Shiny package in R. It allows users to explore data on the population and GDP of different countries and visualize it using various plots and maps. The dashboard consists of a sidebar with filters for the population and GDP ranges, a tabset with different visualizations, and a Leaflet map.

1. **Load the required libraries:**

```r
library(tidyverse)
library(shiny)
library(ggplot2)
library(leaflet)
```

The `tidyverse` library provides a collection of packages for data science in R, including packages for data manipulation, visualization, and modeling. The `shiny` library is used for creating interactive web applications in R. The `ggplot2` library is used for creating various types of plots, and the `leaflet` library is used for creating interactive maps.

2. **Sample Data:**

Two data frames, `population_data` and `gdp_data`, are created with sample data on the population and GDP of the top 10 countries, respectively. This data is used to populate the visualizations and maps in the dashboard.

3. **Interactive Dashboard:**

The dashboard's user interface is defined using the `shiny` package's `ui` function. The user interface consists of a sidebar with filters for the population and GDP ranges, a tabset with different visualizations, and a Leaflet map.

4. **Server Logic:**

The server logic for the dashboard is defined using the `shiny` package's `server` function. The server logic includes reactive expressions for the filtered population and GDP data, as well as the code for generating the plots and maps.

5. **Output: Population Plot:**

The `renderPlot()` function is used to generate a bar chart showing the population of the selected countries. The plot is created using the `ggplot()` function from the `ggplot2` library.

6. **Output: GDP Plot:**

The `renderPlot()` function is used to generate a bar chart showing the GDP of the selected countries. The plot is created using the `ggplot()` function from the `ggplot2` library.

7. **Output: Map:**

The `renderLeaflet()` function is used to generate an interactive map showing the location of the selected countries. The map is created using the `leaflet()` function from the `leaflet` library.

8. **Run the Shiny App:**

Finally, the `shinyApp()` function is used to run the Shiny app. When you run the code, a web browser window will open, displaying the interactive dashboard. You can use the filters in the sidebar to explore the data and see how the visualizations and maps change accordingly.