```
# Carga de las bibliotecas necesarias
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(plotly)
library(shiny)

# Se crea un conjunto de datos de ejemplo
datos <- data.frame(
  fecha = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
  valor = runif(365, 0, 100)
)

# Se crea una función para generar el gráfico
crear_grafico <- function(datos, tipo_grafico) {
  if (tipo_grafico == "línea") {
    ggplot(datos, aes(x = fecha, y = valor)) +
      geom_line() +
      labs(title = "Evolución del valor a lo largo del tiempo",
           x = "Fecha",
           y = "Valor")
  } else if (tipo_grafico == "barra") {
    ggplot(datos, aes(x = fecha, y = valor)) +
      geom_bar(stat = "identity") +
      labs(title = "Evolución del valor a lo largo del tiempo",
           x = "Fecha",
           y = "Valor")
  } else if (tipo_grafico == "dispersión") {
    ggplot(datos, aes(x = fecha, y = valor)) +
      geom_point() +
      labs(title = "Evolución del valor a lo largo del tiempo",
           x = "Fecha",
           y = "Valor")
  } else if (tipo_grafico == "gráfico interactivo") {
    p <- plot_ly(datos, x = ~fecha, y = ~valor, type = 'scatter', mode = 'lines') %>%
      layout(hovermode = 'closest')
    p
  }
}

# Se crea una interfaz de usuario con la función `shiny`
ui <- fluidPage(
  titlePanel("Visualización de datos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tipo_grafico", "Tipo de gráfico", choices = c("línea", "barra", "dispersión", "gráfico interactivo"))
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)

# Se crea la función para generar la salida
server <- function(input, output) {
  output$grafico <- renderPlot({
    crear_grafico(datos, input$tipo_grafico)
  })
}

# Se ejecuta la aplicación
shinyApp(ui, server)
```

Explicación del código:

1. Se cargan las bibliotecas necesarias.
2. Se crea un conjunto de datos de ejemplo con la función `data.frame()`.
3. Se crea una función `crear_grafico()` que recibe un conjunto de datos y un tipo de gráfico como argumentos y devuelve un gráfico.
4. Se crea una interfaz de usuario con la función `shiny::fluidPage()`.
5. Se crea la función `server()` que recibe los datos de entrada y salida como argumentos y genera la salida.
6. Se ejecuta la aplicación con la función `shinyApp()`.