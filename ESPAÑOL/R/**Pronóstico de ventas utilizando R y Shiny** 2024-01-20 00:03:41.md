# **Cargamos las bibliotecas necesarias.**

```
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(forecast)
library(tseries)
library(xts)
library(zoo)
library(plotly)
library(shiny)
```

# **Creamos un conjunto de datos de ejemplo.**

```
datos <- data.frame(
  fecha = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month"),
  ventas = c(100, 120, 150, 180, 200, 220, 250, 280, 300, 320, 350, 380)
)
```

# **Exploramos los datos.**

```
ggplot(datos, aes(x = fecha, y = ventas)) +
  geom_line() +
  labs(title = "Ventas mensuales",
       x = "Fecha",
       y = "Ventas")
```

# **Preprocesamos los datos.**

```
datos <- datos %>%
  mutate(
    anio = year(fecha),
    mes = month(fecha),
    dia = day(fecha)
  ) %>%
  select(-fecha)
```

# **Creamos un modelo de pronóstico de series temporales.**

```
modelo <- auto.arima(datos$ventas, seasonal = FALSE)
```

# **Pronosticamos las ventas futuras.**

```
pronostico <- forecast(modelo, h = 12, level = 0.95)
```

# **Visualizamos el pronóstico.**

```
ggplot(datos, aes(x = fecha, y = ventas)) +
  geom_line() +
  geom_line(data = pronostico, aes(y = .pred), color = "red") +
  labs(title = "Ventas mensuales",
       x = "Fecha",
       y = "Ventas")
```

# **Creamos una aplicación Shiny para interactuar con el modelo.**

```
ui <- fluidPage(
  titlePanel("Pronóstico de ventas mensuales"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("horizonte", "Horizonte de pronóstico:", min = 1, max = 12, value = 6),
      sliderInput("nivel", "Nivel de confianza:", min = 0.5, max = 1, value = 0.95)
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)

server <- function(input, output) {
  output$grafico <- renderPlot({
    pronostico <- forecast(modelo, h = input$horizonte, level = input$nivel)
    
    ggplot(datos, aes(x = fecha, y = ventas)) +
      geom_line() +
      geom_line(data = pronostico, aes(y = .pred), color = "red") +
      labs(title = "Ventas mensuales",
           x = "Fecha",
           y = "Ventas")
  })
}

shinyApp(ui, server)
```

# **Explicación del código:**

* El código comienza cargando las bibliotecas necesarias para realizar el análisis.
* Luego, se crea un conjunto de datos de ejemplo que contiene las ventas mensuales de un producto.
* A continuación, se exploran los datos mediante un gráfico de líneas.
* Posteriormente, se preprocesan los datos para eliminar la columna de fecha y crear nuevas columnas para el año, el mes y el día.
* Seguidamente, se crea un modelo de pronóstico de series temporales utilizando la función `auto.arima()`.
* Entonces, se pronostican las ventas futuras utilizando la función `forecast()`.
* A continuación, se visualiza el pronóstico junto con los datos históricos en un gráfico de líneas.
* Por último, se crea una aplicación Shiny que permite a los usuarios interactuar con el modelo y visualizar el pronóstico para diferentes horizontes y niveles de confianza.