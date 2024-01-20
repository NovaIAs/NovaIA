```
# Carga de librerías necesarias
library(tidyverse)
library(ggplot2)
library(shiny)

# Creación de datos
datos <- data.frame(
  fecha = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
  valor = rnorm(365, 100, 15)
)

# Creación de la aplicación Shiny
ui <- fluidPage(
  titlePanel("Datos Complejos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable:", c("fecha", "valor")),
      sliderInput("rango", "Rango:", min(datos$valor), max(datos$valor), value = c(min(datos$valor), max(datos$valor)))
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)

server <- function(input, output) {
  output$grafico <- renderPlot({
    datos %>%
      filter(input$variable == fecha) %>%
      filter(valor >= input$rango[1] & valor <= input$rango[2]) %>%
      ggplot(aes(x = fecha, y = valor)) +
      geom_line() +
      labs(title = "Datos Complejos", x = "Fecha", y = "Valor")
  })
}

# Ejecución de la aplicación Shiny
shinyApp(ui, server)
```

Explicación del código:

* Carga de librerías necesarias: se cargan las librerías `tidyverse`, `ggplot2` y `shiny`.
* Creación de datos: se crea un marco de datos llamado `datos` con una columna de fechas y una columna de valores.
* Creación de la aplicación Shiny: se crea una aplicación Shiny llamada "Datos Complejos".
* `ui`: se define la interfaz de usuario de la aplicación.
* `server`: se define la lógica de la aplicación.
* `output$grafico`: se define la salida de la aplicación, que es un gráfico.
* `shinyApp`: se ejecuta la aplicación Shiny.