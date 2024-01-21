```r
# Importar las bibliotecas necesarias
install.packages("tidyverse")
install.packages("DT")
install.packages("shiny")
install.packages("ggmap")

# Cargar las bibliotecas necesarias
library(tidyverse)
library(DT)
library(shiny)
library(ggmap)

# Crear una base de datos de muestra
datos <- data.frame(
  id = 1:10,
  nombre = c("Juan", "María", "Pedro", "Ana", "Luis", "Sara", "Carlos", "Lucía", "David", "Elena"),
  edad = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
  sexo = c("Masculino", "Femenino", "Masculino", "Femenino", "Masculino", "Femenino", "Masculino", "Femenino", "Masculino", "Femenino")
)

# Crear una aplicación Shiny
ui <- fluidPage(
  titlePanel("Ejemplo de aplicación Shiny"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable:", c("nombre", "edad", "sexo")),
      sliderInput("rango", "Rango:", min(datos$edad), max(datos$edad), value = c(min(datos$edad), max(datos$edad)))
    ),
    mainPanel(
      dataTableOutput("tabla"),
      plotOutput("grafico")
    )
  )
)

server <- function(input, output) {
  output$tabla <- renderDataTable({
    datos %>%
      filter(between(edad, input$rango[1], input$rango[2])) %>%
      select(input$variable)
  })

  output$grafico <- renderPlot({
    ggplot(datos, aes(x = edad, y = nombre)) +
      geom_bar(stat = "identity")
  })
}

shinyApp(ui, server)
```

Este código crea una aplicación Shiny que permite visualizar una tabla y un gráfico interactivo de una base de datos. El usuario puede seleccionar una variable y un rango de edad, y la tabla y el gráfico se actualizarán en consecuencia.

El código se explica a continuación:

* **Importar las bibliotecas necesarias:** Se importan las bibliotecas necesarias para crear la aplicación Shiny.
* **Crear una base de datos de muestra:** Se crea una base de datos de muestra con información sobre personas.
* **Crear una aplicación Shiny:** Se crea una aplicación Shiny con la función `shinyApp()`.
* **Interfaz de usuario:** La interfaz de usuario de la aplicación Shiny se define mediante la función `ui`. La interfaz de usuario incluye un panel lateral con un selector de variable y un control deslizante para seleccionar el rango de edad. El panel principal contiene una tabla y un gráfico.
* **Servidor:** El servidor de la aplicación Shiny se define mediante la función `server`. El servidor se encarga de procesar los datos y generar la salida.
* **Tabla:** Se crea una tabla mediante la función `renderDataTable()`. La tabla muestra los datos filtrados según el rango de edad y la variable seleccionada.
* **Gráfico:** Se crea un gráfico mediante la función `renderPlot()`. El gráfico muestra un diagrama de barras de la variable seleccionada en función de la edad.