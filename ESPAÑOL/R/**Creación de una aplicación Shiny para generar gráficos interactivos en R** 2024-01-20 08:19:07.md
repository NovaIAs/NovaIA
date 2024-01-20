```
# Importar las librerías necesarias
library(tidyverse)
library(ggplot2)
library(shiny)

# Crear un conjunto de datos
datos <- tibble(
  x = runif(100, 0, 1),
  y = runif(100, 0, 1),
  color = sample(c("rojo", "azul", "verde"), 100, replace = TRUE)
)

# Crear una función para generar un gráfico
grafica <- function(datos) {
  ggplot(datos, aes(x, y, color = color)) +
    geom_point()
}

# Crear una aplicación brillante
ui <- fluidPage(
  plotOutput("grafica")
)

server <- function(input, output) {
  output$grafica <- renderPlot({
    grafica(datos)
  })
}

shinyApp(ui, server)
```

Este código crea un conjunto de datos con 100 filas y 3 columnas. La columna x contiene valores aleatorios entre 0 y 1. La columna y contiene valores aleatorios entre 0 y 1. La columna color contiene uno de los tres colores: rojo, azul o verde.

Luego, el código crea una función llamada grafica() que toma un conjunto de datos como entrada y genera un gráfico de dispersión de los datos.

El código también crea una aplicación Shiny que aloja el gráfico. La aplicación Shiny permite al usuario interactuar con el gráfico, por ejemplo, cambiando el tamaño de los puntos o el color de las líneas.

Para ejecutar la aplicación Shiny, puede usar el siguiente comando:

```
shiny::runApp()
```

Esto abrirá una ventana del navegador con la aplicación Shiny en ejecución. Puede interactuar con el gráfico haciendo clic y arrastrando en él.

Este código es complejo porque utiliza varias librerías diferentes y combina elementos de R base, tidyverse y Shiny. Sin embargo, también es un ejemplo poderoso de cómo R se puede utilizar para crear aplicaciones interactivas y visualizaciones.