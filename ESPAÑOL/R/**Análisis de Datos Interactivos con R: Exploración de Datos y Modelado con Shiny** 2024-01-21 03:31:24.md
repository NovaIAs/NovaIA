```
# Carga de bibliotecas necesarias
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shiny)

# Lectura de datos
datos <- read.csv("datos.csv")

# Limpieza de datos
datos <- datos %>%
  drop_na() %>%
  mutate(fecha = ymd(fecha))

# Creación de modelo de regresión lineal
modelo <- lm(valor ~ ., data = datos)

# Evaluación del modelo
summary(modelo)

# Creación de gráfico interactivo
ui <- fluidPage(
  titlePanel("Modelo de regresión lineal"),
  sidebarPanel(
    sliderInput("variable_x",
                "Variable X:",
                choices = names(datos),
                selected = "x1"),
    sliderInput("variable_y",
                "Variable Y:",
                choices = names(datos),
                selected = "y")
  ),
  mainPanel(
    plotOutput("plot")
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(datos, aes(x = !!input$variable_x, y = !!input$variable_y)) +
      geom_point() +
      geom_smooth(method = "lm")
  })
}

shinyApp(ui, server)

# Creación de función personalizada
mi_funcion <- function(x, y) {
  x + y
}

# Ejemplo de uso de la función
resultado <- mi_funcion(3, 4)

# Exportación de datos
write.csv(datos, "datos_limpios.csv")

# Exportación de modelo
save(modelo, file = "modelo.RData")
```

Explicación del código:

* Se cargan las bibliotecas necesarias.
* Se leen los datos de un archivo CSV.
* Se limpian los datos eliminando los valores faltantes y convirtiendo la columna de fecha a un formato adecuado.
* Se crea un modelo de regresión lineal usando la función `lm()`.
* Se evalúa el modelo usando la función `summary()`.
* Se crea un gráfico interactivo usando la librería `shiny`. El gráfico permite al usuario seleccionar las variables X e Y para ver el modelo de regresión lineal correspondiente.
* Se crea una función personalizada llamada `mi_funcion()`. La función toma dos argumentos, `x` e `y`, y devuelve su suma.
* Se utiliza la función `mi_funcion()` para sumar dos números.
* Se exportan los datos limpios a un archivo CSV.
* Se exporta el modelo a un archivo RData.