```r
#Carga de las bibliotecas necesarias
install.packages("tidyverse")
install.packages("plotly")
install.packages("ggfortify")
library(tidyverse)
library(plotly)
library(ggfortify)

#Carga de los datos
datos <- read.csv("datos.csv")

#Limpieza de datos
datos <- datos %>%
  drop_na() %>%
  mutate(edad = as.numeric(edad))

#Exploración de los datos
ggplot(datos, aes(x = edad, y = altura)) +
  geom_point() +
  geom_smooth(method = "lm")

#Modelo de regresión lineal
modelo <- lm(altura ~ edad, data = datos)

#Evaluación del modelo
summary(modelo)

#Predicción de la altura
predicciones <- predict(modelo, newdata = datos)

#Visualización de las predicciones
ggplot(datos, aes(x = edad, y = altura)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_line(aes(y = predicciones))

#Creación de una aplicación interactiva con Shiny
ui <- fluidPage(
  titlePanel("Predicción de la altura"),
  sidebarPanel(
    sliderInput("edad", "Edad", min = min(datos$edad), max = max(datos$edad), value = 18)
  ),
  mainPanel(
    plotOutput("prediccion")
  )
)

server <- function(input, output) {
  output$prediccion <- renderPlot({
    prediccion <- predict(modelo, newdata = data.frame(edad = input$edad))
    ggplot(datos, aes(x = edad, y = altura)) +
      geom_point() +
      geom_smooth(method = "lm") +
      geom_line(aes(y = prediccion), color = "red")
  })
}

shinyApp(ui, server)
```
Explicación del código:

1. Cargamos las bibliotecas necesarias:

* `tidyverse`: una colección de paquetes para el análisis de datos en R.
* `plotly`: un paquete para crear gráficos interactivos.
* `ggfortify`: un paquete para crear gráficos más bonitos.

2. Cargamos los datos de un archivo CSV.

3. Limpiamos los datos eliminando las filas con valores faltantes y convirtiendo la columna `edad` a numérica.

4. Exploramos los datos con un gráfico de dispersión.

5. Creamos un modelo de regresión lineal para predecir la altura en función de la edad.

6. Evaluamos el modelo con la función `summary`.

7. Predecimos la altura para cada fila de los datos.

8. Visualizamos las predicciones en un gráfico.

9. Creamos una aplicación interactiva con Shiny para permitir al usuario predecir su propia altura.

10. La función `ui` define la interfaz de usuario de la aplicación, incluyendo un panel lateral con un control deslizante para introducir la edad y un panel principal con un gráfico.

11. La función `server` define la lógica de la aplicación. Cuando el usuario cambia el valor del control deslizante, la función `renderPlot` se ejecuta para actualizar el gráfico con la nueva predicción.

12. Ejecutamos la aplicación con el comando `shinyApp`, que abre una ventana del navegador web con la aplicación en ejecución.