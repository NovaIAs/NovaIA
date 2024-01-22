```r
# Importar librerías necesarias
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shiny)

# Datos y limpieza
datos <- read.csv("datos.csv")
datos <- datos %>%
  mutate(fecha = ymd(fecha)) %>%
  filter(fecha > "2020-01-01")

# Modelo predictivo
modelo <- lm(valor_objetivo ~ variable_1 + variable_2, data = datos)

# Crear aplicación Shiny
ui <- fluidPage(
  titlePanel("Predicción de valor objetivo"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("variable_1", "Variable 1", min = min(datos$variable_1), max = max(datos$variable_1), value = mean(datos$variable_1)),
      sliderInput("variable_2", "Variable 2", min = min(datos$variable_2), max = max(datos$variable_2), value = mean(datos$variable_2))
    ),
    mainPanel(
      plotOutput("prediccion")
    )
  )
)

server <- function(input, output) {
  output$prediccion <- renderPlot({
    datos_nuevos <- data.frame(variable_1 = input$variable_1, variable_2 = input$variable_2)
    prediccion <- predict(modelo, newdata = datos_nuevos)
    
    ggplot(data = datos_nuevos, aes(x = variable_1, y = variable_2, color = prediccion)) +
      geom_point() +
      labs(title = "Predicción de valor objetivo", x = "Variable 1", y = "Variable 2", color = "Valor objetivo")
  })
}

shinyApp(ui, server)
```

**Explicación del código:**

1. Se importan las librerías necesarias.


2. Se leen y limpian los datos.


3. Se entrena un modelo predictivo.


4. Se crea una aplicación Shiny con `shiny`. La aplicación incluye una interfaz de usuario (UI) y un servidor (server).


5. En la UI, se define el título de la aplicación y un panel lateral con dos sliders para introducir los valores de las variables predictoras. El panel principal contiene un espacio para mostrar la gráfica de predicción.


6. En el servidor, se define la función `renderPlot()`, que se ejecuta cada vez que los valores de los sliders cambian. Esta función genera una gráfica de dispersión con los valores de las variables predictoras y el valor objetivo predicho por el modelo.