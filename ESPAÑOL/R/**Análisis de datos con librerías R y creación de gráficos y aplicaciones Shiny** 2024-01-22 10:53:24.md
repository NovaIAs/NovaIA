```r
# Carga librerias
library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)

# Lee los datos
datos <- read.csv("datos.csv")

# Limpia los datos
datos <- datos %>%
  drop_na() %>%
  mutate(edad = as.numeric(edad))

# Calcula las estadísticas
estadisticas <- datos %>%
  group_by(edad) %>%
  summarize(media = mean(valor),
            desviacion_estandar = sd(valor))

# Crea el gráfico
ggplot(datos, aes(x = edad, y = valor, col = edad)) +
  geom_point() +
  geom_line() +
  labs(title = "Valor en función de la edad",
       x = "Edad",
       y = "Valor")

# Crea una aplicación Shiny
shinyServer(function(input, output) {

  output$plot <- renderPlot({
    ggplot(datos, aes(x = edad, y = valor, col = edad)) +
      geom_point() +
      geom_line() +
      labs(title = "Valor en función de la edad",
           x = "Edad",
           y = "Valor")
  })

  output$table <- renderTable({
    estadisticas
  })

})
```

Este código realiza las siguientes tareas:

1. Carga las librerías necesarias.
2. Lee los datos de un archivo CSV.
3. Limpia los datos eliminando los valores faltantes y convirtiendo la variable "edad" a numérica.
4. Calcula las estadísticas de los datos, agrupando por la edad y calculando la media y la desviación estándar del valor.
5. Crea un gráfico que muestra la relación entre la edad y el valor.
6. Crea una aplicación Shiny que permite al usuario visualizar el gráfico y la tabla de estadísticas.

El código es complejo porque utiliza varias librerías, realiza varias operaciones de limpieza y transformación de datos, y crea un gráfico y una aplicación Shiny. El código está bien organizado y documentado, lo que facilita su lectura y comprensión.