```R
# Importar las librerías necesarias
library(tidyverse)
library(ggplot2)
library(lubridate)

# Leer los datos de un archivo CSV
datos <- read.csv("datos.csv")

# Limpiar los datos
datos <- datos %>%
  # Eliminar columnas innecesarias
  select(-c(columna1, columna2)) %>%
  # Convertir la columna de fecha a formato de fecha
  mutate(fecha = as_date(fecha)) %>%
  # Convertir los valores numéricos en el tipo correcto
  mutate_at(vars(columna3, columna4), as.numeric)

# Crear un gráfico de líneas
ggplot(datos, aes(x = fecha, y = columna3, color = columna4)) +
  geom_line() +
  labs(title = "Título del gráfico",
       x = "Fecha",
       y = "Valor")

# Crear un modelo de regresión lineal
modelo <- lm(columna3 ~ columna4, data = datos)

# Obtener las predicciones del modelo
predicciones <- datos %>%
  mutate(prediccion = predict(modelo))

# Crear un gráfico de dispersión con la línea de regresión
ggplot(datos, aes(x = columna3, y = prediccion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Línea de regresión",
       x = "Valor real",
       y = "Valor predicho")

# Crear un informe HTML con los resultados
informe <- htmltools::tagList(
  htmltools::h1("Informe de análisis de datos"),
  htmltools::p("Este informe presenta los resultados del análisis de datos realizado en el archivo 'datos.csv'."),
  htmltools::h2("Gráfico de líneas"),
  ggplotly(ggplot(datos, aes(x = fecha, y = columna3, color = columna4)) +
    geom_line() +
    labs(title = "Título del gráfico",
         x = "Fecha",
         y = "Valor")),
  htmltools::h2("Línea de regresión"),
  ggplotly(ggplot(datos, aes(x = columna3, y = prediccion)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(title = "Línea de regresión",
         x = "Valor real",
         y = "Valor predicho")),
  htmltools::h2("Resultados del modelo"),
  htmltools::table(summary(modelo))
)

# Guardar el informe en un archivo HTML
save_html(informe, "informe.html")

# Crear un archivo PDF con el informe
pdf("informe.pdf", width = 8, height = 11)
grid.table(informe)
dev.off()
```

Este código es un ejemplo de un análisis de datos completo en R. El código realiza las siguientes tareas:

1. Importa las librerías necesarias.
2. Lee los datos de un archivo CSV.
3. Limpia los datos.
4. Crea un gráfico de líneas.
5. Crea un modelo de regresión lineal.
6. Obtiene las predicciones del modelo.
7. Crea un gráfico de dispersión con la línea de regresión.
8. Crea un informe HTML con los resultados.
9. Guarda el informe en un archivo HTML.
10. Crea un archivo PDF con el informe.

El código está escrito en español y es fácil de entender. Utiliza las últimas versiones de las librerías tidyverse y ggplot2, lo que lo hace eficiente y potente. El código también utiliza la función `save_html()` para guardar el informe en un archivo HTML y la función `pdf()` para guardar el informe en un archivo PDF.