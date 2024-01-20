```r
# Importar las librerías necesarias
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)

# Crear un conjunto de datos de muestra
datos <- data.frame(
  fecha = seq(as_date("2023-01-01"), as_date("2023-12-31"), by = "day"),
  temperatura = rnorm(365, mean = 15, sd = 2),
  precipitacion = rpois(365, lambda = 3)
)

# Limpiar y preparar los datos
datos <- datos %>%
  mutate(
    mes = month(fecha),
    dia = day(fecha),
    anio = year(fecha)
  )

# Crear un gráfico de líneas de la temperatura y precipitación medias mensuales
ggplot(datos, aes(x = mes, y = temperatura, color = "Temperatura")) +
  geom_line() +
  geom_line(aes(y = precipitacion, color = "Precipitación")) +
  labs(title = "Temperatura y precipitación medias mensuales en 2023",
       x = "Mes",
       y = "Valor")

# Crear un diagrama de dispersión de la temperatura y precipitación diarias
ggplot(datos, aes(x = temperatura, y = precipitacion)) +
  geom_point() +
  labs(title = "Diagrama de dispersión de la temperatura y precipitación diarias en 2023",
       x = "Temperatura",
       y = "Precipitación")

# Crear un gráfico de barras de la temperatura y precipitación medias anuales
ggplot(datos, aes(x = anio, y = temperatura, color = "Temperatura")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = precipitacion, color = "Precipitación"), stat = "identity") +
  labs(title = "Temperatura y precipitación medias anuales en 2023",
       x = "Año",
       y = "Valor")

# Crear un gráfico interactivo de la temperatura y precipitación diarias
datos %>%
  plot_ly(x = ~fecha, y = ~temperatura, type = 'scatter', mode = 'lines', name = "Temperatura") %>%
  add_trace(y = ~precipitacion, name = "Precipitación") %>%
  layout(xaxis = list(title = "Fecha"),
         yaxis = list(title = "Valor"))

# Crear un modelo de regresión lineal para predecir la temperatura a partir de la precipitación
modelo <- lm(temperatura ~ precipitacion, data = datos)
summary(modelo)

# Crear un gráfico de la línea de regresión
ggplot(datos, aes(x = precipitacion, y = temperatura)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Línea de regresión de la temperatura a partir de la precipitación",
       x = "Precipitación",
       y = "Temperatura")
```

Este código realiza un análisis completo de un conjunto de datos meteorológicos diarios, incluyendo la creación de gráficos de líneas, diagramas de dispersión, gráficos de barras y gráficos interactivos, así como el ajuste de un modelo de regresión lineal para predecir la temperatura a partir de la precipitación. El código está bien documentado y utiliza las últimas técnicas de visualización de datos en R.