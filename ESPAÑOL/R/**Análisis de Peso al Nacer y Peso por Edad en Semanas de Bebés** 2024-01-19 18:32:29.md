```r

# Cargar librerías (dplyr, lubridate, gplot2)
library(dplyr)
library(lubridate)
library(ggplot2)

# Cargar el dataset de datos_bebés
data_bebes <- read.csv("datos_bebés.csv")

# Filtrar los datos para incluir bebés con un peso al nacer mayor a 2500 gramos.
data_bebes_filtrado <- data_bebes %>%
  filter(peso_al_nacer > 2500)

# Crear una columna de "año_nacimiento" a partir de la columna "fecha_nacimiento".
data_bebes_filtrado$año_nacimiento <- year(data_bebes_filtrado$fecha_nacimiento)

# Agrupar los datos por "año_nacimiento" y calcular el promedio del peso al nacer.
data_bebes_agrupado <- data_bebes_filtrado %>%
  group_by(año_nacimiento) %>%
  summarize(peso_promedio = mean(peso_al_nacer))

# Crear un gráfico de líneas para mostrar el promedio del peso al nacer por año de nacimiento.
ggplot(data_bebes_agrupado, aes(x = año_nacimiento, y = peso_promedio)) +
  geom_line() +
  labs(title = "Peso Promedio al Nacer por Año de Nacimiento",
       x = "Año de Nacimiento",
       y = "Peso Promedio (gramos)")

# Crear una columna de "edad_en_semanas" a partir de la columna "fecha_nacimiento" y "fecha_control".
data_bebes_filtrado$edad_en_semanas <- (data_bebes_filtrado$fecha_control - data_bebes_filtrado$fecha_nacimiento) / 7

# Agrupar los datos por "edad_en_semanas" y calcular el promedio del peso.
data_bebes_agrupado_semanas <- data_bebes_filtrado %>%
  group_by(edad_en_semanas) %>%
  summarize(peso_promedio = mean(peso))

# Crear un gráfico de líneas para mostrar el promedio del peso por edad en semanas.
ggplot(data_bebes_agrupado_semanas, aes(x = edad_en_semanas, y = peso_promedio)) +
  geom_line() +
  labs(title = "Peso Promedio por Edad en Semanas",
       x = "Edad en Semanas",
       y = "Peso Promedio (gramos)")

```