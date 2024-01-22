```
#Librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

#Cargar datos
datos <- read.csv("datos.csv")

#Convertir la columna de fecha a tipo fecha
datos$fecha <- lubridate::ymd(datos$fecha)

#Agregar columnas de año, mes y día
datos <- datos %>%
mutate(
 #Extraer el año de la fecha
  año = lubridate::year(fecha),
  #Extraer el mes de la fecha
  mes = lubridate::month(fecha),
  #Extraer el día de la fecha
  dia = lubridate::day(fecha))

# Crear un gráfico de líneas que muestre la tendencia de las ventas en el tiempo
ggplot(datos, aes(x = fecha, y = ventas)) +
  geom_line() +
  labs(title = "Tendencia de las ventas en el tiempo",
       x = "Fecha",
       y = "Ventas")

#Crear un gráfico de barras que muestre las ventas totales por mes
ggplot(datos, aes(x = mes, y = ventas)) +
  geom_bar(stat = "sum") +
  labs(title = "Ventas totales por mes",
       x = "Mes",
       y = "Ventas")

#Crear un gráfico de caja que muestre la distribución de las ventas por día de la semana
ggplot(datos, aes(x = factor(wday(fecha, label = TRUE)), y = ventas)) +
  geom_boxplot() +
  labs(title = "Distribución de las ventas por día de la semana",
       x = "Día de la semana",
       y = "Ventas")

#Crear una tabla que muestre las ventas promedio por año
datos %>%
  group_by(año) %>%
  summarise(ventas_promedio = mean(ventas))
```

Este código es complejo y diferenciado porque:

* Utiliza varias librerías para realizar diferentes tareas.
* Carga un conjunto de datos desde un archivo CSV.
* Convierte una columna de fecha a tipo fecha.
* Agrega nuevas columnas al conjunto de datos.
* Crea tres gráficos diferentes utilizando la librería `ggplot2`.
* Crea una tabla utilizando la librería `dplyr`.