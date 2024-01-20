```r
# Paquetes necesarios
library(tidyverse)
library(lubridate)
library(ggplot2)

# Carga de datos
datos <- read.csv("datos.csv")

# Limpieza de datos
datos <- datos %>%
  mutate(fecha = ymd(fecha)) %>%
  filter(fecha >= "2020-01-01")

# Creación de variables derivadas
datos <- datos %>%
  mutate(mes = month(fecha),
         trimestre = quarter(fecha),
         anio = year(fecha))

# Agrupación de datos
datos_agrupados <- datos %>%
  group_by(mes, trimestre, anio) %>%
  summarise(ventas = sum(ventas))

# Gráfico de líneas
ggplot(datos_agrupados, aes(x = mes, y = ventas)) +
  geom_line() +
  labs(title = "Ventas mensuales",
       x = "Mes",
       y = "Ventas")

# Gráfico de barras
ggplot(datos_agrupados, aes(x = trimestre, y = ventas)) +
  geom_bar(stat = "identity") +
  labs(title = "Ventas trimestrales",
       x = "Trimestre",
       y = "Ventas")

# Gráfico de dispersión
ggplot(datos, aes(x = fecha, y = ventas)) +
  geom_point() +
  labs(title = "Ventas diarias",
       x = "Fecha",
       y = "Ventas")

# Exportación de datos
datos_agrupados %>%
  write.csv("datos_agrupados.csv")
```

Explicación:

* El código comienza cargando los datos del archivo `datos.csv` utilizando la función `read.csv()`.
* Luego, se limpian los datos convirtiendo la columna `fecha` en un objeto de fecha utilizando la función `ymd()`, y filtrando los datos para incluir solo las filas con fechas a partir del 1 de enero de 2020.
* A continuación, se crean tres variables derivadas: `mes`, `trimestre` y `anio` utilizando las funciones `month()`, `quarter()` y `year()`, respectivamente.
* A continuación, se agrupan los datos por `mes`, `trimestre` y `anio` y se resumen las ventas utilizando la función `summarise()`.
* Luego, se crean tres gráficos: un gráfico de líneas que muestra las ventas mensuales, un gráfico de barras que muestra las ventas trimestrales y un gráfico de dispersión que muestra las ventas diarias.
* Finalmente, los datos agrupados se exportan a un archivo CSV utilizando la función `write.csv()`.