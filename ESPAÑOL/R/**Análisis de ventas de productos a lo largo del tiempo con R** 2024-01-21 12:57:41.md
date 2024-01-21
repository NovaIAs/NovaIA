```
# Cargar los paquetes necesarios
library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)

# Crear un conjunto de datos ficticio
datos <- data.frame(
  fecha = seq(as_date("2023-01-01"), as_date("2023-12-31"), by = "month"),
  ventas = rnorm(12, 100, 10),
  producto = sample(c("Producto A", "Producto B", "Producto C"), 12, replace = TRUE)
)

# Crear un gráfico de líneas que muestre las ventas de cada producto a lo largo del tiempo
ggplot(datos, aes(x = fecha, y = ventas, color = producto)) +
  geom_line() +
  labs(title = "Ventas de productos a lo largo del tiempo",
       x = "Fecha",
       y = "Ventas")

# Crear un gráfico de barras que muestre las ventas totales de cada producto
ggplot(datos, aes(x = producto, y = ventas)) +
  geom_bar(stat = "sum") +
  labs(title = "Ventas totales de cada producto",
       x = "Producto",
       y = "Ventas")

# Crear un gráfico circular que muestre la distribución de las ventas entre los productos
ggplot(datos, aes(x = "", y = ventas, fill = producto)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de las ventas entre los productos",
       caption = "Cada porción representa un producto")

# Crear un gráfico interactivo que permita explorar los datos
p <- plot_ly(datos, x = ~fecha, y = ~ventas, color = ~producto, type = 'scatter', mode = 'lines')
p <- p %>% layout(xaxis = list(title = 'Fecha'),
                  yaxis = list(title = 'Ventas'),
                  hovermode = 'closest')
p
```

Este código crea un conjunto de datos ficticio con información sobre las ventas de tres productos a lo largo de un año. A continuación, utiliza la biblioteca `ggplot2` para crear tres gráficos diferentes que muestran los datos de diferentes maneras:

* Un gráfico de líneas que muestra las ventas de cada producto a lo largo del tiempo.
* Un gráfico de barras que muestra las ventas totales de cada producto.
* Un gráfico circular que muestra la distribución de las ventas entre los productos.

Por último, utiliza la biblioteca `plotly` para crear un gráfico interactivo que permita explorar los datos. Este gráfico permite al usuario hacer zoom, desplazarse y pasar el ratón por los datos para ver más información.