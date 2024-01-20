```r
# Carga de librerías
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(stringr)
library(patchwork)
library(scales)

# Carga de datos
datos <- read.csv("datos.csv")

# Limpieza de datos
datos <- datos %>%
  drop_na() %>%
  mutate(
    fecha = ymd(fecha),
    producto = as.factor(producto)
  )

# Cálculo de métricas
metricas <- datos %>%
  group_by(producto, fecha) %>%
  summarise(
    ventas = sum(ventas),
    valor_total = sum(ventas * precio),
    clientes = n_distinct(cliente)
  )

# Visualización de datos
ggplot(metricas, aes(x = fecha, y = ventas, color = producto)) +
  geom_line() +
  facet_wrap(~producto) +
  labs(title = "Ventas por producto",
       x = "Fecha",
       y = "Ventas")

# Análisis de correlación
corr <- cor(metricas[, c("ventas", "valor_total", "clientes")])
heatmap(corr, col = rainbow(20))

# Modelo de regresión lineal
modelo <- lm(ventas ~ valor_total + clientes, data = metricas)
summary(modelo)

# Predicción de ventas
predicciones <- predict(modelo, newdata = datos[, c("valor_total", "clientes")])

# Visualización de predicciones
ggplot(metricas, aes(x = fecha, y = ventas, color = producto)) +
  geom_line() +
  facet_wrap(~producto) +
  labs(title = "Ventas reales y predichas",
       x = "Fecha",
       y = "Ventas") +
  geom_line(aes(y = predicciones), linetype = "dashed")

# Clasificación de clientes
clientes <- datos %>%
  select(cliente, producto, ventas) %>%
  group_by(cliente) %>%
  summarise(
    producto_favorito = max(producto, na.rm = TRUE),
    total_gastado = sum(ventas)
  ) %>%
  mutate(
    segmento = case_when(
      total_gastado > 1000 ~ "Alto",
      total_gastado > 500 ~ "Medio",
      TRUE ~ "Bajo"
    )
  )

# Visualización de segmentos de clientes
ggplot(clientes, aes(x = producto_favorito, y = total_gastado, color = segmento)) +
  geom_point() +
  labs(title = "Segmentación de clientes",
       x = "Producto favorito",
       y = "Total gastado")

# Análisis de supervivencia
supervivencia <- datos %>%
  group_by(cliente, producto) %>%
  filter(fecha == max(fecha)) %>%
  summarise(
    ultima_compra = max(fecha),
    dias_desde_ultima_compra = as.numeric(Sys.Date() - ultima_compra)
  )

ggplot(supervivencia, aes(x = dias_desde_ultima_compra, color = producto)) +
  geom_survival() +
  labs(title = "Análisis de supervivencia de clientes",
       x = "Días desde la última compra",
       y = "Probabilidad de volver a comprar")
```

**Explicación del código:**

Este código realiza un análisis de datos completo sobre un conjunto de datos de ventas. El código incluye:

* Carga y limpieza de datos
* Cálculo de métricas de ventas
* Visualización de datos
* Análisis de correlación
* Modelado de regresión lineal
* Predicción de ventas
* Clasificación de clientes
* Análisis de supervivencia

El código está comentado en español para facilitar su comprensión.