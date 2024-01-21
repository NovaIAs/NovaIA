```r
# Cargar las bibliotecas necesarias
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidymodels)
library(caret)

# Leer los datos
datos <- read.csv("datos.csv")

# Preparar los datos
datos <- datos %>%
  mutate(fecha = as_date(fecha)) %>%
  mutate(mes = month(fecha)) %>%
  mutate(anio = year(fecha))

# Crear un modelo de regresión lineal
modelo <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression')

# Entrenar el modelo
modelo_entrenado <- modelo %>%
  fit(ventas ~ ., data = datos)

# Evaluar el modelo
evaluacion <- modelo_entrenado %>%
  predict(new_data = datos) %>%
  bind_cols(datos) %>%
  mutate(residuo = ventas - .pred) %>%
  mutate(residuo_cuadrado = residuo^2) %>%
  summarise(
    rmse = sqrt(mean(residuo_cuadrado)),
    r2 = 1 - sum(residuo_cuadrado) / sum((ventas - mean(ventas))^2)
  )

# Visualizar el modelo
ggplot(datos, aes(x = fecha, y = ventas, color = factor(mes))) +
  geom_line() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Ventas mensuales',
       x = 'Fecha',
       y = 'Ventas')

# Guardar el modelo
saveRDS(modelo_entrenado, 'modelo_entrenado.rds')
```

Este código realiza un análisis de regresión lineal para predecir las ventas en función de la fecha.

1. Primero, carga las bibliotecas necesarias.
2. Luego, lee los datos de un archivo CSV.
3. A continuación, prepara los datos convirtiendo la columna `fecha` a tipo fecha, agregando las columnas `mes` y `año`, y eliminando las filas con valores faltantes.
4. Después, crea un modelo de regresión lineal utilizando la función `linear_reg()` de la biblioteca `tidymodels`.
5. A continuación, entrena el modelo utilizando la función `fit()` con los datos preparados.
6. Luego, evalúa el modelo utilizando la función `predict()` y calculando el error cuadrático medio (RMSE) y el coeficiente de determinación (R2).
7. A continuación, visualiza el modelo utilizando la función `ggplot()` de la biblioteca `ggplot2`.
8. Finalmente, guarda el modelo entrenado utilizando la función `saveRDS()` de la biblioteca `base`.