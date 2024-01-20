```r
# Se carga la librería `tidyverse`, que contiene un conjunto de paquetes para el análisis de datos en R.
library(tidyverse)

# Se cargan los datos de ejemplo del paquete `tidyverse`, que incluyen un conjunto de datos llamado `mpg`.
data(mpg)

# Se crea un modelo lineal generalizado (GLM) para predecir el consumo de combustible (`hwy`) en función de la cilindrada del motor (`displ`), el peso del vehículo (`weight`) y el año del modelo (`year`).
model <- glm(hwy ~ displ + weight + year, data = mpg, family = gaussian)

# Se realiza una prueba de hipótesis para determinar si existe una relación significativa entre las variables predictoras y la variable respuesta.
anova(model)

# Se crean predicciones del consumo de combustible para un conjunto de nuevos datos.
new_data <- data.frame(displ = c(200, 250, 300), weight = c(2000, 2500, 3000), year = c(1990, 2000, 2010))
predictions <- predict(model, new_data)

# Se grafican los resultados del modelo.
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Consumo de combustible en función de la cilindrada del motor",
       x = "Cilindrada del motor (pulgadas cúbicas)",
       y = "Consumo de combustible (millas por galón)")

# Se imprime el resumen del modelo.
summary(model)

# Se calculan los intervalos de confianza del 95% para las estimaciones de los parámetros del modelo.
confint(model)

# Se diagnostican los residuos del modelo.
resid <- model$resid
qqnorm(resid)
qqline(resid)
```

Este código realiza un análisis estadístico de los datos de consumo de combustible de los vehículos del conjunto de datos `mpg`. Se crea un modelo de regresión lineal generalizado (GLM) para predecir el consumo de combustible en función de la cilindrada del motor, el peso del vehículo y el año del modelo. Se realiza una prueba de hipótesis para determinar si existe una relación significativa entre las variables predictoras y la variable respuesta. Se crean predicciones del consumo de combustible para un conjunto de nuevos datos. Se grafican los resultados del modelo. Se imprime el resumen del modelo. Se calculan los intervalos de confianza del 95% para las estimaciones de los parámetros del modelo. Se diagnostican los residuos del modelo.