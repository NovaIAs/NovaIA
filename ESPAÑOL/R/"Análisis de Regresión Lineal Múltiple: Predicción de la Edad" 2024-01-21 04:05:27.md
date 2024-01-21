```r
# Carga de las librerías
library(tidyverse)
library(tidymodels)
library(patchwork)
library(scales)

# Carga de los datos
datos <- read.csv("datos.csv")

# Preprocesamiento de los datos
datos_limpios <- datos %>%
  drop_na() %>%
  mutate(edad = as.factor(edad))

# Partición de los datos en train y test
datos_split <- initial_split(datos_limpios, prop = 0.75)
datos_train <- training(datos_split)
datos_test <- testing(datos_split)

# Creación de un modelo de regresión lineal múltiple
modelo <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(edad ~ ., data = datos_train)

# Evaluación del modelo en los datos de test
predicciones <- predict(modelo, datos_test)
rmse <- sqrt(mean((predicciones - datos_test$edad)^2))
print(rmse)

# Visualización de los resultados
ggplot(datos_test, aes(x = edad, y = predicciones)) +
  geom_point() +
  geom_abline(color = "red") +
  labs(title = "Regresión lineal múltiple",
       x = "Edad",
       y = "Predicción")

# Búsqueda de los mejores hiperparámetros
tune_grid <- grid_regular(penalty = c(0, 0.1, 0.5, 1),
                         tolerance = c(1e-6, 1e-4, 1e-2))
modelo_tuneado <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  tune_grid(tune_grid)

# Evaluación del modelo tuneado en los datos de test
predicciones_tuneadas <- predict(modelo_tuneado, datos_test)
rmse_tuneado <- sqrt(mean((predicciones_tuneadas - datos_test$edad)^2))
print(rmse_tuneado)

# Comparación de los resultados
plot_model(modelo, modelo_tuneado) +
  theme_minimal()
```

Este código es un ejemplo de un análisis de regresión lineal múltiple en R. El código comienza cargando las librerías necesarias y los datos. A continuación, los datos se limpian y se dividen en datos de entrenamiento y datos de prueba.

Luego, se crea un modelo de regresión lineal múltiple utilizando la función `linear_reg()`. El modelo se ajusta a los datos de entrenamiento y se evalúa en los datos de prueba utilizando la función `predict()`. El error cuadrático medio (RMSE) se calcula como una medida del rendimiento del modelo.

A continuación, se visualizan los resultados del modelo utilizando la función `ggplot()`. La gráfica muestra la relación entre la edad y la predicción del modelo.

Por último, se realiza una búsqueda de los mejores hiperparámetros para el modelo utilizando la función `tune_grid()`. Los hiperparámetros son los parámetros del modelo que se pueden ajustar para mejorar su rendimiento. Una vez que se han encontrado los mejores hiperparámetros, el modelo se vuelve a evaluar en los datos de prueba y se compara con el modelo original.