```r
# Importación de bibliotecas
library(tidyverse)
library(tidymodels)
library(bayesrules)
library(sf)
library(tidytext)

# Carga de datos
datos <- read.csv("datos.csv") %>%
  mutate(fecha = lubridate::mdy(fecha))

# Limpieza de datos
datos <- datos %>%
  clean_names() %>%
  drop_na() %>%
  mutate(categoria = as.factor(categoria))

# Creación de características
datos <- datos %>%
  mutate(longitud_texto = str_length(texto)) %>%
  mutate(num_palabras = str_count(texto, "\\w+")) %>%
  mutate(num_signos_puntuacion = str_count(texto, "\\p{punct}"))

# División de datos en entrenamiento y prueba
set.seed(123)
datos_train <- initial_split(datos, prop = 0.75)
datos_test <- testing(datos_train)

# Creación de clasificador bayesiano
modelo_bayesiano <- bayesrules(categoria ~ ., data = datos_train)

# Evaluación del clasificador
predicciones <- predict(modelo_bayesiano, datos_test)
evaluacion <- calculate_metrics(datos_test$categoria, predicciones)

# Visualización de resultados
bayes_performance <- perf(datos_test$categoria, predicciones, measure = "auc")
bayes_curve <- create_perf_curve(bayes_performance)
ggplot(bayes_curve, aes(x = recall, y = specificity)) +
  geom_line() +
  labs(title = "Curva ROC del clasificador bayesiano")

# Guardado del modelo
saveRDS(modelo_bayesiano, "modelo_bayesiano.rds")

# Creación de mapa
datos_mapa <- datos %>%
  st_as_sf(coords = c("longitud", "latitud"))

ggplot(datos_mapa, aes(geometry = geometry, fill = categoria)) +
  geom_sf() +
  labs(title = "Mapa de categorías")

# Creación de modelo de aprendizaje automático
modelo_ml <- rand_forest(categoria ~ ., data = datos_train)

# Evaluación del modelo de aprendizaje automático
predicciones_ml <- predict(modelo_ml, datos_test)
evaluacion_ml <- calculate_metrics(datos_test$categoria, predicciones_ml)

# Visualización de resultados del modelo de aprendizaje automático
ml_performance <- perf(datos_test$categoria, predicciones_ml, measure = "auc")
ml_curve <- create_perf_curve(ml_performance)
ggplot(ml_curve, aes(x = recall, y = specificity)) +
  geom_line() +
  labs(title = "Curva ROC del modelo de aprendizaje automático")

# Guardado del modelo de aprendizaje automático
saveRDS(modelo_ml, "modelo_ml.rds")
```

Explicación del código:

1. Importación de bibliotecas necesarias para el análisis de datos, limpieza de datos, creación de características, división de datos, creación de modelos estadísticos, evaluación de modelos y visualización de resultados.

2. Carga de datos desde un archivo CSV y limpieza de los datos, eliminando filas con valores faltantes y convirtiendo la variable "categoria" a un factor.

3. Creación de características adicionales, como la longitud del texto, el número de palabras y el número de signos de puntuación en cada observación.

4. División de los datos en un conjunto de entrenamiento y un conjunto de prueba, utilizando una proporción de 75% para entrenamiento y 25% para prueba.

5. Creación de un clasificador bayesiano utilizando la función `bayesrules()` del paquete `bayesrules`, utilizando las características creadas anteriormente como predictores de la variable "categoria".

6. Evaluación del clasificador bayesiano utilizando el conjunto de datos de prueba y cálculo de las métricas de evaluación, como la precisión, el recall y el F1-score.

7. Visualización de los resultados de la evaluación del clasificador bayesiano mediante una curva ROC.

8. Guardado del modelo bayesiano en un archivo RDS para poder utilizarlo posteriormente.

9. Creación de un mapa utilizando la biblioteca `sf` para visualizar los datos en un mapa geográfico.

10. Creación de un modelo de aprendizaje automático utilizando la función `rand_forest()` del paquete `tidymodels`, utilizando las mismas características que se utilizaron para crear el clasificador bayesiano.

11. Evaluación del modelo de aprendizaje automático utilizando el conjunto de datos de prueba y cálculo de las métricas de evaluación.

12. Visualización de los resultados de la evaluación del modelo de aprendizaje automático mediante una curva ROC.

13. Guardado del modelo de aprendizaje automático en un archivo RDS para poder utilizarlo posteriormente.