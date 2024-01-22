```r
# Cargamos las bibliotecas necesarias.
library(tidyverse)
library(tidymodels)
library(caret)
library(rsample)

# Creamos el marco de datos con los datos de ejemplo.
datos_ejemplo <- data.frame(
  variable_1 = c(1, 2, 3, 4, 5),
  variable_2 = c("A", "B", "C", "D", "E"),
  variable_3 = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)

# Exploramos los datos.
glimpse(datos_ejemplo)

# Creamos el modelo de regresión lineal.
modelo <- lm(variable_1 ~ variable_2 + variable_3, data = datos_ejemplo)

# Evaluamos el modelo.
summary(modelo)

# Creamos un objeto de partición de datos para dividir los datos en conjuntos de entrenamiento y prueba.
particion <- initial_split(datos_ejemplo, prop = 0.75)

# Dividimos los datos en conjuntos de entrenamiento y prueba.
datos_entrenamiento <- training(particion)
datos_prueba <- testing(particion)

# Entrenamos el modelo con los datos de entrenamiento.
modelo_entrenado <- train(modelo, datos_entrenamiento)

# Evaluamos el modelo entrenado con los datos de prueba.
predicciones <- predict(modelo_entrenado, datos_prueba)
evaluacion <- eval(datos_prueba, predicciones)

# Imprimimos la evaluación del modelo.
print(evaluacion)

# Creamos un gráfico con la curva ROC del modelo.
auc <- roc_auc(datos_prueba$variable_1, predicciones)
plot_roc(datos_prueba$variable_1, predicciones, print.auc = TRUE)

# Creamos un gráfico con la curva de calibración del modelo.
plot_calibration(datos_prueba$variable_1, predicciones)

# Ajustamos un árbol de decisión.
modelo_arbol <- train(
  rpart,
  datos_entrenamiento,
  metric = "mse",
  trControl = trainControl(method = "cv", number = 10)
)

# Evaluamos el modelo de árbol de decisión.
predicciones_arbol <- predict(modelo_arbol, datos_prueba)
evaluacion_arbol <- eval(datos_prueba, predicciones_arbol)

# Imprimimos la evaluación del modelo de árbol de decisión.
print(evaluacion_arbol)

# Creamos un gráfico con el árbol de decisión.
plot(modelo_arbol)

# Creamos un conjunto de datos nuevos para hacer predicciones.
datos_nuevos <- data.frame(
  variable_2 = c("A", "B"),
  variable_3 = c(TRUE, FALSE)
)

# Hacemos predicciones con los datos nuevos.
predicciones_nuevos <- predict(modelo_entrenado, datos_nuevos)

# Imprimimos las predicciones.
print(predicciones_nuevos)
```

Este código realiza un análisis completo de datos, incluyendo la exploración de datos, la creación de un modelo de regresión lineal, la evaluación del modelo, la creación de un gráfico con la curva ROC, la creación de un gráfico con la curva de calibración, el ajuste de un árbol de decisión, la evaluación del modelo de árbol de decisión, la creación de un gráfico con el árbol de decisión y la realización de predicciones con datos nuevos.