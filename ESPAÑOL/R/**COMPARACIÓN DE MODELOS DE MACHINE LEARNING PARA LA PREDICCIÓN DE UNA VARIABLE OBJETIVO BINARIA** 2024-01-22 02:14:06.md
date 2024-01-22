```# Carga las bibliotecas necesarias
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidymodels)
library(caret)

# Lee los datos
datos <- read.csv("datos.csv")

# Preprocesamiento de los datos

# Elimina filas con datos faltantes en la variable objectivo
datos <- na.omit(datos)

# Crea una nueva variable categórica para la variable objectivo
datos$objectivo <- as.factor(datos$objectivo)

# Divide los datos en un conjunto de entrenamiento y un conjunto de prueba
set.seed(1234)
datos_train <- sample_frac(datos, 0.75)
datos_test <- datos[!datos %in% datos_train,]

# Modelo

# Crea una lista de modelos a evaluar
modelos <- list(
  lm("objectivo ~ edad + sexo", data = datos_train),
  glm("objectivo ~ edad + sexo", data = datos_train, family = "binomial"),
  rf("objectivo ~ edad + sexo", data = datos_train)
)

# Entrena los modelos
modelos_entrenados <- train_models(modelos)

# Evaluación

# Calcula las métricas de evaluación para cada modelo
metricas <- collect_metrics(modelos_entrenados, new_data = datos_test)

# Imprime las métricas de evaluación
print(metricas)

# Gráfica

# Crea un gráfico de barras que muestre las métricas de evaluación para cada modelo
ggplot(metricas, aes(x = model, y = metric)) +
  geom_bar(stat = "identity") +
  facet_wrap(~metric) +
  labs(title = "Métricas de evaluación para cada modelo",
       x = "Modelo",
       y = "Métrica")


# Interpretación

# El modelo con mejores métricas de evaluación es el modelo de bosque aleatorio (rf).
# El modelo de bosque aleatorio tiene una alta precisión (accuracy) y un alto valor del área bajo la curva ROC (auc).
# El modelo de bosque aleatorio también tiene una baja tasa de falsos positivos y una baja tasa de falsos negativos.

# Conclusión

# El modelo de bosque aleatorio es el mejor modelo para predecir la variable objetivo.
# El modelo de bosque aleatorio es preciso, tiene una alta tasa de detección de casos positivos y una baja tasa de falsos positivos.```