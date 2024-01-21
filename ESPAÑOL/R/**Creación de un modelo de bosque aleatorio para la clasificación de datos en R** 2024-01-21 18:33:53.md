```
# Carga las bibliotecas necesarias
library(tidyverse)
library(tidymodels)
library(caret)
library(rsample)
library(ggpubr)

# Carga el conjunto de datos
data <- read.csv("datos.csv")

# Separa los datos en conjuntos de entrenamiento y prueba
set.seed(123)
data_split <- initial_split(data, prop = 0.75)
data_train <- training(data_split)
data_test <- testing(data_split)

# Preprocesa los datos
data_train <- data_train %>%
  mutate(sexo = as.factor(sexo)) %>%
  mutate(edad = as.numeric(edad)) %>%
  mutate(altura = as.numeric(altura)) %>%
  mutate(peso = as.numeric(peso))

# Crea un modelo de bosque aleatorio
modelo <- rand_forest(formula = y ~ ., data = data_train)

# Ajusta el modelo
modelo_ajustado <- modelo %>%
  fit()

# Predice las etiquetas de los datos de prueba
predicciones <- predict(modelo_ajustado, new_data = data_test)

# Evalúa el desempeño del modelo
resultados <- eval_model(modelo_ajustado, data = data_test)
print(resultados)

# Crea una gráfica de confusión
ggplot(data = data_test, aes(x = y, y = predicciones)) +
  geom_tile() +
  labs(title = "Gráfica de confusión", x = "Etiquetas reales", y = "Etiquetas predichas")

# Crea una gráfica ROC
ggplot(data = resultados, aes(x = specificity, y = sensitivity)) +
  geom_line() +
  labs(title = "Curva ROC", x = "Especificidad", y = "Sensibilidad")

# Crea una tabla de clasificación
tabla_clasificacion <- confusion_mat(data = data_test, reference = y, estimate = predicciones)
print(tabla_clasificacion)

# Usa el modelo para hacer predicciones en nuevos datos
nuevos_datos <- data.frame(
  sexo = c("masculino", "femenino"),
  edad = c(25, 35),
  altura = c(1.80, 1.65),
  peso = c(80, 60)
)

predicciones_nuevas <- predict(modelo_ajustado, new_data = nuevos_datos)
print(predicciones_nuevas)
```

Este código realiza las siguientes tareas:

1. Carga las bibliotecas necesarias.
2. Carga el conjunto de datos.
3. Separa los datos en conjuntos de entrenamiento y prueba.
4. Preprocesa los datos.
5. Crea un modelo de bosque aleatorio.
6. Ajusta el modelo.
7. Predice las etiquetas de los datos de prueba.
8. Evalúa el desempeño del modelo.
9. Crea una gráfica de confusión.
10. Crea una gráfica ROC.
11. Crea una tabla de clasificación.
12. Usa el modelo para hacer predicciones en nuevos datos.

Este código es complejo porque realiza una variedad de tareas, incluyendo preprocesamiento de datos, creación de modelos, evaluación de modelos y visualización de resultados. También usa una variedad de bibliotecas de R, incluyendo tidyverse, tidymodels, caret y rsample.

Este código es útil para realizar tareas de aprendizaje automático en R. Se puede usar para crear modelos predictivos, evaluar el desempeño de los modelos y visualizar los resultados.