```
# Librerías necesarias.
library(tidyverse)
library(tidymodels)
library(caret)
library(ggplot2)

# Cargamos el dataset.
datos <- read.csv("path/to/dataset.csv")

# Preprocesamiento de los datos.
datos <- datos %>%
  na.omit() %>%
  mutate(variable_categorica = as.factor(variable_categorica))

# Creamos el modelo.
modelo <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression')

# Entrenamos el modelo.
modelo_entrenado <- modelo %>%
  fit(formula = variable_de_respuesta ~ ., data = datos)

# Evaluamos el modelo.
evaluacion <- modelo_entrenado %>%
  rsq()
  rmse()

# Hacemos predicciones.
predicciones <- modelo_entrenado %>%
  predict(new_data)

# Visualizamos los resultados.
ggplot(datos, aes(x = variable_independiente, y = variable_de_respuesta)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

```

Explicaciones del código:

* Cargamos las librerías necesarias.
* Cargamos el dataset.
* Hacemos un preprocesamiento de los datos, eliminando los valores faltantes y convirtiendo las variables categóricas a factores.
* Creamos un modelo lineal generalizado utilizando la función `linear_reg()`.
* Entrenamos el modelo utilizando la función `fit()`.
* Evaluamos el modelo utilizando las funciones `rsq()` y `rmse()`.
* Hacemos predicciones utilizando la función `predict()`.
* Visualizamos los resultados utilizando la librería `ggplot2`.