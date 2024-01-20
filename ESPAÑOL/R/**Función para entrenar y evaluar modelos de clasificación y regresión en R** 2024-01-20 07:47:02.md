```r
# Definición de la función principal

mi_funcion <- function(datos, variable_objetivo, variables_predictoras, metodo = "regresion_lineal") {
  # Comprobación de los argumentos de entrada
  if (!is.data.frame(datos)) {
    stop("El argumento 'datos' debe ser un data frame.")
  }
  if (!is.character(variable_objetivo)) {
    stop("El argumento 'variable_objetivo' debe ser una cadena de caracteres.")
  }
  if (!is.character(variables_predictoras)) {
    stop("El argumento 'variables_predictoras' debe ser una cadena de caracteres o un vector de cadenas de caracteres.")
  }
  if (!is.character(metodo)) {
    stop("El argumento 'metodo' debe ser una cadena de caracteres.")
  }

  # División de los datos en conjuntos de entrenamiento y prueba
  set.seed(123)
  train_index <- sample(1:nrow(datos), 0.75 * nrow(datos))
  datos_train <- datos[train_index, ]
  datos_test <- datos[-train_index, ]

  # Entrenamiento del modelo
  if (metodo == "regresion_lineal") {
    modelo <- lm(variable_objetivo ~ variables_predictoras, data = datos_train)
  } else if (metodo == "regresion_logistica") {
    modelo <- glm(variable_objetivo ~ variables_predictoras, data = datos_train, family = "binomial")
  } else if (metodo == "arbol_de_decision") {
    modelo <- rpart(variable_objetivo ~ variables_predictoras, data = datos_train)
  } else {
    stop("El método especificado no es válido.")
  }

  # Evaluación del modelo
  predicciones <- predict(modelo, datos_test)
  if (is.factor(datos_test[, variable_objetivo])) {
    accuracy <- mean(predicciones == datos_test[, variable_objetivo])
  } else {
    rmse <- sqrt(mean((predicciones - datos_test[, variable_objetivo])^2))
  }

  # Impresión de los resultados
  print("Accuracy:")
  print(accuracy)
  print("RMSE:")
  print(rmse)
}

# Ejemplo de uso de la función

datos <- data.frame(
  variable_objetivo = c(1, 0, 1, 0, 1),
  variable_predictora1 = c(10, 20, 30, 40, 50),
  variable_predictora2 = c("A", "B", "C", "D", "E")
)

mi_funcion(datos, "variable_objetivo", c("variable_predictora1", "variable_predictora2"), "regresion_logistica")
```

Explicación del código:

1. Definición de la función `mi_funcion`: Esta función toma como argumentos los datos, la variable objetivo, las variables predictoras y el método que se quiere utilizar para entrenar el modelo.

2. Comprobación de los argumentos de entrada: La función comprueba que los argumentos de entrada sean válidos, es decir, que los datos sean un data frame, la variable objetivo y las variables predictoras sean cadenas de caracteres y el método sea una cadena de caracteres válida.

3. División de los datos en conjuntos de entrenamiento y prueba: La función divide los datos en dos conjuntos, el conjunto de entrenamiento y el conjunto de prueba. El conjunto de entrenamiento se utiliza para entrenar el modelo y el conjunto de prueba se utiliza para evaluar el modelo.

4. Entrenamiento del modelo: La función entrena el modelo utilizando el método especificado. En este ejemplo, se utilizan tres métodos diferentes: regresión lineal, regresión logística y árbol de decisión.

5. Evaluación del modelo: La función evalúa el modelo utilizando el conjunto de prueba. Si la variable objetivo es un factor, la función calcula la precisión del modelo. Si la variable objetivo es numérica, la función calcula el error cuadrático medio (RMSE) del modelo.

6. Impresión de los resultados: La función imprime los resultados de la evaluación del modelo.

7. Ejemplo de uso de la función: El código incluye un ejemplo de uso de la función `mi_funcion` para entrenar y evaluar un modelo de regresión logística utilizando los datos del data frame `datos`.