```r
# Leer los datos de un archivo CSV
datos <- read.csv("datos.csv", sep = ",", header = TRUE)

# Imprimir los primeros 5 registros de los datos
print(head(datos, 5))

# Crear un modelo de regresión lineal simple
modelo <- lm(y ~ x, data = datos)

# Imprimir el resumen del modelo
summary(modelo)

# Crear un gráfico de dispersión con la línea de regresión
plot(x, y, data = datos)
abline(modelo)

# Calcular el valor pronosticado para un valor dado de x
x_nuevo <- 10
y_pronosticado <- predict(modelo, newdata = data.frame(x = x_nuevo))

# Imprimir el valor pronosticado
print(y_pronosticado)

# Crear un modelo de regresión logística
modelo_logistico <- glm(y ~ x, data = datos, family = "binomial")

# Imprimir el resumen del modelo
summary(modelo_logistico)

# Crear una matriz de confusión
matriz_confusion <- table(datos$y, predict(modelo_logistico, type = "response"))

# Imprimir la matriz de confusión
print(matriz_confusion)

# Crear un árbol de decisión
modelo_arbol <- tree(y ~ ., data = datos)

# Imprimir el árbol de decisión
plot(modelo_arbol)
text(modelo_arbol)

# Crear un modelo de bosque aleatorio
modelo_bosque_aleatorio <- randomForest(y ~ ., data = datos)

# Imprimir la importancia de las variables
print(importance(modelo_bosque_aleatorio))

# Crear un modelo de máquina de vectores de soporte
modelo_svm <- svm(y ~ ., data = datos)

# Imprimir el resumen del modelo
summary(modelo_svm)

# Crear un modelo de redes neuronales
modelo_redes_neuronales <- nnet(y ~ ., data = datos, size = c(10, 10), maxit = 1000)

# Imprimir el resumen del modelo
summary(modelo_redes_neuronales)
```

**Explicación del código:**

* La primera línea lee los datos de un archivo CSV llamado "datos.csv".
* La segunda línea imprime los primeros 5 registros de los datos.
* La tercera línea crea un modelo de regresión lineal simple.
* La cuarta línea imprime el resumen del modelo.
* La quinta línea crea un gráfico de dispersión con la línea de regresión.
* La sexta línea calcula el valor pronosticado para un valor dado de x.
* La séptima línea imprime el valor pronosticado.
* La octava línea crea un modelo de regresión logística.
* La novena línea imprime el resumen del modelo.
* La décima línea crea una matriz de confusión.
* La undécima línea imprime la matriz de confusión.
* La duodécima línea crea un árbol de decisión.
* La decimotercera línea imprime el árbol de decisión.
* La decimocuarta línea crea un modelo de bosque aleatorio.
* La decimoquinta línea imprime la importancia de las variables.
* La decimosexta línea crea un modelo de máquina de vectores de soporte.
* La decimoséptima línea imprime el resumen del modelo.
* La decimoctava línea crea un modelo de redes neuronales.
* La decimonovena línea imprime el resumen del modelo.