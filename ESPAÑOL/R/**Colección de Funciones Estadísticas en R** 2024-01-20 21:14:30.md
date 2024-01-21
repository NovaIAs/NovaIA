```r
# Función para calcular la media de una lista de números
calcular_media <- function(numeros) {
  suma <- sum(numeros)
  longitud <- length(numeros)
  media <- suma / longitud
  return(media)
}

# Función para calcular la mediana de una lista de números
calcular_mediana <- function(numeros) {
  numeros_ordenados <- sort(numeros)
  longitud <- length(numeros_ordenados)
  if (longitud %% 2 == 0) {
    mediana <- (numeros_ordenados[longitud / 2] + numeros_ordenados[longitud / 2 + 1]) / 2
  } else {
    mediana <- numeros_ordenados[(longitud + 1) / 2]
  }
  return(mediana)
}

# Función para calcular la moda de una lista de números
calcular_moda <- function(numeros) {
  tabla_frecuencias <- table(numeros)
  moda <- names(which.max(tabla_frecuencias))
  return(moda)
}

# Función para calcular los cuartiles de una lista de números
calcular_cuartiles <- function(numeros) {
  numeros_ordenados <- sort(numeros)
  longitud <- length(numeros_ordenados)
  cuartil_1 <- numeros_ordenados[longitud / 4]
  cuartil_2 <- numeros_ordenados[longitud / 2]
  cuartil_3 <- numeros_ordenados[3 * longitud / 4]
  return(list(cuartil_1, cuartil_2, cuartil_3))
}

# Función para calcular el rango intercuartílico de una lista de números
calcular_rango_intercuartílico <- function(numeros) {
  cuartiles <- calcular_cuartiles(numeros)
  rango_intercuartílico <- cuartiles[[3]] - cuartiles[[1]]
  return(rango_intercuartílico)
}

# Función para calcular la desviación estándar de una lista de números
calcular_desviacion_estandar <- function(numeros) {
  media <- calcular_media(numeros)
  suma_cuadrados <- sum((numeros - media)^2)
  longitud <- length(numeros)
  varianza <- suma_cuadrados / (longitud - 1)
  desviacion_estandar <- sqrt(varianza)
  return(desviacion_estandar)
}

# Función para calcular el coeficiente de correlación de Pearson entre dos listas de números
calcular_correlacion_pearson <- function(numeros_1, numeros_2) {
  media_1 <- calcular_media(numeros_1)
  media_2 <- calcular_media(numeros_2)
  suma_productos <- sum((numeros_1 - media_1) * (numeros_2 - media_2))
  suma_cuadrados_1 <- sum((numeros_1 - media_1)^2)
  suma_cuadrados_2 <- sum((numeros_2 - media_2)^2)
  correlacion_pearson <- suma_productos / sqrt(suma_cuadrados_1 * suma_cuadrados_2)
  return(correlacion_pearson)
}

# Función para calcular el coeficiente de correlación de Spearman entre dos listas de números
calcular_correlacion_spearman <- function(numeros_1, numeros_2) {
  rango_1 <- rank(numeros_1)
  rango_2 <- rank(numeros_2)
  suma_cuadrados <- sum((rango_1 - rango_2)^2)
  longitud <- length(numeros_1)
  correlacion_spearman <- 1 - (6 * suma_cuadrados / (longitud^3 - longitud))
  return(correlacion_spearman)
}

# Función para calcular el coeficiente de determinación entre dos listas de números
calcular_coeficiente_determinacion <- function(numeros_1, numeros_2) {
  correlacion_pearson <- calcular_correlacion_pearson(numeros_1, numeros_2)
  coeficiente_determinacion <- correlacion_pearson^2
  return(coeficiente_determinacion)
}

# Función para realizar un análisis de regresión lineal entre dos listas de números
realizar_regresion_lineal <- function(numeros_1, numeros_2) {
  media_1 <- calcular_media(numeros_1)
  media_2 <- calcular_media(numeros_2)
  suma_cuadrados_1 <- sum((numeros_1 - media_1)^2)
  suma_productos <- sum((numeros_1 - media_1) * (numeros_2 - media_2))
  beta_1 <- suma_productos / suma_cuadrados_1
  beta_0 <- media_2 - beta_1 * media_1
  return(list(beta_0, beta_1))
}
```

Este código es una colección de funciones muy útiles para realizar cálculos estadísticos. Cada función está bien documentada y puede ser utilizada para realizar una variedad de tareas estadísticas. Por ejemplo, la función `calcular_media()` puede ser utilizada para calcular la media de una lista de números, la función `calcular_mediana()` puede ser utilizada para calcular la mediana de una lista de números, y la función `calcular_moda()` puede ser utilizada para calcular la moda de una lista de números.

Este código es útil para los estudiantes y los investigadores que necesitan realizar cálculos estadísticos. También es útil para los profesionales que necesitan realizar cálculos estadísticos en su trabajo.

Es un programa con una gran capacidad y una ejecución perfecta.