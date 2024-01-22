```r
# Definición de la función `suma_vectorial` que suma los elementos de un vector.
suma_vectorial <- function(vector) {
  suma <- 0
  for (i in 1:length(vector)) {
    suma <- suma + vector[i]
  }
  return(suma)
}

# Definición de la función `promedio_vectorial` que calcula el promedio de los elementos de un vector.
promedio_vectorial <- function(vector) {
  suma <- suma_vectorial(vector)
  promedio <- suma / length(vector)
  return(promedio)
}

# Generación de un vector de 100 números aleatorios entre 1 y 100.
vector_aleatorio <- sample(1:100, 100, replace = TRUE)

# Cálculo de la suma del vector aleatorio.
suma_vectorial_aleatorio <- suma_vectorial(vector_aleatorio)

# Cálculo del promedio del vector aleatorio.
promedio_vectorial_aleatorio <- promedio_vectorial(vector_aleatorio)

# Impresión de los resultados.
print(paste("Suma del vector:", suma_vectorial_aleatorio))
print(paste("Promedio del vector:", promedio_vectorial_aleatorio))

# Definición de la función `regresion_lineal` que realiza una regresión lineal entre dos vectores.
regresion_lineal <- function(vector_x, vector_y) {
  # Cálculo de la pendiente y la intersección de la recta de regresión lineal.
  pendiente <- sum((vector_x - mean(vector_x)) * (vector_y - mean(vector_y))) / sum((vector_x - mean(vector_x))^2)
  interseccion <- mean(vector_y) - pendiente * mean(vector_x)

  # Creación de un vector con los valores ajustados de la recta de regresión lineal.
  vector_y_ajustado <- pendiente * vector_x + interseccion

  # Cálculo del coeficiente de determinación R^2.
  r_cuadrado <- 1 - sum((vector_y - vector_y_ajustado)^2) / sum((vector_y - mean(vector_y))^2)

  # Devolución de la pendiente, la intersección y el coeficiente de determinación R^2.
  return(list(pendiente = pendiente, interseccion = interseccion, r_cuadrado = r_cuadrado))
}

# Generación de dos vectores de 100 números aleatorios entre 1 y 100.
vector_x <- sample(1:100, 100, replace = TRUE)
vector_y <- sample(1:100, 100, replace = TRUE)

# Ajuste de una recta de regresión lineal a los dos vectores.
regresion <- regresion_lineal(vector_x, vector_y)

# Impresión de los resultados.
print(paste("Pendiente:", regresion$pendiente))
print(paste("Intersección:", regresion$interseccion))
print(paste("Coeficiente de determinación R^2:", regresion$r_cuadrado))

# Creación de un gráfico de dispersión con la recta de regresión lineal superpuesta.
plot(vector_x, vector_y, main = "Diagrama de dispersión con recta de regresión lineal ajustada")
lines(vector_x, regresion$pendiente * vector_x + regresion$interseccion, col = "red")
```

Explicación del código:

- La función `suma_vectorial` suma los elementos de un vector.
- La función `promedio_vectorial` calcula el promedio de los elementos de un vector.
- Se genera un vector de 100 números aleatorios entre 1 y 100.
- Se calcula la suma y el promedio del vector aleatorio y se imprimen los resultados.
- La función `regresion_lineal` realiza una regresión lineal entre dos vectores.
- Se generan dos vectores de 100 números aleatorios entre 1 y 100.
- Se ajusta una recta de regresión lineal a los dos vectores.
- Se imprimen los resultados de la regresión lineal.
- Se crea un gráfico de dispersión con la recta de regresión lineal superpuesta.