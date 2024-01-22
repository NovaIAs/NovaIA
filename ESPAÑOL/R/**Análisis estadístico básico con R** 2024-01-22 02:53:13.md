```
# Definimos una función para calcular el factorial de un número
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

# Creamos un vector con los primeros 10 números naturales
numeros <- 1:10

# Aplicamos la función factorial a cada elemento del vector
factoriales <- factorial(numeros)

# Mostramos los resultados en una tabla
data.frame(Número = numeros, Factorial = factoriales)

# Definimos una función para calcular la media de un vector de números
media <- function(x) {
  suma <- sum(x)
  n <- length(x)
  return(suma / n)
}

# Calculamos la media de los primeros 10 números naturales
media_numeros <- media(numeros)

# Definimos una función para calcular la varianza de un vector de números
varianza <- function(x) {
  media_x <- media(x)
  sum_cuadrados <- sum((x - media_x)^2)
  n <- length(x)
  return(sum_cuadrados / (n - 1))
}

# Calculamos la varianza de los primeros 10 números naturales
varianza_numeros <- varianza(numeros)

# Mostramos los resultados en una tabla
data.frame(Estadística = c("Media", "Varianza"), Valor = c(media_numeros, varianza_numeros))

# Definimos una función para ajustar un modelo lineal a un conjunto de datos
lm_fit <- function(x, y) {
  modelo <- lm(y ~ x)
  return(modelo)
}

# Creamos un conjunto de datos con 100 observaciones
datos <- data.frame(x = rnorm(100), y = rnorm(100))

# Ajustamos un modelo lineal a los datos
modelo_ajustado <- lm_fit(datos$x, datos$y)

# Mostramos los resultados del modelo lineal
summary(modelo_ajustado)
```

Este código es bastante complejo y realiza una serie de tareas diferentes. En primer lugar, define una función para calcular el factorial de un número. A continuación, crea un vector con los primeros 10 números naturales y aplica la función factorial a cada elemento del vector. Los resultados se muestran en una tabla.

En segundo lugar, define una función para calcular la media de un vector de números. A continuación, calcula la media de los primeros 10 números naturales y la muestra en una tabla.

En tercer lugar, define una función para calcular la varianza de un vector de números. A continuación, calcula la varianza de los primeros 10 números naturales y la muestra en una tabla.

En cuarto lugar, define una función para ajustar un modelo lineal a un conjunto de datos. A continuación, crea un conjunto de datos con 100 observaciones y ajusta un modelo lineal a los datos. Los resultados del modelo lineal se muestran en una tabla.

Este código es bastante complejo y realiza una serie de tareas diferentes. Es un buen ejemplo de cómo R puede utilizarse para realizar análisis estadísticos complejos.