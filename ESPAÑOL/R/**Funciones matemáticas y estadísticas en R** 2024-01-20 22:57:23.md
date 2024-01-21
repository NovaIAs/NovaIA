```r
# Función para encontrar el máximo común divisor (MCD) de dos números
mcd <- function(a, b) {
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}

# Función para encontrar el mínimo común múltiplo (MCM) de dos números
mcm <- function(a, b) {
  return(a * b / mcd(a, b))
}

# Función para encontrar el factorial de un número
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

# Función para encontrar el coeficiente binomial de dos números
coeficiente_binomial <- function(n, k) {
  return(factorial(n) / (factorial(k) * factorial(n - k)))
}

# Función para encontrar la media de una lista de números
media <- function(x) {
  return(sum(x) / length(x))
}

# Función para encontrar la mediana de una lista de números
mediana <- function(x) {
  x <- sort(x)
  n <- length(x)
  if (n %% 2 == 0) {
    return((x[n / 2] + x[n / 2 + 1]) / 2)
  } else {
    return(x[(n + 1) / 2])
  }
}

# Función para encontrar la moda de una lista de números
moda <- function(x) {
  moda_temp <- table(x)
  moda_temp <- sort(moda_temp, decreasing = TRUE)
  return(names(moda_temp)[1])
}

# Función para encontrar la desviación estándar de una lista de números
desviacion_estandar <- function(x) {
  media_x <- media(x)
  varianza_x <- mean((x - media_x)^2)
  return(sqrt(varianza_x))
}

# Función para encontrar la correlación entre dos listas de números
correlacion <- function(x, y) {
  media_x <- media(x)
  media_y <- media(y)
  covarianza_xy <- mean((x - media_x) * (y - media_y))
  desviacion_estandar_x <- desviacion_estandar(x)
  desviacion_estandar_y <- desviacion_estandar(y)
  return(covarianza_xy / (desviacion_estandar_x * desviacion_estandar_y))
}

# Función para encontrar la regresión lineal entre dos listas de números
regresion_lineal <- function(x, y) {
  media_x <- media(x)
  media_y <- media(y)
  covarianza_xy <- mean((x - media_x) * (y - media_y))
  varianza_x <- mean((x - media_x)^2)
  b1 <- covarianza_xy / varianza_x
  b0 <- media_y - b1 * media_x
  return(list(b0 = b0, b1 = b1))
}

# Función para encontrar el área de un triángulo
area_triangulo <- function(base, altura) {
  return(0.5 * base * altura)
}

# Función para encontrar el área de un círculo
area_circulo <- function(radio) {
  return(pi * radio^2)
}

# Función para encontrar el volumen de una esfera
volumen_esfera <- function(radio) {
  return((4 / 3) * pi * radio^3)
}

# Función para encontrar el volumen de un cilindro
volumen_cilindro <- function(radio, altura) {
  return(pi * radio^2 * altura)
}
```

Este código en R proporciona una colección diversa de funciones para realizar cálculos matemáticos y estadísticos. Estas funciones van desde operaciones básicas como encontrar el máximo común divisor (MCD) y el mínimo común múltiplo (MCM) hasta cálculos más avanzados como encontrar el factorial de un número, el coeficiente binomial, la media, la mediana, la moda, la desviación estándar, la correlación y la regresión lineal. También se incluyen funciones para calcular el área de un triángulo, el área de un círculo, el volumen de una esfera y el volumen de un cilindro. Este código es una herramienta valiosa para cualquier persona que necesite realizar cálculos matemáticos o estadísticos en R.