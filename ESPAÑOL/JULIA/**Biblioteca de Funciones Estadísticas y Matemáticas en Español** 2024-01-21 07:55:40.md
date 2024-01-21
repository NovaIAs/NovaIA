```julia
# Definición de la función para calcular el factorial de un número
factorial(n) =
    if n == 0
        1
    else
        n * factorial(n-1)
    end

# Definición de la función para calcular el número de combinaciones de un conjunto de n elementos tomados de k en k
combinaciones(n, k) =
    factorial(n) / (factorial(k) * factorial(n-k))

# Definición de la función para calcular la probabilidad de ocurrencia de un evento
probabilidad(evento, espacio_muestral) =
    cardinalidad(evento) / cardinalidad(espacio_muestral)

# Definición de la función para calcular el valor esperado de una variable aleatoria discreta
valor_esperado(x, p) =
    sum(x * p)

# Definición de la función para calcular la varianza de una variable aleatoria discreta
varianza(x, p) =
    sum((x - valor_esperado(x, p))^2 * p)

# Definición de la función para calcular la desviación estándar de una variable aleatoria discreta
desviacion_estandar(x, p) =
    sqrt(varianza(x, p))

# Definición de la función para calcular la mediana de una lista de números
mediana(x) =
    sort(x)[floor(length(x)/2) + 1]

# Definición de la función para calcular la moda de una lista de números
moda(x) =
    maximum(count_occurrences(x))

# Definición de la función para calcular la correlación entre dos listas de números
correlacion(x, y) =
    cov(x, y) / (desviacion_estandar(x) * desviacion_estandar(y))

# Definición de la función para calcular la regresión lineal entre dos listas de números
regresion_lineal(x, y) =
    lm(x, y)

# Definición de la función para calcular el área de un triángulo
area_triangulo(base, altura) =
    0.5 * base * altura

# Definición de la función para calcular el área de un círculo
area_circulo(radio) =
    pi * radio^2

# Definición de la función para calcular el volumen de una esfera
volumen_esfera(radio) =
    4/3 * pi * radio^3

# Definición de la función para calcular la distancia entre dos puntos en un espacio bidimensional
distancia(x1, y1, x2, y2) =
    sqrt((x2 - x1)^2 + (y2 - y1)^2)

# Definición de la función para generar una lista de números aleatorios uniformes entre 0 y 1
numeros_aleatorios(n) =
    rand(Uniform(0, 1), n)

# Definición de la función para generar una lista de números aleatorios normales con media 0 y desviación estándar 1
numeros_aleatorios_normales(n) =
    rand(Normal(0, 1), n)

# Definición de la función para generar una lista de números aleatorios exponenciales con parámetro λ
numeros_aleatorios_exponenciales(n, lambda) =
    rand(Exponential(lambda), n)

# Definición de la función para generar una lista de números aleatorios de Poisson con parámetro λ
numeros_aleatorios_poisson(n, lambda) =
    rand(Poisson(lambda), n)

# Definición de la función para generar una lista de números aleatorios binomiales con parámetros n y p
numeros_aleatorios_binomiales(n, p) =
    rand(Binomial(n, p), n)
```

Este código es un conjunto de funciones que implementan diversos cálculos estadísticos y matemáticos. Las funciones están escritas en español y utilizan la sintaxis de Julia.

El código incluye funciones para calcular el factorial de un número, el número de combinaciones de un conjunto de elementos, la probabilidad de ocurrencia de un evento, el valor esperado de una variable aleatoria discreta, la varianza de una variable aleatoria discreta, la desviación estándar de una variable aleatoria discreta, la mediana de una lista de números, la moda de una lista de números, la correlación entre dos listas de números, la regresión lineal entre dos listas de números, el área de un triángulo, el área de un círculo, el volumen de una esfera, la distancia entre dos puntos en un espacio bidimensional, la generación de una lista de números aleatorios uniformes entre 0 y 1, la generación de una lista de números aleatorios normales con media 0 y desviación estándar 1, la generación de una lista de números aleatorios exponenciales con parámetro λ, la generación de una lista de números aleatorios de Poisson con parámetro λ y la generación de una lista de números aleatorios binomiales con parámetros n y p.