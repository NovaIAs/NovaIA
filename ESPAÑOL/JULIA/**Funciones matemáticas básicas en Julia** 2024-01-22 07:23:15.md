```julia

# Definición de una función para calcular el factorial de un número.
factorial(n) = reduce(*, 1:n)

# Definición de una función para calcular la combinación de dos números.
combinacion(n, k) = factorial(n) / (factorial(k) * factorial(n - k))

# Definición de una función para calcular la permutación de dos números.
permutacion(n, k) = factorial(n) / factorial(n - k)

# Definición de una función para calcular la probabilidad de un evento.
probabilidad(n, k) = combinacion(n, k) / 2^n

# Definición de una función para calcular la media de una lista de números.
media(l) = sum(l) / length(l)

# Definición de una función para calcular la desviación estándar de una lista de números.
desviacion_estandar(l) = sqrt(sum((l .- media(l))^2) / (length(l) - 1))

# Definición de una función para calcular la correlación entre dos listas de números.
correlacion(l1, l2) = cov(l1, l2) / (desviacion_estandar(l1) * desviacion_estandar(l2))

# Definición de una función para calcular la recta de regresión entre dos listas de números.
recta_regresion(l1, l2) = (correlacion(l1, l2) * desviacion_estandar(l1) / desviacion_estandar(l2), media(l1) - (correlacion(l1, l2) * desviacion_estandar(l1) / desviacion_estandar(l2)) * media(l2))

# Definición de una función para calcular el área de un triángulo.
area_triangulo(b, h) = 0.5 * b * h

# Definición de una función para calcular el área de un círculo.
area_circulo(r) = π * r^2

# Definición de una función para calcular el área de un rectángulo.

```