```R
# Definimos una función recursiva para calcular el factorial de un número.
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

# Creamos un vector con los números del 1 al 10.
numeros <- 1:10

# Calculamos el factorial de cada número en el vector usando la función recursiva.
factoriales <- sapply(numeros, factorial)

# Imprimimos los resultados en la consola.
print(factoriales)

# Creamos una matriz de datos con los números y sus factoriales.
datos <- data.frame(numeros, factoriales)

# Graficamos los datos usando un diagrama de dispersión.
plot(datos, x = "numeros", y = "factoriales", type = "p", pch = 19, col = "blue")
title("Diagrama de dispersión de números y sus factoriales")
xlabel("Números")
ylabel("Factoriales")
grid()

# Añadimos una línea de tendencia polinomial de grado 2 al gráfico.
lines(loess(factoriales ~ numeros, data = datos), col = "red")

# Imprimimos el gráfico en la consola.
print(plot)
```

**Explicación del código:**

* La función `factorial` calcula el factorial de un número usando recursión. El factorial de un número es el producto de todos los números positivos enteros menores o iguales que ese número. Por ejemplo, el factorial de 5 es 5 * 4 * 3 * 2 * 1 = 120.
* El vector `numeros` contiene los números del 1 al 10.
* La función `sapply` se usa para aplicar la función `factorial` a cada número en el vector `numeros`. El resultado es un vector de factoriales.
* La función `print` se usa para imprimir los resultados en la consola.
* La función `data.frame` se usa para crear una matriz de datos a partir de los vectores `numeros` y `factoriales`.
* La función `plot` se usa para crear un diagrama de dispersión de los datos en la matriz de datos `datos`.
* La función `title` se usa para añadir un título al gráfico.
* La función `xlabel` se usa para añadir una etiqueta al eje x del gráfico.
* La función `ylabel` se usa para añadir una etiqueta al eje y del gráfico.
* La función `grid` se usa para añadir una cuadrícula al gráfico.
* La función `lines` se usa para añadir una línea de tendencia polinomial de grado 2 al gráfico.
* La función `print` se usa para imprimir el gráfico en la consola.