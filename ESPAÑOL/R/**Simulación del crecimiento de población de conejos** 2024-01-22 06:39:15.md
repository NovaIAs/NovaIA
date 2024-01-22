```r
# Introducción
# Este código es una simulación del crecimiento de una población de conejos en un entorno cerrado. La población de conejos crece exponencialmente con una tasa de crecimiento r. Sin embargo, hay un límite en el tamaño de la población que el entorno puede soportar, conocido como capacidad de carga (K).

# Definir los parámetros
# Empezamos definiendo los parámetros del modelo. Estos parámetros incluyen:
  # r: tasa de crecimiento
  # K: capacidad de carga
  # n: población inicial

r <- 0.5  # tasa de crecimiento
K <- 100  # capacidad de carga
n <- 10   # población inicial

# Crear un vector de tiempo
# Creamos un vector de tiempo que abarca el periodo de simulación. En este caso, simularemos el crecimiento de la población durante 20 años.

t <- seq(0, 20, by = 1)

# Crear un vector de población
# Creamos un vector de población que almacenará el tamaño de la población en cada año.

n <- numeric(length(t))
n[1] <- n

# Simular el crecimiento de la población
# Utilizamos un bucle for para simular el crecimiento de la población a lo largo del tiempo. En cada iteración del bucle, calculamos el tamaño de la población en el siguiente año utilizando la siguiente ecuación:

n[i+1] <- n[i] + r * n[i] * (1 - n[i]/K)

# Visualizar los resultados
# Una vez que hemos simulado el crecimiento de la población, podemos visualizar los resultados utilizando un gráfico de líneas.

plot(t, n, type = "l", col = "blue", lwd = 2, main = "Crecimiento de la población de conejos", xlab = "Tiempo (años)", ylab = "Tamaño de la población")
abline(h = K, col = "red", lty = 2)
legend("topright", legend = c("Población", "Capacidad de carga"), col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 2))

# Explicación del código
# El código anterior es una simulación del crecimiento de una población de conejos en un entorno cerrado. La simulación utiliza la siguiente ecuación:

n[i+1] <- n[i] + r * n[i] * (1 - n[i]/K)

donde:

n[i] es el tamaño de la población en el año i
n[i+1] es el tamaño de la población en el año i+1
r es la tasa de crecimiento
K es la capacidad de carga

La simulación empieza con una población inicial de 10 conejos y una tasa de crecimiento de 0.5. La capacidad de carga del entorno es de 100 conejos. La simulación dura 20 años.

El gráfico de líneas muestra el crecimiento de la población de conejos a lo largo del tiempo. La población crece exponencialmente al principio, pero luego se ralentiza a medida que se acerca a la capacidad de carga. La línea roja horizontal representa la capacidad de carga.

El código también incluye una leyenda que explica los colores y los símbolos utilizados en el gráfico.
```