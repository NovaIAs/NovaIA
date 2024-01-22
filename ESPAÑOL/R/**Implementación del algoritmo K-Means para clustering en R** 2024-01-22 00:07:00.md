```r
# Este código es una implementación del método de K-Means para clustering en R.

# Cargamos las bibliotecas necesarias.
library(tidyverse)
library(ggplot2)

# Generamos un conjunto de datos aleatorio.
datos <- data.frame(
  x = rnorm(1000, mean = 0, sd = 1),
  y = rnorm(1000, mean = 0, sd = 1)
)

# Definimos el número de clusters que queremos crear.
k <- 3

# Inicializamos los centroides de los clusters de forma aleatoria.
centroides <- datos[sample(nrow(datos), k), ]

# Iteramos sobre los datos, asignando cada punto al cluster más cercano.
for (i in 1:100) {
  # Calculamos la distancia de cada punto a cada centroide.
  distancias <- dist(datos, centroids)

  # Asignamos cada punto al cluster más cercano.
  asignaciones <- which.min(distancias)

  # Recalculamos los centroides de los clusters como la media de los puntos asignados a cada cluster.
  centroides <- aggregate(datos, list(asignaciones), FUN = mean)

  # Visualizamos los datos y los clusters.
  ggplot(datos, aes(x = x, y = y, color = asignaciones)) +
    geom_point() +
    geom_text(aes(label = asignaciones), vjust = 1.5) +
    geom_point(data = centroids, aes(color = "black"), size = 5)
}

# Imprimimos los centroides de los clusters.
print(centroides)
```

Este código implementa el algoritmo K-Means para clustering en R. El algoritmo K-Means es un algoritmo de agrupamiento sin supervisión que divide un conjunto de datos en un número especificado de clusters. El algoritmo funciona iterativamente, asignando cada punto al cluster más cercano y luego recalculando los centroides de los clusters como la media de los puntos asignados a cada cluster. Este proceso se repite hasta que los centroides de los clusters ya no cambian.

El código comienza cargando las bibliotecas necesarias y generando un conjunto de datos aleatorio. Luego, se define el número de clusters que queremos crear e inicializamos los centroides de los clusters de forma aleatoria. A continuación, iteramos sobre los datos, asignando cada punto al cluster más cercano y recalculando los centroides de los clusters. Visualizamos los datos y los clusters, e imprimimos los centroides de los clusters.