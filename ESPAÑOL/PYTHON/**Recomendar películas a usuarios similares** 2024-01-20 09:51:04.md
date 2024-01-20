```python
# Este código implementa un sistema de recomendaciones de películas basado en usuarios similares.

# Importamos las librerías necesarias.
import numpy as np
import pandas as pd
import scipy.sparse as sp
from sklearn.neighbors import NearestNeighbors

# Cargamos los datos de las películas.
películas = pd.read_csv("peliculas.csv")

# Creamos una matriz de valoraciones de las películas.
valoraciones = sp.dok_matrix((películas.shape[0], películas.shape[0]), dtype=np.int32)
for i, película in películas.iterrows():
    for valoración in película["valoraciones"].split(","):
        valoración = valoración.split(":")
        valoraciones[int(valoración[0]), i] = int(valoración[1])

# Calculamos las similitudes entre los usuarios.
similaridades = NearestNeighbors(metric="cosine").fit(valoraciones)

# Obtenemos las películas recomendadas para un usuario.
def recomendar_películas(usuario, n_recomendaciones=10):
    # Obtenemos los índices de las películas más similares a las que ha visto el usuario.
    indices_peliculas_similares = similaridades.kneighbors(valoraciones[usuario, :])[1][0]

    # Obtenemos las puntuaciones de las películas más similares.
    puntuaciones_peliculas_similares = valoraciones[indices_peliculas_similares, usuario]

    # Obtenemos las películas más recomendadas.
    películas_recomendadas = películas.iloc[indices_peliculas_similares][puntuaciones_peliculas_similares > 0].sort_values(by="valoración", ascending=False).head(n_recomendaciones)

    # Devolvemos las películas recomendadas.
    return películas_recomendadas

# Probamos el sistema de recomendaciones.
usuario = 1
películas_recomendadas = recomendar_películas(usuario)
print("Películas recomendadas para el usuario {}:".format(usuario))
print(películas_recomendadas)
```

Explicación del código:

* Importamos las librerías necesarias.
* Cargamos los datos de las películas.
* Creamos una matriz de valoraciones de las películas.
* Calculamos las similitudes entre los usuarios.
* Obtenemos las películas recomendadas para un usuario.
* Probamos el sistema de recomendaciones.