```python
# Importar las bibliotecas necesarias
import random
import numpy as np
import matplotlib.pyplot as plt

# Definir los parámetros del sistema dinámico
longitud_cadena = 50  # Número de elementos en la cadena
probabilidad_cambio = 0.5  # Probabilidad de que un elemento cambie de estado
estados_posibles = ['A', 'B', 'C']  # Estados posibles de un elemento

# Crear la cadena inicial
cadena = []
for i in range(longitud_cadena):
    estado = random.choice(estados_posibles)
    cadena.append(estado)

# Definir la función que actualiza la cadena
def actualizar_cadena(cadena, probabilidad_cambio):
    # Crear una nueva cadena
    nueva_cadena = []

    # Recorrer la cadena original
    for estado in cadena:
        # Con probabilidad probabilidad_cambio, el estado cambia
        if random.random() < probabilidad_cambio:
            nuevo_estado = random.choice(estados_posibles)
        # De lo contrario, el estado se mantiene igual
        else:
            nuevo_estado = estado

        # Añadir el nuevo estado a la nueva cadena
        nueva_cadena.append(nuevo_estado)

    # Devolver la nueva cadena
    return nueva_cadena

# Ejecutar el sistema dinámico
num_iteraciones = 100  # Número de iteraciones
cadena_iteraciones = []  # Lista para almacenar la cadena en cada iteración

# Recorrer las iteraciones
for i in range(num_iteraciones):
    # Actualizar la cadena
    cadena = actualizar_cadena(cadena, probabilidad_cambio)

    # Añadir la cadena a la lista de cadenas
    cadena_iteraciones.append(cadena)

# Crear una matriz con las cadenas en cada iteración
matriz_cadenas = np.array(cadena_iteraciones)

# Crear una figura y un eje
fig, ax = plt.subplots()

# Recorrer las cadenas
for cadena in matriz_cadenas:
    # Graficar la cadena
    ax.plot(cadena, color='blue')

# Establecer las etiquetas de los ejes y el título
plt.xlabel('Iteración')
plt.ylabel('Estado')
plt.title('Evolución de la cadena a lo largo de las iteraciones')

# Mostrar la figura
plt.show()
```

Explicación del código:

* El código importa las bibliotecas necesarias: `random` para generar números aleatorios, `numpy` para trabajar con matrices y `matplotlib.pyplot` para graficar.
* A continuación, se definen los parámetros del sistema dinámico: la longitud de la cadena, la probabilidad de que un elemento cambie de estado y los estados posibles de un elemento.
* Se crea la cadena inicial generando aleatoriamente los estados de sus elementos.
* Se define la función `actualizar_cadena` que toma como argumentos la cadena y la probabilidad de cambio. La función devuelve una nueva cadena actualizada según las reglas del sistema dinámico.
* Se ejecuta el sistema dinámico actualizando la cadena en cada iteración y almacenando la cadena en cada iteración en una lista.
* Se crea una matriz con las cadenas en cada iteración usando la función `np.array`.
* Se crea una figura y un eje para graficar la evolución de la cadena a lo largo de las iteraciones.
* Se recorren las cadenas y se grafican en el eje.
* Se establecen las etiquetas de los ejes y el título del gráfico.
* Finalmente, se muestra la figura.