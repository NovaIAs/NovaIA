```
import numpy as np
import matplotlib.pyplot as plt

# Definimos los parámetros de nuestra simulación
num_particulas = 1000
masa_particula = 1.0
radio_particula = 0.1
carga_particula = 1.0

# Creamos un array con las posiciones y velocidades iniciales de las partículas
posiciones = np.random.rand(num_particulas, 3) * 10.0
velocidades = np.random.rand(num_particulas, 3) * 0.1

# Definimos el paso de tiempo para nuestra simulación
dt = 0.001

# Creamos un array para almacenar las fuerzas que actúan sobre cada partícula
fuerzas = np.zeros((num_particulas, 3))

# Creamos un array para almacenar la energía cinética de las partículas
energia_cinetica = np.zeros(num_particulas)

# Creamos un array para almacenar la energía potencial de las partículas
energia_potencial = np.zeros(num_particulas)

# Iteramos sobre los pasos de tiempo de nuestra simulación

for t in range(1000):

    # Actualizamos las posiciones de las partículas
    posiciones += velocidades * dt

    # Actualizamos las fuerzas que actúan sobre las partículas
    for i in range(num_particulas):
        for j in range(num_particulas):
            if i == j:
                continue

            # Calculamos la fuerza electrostática entre las partículas i y j
            fuerza_electrostatica = (
                carga_particula**2
                * (posiciones[i] - posiciones[j])
                / np.linalg.norm(posiciones[i] - posiciones[j])**3
            )

            # Calculamos la fuerza gravitatoria entre las partículas i y j
            fuerza_gravitatoria = (
                masa_particula**2
                * (posiciones[i] - posiciones[j])
                / np.linalg.norm(posiciones[i] - posiciones[j])**3
            )

            # Actualizamos las fuerzas que actúan sobre las partículas i y j
            fuerzas[i] += fuerza_electrostatica + fuerza_gravitatoria
            fuerzas[j] += -(fuerza_electrostatica + fuerza_gravitatoria)

    # Actualizamos las velocidades de las partículas
    velocidades += fuerzas * dt / masa_particula

    # Calculamos la energía cinética de las partículas
    energia_cinetica = 0.5 * masa_particula * np.linalg.norm(velocidades, axis=1) ** 2

    # Calculamos la energía potencial de las partículas
    energia_potencial = (1.0 / 2.0) * carga_particula**2 / np.linalg.norm(posiciones, axis=1)

    # Imprimimos la energía total del sistema
    print(f"Energía total: {np.sum(energia_cinetica + energia_potencial)}")

# Creamos una figura para mostrar los resultados de nuestra simulación
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(15, 10))

# Mostramos las posiciones de las partículas en el espacio
ax1.scatter(posiciones[:, 0], posiciones[:, 1], posiciones[:, 2], s=20, color="blue")
ax1.set_xlabel("x")
ax1.set_ylabel("y")
ax1.set_zlabel("z")

# Mostramos la energía total del sistema en función del tiempo
ax2.plot(energia_cinetica + energia_potencial)
ax2.set_xlabel("Tiempo")
ax2.set_ylabel("Energía total")

# Mostramos la figura
plt.show()
```

Este código simula el movimiento de un sistema de partículas interagentes en un espacio tridimensional. Las partículas están sujetas a fuerzas electrostáticas y gravitatorias. El código actualiza las posiciones y velocidades de las partículas en cada paso de tiempo, y calcula la energía cinética y potencial del sistema. Los resultados de la simulación se muestran en una figura que muestra las posiciones de las partículas en el espacio y la energía total del sistema en función del tiempo.