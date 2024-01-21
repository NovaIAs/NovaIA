```julia
# Definir una función para calcular la raíz cuadrada de un número.
raiz_cuadrada(x) = √(x)

# Definir una función para calcular el área de un círculo.
area_circulo(radio) = π * radio^2

# Definir una función para calcular el volumen de una esfera.
volumen_esfera(radio) = (4/3) * π * radio^3

# Definir una constante para el número de Avogadro.
N_AVOGADRO = 6.02214076e23

# Definir una función para calcular la masa de una muestra de gas ideal a partir de su número de moles y su temperatura.
masa_gas_ideal(n, T) = n * R * T / P

# Definir una función para calcular la energía cinética promedio de las moléculas de un gas ideal a partir de su temperatura.
energia_cinetica_promedio(T) = (3/2) * R * T

# Definir una función para calcular la velocidad cuadrática media de las moléculas de un gas ideal a partir de su temperatura.
velocidad_cuadratica_media(T) = √(8 * R * T / π * M)

# Definir una función para calcular la presión de un gas ideal a partir de su número de moles, su volumen y su temperatura.
presion_gas_ideal(n, V, T) = n * R * T / V

# Definir una función para calcular el trabajo realizado por un gas ideal durante una expansión isotérmica.
trabajo_expansión_isotermica(n, V_i, V_f) = n * R * T * log(V_f / V_i)

# Definir una función para calcular el calor transferido a un gas ideal durante un proceso isotérmico.
calor_transferido_isotermico(n, V_i, V_f) = n * R * T * log(V_f / V_i)

# Definir una función para calcular el trabajo realizado por un gas ideal durante una expansión adiabática.
trabajo_expansión_adiabática(n, V_i, V_f, γ) = (n * R * T_i) / (γ - 1) * (1 - (V_f / V_i)^(γ - 1))

# Definir una función para calcular el calor transferido a un gas ideal durante un proceso adiabático.
calor_transferido_adiabático(n, V_i, V_f, γ) = (n * R * T_i) / (1 - γ) * (1 - (V_f / V_i)^(1 - γ))
```

Explicación:

* La función `raiz_cuadrada(x)` calcula la raíz cuadrada de un número `x`.
* La función `area_circulo(radio)` calcula el área de un círculo con radio `radio`.
* La función `volumen_esfera(radio)` calcula el volumen de una esfera con radio `radio`.
* La variable `N_AVOGADRO` es una constante que representa el número de Avogadro.
* La función `masa_gas_ideal(n, T)` calcula la masa de una muestra de gas ideal a partir de su número de moles `n` y su temperatura `T`.
* La función `energia_cinetica_promedio(T)` calcula la energía cinética promedio de las moléculas de un gas ideal a partir de su temperatura `T`.
* La función `velocidad_cuadratica_media(T)` calcula la velocidad cuadrática media de las moléculas de un gas ideal a partir de su temperatura `T`.
* La función `presion_gas_ideal(n, V, T)` calcula la presión de un gas ideal a partir de su número de moles `n`, su volumen `V` y su temperatura `T`.
* La función `trabajo_expansión_isotermica(n, V_i, V_f)` calcula el trabajo realizado por un gas ideal durante una expansión isotérmica.
* La función `calor_transferido_isotermico(n, V_i, V_f)` calcula el calor transferido a un gas ideal durante un proceso isotérmico.
* La función `trabajo_expansión_adiabática(n, V_i, V_f, γ)` calcula el trabajo realizado por un gas ideal durante una expansión adiabática.
* La función `calor_transferido_adiabático(n, V_i, V_f, γ)` calcula el calor transferido a un gas ideal durante un proceso adiabático.