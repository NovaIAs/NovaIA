```julia

# Importamos las librerías necesarias.
using DataFrames, Plots, Optim

# Creamos un conjunto de datos de ejemplo.
datos = DataFrame(
    x = [1, 2, 3, 4, 5],
    y = [2, 4, 6, 8, 10]
)

# Definimos una función objetivo que queremos minimizar.
function objetivo(params)
    a, b = params[1], params[2]
    # Calculamos el error cuadrático medio.
    rmse = sqrt(mean((datos.y - (a * datos.x + b)).^2))
    # Devolvemos el error.
    return rmse
end

# Definimos los parámetros iniciales para la optimización.
params_iniciales = [1.0, 0.0]

# Creamos un optimizador de mínimos cuadrados.
optimizador = Optim.LM()

# Optimizamos la función objetivo.
params_optimos, _ = optimize(optimizador, objetivo, params_iniciales)

# Imprimimos los parámetros óptimos.
println("Parámetros óptimos: a = $(params_optimos[1]), b = $(params_optimos[2])")

# Creamos un gráfico con los datos y la recta de regresión.
plot(datos.x, datos.y, label="Datos")
plot!(linspace(0, 6, 100), params_optimos[1] * linspace(0, 6, 100) + params_optimos[2], label="Recta de regresión")

```

Explicación del código:

* Primero importamos las librerías necesarias.
* Creamos un conjunto de datos de ejemplo con la función `DataFrame`.
* Definimos una función objetivo que queremos minimizar. En este caso, la función objetivo es el error cuadrático medio entre los datos y la recta de regresión.
* Definimos los parámetros iniciales para la optimización.
* Creamos un optimizador de mínimos cuadrados con la función `Optim.LM()`.
* Optimizamos la función objetivo con la función `optimize`.
* Imprimimos los parámetros óptimos.
* Creamos un gráfico con los datos y la recta de regresión con la función `plot`.