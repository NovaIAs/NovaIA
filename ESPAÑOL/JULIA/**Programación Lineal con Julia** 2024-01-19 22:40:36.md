```julia
using LinearAlgebra, Plots, Statistics

# Definición de la función objetivo
objetivo(x) = sum(x.^2)

# Definición de las restricciones
restricciones = [
    x[1] + x[2] <= 10,
    x[1] - x[2] >= 2,
    x[1] >= 0,
    x[2] >= 0
]

# Resolución del problema de programación lineal
resultado = optimize(objetivo, restricciones)

# Impresión del resultado
println("Solución óptima:")
println(resultado.x)
println("Valor objetivo óptimo:")
println(resultado.fval)

# Gráfica de la función objetivo y las restricciones
x1 = range(-5, 15, 0.1)
x2 = range(-5, 15, 0.1)

plot3d(x1, x2, objetivo(x1, x2))
for restricción in restricciones
    plot!([(x, y) for (x, y) in zip(x1, x2) if restricción(x, y)])
end
```

Explicación:

* La función `objetivo` define la función objetivo que se quiere minimizar. En este caso, es la suma de los cuadrados de los componentes del vector `x`.

* La variable `restricciones` define las restricciones que debe satisfacer el vector `x`. En este caso, son cuatro:

    * `x[1] + x[2] <= 10`
    * `x[1] - x[2] >= 2`
    * `x[1] >= 0`
    * `x[2] >= 0`

* La función `optimize` resuelve el problema de programación lineal definido por la función objetivo y las restricciones. Devuelve un objeto del tipo `OptimizationResult` que contiene la solución óptima y el valor objetivo óptimo.

* La llamada a `println` imprime la solución óptima.

* La llamada a `plot3d` grafica la función objetivo y las restricciones.

Este código es un ejemplo de cómo utilizar Julia para resolver un problema de programación lineal. Julia es un lenguaje de programación de alto rendimiento que es especialmente adecuado para tareas computacionalmente intensivas como la optimización.