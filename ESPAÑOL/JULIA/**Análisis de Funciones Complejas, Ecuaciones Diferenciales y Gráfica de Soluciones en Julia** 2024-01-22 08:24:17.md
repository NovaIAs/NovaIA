```julia
using SpecialFunctions, LinearAlgebra, DifferentialEquations, Plots

# Definir una función compleja
f(z) = (z^2 - 1) / (z^2 + 1)

# Calcular los ceros y polos de la función
ceros, polos = roots(f), roots(simplify(f'))

# Mostrar los ceros y polos en un plano complejo
plot(ceros, polos, legend=:bottomright)

# Definir una ecuación diferencial lineal de segundo orden
ode = diff(u(t), t, 2) + 2*diff(u(t), t) + 4*u(t) == cos(t)

# Resolver la ecuación diferencial para obtener la solución general
sol = desolve(ode, u(t), t)

# Mostrar la solución general
println(sol)

# Definir una función para evaluar la solución general en un punto dado
u(t) = sol(t)

# Calcular los valores de la solución en una lista de puntos
t_vals = range(0.0, 2*pi, 0.1)
u_vals = map(u, t_vals)

# Mostrar los valores de la solución en un gráfico
plot(t_vals, u_vals)
```

Explicación del código:

* La primera línea importa las bibliotecas necesarias para el código.
* La segunda línea define una función compleja `f`.
* La tercera línea calcula los ceros y polos de la función `f`.
* La cuarta línea muestra los ceros y polos en un plano complejo.
* La quinta línea define una ecuación diferencial lineal de segundo orden.
* La sexta línea resuelve la ecuación diferencial para obtener la solución general.
* La séptima línea muestra la solución general.
* La octava línea define una función para evaluar la solución general en un punto dado.
* La novena línea calcula los valores de la solución en una lista de puntos.
* La décima línea muestra los valores de la solución en un gráfico.