```julia
using Plots, DifferentialEquations

# Definición de los parámetros del sistema
α = 1.0
β = 0.5
γ = 0.2

# Definición de las ecuaciones diferenciales
function f(u, t)
    dudt = [-α * u[1] + β * u[2]; -γ * u[2] + α * u[1]]
    return dudt
end

# Definición de las condiciones iniciales
u0 = [1.0, 0.0]

# Resolución del sistema de ecuaciones diferenciales
prob = ODEProblem(f, u0, 0.0, 10.0)
sol = solve(prob)

# Trazado de las soluciones
plot(sol[1], sol[2], label="Población de presas")
plot!(sol[1] + sol[2], label="Población total")

```

Explicación del código:

* La primera línea importa las bibliotecas Plots y DifferentialEquations, que se utilizarán para trazar las soluciones y resolver las ecuaciones diferenciales, respectivamente.
* La segunda línea define los parámetros del sistema, α, β y γ.
* La tercera línea define las ecuaciones diferenciales que describen el sistema.
* La cuarta línea define las condiciones iniciales del sistema.
* La quinta línea crea un objeto ODEProblem que contiene las ecuaciones diferenciales, las condiciones iniciales y el intervalo de tiempo en el que se resolverán las ecuaciones.
* La sexta línea resuelve el sistema de ecuaciones diferenciales utilizando el método de Runge-Kutta de cuarto orden.
* La séptima línea crea una gráfica que muestra las poblaciones de presas y depredadores en función del tiempo.
* La octava línea añade la población total de presas y depredadores a la gráfica.