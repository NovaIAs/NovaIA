```julia
using DifferentialEquations, Plots
const u₀ = 4  # initial condition
t₀, tfinal = 0.0, 10.0  # time interval
tspan = (t₀, tfinal)
function dudt(du, u, t)
    return -u
end
prob = ODEProblem(dudt, u₀, tspan)
sol = solve(prob, Tsit5())
plot(sol, legend=:false, title="Solution of du/dt = -u")
```

This code solves the differential equation `du/dt = -u` with initial condition `u(0) = 4` on the time interval `[0, 10]`. It uses the `DifferentialEquations.jl` package to define the differential equation and solve it. The `Plots.jl` package is used to plot the solution.

Here is a breakdown of the code:

* `using DifferentialEquations, Plots`: This line imports the `DifferentialEquations.jl` and `Plots.jl` packages.
* `const u₀ = 4`: This line defines the initial condition `u(0) = 4` as a constant named `u₀`.
* `t₀, tfinal = 0.0, 10.0`: This line defines the time interval as `[0, 10]`.
* `tspan = (t₀, tfinal)`: This line creates a tuple containing the time interval.
* `function dudt(du, u, t)`: This line defines the differential equation `du/dt = -u`. The function takes three arguments: `du`, `u`, and `t`, which represent the derivative of `u`, the value of `u`, and the time, respectively.
* `prob = ODEProblem(dudt, u₀, tspan)`: This line creates an `ODEProblem` object, which represents the differential equation and its initial condition.
* `sol = solve(prob, Tsit5())`: This line solves the differential equation using the `Tsit5()` method. The `sol` variable contains the solution.
* `plot(sol, legend=:false, title="Solution of du/dt = -u")`: This line plots the solution. The `legend=:false` option removes the legend from the plot, and the `title` option sets the title of the plot.

This code demonstrates how to solve a differential equation in Julia using the `DifferentialEquations.jl` and `Plots.jl` packages.