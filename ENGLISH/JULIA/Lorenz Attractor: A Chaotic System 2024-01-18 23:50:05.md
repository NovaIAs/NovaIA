```julia
# Import requisite libraries
using DifferentialEquations, Plots

# Define the Lorenz equations
lorenz_eqs(t, u; σ, ρ, β) = [
    σ * (u[2] - u[1]),
    ρ * u[1] - u[2] - u[1] * u[3],
    u[1] * u[2] - β * u[3]
]

# Define the parameters
σ = 10.0
ρ = 28.0
β = 8/3.0

# Define the initial condition
u0 = [1.0, 1.0, 1.0]

# Define a time range
t_range = 0.0:0.01:100.0

# Solve the equations
sol = solve(lorenz_eqs, u0, t_range, σ=σ, ρ=ρ, β=β)

# Extract the solution components
x = sol[1, :]
y = sol[2, :]
z = sol[3, :]

# Plot the solution
plot(x, y, z, color=:black, marker=:nothing, label="Solution Trajectory")

# Add axis labels and title
xlabel("x")
ylabel("y")
zlabel("z")
title("Lorenz Attractor")

# Add legend
legend()

# Show the plot
show()
```

This code implements the Lorenz equations, a system of three differential equations that exhibit chaotic behavior. The equations are defined in the `lorenz_eqs` function, which takes as input the time `t`, the state vector `u`, and three parameters `σ`, `ρ`, and `β`. The equations are solved using the `solve` function, which integrates the equations over the time range `t_range`. The solution is stored in the `sol` variable, which is a matrix with three rows (one for each component of the state vector) and `length(t_range)` columns.

The `x`, `y`, and `z` variables are extracted from the solution matrix and plotted using the `plot` function. The plot shows the trajectory of the solution in three-dimensional space. The `xlabel`, `ylabel`, and `zlabel` functions add labels to the axes, and the `title` function adds a title to the plot. The `legend` function adds a legend to the plot, and the `show` function displays the plot.