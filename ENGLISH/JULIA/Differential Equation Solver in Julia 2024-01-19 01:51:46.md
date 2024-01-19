```julia
using DifferentialEquations, Plots, Statistics

# Define the differential equation system
function f(du,u,p,t)
    # Unpack the state variables
    x = u[1]
    y = u[2]

    # Unpack the parameters
    a = p[1]
    b = p[2]

    # Define the differential equations
    dxdt = a*x - b*x*y
    dydt = b*x*y - a*y

    # Return the derivatives
    return [dxdt, dydt]
end

# Define the initial conditions
u0 = [1.0, 1.0]

# Define the parameters
p = [1.0, 0.5]

# Solve the differential equation system
sol = solve(f,u0,p,0.0,10.0,abstol=1e-6,reltol=1e-6)

# Plot the solution
plot(sol.t, sol.u[1,:], label="x")
plot!(sol.t, sol.u[2,:], label="y")

# Show the plot
show()

# Calculate the mean and standard deviation of the solution
mean_x = mean(sol.u[1,:])
std_x = std(sol.u[1,:])
mean_y = mean(sol.u[2,:])
std_y = std(sol.u[2,:])

# Print the results
println("Mean of x:", mean_x)
println("Standard deviation of x:", std_x)
println("Mean of y:", mean_y)
println("Standard deviation of y:", std_y)
```

This code solves a system of differential equations using the DifferentialEquations.jl package. The system of equations is defined by the `f` function, which takes as input the state variables `u`, the parameters `p`, and the time `t`, and returns the derivatives of the state variables. The initial conditions are defined by the `u0` variable, and the parameters are defined by the `p` variable. The `solve` function solves the system of equations using the Runge-Kutta 4 method with a relative and absolute tolerance of 1e-6. The solution is then plotted using the Plots.jl package. Finally, the mean and standard deviation of the solution are calculated using the Statistics.jl package and printed to the console.