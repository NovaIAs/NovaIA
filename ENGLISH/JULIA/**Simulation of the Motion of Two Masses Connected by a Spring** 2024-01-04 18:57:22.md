```julia
# Import the necessary libraries
using Plots
using DifferentialEquations

# Define the differential equation system
function f(t, u)
    # u[1] is the position of the first mass
    # u[2] is the velocity of the first mass
    # u[3] is the position of the second mass
    # u[4] is the velocity of the second mass

    # The equations of motion are:
    # m1 * d^2u[1]/dt^2 = -k * (u[1] - u[3])
    # m2 * d^2u[3]/dt^2 = k * (u[1] - u[3])

    # where m1 and m2 are the masses of the two masses, and k is the spring constant.

    # Define the parameters
    m1 = 1.0
    m2 = 2.0
    k = 10.0

    # Calculate the derivatives
    dudt = zeros(4)
    dudt[1] = u[2]
    dudt[2] = -k * (u[1] - u[3]) / m1
    dudt[3] = u[4]
    dudt[4] = k * (u[1] - u[3]) / m2

    return dudt
end

# Define the initial conditions
u0 = [0.0, 0.0, 1.0, 0.0]

# Solve the differential equation system
tspan = (0.0, 10.0)
prob = ODEProblem(f, u0, tspan)
sol = solve(prob, Tsit5())

# Plot the results
plot(sol.t, sol.u[1,:], label="Position of the first mass")
plot!(sol.t, sol.u[3,:], label="Position of the second mass")

xlabel("Time (s)")
ylabel("Position (m)")
legend!()
```

This code simulates the motion of two masses connected by a spring. The first mass has a mass of 1.0 kg, the second mass has a mass of 2.0 kg, and the spring constant is 10.0 N/m. The initial conditions are that the first mass is at position 0.0 m and has a velocity of 0.0 m/s, and the second mass is at position 1.0 m and has a velocity of 0.0 m/s.

The code uses the DifferentialEquations.jl library to solve the differential equation system. The `f` function defines the differential equation system, and the `u0` variable defines the initial conditions. The `solve` function solves the differential equation system, and the `sol` variable contains the solution.

The `plot` function plots the position of the two masses over time. The `xlabel` and `ylabel` functions set the labels for the x-axis and y-axis, respectively. The `legend!()` function adds a legend to the plot.

This code is complex because it involves solving a system of differential equations. It is also differentiated because it is unlikely to be repeated again.