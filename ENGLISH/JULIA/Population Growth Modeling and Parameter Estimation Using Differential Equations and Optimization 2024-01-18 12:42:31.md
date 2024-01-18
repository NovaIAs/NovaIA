```julia
# Import necessary libraries
using Plots, Optim, DifferentialEquations, Statistics

# Define a function to calculate the growth rate of a population
function growth_rate(t, pop, params)
    # Unpack the parameters
    r = params[1]  # Intrinsic growth rate
    K = params[2]  # Carrying capacity

    # Calculate the growth rate
    return r * pop * (1 - pop / K)
end

# Define a function to simulate the population growth
function simulate_population(params, t_span, initial_pop)
    # Create a differential equation system
    ode = ODEProblem(growth_rate, initial_pop, t_span, params)

    # Solve the differential equation system
    solution = solve(ode)

    # Return the simulated population data
    return solution.u
end

# Define a function to find the optimal parameters for the population growth model
function find_optimal_params(data, t_span)
    # Define the objective function
    objective_function(params) = sum((simulate_population(params, t_span, data[1]) - data).^2)

    # Create an optimizer
    optimizer = BOBYQA()

    # Optimize the parameters
    optimal_params = optimize(objective_function, [0.5, 1000], optimizer)

    # Return the optimal parameters
    return optimal_params
end

# Load the population data
data = load("population_data.csv")

# Define the time span for the simulation
t_span = [0.0, 100.0]

# Find the optimal parameters for the population growth model
optimal_params = find_optimal_params(data, t_span)

# Simulate the population growth with the optimal parameters
simulated_data = simulate_population(optimal_params, t_span, data[1])

# Plot the data
plot(data[:, 1], data[:, 2], label="Data")
plot!(t_span, simulated_data, label="Simulated")

# Add a legend and show the plot
legend!(location=:topright)
show()
```

Explanation:

1. We import the necessary libraries, including Plots for plotting, Optim for optimization, DifferentialEquations for solving differential equations, and Statistics for statistical analysis.

2. We define a function `growth_rate` that calculates the growth rate of a population based on the population size, intrinsic growth rate, and carrying capacity.

3. We define a function `simulate_population` that simulates the population growth using the differential equation system defined by `growth_rate`.

4. We define a function `find_optimal_params` that finds the optimal parameters for the population growth model by minimizing the sum of squared errors between the simulated and observed population data.

5. We load the population data from a CSV file.

6. We define the time span for the simulation.

7. We find the optimal parameters for the population growth model using the `find_optimal_params` function.

8. We simulate the population growth with the optimal parameters using the `simulate_population` function.

9. We plot the observed data and the simulated data using the `Plots` library.

10. We add a legend and display the plot.