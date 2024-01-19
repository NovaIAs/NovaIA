```julia
# Import necessary libraries
using LinearAlgebra, Plots, Optim

# Define the objective function
f(x) = sum(x.^2) - 10*sum(x)

# Define the gradient of the objective function
g(x) = 2*x - 10

# Initialize the optimization parameters
x0 = rand(3)  # Initial guess for the solution
max_iter = 1000  # Maximum number of iterations
tol = 1e-6  # Tolerance for the convergence criterion

# Perform optimization using the BFGS algorithm
result = optimize(f, g, x0, BFGS(), max_iter, tol)

# Extract the optimal solution and the corresponding function value
x_optimal = result.x
f_optimal = f(x_optimal)

# Print the results
println("Optimal solution:")
println(x_optimal)
println("Optimal function value:")
println(f_optimal)

# Plot the objective function and the optimal solution
x_values = range(-5.0, 5.0, 0.1)
y_values = [f(x) for x in x_values]
plot(x_values, y_values, label="Objective function")
scatter!(x_optimal, f_optimal, markersize=8, color=:red, label="Optimal solution")
xlabel("x")
ylabel("f(x)")
legend()
```

**Explanation:**

This code demonstrates a complex optimization task in Julia, using the BFGS algorithm to find the minimum of a quadratic function. Here's a breakdown of the code:

1. **Importing Libraries**: We start by importing necessary libraries:
   - `LinearAlgebra`: Provides linear algebra functions and data structures.
   - `Plots`: For plotting the objective function and the optimal solution.
   - `Optim`: Contains optimization algorithms and related functions.

2. **Defining the Objective Function**: We define the objective function `f(x)` as the sum of squared elements of the input vector `x` minus 10 times the sum of `x`.

3. **Defining the Gradient of the Objective Function**: We define the gradient of `f(x)` as the vector of partial derivatives, which is `g(x) = 2*x - 10`.

4. **Initializing Optimization Parameters**: We set the initial guess for the solution (`x0`), the maximum number of iterations (`max_iter`), and the tolerance for convergence (`tol`).

5. **Performing Optimization**: We use the `optimize` function with the BFGS algorithm to find the minimum of `f(x)`, starting from the initial guess `x0`. The result is stored in the `result` variable.

6. **Extracting Results**: From the `result` variable, we extract the optimal solution `x_optimal` and the corresponding function value `f_optimal`.

7. **Printing Results**: We print the optimal solution and the optimal function value.

8. **Plotting the Objective Function**: We create a plot of the objective function `f(x)` over a range of `x` values.

9. **Plotting the Optimal Solution**: We add a scatter plot to the same graph, showing the optimal solution `x_optimal` and the corresponding function value `f_optimal`.

10. **Adding Labels and Legend**: We add labels to the axes and a legend to the plot.

This code showcases a complex optimization task and demonstrates how to use the BFGS algorithm to find the minimum of a quadratic function in Julia.