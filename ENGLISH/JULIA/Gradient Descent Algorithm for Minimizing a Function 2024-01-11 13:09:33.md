```julia
using LinearAlgebra, Plots

# Define the function to be optimized
f(x) = sum(x.^2) - 10*sum(x) + 25

# Define the gradient of the function
∇f(x) = 2*x - 10

# Initialize the optimization parameters
x0 = randn(10)  # Initial guess for the solution
α = 0.1  # Learning rate
ε = 1e-6  # Tolerance for convergence

# Perform gradient descent
while norm(∇f(x0)) > ε
    x0 -= α * ∇f(x0)
end

# Print the optimal solution
println("Optimal solution: $x0")

# Plot the function and the optimal solution
x = range(-5, 5, length=100)
y = f.(x)
plot(x, y, label="f(x)")
scatter!(x0, f(x0), markersize=8, label="Optimal solution")

```

This code implements the gradient descent algorithm to find the minimum of a function. The function to be optimized is defined as `f(x) = sum(x.^2) - 10*sum(x) + 25`, and its gradient is defined as `∇f(x) = 2*x - 10`. The optimization is initialized with a random guess for the solution, a learning rate, and a tolerance for convergence. The algorithm then iteratively updates the solution by subtracting a fraction of the gradient from the current solution. Once the norm of the gradient is below the tolerance, the algorithm terminates and the optimal solution is printed.

The code also plots the function and the optimal solution. The function is plotted as a curve, while the optimal solution is plotted as a scatter plot. This allows us to visualize the function and the optimal solution, and to see how the algorithm converges to the optimal solution.