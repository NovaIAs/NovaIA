```julia
using LinearAlgebra, Plots

# Define the function to be optimized
f(x) = sum(x.^2) - 10*sum(x) + 25

# Initialize the optimization parameters
x0 = rand(10)  # Initial guess for the solution
max_iter = 1000  # Maximum number of iterations
tol = 1e-6  # Tolerance for the stopping criterion

# Define the gradient of the function
gradient(f, x) = 2*x - 10

# Perform gradient descent
for iter = 1:max_iter
    gradient_x = gradient(f, x0)
    x0 -= 0.1 * gradient_x  # Take a step in the direction of the negative gradient

    # Check the stopping criterion
    if norm(gradient_x) < tol
        break
    end
end

# Print the final solution
println("Optimal solution:", x0)

# Plot the convergence of the objective function
iterations = 1:iter
objective_values = [f(x0) for i in iterations]
plot(iterations, objective_values, label="Objective Function")

xlabel("Iteration")
ylabel("Objective Value")
title("Gradient Descent Convergence")

```

**Explanation:**

This code demonstrates the use of gradient descent to optimize a function in Julia. The function to be optimized is defined as `f(x) = sum(x.^2) - 10*sum(x) + 25`, where `x` is a vector of real numbers. The goal is to find the values of `x` that minimize this function.

The optimization is performed using gradient descent, which is an iterative algorithm that starts with an initial guess for the solution and repeatedly moves in the direction of the negative gradient of the function. The gradient of the function is a vector that points in the direction of the steepest increase in the function. By moving in the opposite direction, we are moving towards a minimum of the function.

The code initializes the optimization parameters, including the initial guess for the solution (`x0`), the maximum number of iterations (`max_iter`), and the tolerance for the stopping criterion (`tol`). It then defines the gradient of the function and performs the gradient descent algorithm.

The algorithm iteratively updates the value of `x0` by taking a step in the direction of the negative gradient. The step size is controlled by the learning rate (0.1 in this case). The algorithm stops when the norm of the gradient is less than the tolerance `tol`.

Finally, the code prints the final solution and plots the convergence of the objective function, which shows how the value of the function decreases with each iteration of the algorithm.