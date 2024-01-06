```julia

# Import necessary libraries
using LinearAlgebra, Optim, Distributions, ForwardDiff

# Define the objective function
function objective(x)
    # Define the Rosenbrock function
    f(x1, x2) = 100 * (x2 - x1^2)^2 + (1 - x1)^2
    # Return the value of the function at the given point
    return f(x[1], x[2])
end

# Define the gradient of the objective function
function gradient(x)
    # Define the partial derivatives of the Rosenbrock function
    df_dx1(x1, x2) = -400 * x1 * (x2 - x1^2) - 2 * (1 - x1)
    df_dx2(x1, x2) = 200 * (x2 - x1^2)
    # Return the gradient as a vector
    return [df_dx1(x[1], x[2]), df_dx2(x[1], x[2])]
end

# Define the optimization algorithm
optimizer = BFGS()

# Define the initial guess for the optimization
initial_guess = [0.0, 0.0]

# Perform the optimization
result = optimize(optimizer, objective, gradient, initial_guess)

# Print the result
println("Optimal solution: $(result.x)")
println("Optimal value: $(result.fval)")

```

Explanation:

1. **Importing Libraries**: The code begins by importing necessary libraries, including `LinearAlgebra`, `Optim`, `Distributions`, and `ForwardDiff`. These libraries provide functions and tools for linear algebra, optimization, probability distributions, and automatic differentiation, respectively.

2. **Objective Function**: The `objective` function defines the objective function to be minimized. In this case, it is the Rosenbrock function, a classic test function used in optimization. The function takes two arguments, `x1` and `x2`, and returns the value of the function at that point.

3. **Gradient**: The `gradient` function computes the gradient of the objective function. The gradient is a vector that contains the partial derivatives of the function with respect to each input variable. In this case, the gradient has two components, `df_dx1` and `df_dx2`, which are the partial derivatives of the Rosenbrock function with respect to `x1` and `x2`, respectively.

4. **Optimization Algorithm**: The `optimizer` variable specifies the optimization algorithm to be used. In this case, it is the BFGS (Broyden-Fletcher-Goldfarb-Shanno) algorithm, a quasi-Newton method for unconstrained optimization.

5. **Initial Guess**: The `initial_guess` variable provides the initial guess for the optimization algorithm. Here, it is set to `[0.0, 0.0]`, meaning the algorithm starts its search from the point (0, 0).

6. **Optimization**: The `optimize` function performs the optimization. It takes the objective function, the gradient function, the initial guess, and the optimization algorithm as inputs and returns the result of the optimization.

7. **Printing the Result**: Finally, the code prints the optimal solution and the optimal value of the objective function.

This code demonstrates how to use Julia for unconstrained optimization, including defining the objective function, computing the gradient, specifying the optimization algorithm, and obtaining the optimal solution.