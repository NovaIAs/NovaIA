```julia
# This Julia code defines a function called "rosenbrock" that computes the Rosenbrock function, which is used for testing optimization algorithms.

function rosenbrock(x)
    # Calculate the Rosenbrock function value for the given input vector "x".
    # The Rosenbrock function is defined as: f(x) = sum((1 - x[i])^2 + 100 * (x[i+1] - x[i]^2)^2), where x is a vector of real numbers.

    # Initialize the value of the function to zero.
    f = 0.0

    # Iterate over the elements of the input vector "x" using a for loop.
    for i in 1:(length(x) - 1)
        # Calculate the terms of the Rosenbrock function.
        term1 = (1 - x[i])^2
        term2 = 100 * (x[i+1] - x[i]^2)^2

        # Add the terms to the value of the function.
        f += term1 + term2
    end

    # Return the value of the function.
    return f
end

```

```julia
# This Julia code defines a function called "optimize_rosenbrock" that uses the gradient descent algorithm to find the minimum of the Rosenbrock function.

function optimize_rosenbrock(x0, learning_rate, num_iterations)
    # Initialize the current estimate of the minimum (x) and the gradient.
    x = x0
    gradient = zeros(length(x))

    # Initialize an array to store the values of the function at each iteration.
    f_values = zeros(num_iterations)

    # Iterate over the specified number of iterations.
    for iteration in 1:num_iterations
        # Calculate the gradient of the Rosenbrock function at the current estimate of the minimum.
        gradient = calculate_gradient(x)

        # Update the current estimate of the minimum using gradient descent.
        x -= learning_rate * gradient

        # Calculate the value of the Rosenbrock function at the updated estimate of the minimum.
        f_values[iteration] = rosenbrock(x)
    end

    # Return the final estimate of the minimum and the values of the function at each iteration.
    return x, f_values
end

```

```julia
# This Julia code defines a function called "calculate_gradient" that computes the gradient of the Rosenbrock function.

function calculate_gradient(x)
    # Initialize the gradient vector.
    gradient = zeros(length(x))

    # Iterate over the elements of the input vector "x".
    for i in 1:(length(x) - 1)
        # Calculate the partial derivatives of the Rosenbrock function with respect to each element of "x".
        gradient[i] = -2 * (1 - x[i]) + 200 * x[i] * (x[i+1] - x[i]^2)
        gradient[i+1] = 200 * (x[i+1] - x[i]^2)
    end

    # Return the gradient vector.
    return gradient
end

```

```julia
# This Julia code demonstrates the use of the "rosenbrock" and "optimize_rosenbrock" functions by finding the minimum of the Rosenbrock function starting from an initial guess.

# Set the initial guess for the minimum.
x0 = ones(2)

# Set the learning rate and the number of iterations for the optimization algorithm.
learning_rate = 0.01
num_iterations = 1000

# Call the optimization function to find the minimum.
x_opt, f_values = optimize_rosenbrock(x0, learning_rate, num_iterations)

# Print the final estimate of the minimum and the values of the function at each iteration.
println("Final estimate of the minimum:")
println(x_opt)
println("Values of the function at each iteration:")
println(f_values)

```