```julia
# We define a function that we want to optimize.
# This function is a nonlinear function: it has multiple local minima and maxima.
function f(x)
    return x^2 - 3*x + 2
end

# We use the Gradient Descent method to find the minimum of the function.
# We start from an initial guess of x = 0.
x = 0.0

# We set the learning rate.
# The learning rate determines how much we move in the direction of the gradient.
alpha = 0.01

# We set the maximum number of iterations.
# The maximum number of iterations determines how many times we update the value of x.
max_iterations = 1000

# We iterate until we reach the maximum number of iterations.
for i in 1:max_iterations
    # We compute the gradient of the function at the current value of x.
    gradient = 2*x - 3

    # We update the value of x.
    x = x - alpha * gradient

    # We print the current value of x.
    println("Iteration $i: x = $x")
end

# We print the final value of x.
println("Final value of x: $x")
```

Explanation:

* We define a function `f(x)` that we want to optimize. This function is a nonlinear function, meaning that it has multiple local minima and maxima.
* We use the Gradient Descent method to find the minimum of the function. The Gradient Descent method is an iterative method that starts from an initial guess and then updates the guess in the direction of the negative gradient of the function.
* We set the learning rate `alpha` and the maximum number of iterations `max_iterations`. The learning rate determines how much we move in the direction of the gradient, and the maximum number of iterations determines how many times we update the value of `x`.
* We iterate until we reach the maximum number of iterations. In each iteration, we compute the gradient of the function at the current value of `x` and then update the value of `x` in the direction of the negative gradient.
* We print the current value of `x` in each iteration.
* Finally, we print the final value of `x`.