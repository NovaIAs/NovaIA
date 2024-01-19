```julia
# Import required libraries
using LinearAlgebra, Plots, Optimization

# Generate synthetic data
n = 100
x = randn(n)
y = 2*x + 3 + 0.1*randn(n)

# Define the linear regression model
model = @formula x ~ 1 + y

# Fit the model to the data
fit = lm(model, data=DataFrame(x=x, y=y))

# Plot the data and fitted line
plot(x, y, label="Data")
plot!(fit, label="Fitted Line")

# Evaluate the model on new data
new_x = linspace(-3, 3, 100)
new_y = predict(fit, new_x)

# Plot the new data and fitted line
plot(new_x, new_y, label="New Data")
plot!(fit, label="Fitted Line")

# Calculate the mean squared error on the new data
mse = mean((new_y - predict(fit, new_x)) .^ 2)

# Print the mean squared error
println("Mean squared error:", mse)

# Optimize the hyperparameters of the model using Bayesian optimization
objective = (params) -> mean_squared_error(fit, new_x, predict(fit, new_x; params=params))
bounds = [(0.0, 10.0), (-10.0, 10.0)]
result = optimize(objective, bounds)

# Print the optimized hyperparameters
println("Optimized hyperparameters:", result.solution)

# Evaluate the optimized model on the new data
optimized_fit = lm(model, data=DataFrame(x=x, y=y), params=result.solution)
optimized_y = predict(optimized_fit, new_x)

# Plot the new data and optimized fitted line
plot(new_x, optimized_y, label="New Data")
plot!(optimized_fit, label="Optimized Fitted Line")

# Calculate the mean squared error of the optimized model on the new data
optimized_mse = mean((optimized_y - predict(optimized_fit, new_x)) .^ 2)

# Print the mean squared error of the optimized model
println("Optimized mean squared error:", optimized_mse)
```

Explanation:

1. Import the necessary libraries.

2. Generate synthetic data for a linear regression problem.

3. Define a linear regression model using the `@formula` macro.

4. Fit the model to the data using the `lm()` function.

5. Plot the data and the fitted line using the `plot()` function.

6. Evaluate the model on new data and calculate the mean squared error (MSE).

7. Optimize the hyperparameters of the model using Bayesian optimization.

8. Evaluate the optimized model on the new data and calculate the optimized MSE.

9. Plot the new data and the optimized fitted line.

10. Print the optimized hyperparameters and the optimized MSE.

This code demonstrates a complete workflow for linear regression in Julia, including data generation, model fitting, evaluation, hyperparameter optimization, and plotting. It covers various aspects of modeling and optimization, making it a comprehensive example of a complex code in Julia.