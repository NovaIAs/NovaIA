```julia
using LinearAlgebra, Plots, Optim, Distributed, CSV
Data = CSV.read("data.csv", header=true)
Data = Data[2:end, 2:end]
X = Data[1:end, 1:(end-1)]
y = Data[1:end, end]
m, n = size(X)
theta = rand(n)
lr = 0.01  # Learning Rate
epochs = 1000
for epoch in 1:epochs
    H = X * theta
    J = (1/(2*m)) * sum((H - y)^2)
    println("Epoch $epoch: Cost $J")
    gradient = (1/m) * X' * (H - y)
    theta -= lr * gradient
end
Optim.minimize(f -> (1/(2*m)) * sum((X * theta - y)^2), theta, Optim.LBFGS())
println("Theta:", theta)
plot(X[:, 1], y, label="Data")
plot!(X[:, 1], X * theta, label="Fitted Line")
```

Explanation:

1. **Data Loading and Preprocessing:**
   - We start by loading a dataset from a CSV file using `CSV.read()`.
   - We extract the feature matrix `X` and the target vector `y` from the dataset.

2. **Model Initialization:**
   - We define the initial values for the model parameters `theta`.

3. **Training Loop:**
   - We iterate through a specified number of epochs (`epochs`).
   - In each epoch, we compute the predicted values `H` using the current model parameters.
   - We calculate the cost function `J`, which measures the mean squared error between the predicted values and the actual target values.
   - We compute the gradient of the cost function with respect to the model parameters `theta`.
   - We update the model parameters using gradient descent with a fixed learning rate (`lr`).

4. **Optimization:**
   - After the training loop, we use the `Optim.minimize()` function with the L-BFGS optimization algorithm to find the optimal values for `theta`.

5. **Results and Visualization:**
   - We print the optimized values of `theta`.
   - We plot the original data and the fitted linear regression line using `Plots.plot()` and `Plots.plot!()`.