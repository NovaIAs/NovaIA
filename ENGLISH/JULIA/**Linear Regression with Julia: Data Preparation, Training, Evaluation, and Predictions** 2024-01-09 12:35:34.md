```julia
using LinearAlgebra, Plots, Optim, CSV, Statistics

# Load the data
data = CSV.read("data.csv")

# Convert the data to a matrix
X = data[:, 1:end-1]
y = data[:, end]

# Normalize the data
Xnorm = normalize(X)

# Split the data into training and test sets
train_X = Xnorm[1:round(Int, 0.8 * size(Xnorm, 1)), :]
train_y = y[1:round(Int, 0.8 * size(y, 1))]
test_X = Xnorm[round(Int, 0.8 * size(Xnorm, 1)) + 1:end, :]
test_y = y[round(Int, 0.8 * size(y, 1)) + 1:end]

# Train a linear regression model
model = LinearRegression()
fit!(model, train_X, train_y)

# Evaluate the model on the test set
predictions = predict(model, test_X)
mse = mean((predictions - test_y)^2)
rmse = sqrt(mse)
println("Root Mean Squared Error:", rmse)

# Plot the data and the fitted line
plot(test_X, test_y, label="Actual")
plot!(test_X, predictions, label="Predicted")
xlabel("Feature")
ylabel("Target")
legend()

# Save the model
save("model.jl", model)

# Load the model
loaded_model = load("model.jl")

# Make predictions on new data
new_data = [[1.0, 2.0, 3.0]]
new_predictions = predict(loaded_model, new_data)
println("Predictions for new data:", new_predictions)
```

Explanation:

1. Load the data from a CSV file using `CSV.read`.
2. Convert the data to a matrix using `X = data[:, 1:end-1]` and `y = data[:, end]`.
3. Normalize the data using `Xnorm = normalize(X)`.
4. Split the data into training and test sets using `train_X`, `train_y`, `test_X`, and `test_y`.
5. Train a linear regression model using `LinearRegression()` and `fit!`.
6. Evaluate the model on the test set using `predict`, `mse`, `rmse`, and `println`.
7. Plot the data and the fitted line using `plot`, `plot!`, `xlabel`, `ylabel`, and `legend`.
8. Save the model using `save`.
9. Load the model using `load`.
10. Make predictions on new data using `predict`.