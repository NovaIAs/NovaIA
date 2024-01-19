```julia
using LinearAlgebra, Plots, CSV

# Load the data
data = CSV.read("data.csv")

# Convert the data to a matrix
X = Matrix(data[:, 2:end])  # Features
y = Matrix(data[:, 1])      # Labels

# Split the data into training and testing sets
n = size(X, 1)
train_size = round(Int, 0.8 * n)
test_size = n - train_size
train_indices = randperm(n, train_size)
test_indices = setdiff(1:n, train_indices)
X_train = X[train_indices, :]
y_train = y[train_indices]
X_test = X[test_indices, :]
y_test = y[test_indices]

# Normalize the data
X_train, X_mean, X_std = normalize(X_train, dims=2)
X_test = (X_test .- X_mean) ./ X_std

# Define the model parameters
num_features = size(X_train, 2)
num_classes = unique(y_train)
W = randn(num_classes, num_features)
b = randn(num_classes, 1)

# Define the loss function
loss_fn = (ŷ, y) -> sum((ŷ .- y) .^ 2)

# Define the gradient of the loss function
∇_loss_fn = (ŷ, y) -> 2 * (ŷ .- y)

# Define the optimizer
optimizer = gradient_descent()

# Train the model
for epoch in 1:1000
    y_pred = softmax(X_train * W .+ b)
    loss = loss_fn(y_pred, y_train)
    grads_W, grads_b = ∇_loss_fn(y_pred, y_train)
    optimizer.step!(W, grads_W)
    optimizer.step!(b, grads_b)

    if epoch % 100 == 0
        println("Epoch: $epoch, Loss: $loss")
    end
end

# Evaluate the model on the test set
y_pred_test = softmax(X_test * W .+ b)
loss_test = loss_fn(y_pred_test, y_test)
accuracy = mean(y_pred_test ≈ y_test)

println("Test Loss: $loss_test, Accuracy: $accuracy")

# Plot the training and test loss curves
plot(loss, label="Training Loss")
plot!(loss_test, label="Test Loss")
xlabel("Epoch")
ylabel("Loss")
legend()

# Save the model
save("model.jl", W, b, X_mean, X_std)
```

Explanation:

* The code loads a dataset from a CSV file.
* The dataset is split into training and testing sets.
* The data is normalized to have zero mean and unit variance.
* A multi-class logistic regression model is defined.
* The model is trained using gradient descent.
* The model is evaluated on the test set.
* The training and test loss curves are plotted.
* The model is saved to a file.

This code is relatively complex and sophisticated, and it covers a number of different topics in machine learning, including data preprocessing, model definition, training, evaluation, and visualization. It is a good example of a code that is likely to be difficult to reproduce.