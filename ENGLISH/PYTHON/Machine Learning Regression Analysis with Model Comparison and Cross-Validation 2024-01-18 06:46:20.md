```python
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.linear_model import LinearRegression, Lasso, Ridge
from sklearn.metrics import mean_squared_error, r2_score

# Load the data
data = pd.read_csv('data.csv')

# Split the data into training and testing sets
X = data.drop('target', axis=1)
y = data['target']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Create a linear regression model
model = LinearRegression()

# Train the model
model.fit(X_train, y_train)

# Evaluate the model on the training data
y_pred_train = model.predict(X_train)
mse_train = mean_squared_error(y_train, y_pred_train)
r2_train = r2_score(y_train, y_pred_train)

# Evaluate the model on the test data
y_pred_test = model.predict(X_test)
mse_test = mean_squared_error(y_test, y_pred_test)
r2_test = r2_score(y_test, y_pred_test)

# Print the results
print('Training MSE:', mse_train)
print('Training R2:', r2_train)
print('Test MSE:', mse_test)
print('Test R2:', r2_test)

# Cross-validate the model
scores = cross_val_score(model, X, y, cv=5)
print('Cross-validation scores:', scores)

# Create a Lasso model
lasso = Lasso(alpha=0.1)

# Train the Lasso model
lasso.fit(X_train, y_train)

# Evaluate the Lasso model on the training data
y_pred_train_lasso = lasso.predict(X_train)
mse_train_lasso = mean_squared_error(y_train, y_pred_train_lasso)
r2_train_lasso = r2_score(y_train, y_pred_train_lasso)

# Evaluate the Lasso model on the test data
y_pred_test_lasso = lasso.predict(X_test)
mse_test_lasso = mean_squared_error(y_test, y_pred_test_lasso)
r2_test_lasso = r2_score(y_test, y_pred_test_lasso)

# Print the results
print('Training MSE (Lasso):', mse_train_lasso)
print('Training R2 (Lasso):', r2_train_lasso)
print('Test MSE (Lasso):', mse_test_lasso)
print('Test R2 (Lasso):', r2_test_lasso)

# Create a Ridge model
ridge = Ridge(alpha=0.1)

# Train the Ridge model
ridge.fit(X_train, y_train)

# Evaluate the Ridge model on the training data
y_pred_train_ridge = ridge.predict(X_train)
mse_train_ridge = mean_squared_error(y_train, y_pred_train_ridge)
r2_train_ridge = r2_score(y_train, y_pred_train_ridge)

# Evaluate the Ridge model on the test data
y_pred_test_ridge = ridge.predict(X_test)
mse_test_ridge = mean_squared_error(y_test, y_pred_test_ridge)
r2_test_ridge = r2_score(y_test, y_pred_test_ridge)

# Print the results
print('Training MSE (Ridge):', mse_train_ridge)
print('Training R2 (Ridge):', r2_train_ridge)
print('Test MSE (Ridge):', mse_test_ridge)
print('Test R2 (Ridge):', r2_test_ridge)

# Plot the results
plt.figure()
plt.plot(scores, label='Cross-validation scores')
plt.xlabel('Fold')
plt.ylabel('Score')

plt.figure()
plt.scatter(y_test, y_pred_test, label='True vs predicted values')
plt.xlabel('True values')
plt.ylabel('Predicted values')
plt.legend()

plt.show()
```

This code is a machine learning script that uses the Python programming language to train and evaluate three different linear regression models on a given dataset. The code performs the following steps:

1. Loads the data from a CSV file using the Pandas library.
2. Splits the data into training and testing sets using the Scikit-Learn library.
3. Creates a linear regression model, a Lasso model, and a Ridge model using the Scikit-Learn library.
4. Trains the models on the training data.
5. Evaluates the models on the training and test data using metrics such as mean squared error (MSE) and R-squared (R2).
6. Performs cross-validation to estimate the models' performance on unseen data.
7. Plots the cross-validation scores and the true versus predicted values for the test data.

This code demonstrates a more complex and comprehensive approach to machine learning by comparing different models and evaluating their performance using cross-validation. It also provides a visual representation of the model's predictions.