```python
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.linear_model import LinearRegression, Lasso, Ridge
from sklearn.metrics import mean_squared_error, r2_score, explained_variance_score

# Load the data
data = pd.read_csv('data.csv')

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(data.drop('target', axis=1), data['target'], test_size=0.2, random_state=42)

# Create a linear regression model
model = LinearRegression()

# Fit the model to the training data
model.fit(X_train, y_train)

# Evaluate the model on the test data
y_pred = model.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
rmse = np.sqrt(mse)
r2 = r2_score(y_test, y_pred)
evs = explained_variance_score(y_test, y_pred)

# Print the evaluation results
print(f'MSE: {mse}')
print(f'RMSE: {rmse}')
print(f'R2: {r2}')
print(f'EVS: {evs}')

# Create a cross-validation object
cv = cross_val_score(model, X_train, y_train, cv=5, scoring='neg_mean_squared_error')

# Print the cross-validation results
print(f'Cross-validation score: {np.mean(cv)}')

# Create a lasso regression model
lasso = Lasso(alpha=0.1)

# Fit the model to the training data
lasso.fit(X_train, y_train)

# Evaluate the model on the test data
y_pred_lasso = lasso.predict(X_test)
mse_lasso = mean_squared_error(y_test, y_pred_lasso)
rmse_lasso = np.sqrt(mse_lasso)
r2_lasso = r2_score(y_test, y_pred_lasso)
evs_lasso = explained_variance_score(y_test, y_pred_lasso)

# Print the evaluation results
print(f'MSE (Lasso): {mse_lasso}')
print(f'RMSE (Lasso): {rmse_lasso}')
print(f'R2 (Lasso): {r2_lasso}')
print(f'EVS (Lasso): {evs_lasso}')

# Create a ridge regression model
ridge = Ridge(alpha=0.1)

# Fit the model to the training data
ridge.fit(X_train, y_train)

# Evaluate the model on the test data
y_pred_ridge = ridge.predict(X_test)
mse_ridge = mean_squared_error(y_test, y_pred_ridge)
rmse_ridge = np.sqrt(mse_ridge)
r2_ridge = r2_score(y_test, y_pred_ridge)
evs_ridge = explained_variance_score(y_test, y_pred_ridge)

# Print the evaluation results
print(f'MSE (Ridge): {mse_ridge}')
print(f'RMSE (Ridge): {rmse_ridge}')
print(f'R2 (Ridge): {r2_ridge}')
print(f'EVS (Ridge): {evs_ridge}')

# Plot the predicted values against the actual values
plt.figure(figsize=(10, 10))
plt.scatter(y_test, y_pred)
plt.plot(y_test, y_test, color='red')
plt.xlabel('Actual values')
plt.ylabel('Predicted values')
plt.title('Linear Regression')
plt.show()

# Plot the predicted values against the actual values for Lasso
plt.figure(figsize=(10, 10))
plt.scatter(y_test, y_pred_lasso)
plt.plot(y_test, y_test, color='red')
plt.xlabel('Actual values')
plt.ylabel('Predicted values')
plt.title('Lasso Regression')
plt.show()

# Plot the predicted values against the actual values for Ridge
plt.figure(figsize=(10, 10))
plt.scatter(y_test, y_pred_ridge)
plt.plot(y_test, y_test, color='red')
plt.xlabel('Actual values')
plt.ylabel('Predicted values')
plt.title('Ridge Regression')
plt.show()
```

**Explanation:**

* The code begins by loading the necessary libraries.
* The data is loaded from a CSV file using the `pd.read_csv()` function.
* The data is then split into training and testing sets using the `train_test_split()` function.
* A linear regression model is created and fit to the training data using the `LinearRegression()` function.
* The model is then evaluated on the test data using the `mean_squared_error()`, `r2_score()`, and `explained_variance_score()` functions.
* The cross-validation score is calculated using the `cross_val_score()` function.
* A lasso regression model is created and fit to the training data using the `Lasso()` function.
* The model is then evaluated on the test data using the `mean_squared_error()`, `r2_score()`, and `explained_variance_score()` functions.
* A ridge regression model is created and fit to the training data using the `Ridge()` function.
* The model is then evaluated on the test data using the `mean_squared_error()`, `r2_score()`, and `explained_variance_score()` functions.
* Finally, the predicted values are plotted against the actual values for each of the three models.