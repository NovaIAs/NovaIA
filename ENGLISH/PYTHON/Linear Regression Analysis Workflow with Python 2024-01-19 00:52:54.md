```python
import sympy
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import scipy.stats as stats
import seaborn as sns
import plotly.express as px
import plotly.graph_objects as go
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.preprocessing import StandardScaler

# Define the dataset
data = pd.read_csv('data.csv')

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(data.drop('target', axis=1), data['target'], test_size=0.2, random_state=42)

# Standardize the data
scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

# Train the linear regression model
model = LinearRegression()
model.fit(X_train, y_train)

# Evaluate the model
y_pred = model.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

# Print the results
print('MSE:', mse)
print('R2:', r2)

# Plot the data
plt.scatter(X_test[:, 0], y_test, color='blue', label='Actual')
plt.scatter(X_test[:, 0], y_pred, color='red', label='Predicted')
plt.xlabel('Feature')
plt.ylabel('Target')
plt.legend()
plt.show()

# Create a histogram of the residuals
plt.hist(y_test - y_pred, bins=20)
plt.xlabel('Residuals')
plt.ylabel('Frequency')
plt.show()

# Create a QQ plot of the residuals
stats.probplot(y_test - y_pred, dist='norm', plot=plt)
plt.show()
```

This code is a complete data analysis workflow using Python. It includes data reading, data preprocessing, model training, model evaluation, and data visualization. The code is well-commented and easy to understand.

Here is a step-by-step explanation of the code:

1. Import the necessary libraries.
2. Define the dataset by reading it from a CSV file.
3. Split the data into training and testing sets.
4. Standardize the data to make it more suitable for linear regression.
5. Train the linear regression model on the training data.
6. Evaluate the model on the testing data.
7. Print the results of the evaluation.
8. Plot the actual and predicted values of the target variable.
9. Create a histogram of the residuals.
10. Create a QQ plot of the residuals.

This code can be used to analyze any dataset and build a linear regression model to predict the target variable.