```python
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import sklearn
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Load the data
data = pd.read_csv('data.csv')

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(data[['x1', 'x2']], data['y'], test_size=0.2, random_state=42)

# Create a linear regression model
model = LinearRegression()

# Train the model
model.fit(X_train, y_train)

# Make predictions on the test set
y_pred = model.predict(X_test)

# Evaluate the model
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

# Print the results
print('MSE:', mse)
print('R2:', r2)

# Plot the results
plt.scatter(X_test['x1'], y_test, color='blue', label='Actual')
plt.scatter(X_test['x1'], y_pred, color='red', label='Predicted')
plt.xlabel('x1')
plt.ylabel('y')
plt.legend()
plt.show()

# Save the model
import pickle
with open('model.pkl', 'wb') as f:
    pickle.dump(model, f)

# Load the model
with open('model.pkl', 'rb') as f:
    model = pickle.load(f)

# Make predictions on new data
new_data = pd.DataFrame({'x1': [10, 15, 20]})
y_pred = model.predict(new_data)

# Print the predictions
print(y_pred)
```

This code performs a simple linear regression on a dataset. It loads the data from a CSV file, splits it into training and testing sets, trains a linear regression model, evaluates the model on the test set, and plots the results. The code also saves the trained model to a pickle file and loads it back to make predictions on new data.

Here is a breakdown of the code:

* The first few lines import the necessary libraries.
* The `data` variable is assigned the data loaded from the CSV file.
* The `X_train`, `X_test`, `y_train`, and `y_test` variables are assigned the training and testing sets.
* The `model` variable is assigned a linear regression model.
* The `model.fit()` method trains the model on the training data.
* The `y_pred` variable is assigned the predictions on the test data.
* The `mse` and `r2` variables are assigned the mean squared error and R^2 score of the model on the test data.
* The `print()` statements print the MSE and R^2 score.
* The `plt.scatter()` method plots the actual and predicted values.
* The `plt.xlabel()`, `plt.ylabel()`, and `plt.legend()` methods set the labels and legend for the plot.
* The `plt.show()` method displays the plot.
* The `with open()` statement saves the trained model to a pickle file.
* The `with open()` statement loads the trained model from the pickle file.
* The `y_pred` variable is assigned the predictions on the new data.
* The `print()` statement prints the predictions.