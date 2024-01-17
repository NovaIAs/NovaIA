```python
# This is a complex Python code that performs various operations, including:
# 1. Importing necessary libraries
# 2. Reading data from a CSV file
# 3. Cleaning the data
# 4. Performing data transformations
# 5. Creating a machine learning model
# 6. Training and evaluating the model
# 7. Making predictions using the model

# Import necessary libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Read data from a CSV file
data = pd.read_csv('data.csv')

# Clean the data
data = data.dropna() # Drop rows with missing values

# Perform data transformations
data['age'] = data['age'].astype('int') # Convert age from float to integer
data['gender'] = data['gender'].astype('category') # Convert gender to a categorical variable

# Create a machine learning model
model = LinearRegression() # Create a linear regression model

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(data[['age', 'gender']], data['salary'], test_size=0.2) # Split the data into training and testing sets

# Train the model
model.fit(X_train, y_train) # Train the model

# Evaluate the model
y_pred = model.predict(X_test) # Make predictions using the model
mse = mean_squared_error(y_test, y_pred) # Calculate the mean squared error
r2 = r2_score(y_test, y_pred) # Calculate the R-squared score

# Print the results
print('Mean squared error:', mse)
print('R-squared score:', r2)

# Plot the results
plt.scatter(y_test, y_pred) # Plot the actual vs. predicted values
plt.xlabel('Actual salary') # Set the x-axis label
plt.ylabel('Predicted salary') # Set the y-axis label
plt.title('Actual vs. Predicted Salary') # Set the title of the plot
plt.show() # Display the plot

# Save the model
model.save('salary_prediction_model.pkl') # Save the model to a file
```

This code performs the following steps:

1. Imports necessary libraries.
2. Reads data from a CSV file.
3. Cleans the data by dropping rows with missing values.
4. Performs data transformations by converting age from float to integer and gender to a categorical variable.
5. Creates a machine learning model, which in this case is a linear regression model.
6. Splits the data into training and testing sets.
7. Trains the model using the training data.
8. Evaluates the model by calculating the mean squared error and R-squared score on the testing data.
9. Prints the results of the evaluation.
10. Plots the actual vs. predicted values of the model.
11. Saves the model to a file.