```python
# Import necessary libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Load the dataset from a CSV file
df = pd.read_csv('data.csv')

# Split the dataset into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(df[['feature1', 'feature2', 'feature3']], df['target'], test_size=0.2, random_state=42)

# Create a Linear Regression model
model = LinearRegression()

# Train the model on the training data
model.fit(X_train, y_train)

# Evaluate the model's performance on the testing data
y_pred = model.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

# Print the model's performance metrics
print('Mean Squared Error:', mse)
print('R2 Score:', r2)

# Plot the actual and predicted values
plt.scatter(y_test, y_pred)
plt.xlabel('Actual Values')
plt.ylabel('Predicted Values')
plt.title('Actual vs Predicted Values')
plt.show()

# Save the trained model
import pickle
filename = 'model.pkl'
pickle.dump(model, open(filename, 'wb'))

# Load the saved model
loaded_model = pickle.load(open(filename, 'rb'))

# Make predictions using the loaded model
new_data = np.array([[1, 2, 3]])
y_new_pred = loaded_model.predict(new_data)
print('Predicted value for the new data:', y_new_pred)
```

This code demonstrates the following operations:

1. Importing necessary libraries.
2. Loading the dataset from a CSV file.
3. Splitting the dataset into training and testing sets.
4. Creating a Linear Regression model.
5. Training the model on the training data.
6. Evaluating the model's performance on the testing data.
7. Printing the model's performance metrics.
8. Plotting the actual and predicted values.
9. Saving the trained model to a file.
10. Loading the saved model from a file.
11. Making predictions using the loaded model.

This code demonstrates a complete workflow for training, evaluating, and saving a machine learning model using Python.