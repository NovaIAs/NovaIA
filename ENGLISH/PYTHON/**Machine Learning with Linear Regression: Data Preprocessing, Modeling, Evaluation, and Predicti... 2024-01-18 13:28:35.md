```python
# Import necessary libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Load the data
data = pd.read_csv('data.csv')
print('Data loaded successfully!')

# Preprocessing the data
# Split the data into features and target
features = data.drop('target', axis=1)
target = data['target']

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(features, target, test_size=0.2, random_state=42)

# Train the model
model = LinearRegression()
model.fit(X_train, y_train)
print('Model trained successfully!')

# Evaluate the model
y_pred = model.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)
print('Model evaluation results:')
print('Mean Squared Error:', mse)
print('R2 score:', r2)

# Save the model
import pickle
pickle.dump(model, open('model.pkl', 'wb'))
print('Model saved successfully!')

# Load the saved model
model = pickle.load(open('model.pkl', 'rb'))
print('Model loaded successfully!')

# Make predictions on new data
new_data = pd.DataFrame({'feature1': [10], 'feature2': [20]})
prediction = model.predict(new_data)
print('Prediction on new data:', prediction)
```

This Python code performs the following tasks:

1. Import necessary libraries: It imports essential libraries like NumPy, Pandas, Matplotlib, and Scikit-Learn for data manipulation, visualization, and machine learning.

2. Load the Data: It loads a CSV file named 'data.csv' containing the dataset into a Pandas DataFrame and informs that the data has been loaded successfully.

3. Preprocessing the Data:
   - It separates the features (input variables) from the target variable (output variable) in the DataFrame.
   - It splits the data into training and testing sets using the 'train_test_split' function for model evaluation.

4. Train the Model: It trains a linear regression model with the training data using the 'LinearRegression' class from Scikit-Learn.

5. Evaluate the Model:
   - It predicts the target variable values for the testing data using the trained model.
   - It calculates the Mean Squared Error (MSE) and R2 score to evaluate the performance of the model.

6. Save the Model: It serializes the trained model using Python's pickle module and saves it to a file named 'model.pkl'.

7. Load the Saved Model: It demonstrates how to load the saved model from the file and inform that the model has been loaded successfully.

8. Make Predictions on New Data: It predicts the target variable value for a new data point using the loaded model and displays the prediction.

This code exemplifies various data manipulation, modeling, evaluation, persistence, and prediction tasks using Python and Scikit-Learn. It showcases more complex code that combines multiple operations and provides detailed explanations of each step.