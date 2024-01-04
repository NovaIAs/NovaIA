```python
# Import required modules
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression

# Load the data
data = pd.read_csv('data.csv')

# Preprocess the data
data = data.dropna()  # Drop missing values
data['Age'] = data['Age'].astype('int64')  # Convert 'Age' to integer
data['Gender'] = data['Gender'].astype('category')  # Convert 'Gender' to category

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(data.drop('Salary', axis=1), data['Salary'], test_size=0.2, random_state=0)

# Train the linear regression model
model = LinearRegression()
model.fit(X_train, y_train)

# Evaluate the model
score = model.score(X_test, y_test)
print('R^2:', score)

# Make predictions
y_pred = model.predict(X_test)

# Plot the actual and predicted values
plt.scatter(y_test, y_pred)
plt.xlabel('Actual Salary')
plt.ylabel('Predicted Salary')
plt.title('Actual vs. Predicted Salary')
plt.show()

# Calculate the mean absolute error
mae = np.mean(np.abs(y_test - y_pred))
print('Mean Absolute Error:', mae)

# Calculate the root mean squared error
rmse = np.sqrt(np.mean((y_test - y_pred) ** 2))
print('Root Mean Squared Error:', rmse)
```

Explanation:

This code demonstrates a comprehensive data analysis workflow using Python's powerful data science libraries: NumPy, Pandas, Matplotlib, and Scikit-Learn. It highlights various data preprocessing, model training, evaluation, and visualization techniques.

1. Data Loading and Preprocessing:
   - We load the data from a CSV file using Pandas.
   - Missing values are dropped.
   - 'Age' is converted to an integer data type.
   - 'Gender' is converted to a category data type.

2. Train-Test Split:
   - The data is split into training and testing sets using Scikit-Learn's `train_test_split` function. This is essential for model evaluation.

3. Linear Regression Model Training:
   - A linear regression model is trained using Scikit-Learn's `LinearRegression` class. The model learns the relationship between the independent variables (features) and the dependent variable (salary).

4. Model Evaluation:
   - The model's performance is evaluated using the R^2 score, which measures the proportion of variance explained by the model.

5. Predictions and Visualization:
   - Predictions are made on the test data using the trained model.
   - The actual and predicted values are plotted to visually assess the model's performance.

6. Error Calculation:
   - The mean absolute error (MAE) and root mean squared error (RMSE) are calculated to quantify the model's accuracy.

This code showcases advanced data analysis techniques and provides a framework for building and evaluating machine learning models in Python.