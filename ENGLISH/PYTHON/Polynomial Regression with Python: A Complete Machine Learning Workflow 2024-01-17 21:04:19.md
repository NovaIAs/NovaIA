```python
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import sklearn
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures
from sklearn.metrics import mean_squared_error, r2_score

# Load the data
data = pd.read_csv('data.csv')

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(data[['x']], data['y'], test_size=0.2, random_state=42)

# Create a polynomial regression model
model = PolynomialFeatures(degree=3)
X_train_poly = model.fit_transform(X_train)
X_test_poly = model.transform(X_test)

# Train the model
model = LinearRegression()
model.fit(X_train_poly, y_train)

# Make predictions
y_pred = model.predict(X_test_poly)

# Evaluate the model
rmse = np.sqrt(mean_squared_error(y_test, y_pred))
r2 = r2_score(y_test, y_pred)
print('RMSE:', rmse)
print('R2:', r2)

# Plot the results
plt.scatter(X_test, y_test, color='blue', label='Actual')
plt.plot(X_test, y_pred, color='red', label='Predicted')
plt.xlabel('x')
plt.ylabel('y')
plt.legend()
plt.show()
```

This code demonstrates a complete machine learning workflow for predicting a continuous target variable using polynomial regression. It includes data loading, splitting, preprocessing, model training, evaluation, and visualization.

Here's a breakdown of the code:

1. **Data Loading**:
   - `pd.read_csv('data.csv')` reads the CSV file containing the data.

2. **Data Splitting**:
   - `train_test_split` splits the data into training and testing sets.

3. **Feature Transformation**:
   - Polynomial features are created using `PolynomialFeatures`.

4. **Model Training**:
   - A linear regression model is trained on the transformed training data.

5. **Predictions**:
   - Predictions are made on the transformed testing data.

6. **Model Evaluation**:
   - Root mean squared error (RMSE) and R-squared (R2) are calculated to evaluate the model's performance.

7. **Visualization**:
   - A scatter plot is created to visualize the actual and predicted values.

This code demonstrates a more complex and differentiated machine learning workflow compared to simple linear regression, showcasing the use of polynomial features and comprehensive evaluation metrics.