```python
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error, r2_score
import matplotlib.pyplot as plt
%matplotlib inline

# Load the data
data = pd.read_csv('housing_data.csv')

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(data[['lot_area', 'bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot', 'floors']], data['price'], test_size=0.2, random_state=42)

# Scale the data
scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

# Train the model
model = LinearRegression()
model.fit(X_train, y_train)

# Evaluate the model
y_pred = model.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
rmse = np.sqrt(mse)
r2 = r2_score(y_test, y_pred)

# Print the results
print('Mean Squared Error:', mse)
print('Root Mean Squared Error:', rmse)
print('R2 Score:', r2)

# Plot the predicted prices against the actual prices
plt.scatter(y_test, y_pred)
plt.xlabel('Actual Prices')
plt.ylabel('Predicted Prices')
plt.title('Actual Prices vs. Predicted Prices')
plt.show()

# Save the model
import pickle
filename = 'housing_model.sav'
pickle.dump(model, open(filename, 'wb'))

# Load the model
loaded_model = pickle.load(open(filename, 'rb'))

# Use the loaded model to make predictions
y_pred = loaded_model.predict(X_test)

# Evaluate the loaded model
mse = mean_squared_error(y_test, y_pred)
rmse = np.sqrt(mse)
r2 = r2_score(y_test, y_pred)

# Print the results
print('Mean Squared Error:', mse)
print('Root Mean Squared Error:', rmse)
print('R2 Score:', r2)
```

This code demonstrates a complex machine learning workflow for predicting housing prices using linear regression. Here's an explanation:

1. **Data Loading**: It imports the necessary libraries and loads the housing data from a CSV file.

2. **Data Splitting**: The data is split into training and testing sets using `train_test_split`, with a test size of 20%.

3. **Data Scaling**: The data is standardized using `StandardScaler` to ensure the features are on the same scale.

4. **Model Training**: A linear regression model is trained using the training data with `LinearRegression().fit()`.

5. **Model Evaluation**: The trained model's performance is evaluated on the test data, calculating the mean squared error (MSE), root mean squared error (RMSE), and R2 score using `mean_squared_error()`, `np.sqrt(mse)`, and `r2_score()` respectively.

6. **Plotting Actual vs. Predicted Prices**: The actual prices are plotted against the predicted prices using `plt.scatter()`.

7. **Model Saving**: The trained model is saved using pickle, allowing it to be reused later.

8. **Model Loading**: The saved model is loaded using pickle, enabling predictions on new data.

9. **Model Evaluation (Loaded Model)**: The loaded model's performance is evaluated again on the test data to ensure it still performs well.

This code demonstrates a complete data science pipeline, including data loading, preprocessing, model training, evaluation, visualization, model saving, and loaded model evaluation. It illustrates the process of building a machine learning model for a real-world problem.