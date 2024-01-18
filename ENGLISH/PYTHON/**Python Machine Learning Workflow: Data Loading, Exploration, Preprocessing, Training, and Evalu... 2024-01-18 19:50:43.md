```python
# Import necessary libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import sklearn
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Load the data
data = pd.read_csv('data.csv')

# Exploratory Data Analysis (EDA)
# Print the first few rows of the data
print(data.head())

# Check for missing values
print(data.isnull().sum())

# Get summary statistics
print(data.describe())

# Visualize the data
sns.pairplot(data)
plt.show()

# Split the data into training and testing sets
X = data.drop('target_variable', axis=1)
y = data['target_variable']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=0)

# Scale the data
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
print('Mean Squared Error:', mse)
print('R2 score:', r2)

# Plot the actual and predicted values
plt.scatter(y_test, y_pred)
plt.xlabel('Actual Values')
plt.ylabel('Predicted Values')
plt.title('Actual vs. Predicted Values')
plt.show()

# Save the model
import pickle
pickle.dump(model, open('model.pkl', 'wb'))

# Load the model
loaded_model = pickle.load(open('model.pkl', 'rb'))

# Make predictions using the saved model
y_pred_loaded = loaded_model.predict(X_test)

# Evaluate the model
mse_loaded = mean_squared_error(y_test, y_pred_loaded)
r2_loaded = r2_score(y_test, y_pred_loaded)
print('Mean Squared Error (Loaded Model):', mse_loaded)
print('R2 score (Loaded Model):', r2_loaded)
```

**Explanation:**

This code demonstrates a comprehensive machine learning workflow using Python, involving data loading, exploration, preprocessing, model training, evaluation, and model saving and loading. It uses the popular libraries NumPy, Pandas, Matplotlib, Seaborn, and Scikit-Learn for data manipulation, visualization, and modeling.

1. **Data Loading and Exploration:**
   - The `data.csv` is loaded using `pd.read_csv`.
   - Basic information about the data, including column names, data types, and missing values, is displayed.
   - Summary statistics and pairwise relationships between variables are explored using Pandas and Seaborn.

2. **Data Preprocessing:**
   - The data is split into training and testing sets using `train_test_split`.
   - Feature scaling is applied to all features using `StandardScaler`.

3. **Model Training:**
   - A linear regression model is trained using `LinearRegression`.

4. **Model Evaluation:**
   - The model is evaluated on the test set using mean squared error (MSE) and R2 score.
   - The actual and predicted values are plotted to visualize the model's performance.

5. **Model Saving and Loading:**
   - The trained model is saved to a file using `pickle.dump`.
   - The saved model is loaded from the file using `pickle.load`.

6. **Evaluation of Saved Model:**
   - Similar to the evaluation of the original model, the loaded model is evaluated on the test set using MSE and R2 score.