```python
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# 1. Data Loading and Preprocessing
data = pd.read_csv('data.csv')
data['Date'] = pd.to_datetime(data['Date'])
data = data.dropna()

# 2. Feature Selection
features = ['Age', 'Gender', 'Education', 'Income', 'Location']

# 3. Data Standardization
data[features] = data[features].apply(lambda x: (x - x.mean()) / x.std())

# 4. Data Split into Training and Test Sets
X_train, X_test, y_train, y_test = train_test_split(data[features], data['Target'], test_size=0.2, random_state=42)

# 5. Linear Regression Model Training
model = LinearRegression()
model.fit(X_train, y_train)

# 6. Model Evaluation
y_pred = model.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)
print('MSE: {:.2f}, R2: {:.2f}'.format(mse, r2))

# 7. Feature Importance Analysis:
coefs = pd.DataFrame(model.coef_, features)
coefs.index = ['Coefficient']
coefs = coefs.sort_values(by='Coefficient', ascending=False)
print(coefs)

# 8. Create a Function to Deploy the Model
def predict_target(age, gender, education, income, location):
    data = np.array([age, gender, education, income, location])
    data = (data - data.mean()) / data.std()
    return model.predict([data])[0]

# 9. Visualization of the Predictions:
plt.figure(figsize=(10, 6))
plt.scatter(y_test, y_pred)
plt.plot(y_test, y_test, color='red')
plt.xlabel('Actual Target')
plt.ylabel('Predicted Target')
plt.show()

# 10. Save the Trained Model
import pickle
filename = 'saved_model.sav'
pickle.dump(model, open(filename, 'wb'))
```

Explanation:

1. Data Loading and Preprocessing:
   - Load the data from the 'data.csv' file.
   - Convert the 'Date' column to a datetime format.
   - Drop any rows with missing values.

2. Feature Selection:
   - Select the relevant features ('Age', 'Gender', 'Education', 'Income', 'Location') for modeling.

3. Data Standardization:
   - Apply standardization to the features to have a mean of 0 and a standard deviation of 1.

4. Data Split into Training and Test Sets:
   - Split the data into training and test sets (80% training, 20% testing) using 'train_test_split'.

5. Linear Regression Model Training:
   - Create a 'LinearRegression' model and train it on the training data.

6. Model Evaluation:
   - Make predictions on the test data and calculate the Mean Squared Error (MSE) and R2 score.

7. Feature Importance Analysis:
   - Compute the coefficients of the features and display them in descending order of importance.

8. Create a Function to Deploy the Model:
   - Define a Python function 'predict_target' that takes a set of input features and returns the predicted target variable.

9. Visualization of the Predictions:
   - Plot the actual target values against the predicted target values to assess the model's performance graphically.

10. Save the Trained Model:
    - Serialize the trained model using Pickle and save it to a file for future deployment.