```python
# Import the necessary libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.linear_model import LinearRegression, Lasso, Ridge, ElasticNet
from sklearn.metrics import mean_squared_error, r2_score

# Load the data
data = pd.read_csv('data.csv')

# Preprocess the data
data = data.dropna()
data['Date'] = pd.to_datetime(data['Date'])
data['Month'] = data['Date'].dt.month
data['Year'] = data['Date'].dt.year

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(data.drop('Price', axis=1), data['Price'], test_size=0.2, random_state=42)

# Create and fit the linear regression models
models = {
    'Linear Regression': LinearRegression(),
    'Lasso': Lasso(),
    'Ridge': Ridge(),
    'ElasticNet': ElasticNet()
}

for name, model in models.items():
    model.fit(X_train, y_train)

# Evaluate the models
scores = {}
for name, model in models.items():
    scores[name] = {
        'MSE': mean_squared_error(y_test, model.predict(X_test)),
        'R2': r2_score(y_test, model.predict(X_test))
    }

# Print the results
print('Model Evaluation Results:')
for name, score in scores.items():
    print(f'{name}: MSE = {score["MSE"]}, R2 = {score["R2"]}')

# Plot the feature importances
feature_importances = pd.Series(LinearRegression().fit(X_train, y_train).coef_, index=X_train.columns)
feature_importances.sort_values(inplace=True)
plt.figure(figsize=(12, 6))
sns.barplot(x=feature_importances, y=feature_importances.index)
plt.xlabel('Feature Importance')
plt.ylabel('Feature')
plt.title('Feature Importances')
plt.show()

# Perform cross-validation
cv_scores = {}
for name, model in models.items():
    cv_scores[name] = cross_val_score(model, X_train, y_train, cv=5, scoring='neg_mean_squared_error')

# Print the cross-validation results
print('Cross-Validation Results:')
for name, score in cv_scores.items():
    print(f'{name}: Mean MSE = {-np.mean(score)}')
```

Explanation:

This code is a comprehensive analysis of a dataset using various linear regression models. Here's a detailed explanation:

1. Import Libraries: The code starts by importing essential libraries for data manipulation, visualization, and modeling.

2. Load the Data: The data is loaded from a CSV file using `pd.read_csv()`.

3. Preprocess the Data:
   - Missing values are dropped using `dropna()`.
   - The `Date` column is converted to datetime format using `pd.to_datetime()`.
   - New columns for month and year are created from the `Date` column.

4. Split the Data: The data is split into training and testing sets using `train_test_split()`. This is done to evaluate the performance of the models on unseen data.

5. Create and Fit Models:
   - Four linear regression models are created: Linear Regression, Lasso, Ridge, and ElasticNet.
   - Each model is fitted to the training data using `model.fit()`.

6. Evaluate Models:
   - The models are evaluated on the test set using mean squared error (MSE) and R2 score.
   - The results are stored in a dictionary called `scores`.

7. Print Results: The evaluation results are printed, showing the MSE and R2 score for each model.

8. Plot Feature Importances:
   - The feature importances for the Linear Regression model are calculated and plotted using `sns.barplot()`.
   - This helps identify the most influential features in predicting the target variable.

9. Perform Cross-Validation:
   - Cross-validation is performed for each model using `cross_val_score()`.
   - The cross-validation scores are stored in a dictionary called `cv_scores`.

10. Print Cross-Validation Results: The cross-validation results are printed, showing the mean MSE for each model.

This code provides a thorough analysis of the data using multiple linear regression models. It includes data preprocessing, model training and evaluation, feature importance analysis, and cross-validation. The results help determine the best model for the given dataset.