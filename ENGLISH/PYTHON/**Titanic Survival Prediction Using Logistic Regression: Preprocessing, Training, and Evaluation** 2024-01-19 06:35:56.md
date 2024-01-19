```python
# Import the necessary libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Load the data
data = pd.read_csv('data.csv')

# Preprocess the data
data = data.dropna()
data['Age'] = data['Age'].astype(int)
data['Sex'] = data['Sex'].astype('category')

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(data.drop('Survived', axis=1), data['Survived'], test_size=0.2, random_state=42)

# Train the model
model = LogisticRegression()
model.fit(X_train, y_train)

# Evaluate the model
score = model.score(X_test, y_test)
print('The accuracy of the model is:', score)

# Make predictions on the test set
y_pred = model.predict(X_test)

# Create a confusion matrix
confusion_matrix = pd.crosstab(y_test, y_pred, rownames=['Actual'], colnames=['Predicted'])
print(confusion_matrix)

# Plot the confusion matrix
plt.figure(figsize=(10, 10))
sns.heatmap(confusion_matrix, annot=True, fmt='d')
plt.title('Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.show()

# Plot the ROC curve
fpr, tpr, thresholds = roc_curve(y_test, y_pred)
plt.figure(figsize=(10, 10))
plt.plot(fpr, tpr, label='ROC curve')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.legend()
plt.show()

# Plot the AUC score
auc = roc_auc_score(y_test, y_pred)
print('The AUC score of the model is:', auc)
```

This code is a comprehensive example of data preprocessing, model training, and evaluation for a binary classification problem using the Logistic Regression model. It includes data loading, preprocessing, splitting, training, evaluation, prediction, confusion matrix visualization, ROC curve plotting, and AUC score calculation. The comments are in English and provide a clear explanation of each step, making the code easy to understand and modify for other datasets and models.