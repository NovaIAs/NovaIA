```python
# Import the necessary libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Load the data
data = pd.read_csv('data.csv')

# Preprocess the data
data['Date'] = pd.to_datetime(data['Date'])
data['Month'] = data['Date'].dt.month_name()
data['Year'] = data['Date'].dt.year

# Group the data by month and year
grouped_data = data.groupby(['Month', 'Year'])

# Calculate the average temperature for each month and year
average_temperature = grouped_data['Temperature'].mean()

# Reshape the data into a DataFrame
average_temperature = average_temperature.reset_index()

# Plot the data
plt.figure(figsize=(12, 6))
sns.lineplot(x='Month', y='Temperature', data=average_temperature)
plt.title('Average Temperature by Month and Year')
plt.xlabel('Month')
plt.ylabel('Temperature (Celsius)')
plt.show()

# Create a regression model to predict the temperature for a given month and year
model = LinearRegression()
x = average_temperature[['Month', 'Year']]
y = average_temperature['Temperature']
model.fit(x, y)

# Make predictions for new data
new_data = pd.DataFrame({
    'Month': ['January', 'February', 'March'],
    'Year': [2023, 2023, 2023]
})
predictions = model.predict(new_data)

# Print the predictions
print('Predicted temperatures for new data:')
print(predictions)
```

This code is a complex data science script that:

1. Loads and preprocesses a dataset containing temperature data.
2. Groups the data by month and year and calculates the average temperature for each month and year.
3. Reshapes the data into a DataFrame and plots the average temperature by month and year.
4. Creates a linear regression model to predict the temperature for a given month and year.
5. Makes predictions for new data and prints the results.

This code demonstrates several advanced data science techniques, including data preprocessing, data visualization, linear regression, and making predictions using a trained model. It is a comprehensive example of how to use Python for data analysis and modeling tasks.