```python
import sympy
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import scipy.stats as stats

# Define the input parameters
n = 100  # Number of data points
mu = 0  # Mean of the normal distribution
sigma = 1  # Standard deviation of the normal distribution

# Generate the normally distributed data
x = np.random.normal(mu, sigma, n)

# Plot the histogram of the data
plt.hist(x, bins=20, density=True)
plt.xlabel("Data")
plt.ylabel("Frequency")
plt.title("Histogram of the Normally Distributed Data")
plt.show()

# Calculate the sample mean and sample standard deviation
sample_mean = np.mean(x)
sample_std = np.std(x)

# Print the sample mean and sample standard deviation
print("Sample mean:", sample_mean)
print("Sample standard deviation:", sample_std)

# Perform a t-test to test the hypothesis that the sample mean is equal to the population mean
t_stat, p_value = stats.ttest_1samp(x, mu)

# Print the t-statistic and p-value
print("T-statistic:", t_stat)
print("P-value:", p_value)

# If the p-value is less than 0.05, reject the null hypothesis
if p_value < 0.05:
    print("Reject the null hypothesis")
else:
    print("Fail to reject the null hypothesis")

# Plot the normal distribution curve
x_values = np.linspace(mu - 3 * sigma, mu + 3 * sigma, 100)
y_values = stats.norm.pdf(x_values, mu, sigma)
plt.plot(x_values, y_values)
plt.xlabel("Data")
plt.ylabel("Probability")
plt.title("Normal Distribution Curve")
plt.show()

```

This code generates normally distributed data, plots the histogram of the data, calculates the sample mean and sample standard deviation, performs a t-test to test the hypothesis that the sample mean is equal to the population mean, and plots the normal distribution curve. The code also provides a detailed explanation of each step.