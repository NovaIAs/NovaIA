```python
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

# Defining the function to be fitted
def func(x, a, b, c, d):
    return a*np.exp(-b*x) + c*np.sin(d*x)

# Creating data points with added noise
x = np.linspace(0, 2*np.pi, 100)
y = func(x, 1.5, 0.3, 2.5, 0.7) + 0.1*np.random.randn(100)

# Fitting the data to the function
popt, pcov = curve_fit(func, x, y)

# Printing the fitted parameters
print("Fitted parameters:")
print("a =", popt[0])
print("b =", popt[1])
print("c =", popt[2])
print("d =", popt[3])

# Creating a plot of the data and fitted curve
plt.scatter(x, y, label="Data")
plt.plot(x, func(x, *popt), label="Fitted curve")
plt.legend()
plt.show()

# Calculating and printing the R-squared value
r2 = np.corrcoef(y, func(x, *popt))[0, 1]**2
print("R-squared value:", r2)

```

Explanation:

1. Import necessary libraries: `numpy`, `matplotlib.pyplot`, and `scipy.optimize`.

2. Define a function `func` to be fitted. In this case, it's an exponential decay function with a sine wave component.

3. Generate data points (`x` and `y`) by evaluating the `func` function at the `x` values and adding some noise.

4. Use the `curve_fit` function from `scipy.optimize` to fit the data to the `func` function. This finds the values of the parameters (`a`, `b`, `c`, and `d`) that minimize the squared error between the data and the fitted curve.

5. Print the fitted parameters.

6. Create a plot of the original data and the fitted curve.

7. Calculate and print the R-squared value, which indicates how well the fitted curve fits the data.

This code demonstrates a more complex and comprehensive curve-fitting process, including data generation, noise addition, model selection, parameter estimation, and evaluation of the fit. It also includes a visualization of the data and the fitted curve, along with the calculation and display of the R-squared value.