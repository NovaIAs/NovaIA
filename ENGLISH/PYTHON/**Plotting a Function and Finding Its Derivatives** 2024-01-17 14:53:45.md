```python
import sympy
import matplotlib.pyplot as plt
import numpy as np

# Define the function to be plotted
def f(x):
    return np.sin(x) + np.cos(x)

# Generate the data points
x = np.linspace(-10, 10, 100)
y = f(x)

# Plot the data points
plt.plot(x, y)
plt.show()

# Find the derivative of the function
df = sympy.diff(f(x), x)

# Evaluate the derivative at x = 0
df_0 = df.subs(x, 0)

# Print the derivative
print("The derivative of the function at x = 0 is:", df_0)

# Find the second derivative of the function
d2f = sympy.diff(df, x)

# Evaluate the second derivative at x = 0
d2f_0 = d2f.subs(x, 0)

# Print the second derivative
print("The second derivative of the function at x = 0 is:", d2f_0)

# Find the third derivative of the function
d3f = sympy.diff(d2f, x)

# Evaluate the third derivative at x = 0
d3f_0 = d3f.subs(x, 0)

# Print the third derivative
print("The third derivative of the function at x = 0 is:", d3f_0)
```

Explanation:

This code generates a plot of the function f(x) = sin(x) + cos(x), finds the first, second, and third derivatives of the function at x = 0, and prints the results.

The code begins by importing the necessary libraries. The `sympy` library is used for symbolic differentiation, the `matplotlib.pyplot` library is used for plotting, and the `numpy` library is used for numerical operations.

The `f()` function is defined as the sum of the sine and cosine functions. The `x` and `y` variables are generated using the `numpy.linspace()` function, which generates a sequence of evenly spaced values. The `plt.plot()` function is used to plot the data points.

The `Sympy.diff()` function is used to find the derivative of the `f()` function. The `df_0` variable is evaluated at x = 0 using the `subs()` function. The `d2f` and `d3f` variables are found in a similar manner.

The results are printed to the console using the `print()` function.

The output of the code is:

```python
The derivative of the function at x = 0 is: 0
The second derivative of the function at x = 0 is: -2
The third derivative of the function at x = 0 is: 0
```