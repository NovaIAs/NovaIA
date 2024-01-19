```
# Import necessary modules
from random import randint
from string import ascii_lowercase, ascii_uppercase, digits
import numpy as np
import matplotlib.pyplot as plt

# Define a function to generate random strings
def generate_random_string(length):
  # Create a list of all possible characters
  characters = ascii_lowercase + ascii_uppercase + digits

  # Create a random string of the specified length
  random_string = ''.join(characters[randint(0, len(characters) - 1)] for i in range(length))

  # Return the random string
  return random_string

# Define a function to generate a random grid
def generate_random_grid(size):
  # Create a grid of the specified size
  grid = np.zeros((size, size))

  # Fill the grid with random numbers
  for i in range(size):
    for j in range(size):
      grid[i, j] = randint(0, 100)

  # Return the grid
  return grid

# Define a function to plot a grid
def plot_grid(grid):
  # Create a figure and axes
  fig, ax = plt.subplots()

  # Plot the grid
  ax.imshow(grid, cmap=plt.cm.hot)

  # Show the figure
  plt.show()

# Generate a random string
random_string = generate_random_string(10)

# Print the random string
print("Random string:", random_string)

# Generate a random grid
random_grid = generate_random_grid(10)

# Plot the random grid
plot_grid(random_grid)
```

This code performs several complex tasks:

1. It imports necessary modules like `random`, `string`, `numpy`, and `matplotlib.pyplot`.
2. It defines a function called `generate_random_string` that generates a random string of a specified length using characters from the ASCII lowercase, uppercase, and digits.
3. It defines a function called `generate_random_grid` that generates a grid of a specified size filled with random numbers between 0 and 100.
4. It defines a function called `plot_grid` that takes a grid as input and plots it using a heatmap colormap.
5. It generates a random string of length 10 and prints it.
6. It generates a random grid of size 10x10 and plots it.

This code demonstrates the use of functions, modules, and different data structures in Python to perform complex tasks like generating random strings and grids and visualizing them. It also showcases the use of loops, conditional statements, and various built-in functions for data manipulation and visualization.