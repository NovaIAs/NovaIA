```python
import random
import numpy as np
import matplotlib.pyplot as plt

# Defining the Cellular Automata class
class CellularAutomata:
    def __init__(self, rule, size, iterations):
        self.rule = rule
        self.size = size
        self.iterations = iterations
        self.grid = np.zeros((size, size), dtype=int)

    def evolve(self):
        for i in range(1, self.size - 1):
            for j in range(1, self.size - 1):
                neighbors = self.get_neighbors(i, j)
                new_state = self.get_new_state(neighbors, self.rule)
                self.grid[i, j] = new_state

    def get_neighbors(self, i, j):
        neighbors = []
        for x in range(i - 1, i + 2):
            for y in range(j - 1, j + 2):
                if x == i and y == j:
                    continue
                neighbors.append(self.grid[x, y])
        return neighbors

    def get_new_state(self, neighbors, rule):
        binary_string = ''.join(map(str, neighbors))
        decimal_value = int(binary_string, 2)
        new_state = rule[decimal_value]
        return new_state

# Creating an instance of the Cellular Automata class
ca = CellularAutomata(rule=30, size=100, iterations=100)

# Evolving the Cellular Automata
for i in range(ca.iterations):
    ca.evolve()

# Plotting the Cellular Automata
plt.imshow(ca.grid, cmap='binary')
plt.show()
```

This code implements a Cellular Automata, which is a system that evolves over time according to a set of rules. In this case, the Cellular Automata is a grid of cells, and each cell can be in one of two states: 0 or 1. The rules determine how the state of each cell changes based on the states of its neighbors.

The `CellularAutomata` class has the following attributes:

* `rule`: The rule that determines how the state of each cell changes.
* `size`: The size of the grid.
* `iterations`: The number of iterations to evolve the Cellular Automata.
* `grid`: A NumPy array representing the grid of cells.

The `evolve()` method evolves the Cellular Automata by applying the rule to each cell in the grid. The `get_neighbors()` method gets the states of the neighbors of a given cell. The `get_new_state()` method uses the rule to determine the new state of a given cell based on the states of its neighbors.

The code then creates an instance of the `CellularAutomata` class, evolves it for a certain number of iterations, and plots the resulting grid of cells.

The resulting plot shows a complex and interesting pattern that emerges from the simple rules of the Cellular Automata.