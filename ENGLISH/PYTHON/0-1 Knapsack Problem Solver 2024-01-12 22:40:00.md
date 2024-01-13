```python
def compute_optimal_solution(n: int, w: int, items: list[tuple[int, int]]) -> tuple[int, list[int]]:
    """Computes the optimal solution for the 0/1 knapsack problem.

    Args:
        n (int): The number of items in the knapsack.
        w (int): The maximum weight the knapsack can hold.
        items (list[tuple[int, int]]): A list of tuples representing the items, where each tuple contains the weight and value of an item.

    Returns:
        tuple[int, list[int]]: A tuple containing the maximum value that can be obtained and the list of items that achieve this value.
    """

    # Create a 2D array to store the optimal solution for each item and weight.
    dp = [[0 for _ in range(w + 1)] for _ in range(n + 1)]

    # Iterate over the items.
    for i in range(1, n + 1):
        weight, value = items[i - 1]

        # Iterate over the weights.
        for j in range(1, w + 1):
            # If the weight of the item is less than or equal to the current weight,
            # then we can consider adding the item to the knapsack.
            if weight <= j:
                # The optimal solution for the current weight is the maximum of the
                # optimal solution without the item and the optimal solution with the item.
                dp[i][j] = max(dp[i - 1][j], dp[i - 1][j - weight] + value)
            # Otherwise, we cannot add the item to the knapsack.
            else:
                # The optimal solution for the current weight is the optimal solution
                # without the item.
                dp[i][j] = dp[i - 1][j]

    # Get the maximum value that can be obtained.
    max_value = dp[n][w]

    # Get the list of items that achieve this value.
    items_taken = []
    i = n
    j = w
    while i > 0 and j > 0:
        if dp[i][j] != dp[i - 1][j]:
            # If the optimal solution for the current weight is not the same as
            # the optimal solution without the item, then we know that the item was
            # added to the knapsack.
            items_taken.append(i - 1)
            j -= items[i - 1][0]
        i -= 1

    # Return the maximum value and the list of items that achieve this value.
    return max_value, items_taken


# Get the number of items, the maximum weight, and the list of items.
n, w = map(int, input().split())
items = []
for _ in range(n):
    weight, value = map(int, input().split())
    items.append((weight, value))

# Compute the optimal solution.
max_value, items_taken = compute_optimal_solution(n, w, items)

# Print the maximum value and the list of items that achieve this value.
print(max_value)
print(*items_taken)
```

This code solves the 0/1 knapsack problem, which is a classic optimization problem in computer science. The problem is as follows:

Given a set of items, each with a weight and a value, and a maximum weight capacity, find the subset of items that has the maximum total value and whose total weight does not exceed the capacity.

The code uses dynamic programming to solve the problem. Dynamic programming is a technique that solves a problem by breaking it down into a collection of overlapping subproblems, solving each of those subproblems just once, and storing their solutions. The next time the same subproblem occurs, instead of recomputing its solution, one simply looks up the previously computed solution, thereby saving computation time.

In this case, the subproblems are the optimal solutions for the knapsack problem with different subsets of items and different maximum weights. The code stores these solutions in a 2D array, where the rows represent the items and the columns represent the weights.

The code then iterates over the items and the weights, and for each item and weight, it computes the optimal solution for the knapsack problem with that item and that weight. The optimal solution is the maximum of the optimal solution without the item and the optimal solution with the item, if the weight of the item is less than or equal to the current weight.

Once the code has iterated over all the items and weights, it can find the maximum value that can be obtained and the list of items that achieve this value by looking at the last row and column of the 2D array.

The code then prints the maximum value and the list of items that achieve this value.