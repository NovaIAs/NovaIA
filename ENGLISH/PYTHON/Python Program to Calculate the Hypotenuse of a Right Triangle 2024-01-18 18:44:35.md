```python
# Program to calculate the length of the hypotenuse of a right triangle using the Pythagorean theorem.

# Define the function to calculate the length of the hypotenuse.
def calculate_hypotenuse(a, b):
    """
    Calculates the length of the hypotenuse of a right triangle given the lengths of the other two sides.

    Args:
        a (float): The length of one of the legs of the triangle.
        b (float): The length of the other leg of the triangle.

    Returns:
        float: The length of the hypotenuse.
    """

    # Calculate the square of the length of the hypotenuse.
    hypotenuse_squared = a**2 + b**2

    # Calculate the length of the hypotenuse.
    hypotenuse = hypotenuse_squared ** 0.5

    return hypotenuse


# Get the lengths of the legs of the triangle from the user.
leg_a = float(input("Enter the length of the first leg of the triangle: "))
leg_b = float(input("Enter the length of the second leg of the triangle: "))

# Calculate the length of the hypotenuse.
hypotenuse = calculate_hypotenuse(leg_a, leg_b)

# Print the length of the hypotenuse.
print("The length of the hypotenuse is:", hypotenuse)
```

Explanation:

* The program defines a function called `calculate_hypotenuse()` that takes two arguments, `a` and `b`, which are the lengths of the two legs of the right triangle.
* Inside the function, the square of the length of the hypotenuse is calculated using the formula `hypotenuse_squared = a**2 + b**2`.
* The length of the hypotenuse is then calculated by taking the square root of `hypotenuse_squared`.
* The function returns the length of the hypotenuse.
* The program then gets the lengths of the legs of the triangle from the user using the `input()` function.
* The program calls the `calculate_hypotenuse()` function to calculate the length of the hypotenuse.
* Finally, the program prints the length of the hypotenuse to the console using the `print()` function.