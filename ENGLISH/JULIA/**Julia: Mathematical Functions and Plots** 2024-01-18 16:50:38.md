```julia
# Define a function to compute the Fibonacci sequence
function fibonacci(n)
    if n < 2
        return [0, 1][n]  # Base case: fib(0) = 0, fib(1) = 1
    else
        # Recursive case: fib(n) = fib(n-1) + fib(n-2)
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

# Get the first 20 Fibonacci numbers
fib_sequence = fibonacci.(1:20)

# Create a bar chart of the Fibonacci sequence
x_axis = 1:20  # X-axis values
y_axis = fib_sequence  # Y-axis values
xlabel = "Position in the Fibonacci Sequence"  # X-axis label
ylabel = "Fibonacci Number"  # Y-axis label
title = "Fibonacci Sequence"  # Title of the chart

using Plots
plot(x_axis, y_axis, title=title, xlabel=xlabel, ylabel=ylabel)

# Clear the current figure
clf()

# Create a line plot of the Fibonacci sequence
plot(x_axis, y_axis, title=title, xlabel=xlabel, ylabel=ylabel, linestyle=:solid)

# Save the plot to a PNG file
savefig("fibonacci_sequence.png")

# Define a function to find the roots of a quadratic equation
function quadratic_roots(a, b, c)
    # Calculate the discriminant
    D = b^2 - 4*a*c

    # Check if the equation has real roots
    if D < 0
        return "No real roots"  # Return an error message
    else
        # Calculate the roots using the quadratic formula
        root1 = (-b + sqrt(D)) / (2*a)
        root2 = (-b - sqrt(D)) / (2*a)

        # Return the roots as a tuple
        return (root1, root2)
    end
end

# Find the roots of the equation 2x^2 + 3x - 5 = 0
roots = quadratic_roots(2, 3, -5)

# Print the roots
println("Roots of the equation 2x^2 + 3x - 5 = 0:")
println(roots)

# Define a function to compute the sum of the first n even numbers
function sum_even_numbers(n)
    # Initialize the sum to 0
    sum = 0

    # Iterate from 1 to n, adding each even number to the sum
    for i in 1:n
        if i % 2 == 0  # Check if i is even
            sum += i  # Add i to the sum
        end
    end

    # Return the sum
    return sum
end

# Compute the sum of the first 10 even numbers
sum_even_10 = sum_even_numbers(10)

# Print the sum
println("Sum of the first 10 even numbers:")
println(sum_even_10)
```

This code is a collection of different functions and tasks that cover a variety of mathematical and programming concepts. It includes:

1. **Fibonacci Sequence:** Calculates the first 20 Fibonacci numbers and creates a bar chart and a line plot of the sequence.
2. **Quadratic Equation Roots:** Finds the roots of a quadratic equation using the quadratic formula and handles the case when there are no real roots.
3. **Sum of Even Numbers:** Computes the sum of the first n even numbers using a loop to add each even number to the sum.