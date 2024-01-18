```julia
# Define the main function
function main()
    # Create a vector of random numbers
    x = rand(10)  # Generate a vector of 10 random numbers

    # Apply a mathematical function to each element of the vector
    y = map(x -> sin(x^2), x)  # Apply the sine function to each element of x

    # Calculate the mean of the vector
    mean_y = mean(y)  # Calculate the mean of the vector y

    # Print the results
    println("Vector x:")
    println(x)
    println("Vector y:")
    println(y)
    println("Mean of y:")
    println(mean_y)
end

# Call the main function
main()
```

Explanation:

1. `function main()`: This line defines a function named `main`, which serves as the entry point of our program.

2. `x = rand(10)`: This line generates a vector `x` containing 10 random numbers. The `rand` function is used to generate random numbers, and `10` specifies the number of elements in the vector.

3. `y = map(x -> sin(x^2), x)`: This line applies the `sin` function to each element of the vector `x` using the `map` function. The `x -> sin(x^2)` part defines a function that takes a value `x` and returns the sine of `x` squared. This function is then applied to each element of `x`, resulting in a new vector `y`.

4. `mean_y = mean(y)`: This line calculates the mean (average) of the vector `y` using the `mean` function.

5. `println("Vector x:")`: This line prints the label "Vector x:" to the console.

6. `println(x)`: This line prints the vector `x` to the console.

7. `println("Vector y:")`: This line prints the label "Vector y:" to the console.

8. `println(y)`: This line prints the vector `y` to the console.

9. `println("Mean of y:")`: This line prints the label "Mean of y:" to the console.

10. `println(mean_y)`: This line prints the mean of the vector `y` to the console.

When you run this program, it will generate a vector of 10 random numbers, apply the sine function to each element of the vector, calculate the mean of the resulting vector, and then print these results to the console.