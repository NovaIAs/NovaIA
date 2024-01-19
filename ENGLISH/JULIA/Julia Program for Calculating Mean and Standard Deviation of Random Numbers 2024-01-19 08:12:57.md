```julia
# Julia code that generates n random numbers between 0 and 1,
# then calculates their mean and standard deviation

const n = 10000

# Generate n random numbers between 0 and 1
data = rand(n, Float64)

# Calculate the mean of the data
mean_value = mean(data)

# Calculate the standard deviation of the data
standard_deviation = std(data)

# Print the results
println("Mean:", mean_value)
println("Standard Deviation:", standard_deviation)

```
Explanation:

1. `const n = 10000`: Declares a constant named `n` with a value of 10,000. This determines the number of random numbers we will generate.

2. `data = rand(n, Float64)`: Generates `n` random numbers between 0 and 1, using the `rand` function. The `Float64` data type specifies that the numbers should be stored as 64-bit floating-point numbers.

3. `mean_value = mean(data)`: Calculates the mean (average) of the `data` array using the `mean` function.

4. `standard_deviation = std(data)`: Calculates the standard deviation of the `data` array using the `std` function.

5. `println("Mean:", mean_value)` and `println("Standard Deviation:", standard_deviation)`: Prints the calculated mean and standard deviation values to the console.

This code demonstrates a basic example of statistical calculations in Julia, where it generates random numbers, finds their mean, and calculates their standard deviation.