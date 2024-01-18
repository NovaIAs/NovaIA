```julia
# Main function
function multi_diff(f, x, n)
    # Initialize the output array
    diffs = zeros(n+1, typeof(f(x)))

    # Compute the first derivative
    diffs[1] = grad(f, x)

    # Compute the higher order derivatives
    for i in 2:n+1
        diffs[i] = grad(diffs[i-1], x)
    end

    # Return the derivatives as a vector
    return diffs
end

# Define the function to differentiate
f(x) = sin(x^2) - cos(x^3)

# Compute the derivatives up to order 10
diffs = multi_diff(f, 1.0, 10)

# Print the results
println("Derivatives of f(x) = sin(x^2) - cos(x^3) at x = 1.0:")
for i in 1:length(diffs)
    println("Order $i: $diffs[i]")
end

```

Explanation:

1.  `multi_diff(f, x, n)`: This function takes a function `f`, a point `x`, and an integer `n` as inputs. It computes the derivatives of `f` up to order `n` at `x`.

2.  `zeros(n+1, typeof(f(x)))`: This line initializes an array `diffs` of size `n+1` to store the derivatives. The data type of the array is set to match the type of the first derivative `f(x)`.

3.  `diffs[1] = grad(f, x)`: This line computes the first derivative of `f` at `x` using the `grad` function. The result is stored in the first element of the `diffs` array.

4.  `for i in 2:n+1`: This loop computes the higher order derivatives of `f` up to order `n`.

5.  `diffs[i] = grad(diffs[i-1], x)`: This line computes the `i`-th derivative of `f` at `x` by taking the derivative of the `(i-1)`-th derivative.

6.  `return diffs`: This line returns the array `diffs` containing the derivatives of `f` up to order `n` at `x`.

7.  `f(x) = sin(x^2) - cos(x^3)`: This line defines the function `f(x)` whose derivatives we want to compute.

8.  `diffs = multi_diff(f, 1.0, 10)`: This line calls the `multi_diff` function to compute the derivatives of `f` up to order 10 at `x = 1.0`.

9.  `println("Derivatives of f(x) = sin(x^2) - cos(x^3) at x = 1.0:")`: This line prints a header for the output.

10.  `for i in 1:length(diffs)`: This loop iterates over the elements of the `diffs` array to print the derivatives.

11.  `println("Order $i: $diffs[i]")`: This line prints the order of the derivative and its value.