```julia
# Define a generic function for finding the roots of a polynomial
function find_roots(coefficients::Vector{Float64})
    # Check if the input is valid
    if length(coefficients) < 2
        throw(ArgumentError("The polynomial must have at least two coefficients."))
    end

    # Initialize the roots vector
    roots = Vector{Float64}()

    # Use the companion matrix method to find the roots
    companion_matrix = zeros(Float64, length(coefficients) - 1, length(coefficients) - 1)
    for i in 1:length(companion_matrix)
        for j in 1:length(companion_matrix)
            if i == j
                companion_matrix[i, j] = -coefficients[i + 1] / coefficients[1]
            elseif i == j + 1
                companion_matrix[i, j] = 1
            end
        end
    end

    # Find the eigenvalues of the companion matrix
    eigenvalues = eig(companion_matrix)

    # Add the eigenvalues to the roots vector
    for eigenvalue in eigenvalues
        roots = push!(roots, eigenvalue)
    end

    # Return the roots vector
    return roots
end

# Define a function to test the find_roots function
function test_find_roots()
    # Define a polynomial with roots at 1, 2, and 3
    coefficients = [1, -6, 11, -6]

    # Find the roots of the polynomial
    roots = find_roots(coefficients)

    # Print the roots
    println("Roots of the polynomial:")
    for root in roots
        println(root)
    end
end

# Call the test_find_roots function
test_find_roots()
```

Explanation:

* The `find_roots` function is a generic function that can be used to find the roots of a polynomial with any number of coefficients.
* The function first checks if the input is valid. It must have at least two coefficients.
* The function then initializes the roots vector, which will store the roots of the polynomial.
* The function uses the companion matrix method to find the roots of the polynomial. The companion matrix is a matrix whose eigenvalues are the roots of the polynomial.
* The function finds the eigenvalues of the companion matrix using the `eig` function.
* The function adds the eigenvalues to the roots vector.
* The function returns the roots vector.
* The `test_find_roots` function is a helper function that tests the `find_roots` function.
* The `test_find_roots` function defines a polynomial with roots at 1, 2, and 3.
* The `test_find_roots` function calls the `find_roots` function to find the roots of the polynomial.
* The `test_find_roots` function prints the roots of the polynomial.
* The `test_find_roots` function is called at the end of the script to test the `find_roots` function.