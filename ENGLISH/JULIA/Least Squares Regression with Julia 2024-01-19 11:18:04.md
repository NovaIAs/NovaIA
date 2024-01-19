```julia
using LinearAlgebra, Calculus, Plots, Statistics

# Define a function to generate a random matrix
function rand_matrix(m, n)
    rand(Float64, m, n)
end

# Define a function to compute the singular value decomposition of a matrix
function svd(A)
    eigvals, eigvecs = eigen(A' * A)
    singular_values = sqrt.(eigvals)
    U = A * eigvecs
    V = eigvecs'
    return U, singular_values, V
end

# Define a function to compute the pseudoinverse of a matrix
function pinv(A)
    U, singular_values, V = svd(A)
    singular_values_inv = 1 ./ singular_values
    pinv_A = V * Diagonal(singular_values_inv) * U'
    return pinv_A
end

# Define a function to compute the least squares solution of a system of linear equations
function least_squares(A, b)
    x = pinv(A) * b
    return x
end

# Generate a random matrix A
A = rand_matrix(100, 50)

# Generate a random vector b
b = rand(Float64, 100)

# Compute the least squares solution of the system of linear equations A * x = b
x = least_squares(A, b)

# Print the solution
println("Solution:")
println(x)

```

This code performs a least squares regression on a randomly generated matrix A and vector b. It first generates a random matrix A of size 100x50 and a random vector b of size 100. It then computes the pseudoinverse of A using the singular value decomposition (SVD) and uses it to compute the least squares solution x of the system of linear equations A * x = b. Finally, it prints the solution.