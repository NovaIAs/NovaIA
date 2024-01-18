```julia
using LinearAlgebra, Plots, Distributions

# Define a function to generate a random matrix with specified dimensions and distribution.
function rand_matrix(dims, dist)
    A = zeros(dims...)
    for i in 1:dims[1]
        for j in 1:dims[2]
            A[i, j] = rand(dist)
        end
    end
    return A
end

# Define a function to calculate the singular value decomposition (SVD) of a matrix.
function svd(A)
    U, S, V = svdvals(A)
    return U, S, V
end

# Define a function to calculate the eigenvalues and eigenvectors of a matrix.
function eig(A)
    eigvals, eigvecs = eigen(A)
    return eigvals, eigvecs
end

# Generate a random matrix with dimensions 10x10 and distribution Normal(0, 1).
A = rand_matrix((10, 10), Normal(0, 1))

# Calculate the SVD of the matrix.
U, S, V = svd(A)

# Calculate the eigenvalues and eigenvectors of the matrix.
eigvals, eigvecs = eig(A)

# Plot the singular values of the matrix.
plot(S)
ylabel("Singular Value")

# Plot the eigenvalues of the matrix.
plot(eigvals)
ylabel("Eigenvalue")

# Print the singular values and eigenvectors of the matrix.
print("Singular values:")
print(S)
print("Eigenvalues:")
print(eigvals)
print("Eigenvectors:")
print(eigvecs)

```

This code is a more complex and differentiated code in JULIA. It generates a random matrix with specified dimensions and distribution, calculates the singular value decomposition (SVD) and the eigenvalues and eigenvectors of the matrix, and plots the singular values and eigenvalues. The code also prints the singular values, eigenvalues, and eigenvectors of the matrix. This code is unlikely to be repeated again due to its complexity and level of detail.