```julia

# Define a function to calculate the area of a triangle
area_triangle(base, height) = 0.5 * base * height

# Define a function to calculate the volume of a sphere
volume_sphere(radius) = (4/3) * pi * radius^3

# Define a function to calculate the derivative of a function
derivative(f, x) = (f(x + h) - f(x)) / h
    where h = 1e-6 # Step size for numerical differentiation

# Define a function to integrate a function
integral(f, a, b) = sum(f(x) * h for x in range(a, b, h))
    where h = (b - a) / 1000 # Number of subintervals

# Define a function to solve a system of linear equations
solve_linear_system(A, b) = A \ b # Using backslash for solving linear systems

# Define a function to find the eigenvalues and eigenvectors of a matrix
eigen(A) = eig(A) # Using the eig function for eigenvalue decomposition

# Define a function to perform singular value decomposition
svd(A) = svd(A) # Using the svd function for singular value decomposition

# Define a function to compute the QR decomposition of a matrix
qr(A) = qr(A) # Using the qr function for QR decomposition

# Define a function to compute the LU decomposition of a matrix
lu(A) = lu(A) # Using the lu function for LU decomposition

# Define a function to compute the Cholesky decomposition of a matrix
cholesky(A) = cholesky(A) # Using the cholesky function for Cholesky decomposition

```