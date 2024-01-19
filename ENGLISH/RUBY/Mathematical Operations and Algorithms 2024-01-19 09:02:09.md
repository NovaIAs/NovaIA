```ruby
# Define a method to find the greatest common divisor (GCD) of two numbers
def gcd(a, b)
  if b == 0
    return a
  else
    return gcd(b, a % b)
  end
end

# Define a method to find the least common multiple (LCM) of two numbers
def lcm(a, b)
  return (a * b) / gcd(a, b)
end

# Define a method to find all the factors of a number
def factors(n)
  factors = []
  for i in 1..n
    if n % i == 0
      factors << i
    end
  end
  return factors
end

# Define a method to find all the prime factors of a number
def prime_factors(n)
  prime_factors = []
  for i in 2..n
    if n % i == 0 and is_prime(i)
      prime_factors << i
    end
  end
  return prime_factors
end

# Define a method to check if a number is prime
def is_prime(n)
  for i in 2..Math.sqrt(n)
    if n % i == 0
      return false
    end
  end
  return true
end

# Define a method to find the nth Fibonacci number
def fibonacci(n)
  if n == 0 or n == 1
    return n
  else
    return fibonacci(n - 1) + fibonacci(n - 2)
  end
end

# Define a method to find the nth Catalan number
def catalan(n)
  if n == 0
    return 1
  else
    catalan_numbers = [1]
    for i in 1..n
      catalan_number = 0
      for j in 0..i - 1
        catalan_number += catalan_numbers[j] * catalan_numbers[i - j - 1]
      end
      catalan_numbers << catalan_number
    end
    return catalan_numbers[n]
  end
end

# Define a method to find the nth Stirling number of the second kind
def stirling2(n, k)
  if k == 0 or k == n
    return 1
  else
    stirling_numbers = [[1]]
    for i in 1..n
      stirling_row = [0]
      for j in 1..i
        stirling_number = 0
        for l in 0..j - 1
          stirling_number += stirling_numbers[i - 1][l] * (j - l) ** k
        end
        stirling_row << stirling_number
      end
      stirling_numbers << stirling_row
    end
    return stirling_numbers[n][k]
  end
end

# Define a method to find the determinant of a matrix
def determinant(matrix)
  # Check if the matrix is square
  if matrix.length != matrix[0].length
    raise "Matrix is not square"
  end

  # Check if the matrix is 1x1
  if matrix.length == 1
    return matrix[0][0]
  end

  # Use the Laplace expansion to find the determinant
  determinant = 0
  for i in 0..matrix.length - 1
    submatrix = []
    for j in 1..matrix.length - 1
      submatrix_row = []
      for k in 0..matrix.length - 1
        if k != i
          submatrix_row << matrix[j][k]
        end
      end
      submatrix << submatrix_row
    end
    determinant += matrix[0][i] * (-1) ** i * determinant(submatrix)
  end

  return determinant
end

# Define a method to find the inverse of a matrix
def inverse(matrix)
  # Check if the matrix is square
  if matrix.length != matrix[0].length
    raise "Matrix is not square"
  end

  # Check if the matrix is invertible
  if determinant(matrix) == 0
    raise "Matrix is not invertible"
  end

  # Use the adjoint matrix to find the inverse
  adjoint_matrix = []
  for i in 0..matrix.length - 1
    adjoint_row = []
    for j in 0..matrix.length - 1
      submatrix = []
      for k in 0..matrix.length - 1
        if k != i and j != j
          submatrix_row = []
          for l in 0..matrix.length - 1
            if l != j
              submatrix_row << matrix[k][l]
            end
          end
          submatrix << submatrix_row
        end
      end
      adjoint_row << (-1) ** (i + j) * determinant(submatrix)
    end
    adjoint_matrix << adjoint_row
  end

  # Transpose the adjoint matrix to find the inverse
  inverse_matrix = []
  for i in 0..matrix.length - 1
    inverse_row = []
    for j in 0..matrix.length - 1
      inverse_row << adjoint_matrix[j][i]
    end
    inverse_matrix << inverse_row
  end

  # Divide the inverse matrix by the determinant of the original matrix
  for i in