```julia
# Let's define a function that takes in a list of numbers and returns the sum of every other element in the list.

function sum_every_other(list)
    # Initialize a variable to store the sum.
    sum = 0

    # Iterate over the list using a for loop.
    for i in 1:length(list)
        # If the current index is odd, we want to add the corresponding element to the sum.
        if i % 2 == 1
            sum += list[i]
        end
    end

    # Return the sum.
    return sum
end

# Now let's define a function that takes in a matrix and returns a new matrix where each element is the sum of the corresponding elements in the input matrix and the matrix obtained by rotating the input matrix by 90 degrees clockwise.

function sum_matrix_and_rotated(matrix)
    # Get the dimensions of the input matrix.
    (m, n) = size(matrix)

    # Initialize the output matrix with zeros.
    output_matrix = zeros(m, n)

    # Iterate over each element in the input matrix.
    for i in 1:m
        for j in 1:n
            # Get the corresponding element in the rotated matrix.
            rotated_element = matrix[n-j+1, i]

            # Add the two elements together and store the result in the output matrix.
            output_matrix[i, j] = matrix[i, j] + rotated_element
        end
    end

    # Return the output matrix.
    return output_matrix
end

# Finally, let's define a function that takes in a string and returns a new string where each character is replaced by its ASCII code.

function ascii_code_string(string)
    # Initialize a variable to store the output string.
    output_string = ""

    # Iterate over each character in the input string.
    for char in string
        # Get the ASCII code of the current character.
        ascii_code = codepoint(char)

        # Convert the ASCII code to a string and append it to the output string.
        output_string = output_string * string(ascii_code)
    end

    # Return the output string.
    return output_string
end

# Let's test our functions with some examples.

# Example 1: Sum of every other element in a list.
list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
sum = sum_every_other(list)
println("Sum of every other element in the list:", sum)

# Example 2: Sum of matrix and rotated matrix.
matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]
output_matrix = sum_matrix_and_rotated(matrix)
println("Sum of matrix and rotated matrix:")
println(output_matrix)

# Example 3: ASCII code string.
string = "Hello, world!"
ascii_string = ascii_code_string(string)
println("ASCII code string:", ascii_string)
```

Explanation:

1. **Sum of Every Other Element in a List:**
   - The `sum_every_other` function takes a list of numbers as input and returns the sum of every other element in the list.
   - It iterates over the list and checks if the current index is odd. If it is, it adds the corresponding element to the sum.
   - Finally, it returns the sum.

2. **Sum of Matrix and Rotated Matrix:**
   - The `sum_matrix_and_rotated` function takes a matrix as input and returns a new matrix where each element is the sum of the corresponding elements in the input matrix and the matrix obtained by rotating the input matrix by 90 degrees clockwise.
   - It first gets the dimensions of the input matrix and initializes the output matrix with zeros.
   - It then iterates over each element in the input matrix, gets the corresponding element in the rotated matrix, and adds the two elements together.
   - The result is stored in the output matrix, which is finally returned.

3. **ASCII Code String:**
   - The `ascii_code_string` function takes a string as input and returns a new string where each character is replaced by its ASCII code.
   - It iterates over each character in the input string, gets its ASCII code, converts it to a string, and appends it to the output string.
   - Finally, it returns the output string.