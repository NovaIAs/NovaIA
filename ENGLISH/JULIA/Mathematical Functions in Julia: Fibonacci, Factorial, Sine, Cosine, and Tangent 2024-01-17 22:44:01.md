```julia
# Define a Julia function to calculate the Fibonacci sequence
function fib(n)
    # Define the base cases
    if n == 0
        return 0
    elseif n == 1
        return 1
    end

    # Calculate the nth Fibonacci number by adding the previous two numbers
    return fib(n-1) + fib(n-2)
end

# Define a Julia function to calculate the factorial of a number
function factorial(n)
    # Define the base case
    if n == 0
        return 1
    end

    # Calculate the factorial of n by multiplying n by the factorial of n-1
    return n * factorial(n-1)
end

# Define a Julia function to calculate the sine of an angle in degrees
function sin(angle)
    # Convert the angle from degrees to radians
    radians = angle * pi / 180

    # Calculate the sine of the angle using the Taylor series expansion
    sin_value = radians
    term = radians
    n = 3
    while abs(term) > 1e-6
        sin_value += term
        term *= -(radians^n) / factorial(n)
        n += 2
    end

    return sin_value
end

# Define a Julia function to calculate the cosine of an angle in degrees
function cos(angle)
    # Convert the angle from degrees to radians
    radians = angle * pi / 180

    # Calculate the cosine of the angle using the Taylor series expansion
    cos_value = 1
    term = 1
    n = 2
    while abs(term) > 1e-6
        cos_value += term
        term *= -(radians^n) / factorial(n)
        n += 2
    end

    return cos_value
end

# Define a Julia function to calculate the tangent of an angle in degrees
function tan(angle)
    # Convert the angle from degrees to radians
    radians = angle * pi / 180

    # Calculate the tangent of the angle using the sine and cosine functions
    tan_value = sin(radians) / cos(radians)

    return tan_value
end

# Test the functions
println("Fibonacci of 10: ", fib(10))
println("Factorial of 5: ", factorial(5))
println("Sine of 30 degrees: ", sin(30))
println("Cosine of 45 degrees: ", cos(45))
println("Tangent of 60 degrees: ", tan(60))
```

Explanation:

1. Fibonacci Function (fib):
   - `fib` calculates the nth Fibonacci number using a recursive approach.
   - It has two base cases: fib(0) = 0 and fib(1) = 1.
   - For other values of n, it calculates the sum of the previous two Fibonacci numbers, fib(n-1) and fib(n-2).

2. Factorial Function (factorial):
   - `factorial` calculates the factorial of a number using a recursive approach.
   - It has a base case: factorial(0) = 1.
   - For other values of n, it calculates the product of n and the factorial of n-1.

3. Sine Function (sin):
   - `sin` calculates the sine of an angle in degrees using the Taylor series expansion.
   - It starts with an initial estimate of sin(angle) = angle.
   - Then, it iteratively adds terms to the estimate until the absolute value of the next term is less than 1e-6.

4. Cosine Function (cos):
   - `cos` calculates the cosine of an angle in degrees using the Taylor series expansion.
   - It starts with an initial estimate of cos(angle) = 1.
   - Then, it iteratively adds terms to the estimate until the absolute value of the next term is less than 1e-6.

5. Tangent Function (tan):
   - `tan` calculates the tangent of an angle in degrees using the sine and cosine functions.
   - It divides the sine of the angle by the cosine of the angle.

6. Testing the Functions:
   - The code prints the results of calling the `fib`, `factorial`, `sin`, `cos`, and `tan` functions with different inputs.
   - It uses the `println` function to display the results.