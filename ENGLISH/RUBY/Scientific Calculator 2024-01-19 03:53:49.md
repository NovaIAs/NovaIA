```ruby
# Define a class called 'Calculator' for performing various mathematical operations.
class Calculator
  # Define instance methods for addition, subtraction, multiplication, and division.
  def add(a, b)
    a + b
  end

  def subtract(a, b)
    a - b
  end

  def multiply(a, b)
    a * b
  end

  def divide(a, b)
    a / b
  end

  # Define a method to calculate the square root of a number.
  def sqrt(a)
    Math.sqrt(a)
  end

  # Define a method to calculate the factorial of a number.
  def factorial(n)
    if n < 0
      return "Error: Factorial is undefined for negative numbers."
    elsif n == 0
      return 1
    else
      (1..n).reduce(:*)
    end
  end

  # Define a method to check if a number is prime.
  def prime?(n)
    return false if n < 2

    (2..(Math.sqrt(n).ceil)).each do |i|
      return false if n % i == 0
    end

    true
  end
end

# Create an instance of the 'Calculator' class.
calculator = Calculator.new

# Use the instance methods to perform various calculations.
result_add = calculator.add(10, 5)
result_subtract = calculator.subtract(20, 7)
result_multiply = calculator.multiply(3, 4)
result_divide = calculator.divide(12, 3)
result_sqrt = calculator.sqrt(25)
result_factorial = calculator.factorial(5)
result_prime = calculator.prime?(17)

# Print the results.
puts "Addition Result: #{result_add}"
puts "Subtraction Result: #{result_subtract}"
puts "Multiplication Result: #{result_multiply}"
puts "Division Result: #{result_divide}"
puts "Square Root Result: #{result_sqrt}"
puts "Factorial Result: #{result_factorial}"
puts "Prime Number Check Result: #{result_prime}"
```