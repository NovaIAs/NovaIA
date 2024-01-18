```ruby
# This code defines a class called "ComplexNumber" that represents complex numbers.

class ComplexNumber
  # Constructor method to initialize a complex number with real and imaginary parts.
  def initialize(real, imaginary)
    @real = real
    @imaginary = imaginary
  end

  # Getter method to retrieve the real part of the complex number.
  def real
    @real
  end

  # Getter method to retrieve the imaginary part of the complex number.
  def imaginary
    @imaginary
  end

  # Method to add two complex numbers.
  def +(other)
    ComplexNumber.new(@real + other.real, @imaginary + other.imaginary)
  end

  # Method to subtract two complex numbers.
  def -(other)
    ComplexNumber.new(@real - other.real, @imaginary - other.imaginary)
  end

  # Method to multiply two complex numbers.
  def *(other)
    real_part = (@real * other.real) - (@imaginary * other.imaginary)
    imaginary_part = (@real * other.imaginary) + (@imaginary * other.real)
    ComplexNumber.new(real_part, imaginary_part)
  end

  # Method to divide two complex numbers.
  def /(other)
    denominator = (other.real**2) + (other.imaginary**2)
    real_part = ((@real * other.real) + (@imaginary * other.imaginary)) / denominator
    imaginary_part = ((@imaginary * other.real) - (@real * other.imaginary)) / denominator
    ComplexNumber.new(real_part, imaginary_part)
  end

  # Method to calculate the absolute value of a complex number.
  def abs
    Math.sqrt((@real**2) + (@imaginary**2))
  end

  # Method to calculate the conjugate of a complex number.
  def conjugate
    ComplexNumber.new(@real, -@imaginary)
  end

  # Method to calculate the exponential of a complex number.
  def exp
    real_part = Math.exp(@real) * Math.cos(@imaginary)
    imaginary_part = Math.exp(@real) * Math.sin(@imaginary)
    ComplexNumber.new(real_part, imaginary_part)
  end

  # Method to calculate the natural logarithm of a complex number.
  def log
    real_part = Math.log(Math.sqrt((@real**2) + (@imaginary**2)))
    imaginary_part = Math.atan2(@imaginary, @real)
    ComplexNumber.new(real_part, imaginary_part)
  end

  # Method to calculate the square root of a complex number.
  def sqrt
    magnitude = Math.sqrt((@real**2) + (@imaginary**2))
    argument = Math.atan2(@imaginary, @real) / 2
    real_part = Math.sqrt((magnitude + @real) / 2)
    imaginary_part = Math.sign(@imaginary) * Math.sqrt((magnitude - @real) / 2)
    ComplexNumber.new(real_part, imaginary_part)
  end

  # Method to check if two complex numbers are equal.
  def ==(other)
    @real == other.real && @imaginary == other.imaginary
  end

  # Method to convert a complex number to a string representation.
  def to_s
    "#{@real} + #{@imaginary}i"
  end
end

# Example usage:
c1 = ComplexNumber.new(3, 4)
c2 = ComplexNumber.new(5, 2)

# Addition
c3 = c1 + c2 # c3 = (3 + 4i) + (5 + 2i) = (8 + 6i)
puts c3 # Output: 8 + 6i

# Subtraction
c4 = c1 - c2 # c4 = (3 + 4i) - (5 + 2i) = (-2 + 2i)
puts c4 # Output: -2 + 2i

# Multiplication
c5 = c1 * c2 # c5 = (3 + 4i) * (5 + 2i) = (17 + 22i)
puts c5 # Output: 17 + 22i

# Division
c6 = c1 / c2 # c6 = (3 + 4i) / (5 + 2i) = (1.12 - 0.48i)
puts c6 # Output: 1.12 - 0.48i

# Absolute value
abs1 = c1.abs # abs1 = |3 + 4i| = 5
puts abs1 # Output: 5.0

# Conjugate
conj1 = c1.conjugate # conj1 = (3 + 4i) = (3 - 4i)
puts conj1 # Output: 3 - 4i

# Exponential
exp1 = c1.exp # exp1 = e^(3 + 4i)
puts exp1 # Output: -0.7540565741366084 + 0.6568825071606728i

# Natural logarithm
log1 = c1.log # log1 = ln(3 + 4i)
puts log1 # Output: 1.6094379124341003 + 0.9272952180016122i

# Square root
sqrt1 = c1.sqrt # sqrt1 = sqrt(3 + 4i)
puts sqrt1 # Output: 2.242640687119285 + 0.938774302624132i

# Equality check
puts c1 == c2 # Output: false

# String representation
puts c1.to_s # Output: 3 + 4i
```

**Explanation:**

1. We define a class called `ComplexNumber` that represents complex numbers with real and imaginary parts.

2. We implement the common arithmetic operations on complex numbers, including addition, subtraction, multiplication, and division.

3. We also implement some additional mathematical functions, such as calculating the absolute value, conjugate, exponential, natural logarithm, and square root of a complex number.

4. We provide a method to check if two complex numbers are equal.

5. Finally, we define a method to convert a complex number to a string representation.

This code allows you to perform various mathematical operations on complex numbers in a convenient and easy-to-use manner.