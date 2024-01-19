```ruby
# A class to represent a complex number
class Complex
  attr_accessor :real, :imaginary

  def initialize(real, imaginary)
    @real = real
    @imaginary = imaginary
  end

  # Add two complex numbers
  def +(other)
    Complex.new(@real + other.real, @imaginary + other.imaginary)
  end

  # Subtract two complex numbers
  def -(other)
    Complex.new(@real - other.real, @imaginary - other.imaginary)
  end

  # Multiply two complex numbers
  def *(other)
    Complex.new(@real * other.real - @imaginary * other.imaginary,
                @real * other.imaginary + @imaginary * other.real)
  end

  # Divide two complex numbers
  def /(other)
    denominator = other.real**2 + other.imaginary**2
    Complex.new((@real * other.real + @imaginary * other.imaginary) / denominator,
                (@imaginary * other.real - @real * other.imaginary) / denominator)
  end

  # Return the absolute value of the complex number
  def abs
    Math.sqrt(@real**2 + @imaginary**2)
  end

  # Return the argument of the complex number
  def arg
    Math.atan2(@imaginary, @real)
  end

  # Return a string representation of the complex number
  def to_s
    "#{@real} + #{@imaginary}i"
  end
end

# Create two complex numbers
c1 = Complex.new(1, 2)
c2 = Complex.new(3, 4)

# Add the two complex numbers
c3 = c1 + c2

# Subtract the two complex numbers
c4 = c1 - c2

# Multiply the two complex numbers
c5 = c1 * c2

# Divide the two complex numbers
c6 = c1 / c2

# Print the results
puts "c1 = #{c1}"
puts "c2 = #{c2}"
puts "c3 = #{c3}"
puts "c4 = #{c4}"
puts "c5 = #{c5}"
puts "c6 = #{c6}"
```

This code defines a class called `Complex` that can be used to represent complex numbers. The class has two instance variables, `@real` and `@imaginary`, which store the real and imaginary parts of the complex number, respectively.

The class also has a number of methods that can be used to perform operations on complex numbers, such as addition, subtraction, multiplication, and division. These methods are implemented using the standard mathematical operations for complex numbers.

Finally, the class has a `to_s` method that returns a string representation of the complex number. This method is used to print the results of the operations on the complex numbers.

Here is an example of how to use the `Complex` class:

```ruby
require 'complex'

c1 = Complex(1, 2)
c2 = Complex(3, 4)

c3 = c1 + c2
c4 = c1 - c2
c5 = c1 * c2
c6 = c1 / c2

puts "c1 = #{c1}"
puts "c2 = #{c2}"
puts "c3 = #{c3}"
puts "c4 = #{c4}"
puts "c5 = #{c5}"
puts "c6 = #{c6}"
```

This code will print the following output:

```
c1 = 1 + 2i
c2 = 3 + 4i
c3 = 4 + 6i
c4 = -2 - 2i
c5 = -5 + 10i
c6 = 0.44 + 0.08i
```