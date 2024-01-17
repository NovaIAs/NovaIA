```ruby
# This Ruby code defines a class called "ComplexNumber" that represents complex numbers.

class ComplexNumber
  # The constructor method initializes a complex number with a real and imaginary part.
  def initialize(real, imaginary)
    @real = real
    @imaginary = imaginary
  end

  # This method adds two complex numbers.
  def +(other)
    ComplexNumber.new(@real + other.real, @imaginary + other.imaginary)
  end

  # This method subtracts two complex numbers.
  def -(other)
    ComplexNumber.new(@real - other.real, @imaginary - other.imaginary)
  end

  # This method multiplies two complex numbers.
  def *(other)
    ComplexNumber.new(@real * other.real - @imaginary * other.imaginary,
                      @real * other.imaginary + @imaginary * other.real)
  end

  # This method divides two complex numbers.
  def /(other)
    denominator = other.real**2 + other.imaginary**2
    ComplexNumber.new((@real * other.real + @imaginary * other.imaginary) / denominator,
                      (@imaginary * other.real - @real * other.imaginary) / denominator)
  end

  # This method returns the absolute value of the complex number.
  def abs
    Math.sqrt(@real**2 + @imaginary**2)
  end

  # This method returns the complex conjugate of the complex number.
  def conj
    ComplexNumber.new(@real, -@imaginary)
  end

  # This method returns a string representation of the complex number.
  def to_s
    "#{@real} + #{@imaginary}i"
  end
end

# Create two complex numbers.
c1 = ComplexNumber.new(3, 4)
c2 = ComplexNumber.new(5, -2)

# Perform various operations on the complex numbers.
sum = c1 + c2
difference = c1 - c2
product = c1 * c2
quotient = c1 / c2
absolute_value = c1.abs
conjugate = c1.conj

# Print the results.
puts "Sum: #{sum}"
puts "Difference: #{difference}"
puts "Product: #{product}"
puts "Quotient: #{quotient}"
puts "Absolute Value: #{absolute_value}"
puts "Conjugate: #{conjugate}"

```