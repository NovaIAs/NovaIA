```ruby
# Define a class to represent a complex number.
class ComplexNumber
  attr_accessor :real, :imaginary

  def initialize(real, imaginary)
    @real = real
    @imaginary = imaginary
  end

  # Define methods to add, subtract, multiply, and divide complex numbers.
  def +(other)
    ComplexNumber.new(@real + other.real, @imaginary + other.imaginary)
  end

  def -(other)
    ComplexNumber.new(@real - other.real, @imaginary - other.imaginary)
  end

  def *(other)
    ComplexNumber.new(@real * other.real - @imaginary * other.imaginary,
                      @real * other.imaginary + @imaginary * other.real)
  end

  def /(other)
    denominator = other.real**2 + other.imaginary**2
    ComplexNumber.new((@real * other.real + @imaginary * other.imaginary) / denominator,
                      (@imaginary * other.real - @real * other.imaginary) / denominator)
  end

  # Define a method to convert a complex number to a string.
  def to_s
    "#{@real} + #{@imaginary}i"
  end
end

# Create two complex numbers.
c1 = ComplexNumber.new(3, 4)
c2 = ComplexNumber.new(5, -2)

# Perform operations on the complex numbers.
sum = c1 + c2
difference = c1 - c2
product = c1 * c2
quotient = c1 / c2

# Print the results.
puts "The sum of #{c1} and #{c2} is #{sum}."
puts "The difference of #{c1} and #{c2} is #{difference}."
puts "The product of #{c1} and #{c2} is #{product}."
puts "The quotient of #{c1} and #{c2} is #{quotient}."
```

Explanation:

1. The `ComplexNumber` class is defined with getter and setter methods for the `real` and `imaginary` attributes, which represent the real and imaginary parts of the complex number, respectively.
2. Methods are defined to perform addition, subtraction, multiplication, and division of complex numbers. The `+`, `-`, `*`, and `/` operators are overloaded to call these methods, allowing you to use the standard arithmetic operators with complex numbers.
3. A method `to_s` is defined to convert a complex number to a string in the format "real + imaginaryi".

~~~
```ruby
# Create two complex numbers.
c1 = ComplexNumber.new(3, 4)
c2 = ComplexNumber.new(5, -2)
```
~~~

Instances of the `ComplexNumber` class are created and assigned to the variables `c1` and `c2`.

~~~
```ruby
# Perform operations on the complex numbers.
sum = c1 + c2
difference = c1 - c2
product = c1 * c2
quotient = c1 / c2
```
~~~

The complex numbers `c1` and `c2` are used to perform addition, subtraction, multiplication, and division operations, and the results are stored in the variables `sum`, `difference`, `product`, and `quotient`.

~~~
```ruby
# Print the results.
puts "The sum of #{c1} and #{c2} is #{sum}."
puts "The difference of #{c1} and #{c2} is #{difference}."
puts "The product of #{c1} and #{c2} is #{product}."
puts "The quotient of #{c1} and #{c2} is #{quotient}."
```
~~~

The results of the operations are printed to the console using the `puts` method.