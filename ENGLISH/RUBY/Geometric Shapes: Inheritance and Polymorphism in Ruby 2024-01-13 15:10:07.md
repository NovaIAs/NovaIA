```ruby
# Define a class to represent a geometric shape
class Shape
  # Initialize the shape with its properties
  def initialize(properties)
    @properties = properties
  end

  # Calculate the area of the shape
  def area
    # Calculate the area based on the shape type
    case self.class.to_s
    when "Square"
      @properties[:side] ** 2
    when "Rectangle"
      @properties[:length] * @properties[:width]
    when "Circle"
      Math::PI * @properties[:radius] ** 2
    when "Triangle"
      0.5 * @properties[:base] * @properties[:height]
    else
      raise "Unsupported shape type: #{self.class}"
    end
  end

  # Calculate the perimeter of the shape
  def perimeter
    # Calculate the perimeter based on the shape type
    case self.class.to_s
    when "Square"
      4 * @properties[:side]
    when "Rectangle"
      2 * (@properties[:length] + @properties[:width])
    when "Circle"
      2 * Math::PI * @properties[:radius]
    when "Triangle"
      @properties[:side1] + @properties[:side2] + @properties[:side3]
    else
      raise "Unsupported shape type: #{self.class}"
    end
  end

  # Display the properties of the shape
  def display_properties
    puts "Shape Properties:"
    @properties.each do |key, value|
      puts "#{key}: #{value}"
    end
  end
end

# Define a subclass of Shape for a square
class Square < Shape
  # Initialize the square with its side length
  def initialize(side)
    super({ side: side })
  end
end

# Define a subclass of Shape for a rectangle
class Rectangle < Shape
  # Initialize the rectangle with its length and width
  def initialize(length, width)
    super({ length: length, width: width })
  end
end

# Define a subclass of Shape for a circle
class Circle < Shape
  # Initialize the circle with its radius
  def initialize(radius)
    super({ radius: radius })
  end
end

# Define a subclass of Shape for a triangle
class Triangle < Shape
  # Initialize the triangle with its base, height, and side lengths
  def initialize(base, height, side1, side2, side3)
    super({ base: base, height: height, side1: side1, side2: side2, side3: side3 })
  end
end

# Create a square object
square = Square.new(5)

# Create a rectangle object
rectangle = Rectangle.new(10, 7)

# Create a circle object
circle = Circle.new(3)

# Create a triangle object
triangle = Triangle.new(6, 8, 5, 7, 9)

# Display the properties of each shape
puts "Square Properties:"
square.display_properties

puts "Rectangle Properties:"
rectangle.display_properties

puts "Circle Properties:"
circle.display_properties

puts "Triangle Properties:"
triangle.display_properties

# Calculate the area and perimeter of each shape
puts "Square Area: #{square.area}"
puts "Square Perimeter: #{square.perimeter}"

puts "Rectangle Area: #{rectangle.area}"
puts "Rectangle Perimeter: #{rectangle.perimeter}"

puts "Circle Area: #{circle.area}"
puts "Circle Perimeter: #{circle.perimeter}"

puts "Triangle Area: #{triangle.area}"
puts "Triangle Perimeter: #{triangle.perimeter}"
```

Explanation:

1. We define a base class called `Shape` with methods for calculating area, perimeter, and displaying properties.
2. We define subclasses for specific shapes like `Square`, `Rectangle`, `Circle`, and `Triangle` that inherit from the `Shape` class.
3. Each subclass has its own constructor to initialize the properties specific to that shape.
4. We create instances of each shape and display their properties.
5. We calculate the area and perimeter of each shape and display the results.

This code demonstrates the use of inheritance and polymorphism in Ruby, where subclasses can inherit properties and methods from a base class and have their own unique implementations.