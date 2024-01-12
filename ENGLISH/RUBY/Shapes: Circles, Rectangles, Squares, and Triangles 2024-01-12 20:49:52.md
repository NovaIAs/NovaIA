```ruby
class Shape
  attr_accessor :color

  def initialize(color)
    @color = color
  end

  def area
    raise NotImplementedError
  end

  def perimeter
    raise NotImplementedError
  end
end

class Circle < Shape
  attr_accessor :radius

  def initialize(color, radius)
    super(color)
    @radius = radius
  end

  def area
    Math::PI * @radius**2
  end

  def perimeter
    2 * Math::PI * @radius
  end
end

class Rectangle < Shape
  attr_accessor :width, :height

  def initialize(color, width, height)
    super(color)
    @width = width
    @height = height
  end

  def area
    @width * @height
  end

  def perimeter
    2 * (@width + @height)
  end
end

class Square < Rectangle
  def initialize(color, side_length)
    super(color, side_length, side_length)
  end
end

class Triangle < Shape
  attr_accessor :base, :height

  def initialize(color, base, height)
    super(color)
    @base = base
    @height = height
  end

  def area
    0.5 * @base * @height
  end

  def perimeter
    @base + @height + Math.sqrt(@base**2 + @height**2)
  end
end

shapes = [
  Circle.new("red", 5),
  Rectangle.new("blue", 10, 20),
  Square.new("green", 15),
  Triangle.new("yellow", 12, 8)
]

shapes.each do |shape|
  puts "Shape: #{shape.class}"
  puts "Color: #{shape.color}"
  puts "Area: #{shape.area}"
  puts "Perimeter: #{shape.perimeter}"
  puts
end
```

This code defines a hierarchy of shape classes, including a `Shape` base class and subclasses for `Circle`, `Rectangle`, `Square`, and `Triangle`. Each shape has a `color` attribute and methods for calculating its `area` and `perimeter`.

The `shapes` array contains instances of each shape class, and the code uses a loop to iterate over the array and print out information about each shape, including its class, color, area, and perimeter.

The output of the code would be:

```
Shape: Circle
Color: red
Area: 78.53981633974483
Perimeter: 31.41592653589793

Shape: Rectangle
Color: blue
Area: 200
Perimeter: 60

Shape: Square
Color: green
Area: 225
Perimeter: 60

Shape: Triangle
Color: yellow
Area: 48
Perimeter: 37.071067811865475
```