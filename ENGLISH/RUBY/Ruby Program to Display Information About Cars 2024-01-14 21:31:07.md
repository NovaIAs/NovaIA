```ruby
# Define a class called `Car`
class Car
  # Define attributes for the `Car` class
  attr_accessor :make, :model, :year, :color, :mileage

  # Define a constructor method for the `Car` class
  def initialize(make, model, year, color, mileage)
    # Assign values to the attributes using the constructor arguments
    @make = make
    @model = model
    @year = year
    @color = color
    @mileage = mileage
  end

  # Define a method called `display_info` to display information about the car
  def display_info
    # Use string interpolation to display the car's information
    puts "Make: #{@make}"
    puts "Model: #{@model}"
    puts "Year: #{@year}"
    puts "Color: #{@color}"
    puts "Mileage: #{@mileage}"
  end
end

# Create an array of car objects
cars = [
  Car.new("Toyota", "Camry", 2018, "Silver", 50000),
  Car.new("Honda", "Accord", 2019, "Blue", 40000),
  Car.new("Ford", "F-150", 2020, "Red", 60000)
]

# Iterate over the array of car objects and display information about each car
cars.each do |car|
  car.display_info
  puts "\n"
end
```

Explanation:

1. We define a class called `Car` with attributes `make`, `model`, `year`, `color`, and `mileage`.

2. We define a constructor method for the `Car` class that takes values for these attributes as arguments and assigns them to the corresponding instance variables.

3. We define a method called `display_info` in the `Car` class to display information about the car. This method uses string interpolation to display the values of the car's attributes.

4. We create an array of `Car` objects, where each object represents a different car with its own make, model, year, color, and mileage.

5. We iterate over the array of car objects using the `each` method and call the `display_info` method on each car object to display its information.

This code demonstrates object-oriented programming principles in Ruby, including defining a class with attributes and methods, creating objects from the class, and using an array to store and iterate over a collection of objects.