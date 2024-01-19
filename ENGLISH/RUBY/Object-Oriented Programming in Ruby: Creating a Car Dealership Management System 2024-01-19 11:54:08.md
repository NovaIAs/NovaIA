```ruby
# Create a class called Car with attributes like make, model, year, and color.
class Car
  attr_accessor :make, :model, :year, :color

  # Initialize the Car object with default values.
  def initialize(make = "Toyota", model = "Camry", year = 2020, color = "Silver")
    @make = make
    @model = model
    @year = year
    @color = color
  end

  # Define a method to display the car's details.
  def display_details
    puts "Make: #{@make}"
    puts "Model: #{@model}"
    puts "Year: #{@year}"
    puts "Color: #{@color}"
  end
end

# Create a class called Dealership with attributes like name, location, and inventory.
class Dealership
  attr_accessor :name, :location, :inventory

  # Initialize the Dealership object with default values.
  def initialize(name = "ABC Motors", location = "123 Main Street", inventory = [])
    @name = name
    @location = location
    @inventory = inventory
  end

  # Define a method to add a car to the dealership's inventory.
  def add_car(car)
    @inventory << car
  end

  # Define a method to display the dealership's inventory.
  def display_inventory
    puts "Inventory:"
    @inventory.each do |car|
      car.display_details
      puts ""
    end
  end
end

# Create a new Car object and set its attributes.
car1 = Car.new("Honda", "Civic", 2018, "Red")

# Create a new Dealership object and set its attributes.
dealership1 = Dealership.new("XYZ Motors", "456 Elm Street")

# Add the car to the dealership's inventory.
dealership1.add_car(car1)

# Display the dealership's inventory.
dealership1.display_inventory
```

Explanation:

* We start by creating a `Car` class with attributes like `make`, `model`, `year`, and `color`. We also define getter and setter methods for these attributes.

* We define a method called `display_details` in the `Car` class that displays the details of the car.

* We create a `Dealership` class with attributes like `name`, `location`, and `inventory`. We also define getter and setter methods for these attributes.

* We define a method called `add_car` in the `Dealership` class that adds a car to the dealership's inventory.

* We define a method called `display_inventory` in the `Dealership` class that displays the dealership's inventory.

* We create a new `Car` object and set its attributes.

* We create a new `Dealership` object and set its attributes.

* We add the car to the dealership's inventory using the `add_car` method.

* We display the dealership's inventory using the `display_inventory` method.

This code demonstrates object-oriented programming concepts in Ruby by creating classes, objects, and defining methods. It also shows how to manage and display data in Ruby.