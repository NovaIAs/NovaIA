```ruby
# Define a class called "Vehicle"
class Vehicle
  # Initialize the class with attributes: make, model, year, and color
  def initialize(make, model, year, color)
    @make = make
    @model = model
    @year = year
    @color = color
  end

  # Define a method to display vehicle details
  def display_details
    puts "Make: #{@make}"
    puts "Model: #{@model}"
    puts "Year: #{@year}"
    puts "Color: #{@color}"
  end

  # Define a method to start the vehicle
  def start
    puts "The vehicle is starting..."
  end

  # Define a method to stop the vehicle
  def stop
    puts "The vehicle is stopping..."
  end
end

# Define a class called "Car" that inherits from "Vehicle"
class Car < Vehicle
  # Initialize the class with additional attributes: num_doors and num_seats
  def initialize(make, model, year, color, num_doors, num_seats)
    super(make, model, year, color) # Call the parent class's initialize method
    @num_doors = num_doors
    @num_seats = num_seats
  end

  # Override the "display_details" method to include car-specific details
  def display_details
    super # Call the parent class's "display_details" method
    puts "Number of Doors: #{@num_doors}"
    puts "Number of Seats: #{@num_seats}"
  end
end

# Define a class called "Truck" that inherits from "Vehicle"
class Truck < Vehicle
  # Initialize the class with additional attributes: bed_length and towing_capacity
  def initialize(make, model, year, color, bed_length, towing_capacity)
    super(make, model, year, color) # Call the parent class's initialize method
    @bed_length = bed_length
    @towing_capacity = towing_capacity
  end

  # Override the "display_details" method to include truck-specific details
  def display_details
    super # Call the parent class's "display_details" method
    puts "Bed Length: #{@bed_length}"
    puts "Towing Capacity: #{@towing_capacity}"
  end
end

# Create an instance of the "Car" class
car = Car.new("Toyota", "Camry", 2018, "Silver", 4, 5)

# Create an instance of the "Truck" class
truck = Truck.new("Ford", "F-150", 2020, "Black", 8, 10000)

# Display the details of the car and truck
puts "Car Details:"
car.display_details

puts "Truck Details:"
truck.display_details

# Start and stop the car and truck
car.start
car.stop

truck.start
truck.stop
```

Explanation:

This Ruby code defines three classes: "Vehicle", "Car", and "Truck".

1. **Vehicle Class:**
   - Defines the basic attributes and methods of a generic vehicle.
   - Includes attributes such as make, model, year, and color.
   - Defines methods to display vehicle details, start the vehicle, and stop the vehicle.

2. **Car Class:**
   - Inherits from the "Vehicle" class.
   - Adds additional attributes specific to cars, such as the number of doors and seats.
   - Overrides the "display_details" method to include car-specific details.

3. **Truck Class:**
   - Inherits from the "Vehicle" class.
   - Adds additional attributes specific to trucks, such as the bed length and towing capacity.
   - Overrides the "display_details" method to include truck-specific details.

In the main part of the code:

- Two instances of the "Car" and "Truck" classes are created, representing a specific car and truck.

- The "display_details" method is called on both the car and truck to display their details.

- The "start" and "stop" methods are called on the car and truck to simulate starting and stopping the vehicles.

This code demonstrates the use of inheritance, method overriding, and polymorphism in Ruby. It allows you to create different types of vehicles with varying attributes and behaviors, while sharing common functionality defined in the parent class.