```
# Define a class called 'Vehicle'
class Vehicle
  # Initialize the 'Vehicle' class with the following attributes:
  # @make: The make of the vehicle
  # @model: The model of the vehicle
  # @year: The year the vehicle was manufactured
  def initialize(make, model, year)
    @make = make
    @model = model
    @year = year
  end

  # Define a method called 'get_make' that returns the make of the vehicle
  def get_make
    @make
  end

  # Define a method called 'get_model' that returns the model of the vehicle
  def get_model
    @model
  end

  # Define a method called 'get_year' that returns the year the vehicle was manufactured
  def get_year
    @year
  end

  # Define a method called 'to_s' that returns a string representation of the vehicle
  def to_s
    "Make: #{@make}, Model: #{@model}, Year: #{@year}"
  end
end

# Define a class called 'Car' that inherits from the 'Vehicle' class
class Car < Vehicle
  # Initialize the 'Car' class with the following attributes:
  # @make: The make of the car
  # @model: The model of the car
  # @year: The year the car was manufactured
  # @num_doors: The number of doors the car has
  def initialize(make, model, year, num_doors)
    # Call the parent class's ('Vehicle') initialize method with the make, model, and year
    super(make, model, year)

    # Assign the number of doors to the 'num_doors' instance variable
    @num_doors = num_doors
  end

  # Define a method called 'get_num_doors' that returns the number of doors the car has
  def get_num_doors
    @num_doors
  end

  # Define a method called 'to_s' that returns a string representation of the car
  def to_s
    # Call the parent class's ('Vehicle') to_s method to get the make, model, and year
    parent_string = super

    # Append the number of doors to the string representation
    "#{parent_string}, Number of Doors: #{@num_doors}"
  end
end

# Define a class called 'Truck' that inherits from the 'Vehicle' class
class Truck < Vehicle
  # Initialize the 'Truck' class with the following attributes:
  # @make: The make of the truck
  # @model: The model of the truck
  # @year: The year the truck was manufactured
  # @bed_size: The size of the truck bed
  def initialize(make, model, year, bed_size)
    # Call the parent class's ('Vehicle') initialize method with the make, model, and year
    super(make, model, year)

    # Assign the bed size to the 'bed_size' instance variable
    @bed_size = bed_size
  end

  # Define a method called 'get_bed_size' that returns the size of the truck bed
  def get_bed_size
    @bed_size
  end

  # Define a method called 'to_s' that returns a string representation of the truck
  def to_s
    # Call the parent class's ('Vehicle') to_s method to get the make, model, and year
    parent_string = super

    # Append the bed size to the string representation
    "#{parent_string}, Bed Size: #{@bed_size}"
  end
end

# Create a new 'Car' object
car = Car.new("Toyota", "Camry", 2018, 4)

# Create a new 'Truck' object
truck = Truck.new("Ford", "F-150", 2020, "8 feet")

# Print the make, model, year, and number of doors of the car
puts "Car: #{car.to_s}"

# Print the make, model, year, and bed size of the truck
puts "Truck: #{truck.to_s}"
```

This code creates two classes, 'Car' and 'Truck', which inherit from a parent class called 'Vehicle'. The 'Vehicle' class has attributes for the make, model, and year of the vehicle. The 'Car' class adds an attribute for the number of doors, and the 'Truck' class adds an attribute for the bed size.

The code then creates instances of the 'Car' and 'Truck' classes and prints out their attributes.

This code is complex and differentiated because it involves inheritance, polymorphism, and multiple classes with different attributes and methods. It also demonstrates the use of the 'super' keyword to call the parent class's methods.