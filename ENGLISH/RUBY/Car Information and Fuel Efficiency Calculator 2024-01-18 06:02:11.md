```ruby
# Define a class called "Car" with attributes and methods
class Car
  attr_accessor :make, :model, :year, :color, :mileage

  # Initialize a new Car object with the given attributes
  def initialize(make, model, year, color, mileage)
    @make = make
    @model = model
    @year = year
    @color = color
    @mileage = mileage
  end

  # Define a method to calculate the car's fuel efficiency
  def calculate_fuel_efficiency
    # Calculate the fuel efficiency in miles per gallon (mpg)
    mpg = mileage / fuel_consumed
    return mpg
  end

  # Define a method to display the car's information
  def display_information
    # Display the car's make, model, year, color, and mileage
    puts "Make: #{make}"
    puts "Model: #{model}"
    puts "Year: #{year}"
    puts "Color: #{color}"
    puts "Mileage: #{mileage}"
  end
end

# Create a new Car object with the given attributes
car = Car.new("Toyota", "Camry", 2018, "Silver", 50000)

# Display the car's information
car.display_information

# Calculate the car's fuel efficiency
fuel_efficiency = car.calculate_fuel_efficiency

# Display the car's fuel efficiency
puts "Fuel efficiency: #{fuel_efficiency} mpg"
```

Explanation:

1. We define a class called Car with attributes and methods. The attributes include make, model, year, color, and mileage. The methods include a constructor to initialize a new Car object with the given attributes, a method to calculate the car's fuel efficiency, and a method to display the car's information.

2. We create a new Car object with the given attributes. In this case, we create a Toyota Camry from 2018 with a silver color and a mileage of 50,000.

3. We display the car's information by calling the display_information method on the car object. This will print out the car's make, model, year, color, and mileage.

4. We calculate the car's fuel efficiency by calling the calculate_fuel_efficiency method on the car object. This method calculates the fuel efficiency in miles per gallon (mpg) based on the car's mileage and fuel consumption.

5. Finally, we display the car's fuel efficiency by printing out the fuel_efficiency variable.

This is a simple example of a Ruby class with attributes, methods, and object instantiation. It demonstrates how to create objects, access and modify their attributes, and call methods to perform operations on those objects.