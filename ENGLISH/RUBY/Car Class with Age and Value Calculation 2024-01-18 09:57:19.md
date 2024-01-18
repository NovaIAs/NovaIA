```ruby
# Define a class called "Car"
class Car
  # Define the attributes of the car
  attr_accessor :make, :model, :year, :color, :mileage

  # Define the constructor method for the car
  def initialize(make, model, year, color, mileage)
    # Assign the values passed in to the attributes of the car
    @make = make
    @model = model
    @year = year
    @color = color
    @mileage = mileage
  end

  # Define a method to calculate the car's age
  def age
    # Calculate the age of the car by subtracting the year it was made from the current year
    Time.now.year - @year
  end

  # Define a method to calculate the car's value
  def value
    # Calculate the car's value based on its age, mileage, and condition
    # In this example, we're using a simple formula for demonstration purposes
    price = 10000 - (@mileage / 1000) * 100 - @age * 500
    # Make sure the value is never negative
    price = 0 if price < 0
    # Return the calculated value
    price
  end

  # Define a method to display the car's information
  def display_info
    # Display the car's make, model, year, color, mileage, and value
    puts "Make: #{@make}"
    puts "Model: #{@model}"
    puts "Year: #{@year}"
    puts "Color: #{@color}"
    puts "Mileage: #{@mileage}"
    puts "Value: #{value}"
  end
end

# Create a new car object
car = Car.new("Toyota", "Camry", 2015, "Silver", 50000)

# Display the car's information
car.display_info

# Calculate the car's age and display it
age = car.age
puts "Age: #{age}"

# Calculate the car's value and display it
value = car.value
puts "Value: #{value}"
```

Explanation:

1. We define a class called "Car" which represents a car with attributes like make, model, year, color, and mileage.

2. We define a constructor method for the class that takes in the values for these attributes and assigns them to the corresponding instance variables.

3. We define methods to calculate the car's age and value. These methods use simple formulas for demonstration purposes.

4. We define a method to display the car's information, which prints out the values of all the attributes and the calculated age and value.

5. We create a new car object using the Car class and pass in values for the attributes.

6. We call the display_info method on the car object to display its information.

7. We calculate the age of the car and display it using the age method.

8. We calculate the value of the car and display it using the value method.