```ruby
# Define a class named "House" to model a house
class House
  # Initialize the house with a name and a list of rooms
  def initialize(name, rooms)
    @name = name
    @rooms = rooms
  end

  # Define a method to add a room to the house
  def add_room(room)
    @rooms << room
  end

  # Define a method to get the total square footage of the house
  def total_square_footage
    total = 0
    @rooms.each do |room|
      total += room.square_footage
    end
    total
  end

  # Define a method to get a list of all the rooms in the house
  def list_rooms
    @rooms.join(", ")
  end
end

# Define a class named "Room" to model a room in a house
class Room
  # Initialize the room with a name and a square footage
  def initialize(name, square_footage)
    @name = name
    @square_footage = square_footage
  end

  # Define a method to get the name of the room
  def name
    @name
  end

  # Define a method to get the square footage of the room
  def square_footage
    @square_footage
  end
end

# Create a new house object named "My House"
my_house = House.new("My House", [])

# Add some rooms to the house
my_house.add_room(Room.new("Living Room", 200))
my_house.add_room(Room.new("Kitchen", 150))
my_house.add_room(Room.new("Bedroom", 100))

# Print the total square footage of the house
puts "The total square footage of the house is #{my_house.total_square_footage} square feet."

# Print a list of all the rooms in the house
puts "The rooms in the house are: #{my_house.list_rooms}."
```

Explanation:

* The code defines two classes: `House` and `Room`.
* The `House` class represents a house with a name and a list of rooms. It has methods to add a room, get the total square footage of the house, and list all the rooms in the house.
* The `Room` class represents a room in a house with a name and a square footage. It has methods to get the name and square footage of the room.
* The code then creates a new `House` object named `my_house`.
* The code adds some rooms to the house using the `add_room` method.
* The code then prints the total square footage of the house and a list of all the rooms in the house.