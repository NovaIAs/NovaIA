```ruby
# Define a class called `Robot` that represents a robot with various attributes and methods.
class Robot
  # Initialize the robot with a name, a model number, and a list of tasks.
  def initialize(name, model, tasks)
    @name = name
    @model = model
    @tasks = tasks
  end

  # Define a method called `greet` that makes the robot introduce itself.
  def greet
    puts "Hello, my name is #{@name} and I am a #{@model} robot."
  end

  # Define a method called `perform_tasks` that makes the robot perform its tasks.
  def perform_tasks
    @tasks.each do |task|
      puts "Performing task: #{task}"
    end
  end
end

# Create a new `Robot` object with the name "R2-D2", the model number "Astromech Droid",
# and a list of tasks.
robot = Robot.new("R2-D2", "Astromech Droid", ["Clean the ship", "Fix the hyperdrive", "Deliver a message"])

# Call the `greet` method to make the robot introduce itself.
robot.greet

# Call the `perform_tasks` method to make the robot perform its tasks.
robot.perform_tasks

# Define a module called `Sensors` that provides sensory capabilities to the robot.
module Sensors
  # Define a method called `scan_environment` that scans the environment and returns a list of objects.
  def scan_environment
    ["Rock", "Tree", "Building"]
  end
end

# Include the `Sensors` module into the `Robot` class to give it sensory capabilities.
class Robot
  include Sensors
end

# Create a new `Robot` object with the name "C-3PO" and the model number "Protocol Droid".
robot2 = Robot.new("C-3PO", "Protocol Droid")

# Call the `scan_environment` method to scan the environment.
objects = robot2.scan_environment

# Print the list of objects detected by the robot.
puts "Objects detected: #{objects.join(", ")}"
```

**Explanation:**

1. We define a class called `Robot` that represents a robot with various attributes and methods.

2. Inside the `Robot` class, we define methods like `greet` and `perform_tasks` to make the robot introduce itself and perform its tasks, respectively.

3. We create a new `Robot` object with the name "R2-D2", the model number "Astromech Droid", and a list of tasks.

4. We call the `greet` and `perform_tasks` methods to make the robot introduce itself and perform its tasks.

5. We define a module called `Sensors` that provides sensory capabilities to the robot.

6. We include the `Sensors` module into the `Robot` class to give it sensory capabilities.

7. We create a new `Robot` object with the name "C-3PO" and the model number "Protocol Droid".

8. We call the `scan_environment` method to scan the environment and print the list of objects detected by the robot.