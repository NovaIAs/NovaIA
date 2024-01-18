```ruby
# Define a class called "Robot"
class Robot
  # Initialize the robot with a name and a set of tasks
  def initialize(name, tasks)
    @name = name
    @tasks = tasks
  end

  # Define a method called "greet" that prints a greeting from the robot
  def greet
    puts "Hello, my name is #{@name}. I am a robot."
  end

  # Define a method called "work" that iterates over the tasks and prints them out
  def work
    puts "I am working on the following tasks:"
    @tasks.each do |task|
      puts task
    end
  end

  # Define a method called "dance" that prints out a dance routine
  def dance
    puts "I am dancing! Look at my moves!"
  end
end

# Create a new robot object called "R2D2" with a set of tasks
r2d2 = Robot.new("R2D2", ["Clean the ship", "Repair the hyperdrive", "Make a sandwich"])

# Call the "greet" method on the robot object
r2d2.greet

# Call the "work" method on the robot object
r2d2.work

# Call the "dance" method on the robot object
r2d2.dance
```

Explanation:

1. We define a class called `Robot` with an `initialize` method that takes a name and a set of tasks as arguments. The `initialize` method sets the `@name` and `@tasks` instance variables to the values of the arguments.

2. We define three methods called `greet`, `work`, and `dance` in the `Robot` class. The `greet` method prints out a greeting from the robot, the `work` method iterates over the tasks and prints them out, and the `dance` method prints out a dance routine.

3. We create a new `Robot` object called `r2d2` and pass in the name "R2D2" and a set of tasks.

4. We call the `greet`, `work`, and `dance` methods on the `r2d2` object.

When you run this code, you will see the robot greet you, list its tasks, and perform a dance routine.