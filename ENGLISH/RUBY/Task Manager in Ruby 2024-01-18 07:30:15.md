```ruby
# Define a module for managing tasks.
module TaskManager

  # Define a class for representing a task.
  class Task

    # Initialize a task with a name and description.
    def initialize(name, description)
      @name = name
      @description = description
      @status = "new"
    end

    # Get the name of the task.
    def name
      @name
    end

    # Get the description of the task.
    def description
      @description
    end

    # Get the status of the task.
    def status
      @status
    end

    # Set the status of the task.
    def status=(new_status)
      @status = new_status
    end

    # Mark the task as complete.
    def complete!
      @status = "complete"
    end

    # Mark the task as incomplete.
    def incomplete!
      @status = "incomplete"
    end
  end

  # Define a class for managing a collection of tasks.
  class TaskList

    # Initialize a task list with an array of tasks.
    def initialize(tasks=[])
      @tasks = tasks
    end

    # Get the array of tasks.
    def tasks
      @tasks
    end

    # Add a task to the list.
    def add(task)
      @tasks << task
    end

    # Remove a task from the list.
    def remove(task)
      @tasks.delete(task)
    end

    # Find a task by its name.
    def find(name)
      @tasks.find { |task| task.name == name }
    end

    # Mark all tasks in the list as complete.
    def complete_all!
      @tasks.each { |task| task.complete! }
    end

    # Mark all tasks in the list as incomplete.
    def incomplete_all!
      @tasks.each { |task| task.incomplete! }
    end
  end

  # Create a task list.
  task_list = TaskList.new

  # Add some tasks to the list.
  task_list.add(Task.new("Groceries", "Buy groceries for the week."))
  task_list.add(Task.new("Laundry", "Wash and dry clothes."))
  task_list.add(Task.new("Dishes", "Wash the dishes."))

  # Find a task by its name.
  task = task_list.find("Laundry")

  # Mark the task as complete.
  task.complete!

  # Print the list of tasks.
  task_list.tasks.each do |task|
    puts "#{task.name} (#{task.status})"
  end
end
```

**Explanation:**

1. We define a module `TaskManager` to encapsulate all the code related to task management.

2. Inside the `TaskManager` module, we define a `Task` class to represent a single task. Each task has a name, description, and status.

3. We also define a `TaskList` class to manage a collection of tasks. A task list can be initialized with an array of tasks, and it provides methods to add, remove, and find tasks.

4. We create a task list and add some tasks to it.

5. We find a task by its name and mark it as complete.

6. Finally, we print the list of tasks to the console.

This code demonstrates various concepts in Ruby, including modules, classes, methods, arrays, and iteration. It also shows how to use method chaining to mark all tasks in the list as complete.