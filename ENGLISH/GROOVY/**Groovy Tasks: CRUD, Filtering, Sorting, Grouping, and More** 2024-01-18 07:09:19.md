```groovy
// Define a class to represent a task
class Task {
    String name
    boolean completed

    // Constructor to initialize the task with a name and completion status
    Task(String name, boolean completed) {
        this.name = name
        this.completed = completed
    }

    // Method to toggle the completion status of the task
    void toggleCompleted() {
        completed = !completed
    }

    // Method to return a string representation of the task
    String toString() {
        "[Task: name=$name, completed=$completed]"
    }
}

// Define a list of tasks
def tasks = [
    new Task("Buy groceries", false),
    new Task("Do laundry", false),
    new Task("Clean the house", false),
    new Task("Pay bills", false),
    new Task("Go to the gym", false)
]

// Print the initial list of tasks
println("Initial task list:")
tasks.each { println it }

// Iterate over the list of tasks and toggle the completion status for each task
println("\nToggling completion status:")
tasks.each { it.toggleCompleted() }

// Print the updated list of tasks
println("\nUpdated task list:")
tasks.each { println it }

// Filter the list of tasks to only include completed tasks
println("\nCompleted tasks:")
def completedTasks = tasks.findAll { it.completed }
completedTasks.each { println it }

// Sort the list of tasks by name
println("\nTasks sorted by name:")
def sortedTasks = tasks.sort { it.name }
sortedTasks.each { println it }

// Group the tasks by completion status
println("\nTasks grouped by completion status:")
def groupedTasks = tasks.groupBy { it.completed }
groupedTasks.each { k, v -> println("$k: $v") }

// Find the first completed task in the list
println("\nFirst completed task:")
def firstCompletedTask = tasks.find { it.completed }
println firstCompletedTask

// Check if all tasks are completed
println("\nAre all tasks completed?")
def allCompleted = tasks.every { it.completed }
println allCompleted

// Check if any tasks are completed
println("\nAre any tasks completed?")
def anyCompleted = tasks.any { it.completed }
println anyCompleted
```
Explanation:

1. We define a `Task` class to represent each task with properties like name and completion status.

2. We create a list of tasks with their initial completion status set to false.

3. We print the initial list of tasks.

4. We iterate over the list and toggle the completion status of each task.

5. We print the updated list of tasks.

6. We filter the list to include only completed tasks and print them.

7. We sort the list of tasks by name and print them.

8. We group the tasks by completion status and print them.

9. We find the first completed task and print it.

10. We check if all tasks are completed and print the result.

11. We check if any tasks are completed and print the result.

This code demonstrates various operations on a list of tasks, including CRUD (create, read, update, delete) operations, filtering, sorting, grouping, and finding elements. It also demonstrates the use of lambda expressions and closure syntax in Groovy.