```kotlin
// Import necessary libraries
import java.util.*
import kotlin.collections.*
import kotlin.system.exitProcess

// Define a data class to represent a task
data class Task(val id: Int, val title: String, val description: String, val priority: Int, val status: String)

// Define a constant for the maximum number of tasks
const val MAX_TASKS = 10

// Create a list to store the tasks
val tasks = mutableListOf<Task>()

// Define a function to add a task to the list
fun addTask(task: Task) {
    if (tasks.size < MAX_TASKS) {
        tasks.add(task)
        println("Task added successfully")
    } else {
        println("Maximum number of tasks reached")
    }
}

// Define a function to remove a task from the list by ID
fun removeTask(id: Int) {
    val taskIndex = tasks.indexOfFirst { it.id == id }
    if (taskIndex != -1) {
        tasks.removeAt(taskIndex)
        println("Task removed successfully")
    } else {
        println("Task with ID $id not found")
    }
}

// Define a function to update a task by ID
fun updateTask(id: Int, newTitle: String, newDescription: String, newPriority: Int, newStatus: String) {
    val taskIndex = tasks.indexOfFirst { it.id == id }
    if (taskIndex != -1) {
        tasks[taskIndex] = Task(id, newTitle, newDescription, newPriority, newStatus)
        println("Task updated successfully")
    } else {
        println("Task with ID $id not found")
    }
}

// Define a function to display all the tasks
fun displayTasks() {
    println("----------------------------------------------------------------------")
    println("| ID | Title                              | Description                                    | Priority | Status |")
    println("----------------------------------------------------------------------")
    for (task in tasks) {
        println("| ${task.id} | ${task.title.padEnd(35)} | ${task.description.padEnd(50)} | ${task.priority} | ${task.status} |")
    }
    println("----------------------------------------------------------------------")
}

// Define a function to get user input
fun getUserInput(message: String): String {
    print("$message: ")
    return readLine()!!
}

// Define a function to validate user input for integer
fun validateInteger(input: String): Int {
    return try {
        input.toInt()
    } catch (e: NumberFormatException) {
        -1
    }
}

// Define a function to validate user input for priority (1-5)
fun validatePriority(input: String): Int {
    val priority = validateInteger(input)
    return if (priority in 1..5) {
        priority
    } else {
        -1
    }
}

// Define a function to validate user input for status (New, In Progress, Completed)
fun validateStatus(input: String): String {
    return if (input.lowercase() in listOf("new", "in progress", "completed")) {
        input.lowercase().capitalize()
    } else {
        ""
    }
}

// Main function
fun main(args: Array<String>) {
    // Create some sample tasks
    addTask(Task(1, "Task 1", "This is task 1", 3, "New"))
    addTask(Task(2, "Task 2", "This is task 2", 5, "In Progress"))
    addTask(Task(3, "Task 3", "This is task 3", 1, "Completed"))

    // Display the tasks
    displayTasks()

    // Get user input for a new task
    val title = getUserInput("Enter task title")
    val description = getUserInput("Enter task description")
    var priority = -1
    while (priority == -1) {
        priority = validatePriority(getUserInput("Enter task priority (1-5)"))
    }
    var status = ""
    while (status.isEmpty()) {
        status = validateStatus(getUserInput("Enter task status (New, In Progress, Completed)"))
    }

    // Add the new task to the list
    addTask(Task(tasks.size + 1, title, description, priority, status))

    // Display the tasks again
    displayTasks()

    // Get user input for a task to remove
    val id = validateInteger(getUserInput("Enter ID of task to remove"))
    if (id != -1) {
        removeTask(id)
    }

    // Display the tasks again
    displayTasks()

    // Get user input for a task to update
    val idToUpdate = validateInteger(getUserInput("Enter ID of task to update"))
    if (idToUpdate != -1) {
        val newTitle = getUserInput("Enter new task title")
        val newDescription = getUserInput("Enter new task description")
        var newPriority = -1
        while (newPriority == -1) {
            newPriority = validatePriority(getUserInput("Enter new task priority (1-5)"))
        }
        var newStatus = ""
        while (newStatus.isEmpty()) {
            newStatus = validateStatus(getUserInput("Enter new task status (New, In Progress, Completed)"))
        }

        // Update the task
        updateTask(idToUpdate, newTitle, newDescription, newPriority, newStatus)
    }

    // Display the tasks again
    displayTasks()

    // Exit the program
    exitProcess(0)
}
```

**Explanation:**

This Kotlin code creates a simple task manager application that allows users to add, remove, and update tasks. The tasks are stored in a mutable list, and the user can interact with the application through a command-line interface (CLI).

Here's a summary of the code:

- **Data Class:** The `Task` data class is used to represent a task. It has properties for the task's ID, title, description, priority, and status.

- **Constants and Variables:** The `MAX_TASKS` constant is used to limit the maximum number of tasks that can be stored in the list. The `tasks` variable is a mutable list that stores the tasks.

- **Functions:**
  - `addTask()`: Adds a new task to the list.
  - `removeTask()`: Removes a task from the list by its ID.
  - `updateTask()`: Updates a task in the list by its ID.
  - `displayTasks()`: Displays all the tasks in the list in a tabular format.
  - `getUserInput()`: Gets user input for a specific message.
  - `validateInteger()`: Validates user input for an integer and returns it, or -1 if the input is invalid.
  - `validatePriority()`: Validates user input for priority (1-5) and returns it, or -1 if the input is invalid.
  - `validateStatus()`: Validates user input for status (New, In Progress, Completed) and returns it, or an empty string if the input is invalid.

- **Main Function:**
  - The `main()` function is the entry point of the program.
  - It creates some sample tasks and displays them.
  - It then gets user input for a new task, adds it to the list, and displays the updated list.
  - It then gets user input for a task to remove, removes it from the list, and displays the updated list.
  - It then gets user input for a task to update, updates it in the list, and displays the updated list.
  - Finally, it exits the program.

This code provides a basic example of how to create a command-line application in Kotlin, utilizing data classes, functions, and user input validation.