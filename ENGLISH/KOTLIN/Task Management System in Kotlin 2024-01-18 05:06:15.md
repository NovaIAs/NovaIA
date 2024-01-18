```kotlin
// Import necessary libraries
import java.util.*
import kotlin.collections.HashMap

// Define a data class to represent a task
data class Task(val id: Int, val title: String, val description: String, val priority: Int, val status: String)

// Define a function to generate a list of tasks
fun generateTaskList(): List<Task> {
    val tasks = mutableListOf<Task>()
    for (i in 1..100) {
        tasks.add(
            Task(
                i,
                "Task $i",
                "This is task $i",
                (1..5).random(),
                when ((1..3).random()) {
                    1 -> "New"
                    2 -> "In Progress"
                    else -> "Completed"
                }
            )
        )
    }
    return tasks
}

// Define a function to print the list of tasks
fun printTaskList(tasks: List<Task>) {
    println("Task List:")
    tasks.forEach { task ->
        println("ID: ${task.id}, Title: ${task.title}, Description: ${task.description}, Priority: ${task.priority}, Status: ${task.status}")
    }
}

// Define a function to sort the list of tasks by priority
fun sortByPriority(tasks: List<Task>): List<Task> {
    return tasks.sortedBy { it.priority }
}

// Define a function to filter the list of tasks by status
fun filterByStatus(tasks: List<Task>, status: String): List<Task> {
    return tasks.filter { it.status == status }
}

// Define a function to group the list of tasks by status
fun groupByStatus(tasks: List<Task>): Map<String, List<Task>> {
    return tasks.groupBy { it.status }
}

// Define a function to find the task with the highest priority
fun findHighestPriorityTask(tasks: List<Task>): Task? {
    return tasks.maxByOrNull { it.priority }
}

// Define a function to find the task with the lowest priority
fun findLowestPriorityTask(tasks: List<Task>): Task? {
    return tasks.minByOrNull { it.priority }
}

// Define a function to count the number of tasks with a given status
fun countTasksByStatus(tasks: List<Task>, status: String): Int {
    return tasks.count { it.status == status }
}

// Define a function to calculate the average priority of the tasks
fun calculateAveragePriority(tasks: List<Task>): Double {
    return tasks.sumOf { it.priority }.toDouble() / tasks.size
}

// Define a function to generate a random task
fun generateRandomTask(): Task {
    return Task(
        (1..1000).random(),
        "Task ${(1..100).random()}",
        "This is task ${(1..100).random()}",
        (1..5).random(),
        when ((1..3).random()) {
            1 -> "New"
            2 -> "In Progress"
            else -> "Completed"
        }
    )
}

// Create a list of tasks
val tasks = generateTaskList()

// Print the list of tasks
printTaskList(tasks)

// Sort the list of tasks by priority
val sortedTasks = sortByPriority(tasks)

// Filter the list of tasks by status
val newTasks = filterByStatus(tasks, "New")

// Group the list of tasks by status
val groupedTasks = groupByStatus(tasks)

// Find the task with the highest priority
val highestPriorityTask = findHighestPriorityTask(tasks)

// Find the task with the lowest priority
val lowestPriorityTask = findLowestPriorityTask(tasks)

// Count the number of tasks with a given status
val countNewTasks = countTasksByStatus(tasks, "New")

// Calculate the average priority of the tasks
val averagePriority = calculateAveragePriority(tasks)

// Generate a random task
val randomTask = generateRandomTask()

// Print the sorted list of tasks
println("Sorted Task List:")
printTaskList(sortedTasks)

// Print the list of new tasks
println("New Tasks:")
printTaskList(newTasks)

// Print the grouped tasks
println("Grouped Tasks:")
groupedTasks.forEach { (status, tasks) ->
    println("Status: $status")
    printTaskList(tasks)
}

// Print the task with the highest priority
println("Highest Priority Task:")
println(highestPriorityTask)

// Print the task with the lowest priority
println("Lowest Priority Task:")
println(lowestPriorityTask)

// Print the count of new tasks
println("Count of New Tasks: $countNewTasks")

// Print the average priority of the tasks
println("Average Priority: $averagePriority")

// Print the random task
println("Random Task:")
println(randomTask)