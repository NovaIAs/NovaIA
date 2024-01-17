```swift
// Import the necessary UIKit and Core Data frameworks
import UIKit
import CoreData

// Define a struct to represent a task
struct Task {
    var name: String
    var completed: Bool
}

// Create an instance of the Core Data stack
let appDelegate = UIApplication.shared.delegate as! AppDelegate
let managedContext = appDelegate.persistentContainer.viewContext

// Create a fetch request for the Task entity
let fetchRequest = NSFetchRequest<NSManagedObject>(entityName: "Task")

// Execute the fetch request and store the results in an array
do {
    let tasks = try managedContext.fetch(fetchRequest) as! [Task]
} catch let error as NSError {
    print("Could not fetch tasks: \(error), \(error.userInfo)")
}

// Create a new task using the Task struct
let newTask = Task(name: "New Task", completed: false)

// Convert the Task struct to an NSManagedObject
let taskEntity = NSEntityDescription.entity(forEntityName: "Task", in: managedContext)!
let taskManagedObject = NSManagedObject(entity: taskEntity, insertInto: managedContext)

// Set the values of the NSManagedObject using the Task struct
taskManagedObject.setValue(newTask.name, forKey: "name")
taskManagedObject.setValue(newTask.completed, forKey: "completed")

// Save the changes to the Core Data stack
do {
    try managedContext.save()
} catch let error as NSError {
    print("Could not save task: \(error), \(error.userInfo)")
}

// Create a new table view controller to display the tasks
let tasksTableViewController = UITableViewController()

// Set the title of the table view controller
tasksTableViewController.title = "Tasks"

// Create a new array to store the cells in the table view
var cells = [UITableViewCell]()

// Loop through the tasks and create a cell for each one
for task in tasks {
    let cell = UITableViewCell(style: .default, reuseIdentifier: "TaskCell")
    cell.textLabel?.text = task.name
    cell.accessoryType = task.completed ? .checkmark : .none
    cells.append(cell)
}

// Set the table view controller's data source and delegate
tasksTableViewController.tableView.dataSource = self
tasksTableViewController.tableView.delegate = self

// Present the table view controller
navigationController?.pushViewController(tasksTableViewController, animated: true)
```

This code demonstrates a more complex and differentiated scenario involving Core Data, structs, table view controllers, and user interface elements. It provides a more comprehensive example of how to manage data and display it in an iOS app.