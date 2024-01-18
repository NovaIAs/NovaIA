```swift
// Import necessary libraries
import UIKit
import CoreData

// Define a custom data structure to represent a task
struct Task: Decodable {
    let id: Int
    let title: String
    let description: String
    let priority: Int
    let dueDate: Date
    let completed: Bool
}

// Define a class to manage the tasks in a Core Data database
class TaskManager {

    // Create a singleton instance of the task manager
    static let shared = TaskManager()

    // Initialize the task manager with the Core Data stack
    private init() {
        guard let modelURL = Bundle.main.url(forResource: "TaskModel", withExtension: "momd") else {
            fatalError("Could not find the TaskModel.momd file.")
        }

        guard let mom = NSManagedObjectModel(contentsOf: modelURL) else {
            fatalError("Could not create the managed object model.")
        }

        let coordinator = NSPersistentStoreCoordinator(managedObjectModel: mom)

        let fileManager = FileManager.default
        let documentsURL = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first!

        let storeURL = documentsURL.appendingPathComponent("TaskModel.sqlite")

        do {
            try coordinator.addPersistentStore(ofType: NSSQLiteStoreType, configurationName: nil, at: storeURL, options: nil)
        } catch {
            fatalError("Could not add the persistent store.")
        }

        context = NSManagedObjectContext(concurrencyType: .mainQueueConcurrencyType)
        context.persistentStoreCoordinator = coordinator
    }

    // Get the managed object context
    private var context: NSManagedObjectContext!

    // Function to add a new task to the database
    func addTask(title: String, description: String, priority: Int, dueDate: Date, completed: Bool) {
        let taskEntity = NSEntityDescription.entity(forEntityName: "Task", in: context)!
        let task = NSManagedObject(entity: taskEntity, insertInto: context)

        task.setValue(title, forKey: "title")
        task.setValue(description, forKey: "description")
        task.setValue(priority, forKey: "priority")
        task.setValue(dueDate, forKey: "dueDate")
        task.setValue(completed, forKey: "completed")

        do {
            try context.save()
        } catch {
            fatalError("Could not save the task to the database.")
        }
    }

    // Function to fetch all tasks from the database
    func fetchTasks() -> [Task] {
        let fetchRequest: NSFetchRequest<TaskEntity> = TaskEntity.fetchRequest()

        do {
            let results = try context.fetch(fetchRequest)

            return results.map { taskEntity in
                Task(id: taskEntity.id, title: taskEntity.title, description: taskEntity.description, priority: taskEntity.priority, dueDate: taskEntity.dueDate, completed: taskEntity.completed)
            }
        } catch {
            fatalError("Could not fetch the tasks from the database.")
        }
    }

    // Function to update a task in the database
    func updateTask(task: Task) {
        let fetchRequest: NSFetchRequest<TaskEntity> = TaskEntity.fetchRequest()
        fetchRequest.predicate = NSPredicate(format: "id == %d", task.id)

        do {
            let results = try context.fetch(fetchRequest)

            if let taskEntity = results.first {
                taskEntity.setValue(task.title, forKey: "title")
                taskEntity.setValue(task.description, forKey: "description")
                taskEntity.setValue(task.priority, forKey: "priority")
                taskEntity.setValue(task.dueDate, forKey: "dueDate")
                taskEntity.setValue(task.completed, forKey: "completed")

                try context.save()
            }
        } catch {
            fatalError("Could not update the task in the database.")
        }
    }

    // Function to delete a task from the database
    func deleteTask(task: Task) {
        let fetchRequest: NSFetchRequest<TaskEntity> = TaskEntity.fetchRequest()
        fetchRequest.predicate = NSPredicate(format: "id == %d", task.id)

        do {
            let results = try context.fetch(fetchRequest)

            if let taskEntity = results.first {
                context.delete(taskEntity)

                try context.save()
            }
        } catch {
            fatalError("Could not delete the task from the database.")
        }
    }
}

// Define a class to represent the table view controller for displaying the tasks
class TasksViewController: UITableViewController {

    // Array to store the tasks
    private var tasks: [Task] = []

    // Function to load the tasks from the database when the view controller is loaded
    override func viewDidLoad() {
        super.viewDidLoad()

        tasks = TaskManager.shared.fetchTasks()
    }

    // Function to add a new task when the user taps the add button
    @IBAction func addTask(_ sender: Any) {
        let alertController = UIAlertController(title: "Add Task", message: "Enter the details of the task.", preferredStyle: .alert)

        alertController.addTextField { (textField) in
            textField.placeholder = "Title"
        }

        alertController.addTextField { (textField) in
            textField.placeholder = "Description"
        }

        alertController.addTextField { (textField) in
            textField.placeholder = "Priority (1-5)"
        }

        alertController.addTextField { (textField) in
            textField.placeholder = "Due Date (yyyy-MM-dd)"
        }

        alertController.addTextField { (textField) in
            textField.placeholder = "Completed (true/false)"
        }

        let saveAction = UIAlertAction(title: "Save", style: .default) { (action) in
            // Get the text fields from the alert controller
            let titleTextField = alertController.textFields![0]
            let descriptionTextField = alertController.textFields![1]
