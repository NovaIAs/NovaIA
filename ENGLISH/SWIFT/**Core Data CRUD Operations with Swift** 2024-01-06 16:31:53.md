```swift
// Import necessary frameworks
import Foundation
import UIKit
import CoreData

// Define a custom error type for data access errors
enum DataAccessError: Error {
    case entityNotFound
    case attributeNotFound
    case saveFailed
}

// Define a protocol for data access objects
protocol DataAccessObject {
    associatedtype EntityType: NSManagedObject
    
    func create() throws -> EntityType
    func update(_ entity: EntityType) throws
    func delete(_ entity: EntityType) throws
    func fetchAll() throws -> [EntityType]
    func fetch(byID id: NSManagedObjectID) throws -> EntityType
}

// Define a concrete data access object for a specific entity
class PersonDAO: DataAccessObject {
    typealias EntityType = Person
    
    // Create a new person entity
    func create() throws -> Person {
        let context = AppDelegate.shared.persistentContainer.viewContext
        let person = Person(context: context)
        return person
    }
    
    // Update an existing person entity
    func update(_ person: Person) throws {
        let context = AppDelegate.shared.persistentContainer.viewContext
        context.update(person)
        try context.save()
    }
    
    // Delete an existing person entity
    func delete(_ person: Person) throws {
        let context = AppDelegate.shared.persistentContainer.viewContext
        context.delete(person)
        try context.save()
    }
    
    // Fetch all person entities
    func fetchAll() throws -> [Person] {
        let context = AppDelegate.shared.persistentContainer.viewContext
        let fetchRequest: NSFetchRequest<Person> = Person.fetchRequest()
        return try context.fetch(fetchRequest)
    }
    
    // Fetch a person entity by its ID
    func fetch(byID id: NSManagedObjectID) throws -> Person {
        let context = AppDelegate.shared.persistentContainer.viewContext
        return try context.existingObject(with: id) as! Person
    }
}

// Define a UIViewController for managing person data
class PersonViewController: UIViewController {
    
    // Outlets for UI elements
    @IBOutlet weak var nameTextField: UITextField!
    @IBOutlet weak var ageTextField: UITextField!
    @IBOutlet weak var tableView: UITableView!
    
    // Data access object for person entities
    private let personDAO = PersonDAO()
    
    // Array to store fetched person entities
    private var people = [Person]()
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        // Fetch all person entities and update the table view
        do {
            people = try personDAO.fetchAll()
            tableView.reloadData()
        } catch {
            // Handle data access error
            print("Error fetching person entities: \(error)")
        }
    }
    
    // Action for the "Save" button
    @IBAction func saveButtonTapped(_ sender: UIButton) {
        
        // Create a new person entity
        do {
            let person = try personDAO.create()
            person.name = nameTextField.text
            person.age = Int16(ageTextField.text ?? "0")!
            
            // Update the table view
            people.append(person)
            tableView.reloadData()
            
            // Clear the text fields
            nameTextField.text = ""
            ageTextField.text = ""
        } catch {
            // Handle data access error
            print("Error creating person entity: \(error)")
        }
    }
    
    // Action for the "Delete" button
    @IBAction func deleteButtonTapped(_ sender: UIButton) {
        
        // Delete the selected person entity
        let indexPath = tableView.indexPathForSelectedRow!
        let person = people[indexPath.row]
        
        do {
            try personDAO.delete(person)
            
            // Update the table view
            people.remove(at: indexPath.row)
            tableView.deleteRows(at: [indexPath], with: .automatic)
        } catch {
            // Handle data access error
            print("Error deleting person entity: \(error)")
        }
    }
}

// Table view data source and delegate methods
extension PersonViewController: UITableViewDataSource, UITableViewDelegate {
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return people.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "PersonCell", for: indexPath)
        
        let person = people[indexPath.row]
        cell.textLabel?.text = person.name
        cell.detailTextLabel?.text = "\(person.age)"
        
        return cell
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let person = people[indexPath.row]
        
        // Pre-populate the text fields with the selected person's data
        nameTextField.text = person.name
        ageTextField.text = "\(person.age)"
    }
}

// AppDelegate for managing the persistent container
@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
    
    static let shared = UIApplication.shared.delegate as! AppDelegate
    
    lazy var persistentContainer: NSPersistentContainer = {
        
        let container = NSPersistentContainer(name: "DataModel")
        container.loadPersistentStores(completionHandler: { (storeDescription, error) in
            if let error = error {
                fatalError("Unresolved error \(error)")
            }
        })
        return container
    }()
    
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
        return true
    }
}
```

Explanation:

1. **Core Data Model:** We define a Core Data model with an entity called "Person" that has two attributes: "name" and "age".

2. **DataAccessObject Protocol:** We create a protocol `DataAccessObject` that defines a set of CRUD operations (create, update, delete, fetch all, and fetch by ID) for a generic entity type.

3. **PersonDAO:** We create a concrete implementation of the `DataAccessObject` protocol for the `Person` entity. This class provides methods to create, update, delete, fetch all, and fetch a person by its ID.

4. **PersonViewController:** We create a UIViewController for managing person data. It has text fields for entering a person's name and age, a table view to display a list of people, and buttons for saving and deleting people.

5. **Table View Data Source and Delegate Methods:** We implement the `UITableViewDataSource` and `UITableViewDelegate` protocols to populate the table view with person data and handle user interactions.

6. **AppDelegate:** We define an `AppDelegate` class that manages the Core Data persistent container and provides easy access to it from anywhere in the app.

This code demonstrates how to use Core Data to manage data in a Swift application, including creating, updating, deleting, and fetching data from a persistent store. It also includes a user interface for interacting with the data.