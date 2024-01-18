```swift
// Import necessary libraries
import UIKit
import CoreData
import Alamofire
import SwiftyJSON

// Define a custom struct to represent a person
struct Person {
    var name: String
    var age: Int
    var city: String
}

// Define a class to manage the core data stack
class CoreDataManager {
    static let sharedInstance = CoreDataManager()
    private let managedObjectModel: NSManagedObjectModel
    private let persistentStoreCoordinator: NSPersistentStoreCoordinator
    private let mainContext: NSManagedObjectContext
    
    // Initialize the core data manager
    private init() {
        // Get the managed object model from the bundle
        guard let modelURL = Bundle.main.url(forResource: "DataModel", withExtension: "momd") else {
            fatalError("Could not find DataModel.momd in the bundle")
        }
        managedObjectModel = NSManagedObjectModel(contentsOf: modelURL)!
        
        // Initialize the persistent store coordinator
        persistentStoreCoordinator = NSPersistentStoreCoordinator(managedObjectModel: managedObjectModel)
        
        // Get the application documents directory
        let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!
        
        // Create the persistent store
        let storeURL = documentsDirectory.appendingPathComponent("DataModel.sqlite")
        do {
            try persistentStoreCoordinator.addPersistentStore(ofType: NSSQLiteStoreType, configurationName: nil, at: storeURL, options: nil)
        } catch {
            fatalError("Could not create persistent store: \(error)")
        }
        
        // Create the main context
        mainContext = NSManagedObjectContext(concurrencyType: .mainQueueConcurrencyType)
        mainContext.persistentStoreCoordinator = persistentStoreCoordinator
    }
    
    // Save the context
    func saveContext() {
        if mainContext.hasChanges {
            do {
                try mainContext.save()
            } catch {
                fatalError("Could not save context: \(error)")
            }
        }
    }
    
    // Get all persons from the core data store
    func getPersons() -> [Person] {
        // Create a fetch request
        let fetchRequest: NSFetchRequest<NSManagedObject> = NSFetchRequest(entityName: "Person")
        
        // Execute the fetch request
        do {
            let results = try mainContext.fetch(fetchRequest) as! [Person]
            return results
        } catch {
            fatalError("Could not fetch persons: \(error)")
        }
    }
    
    // Add a new person to the core data store
    func addPerson(name: String, age: Int, city: String) {
        // Create a new managed object
        let person = NSManagedObject(entity: NSEntityDescription.entity(forEntityName: "Person", in: mainContext)!, insertInto: mainContext)
        
        // Set the values for the new object
        person.setValue(name, forKey: "name")
        person.setValue(age, forKey: "age")
        person.setValue(city, forKey: "city")
        
        // Save the context
        saveContext()
    }
    
    // Delete a person from the core data store
    func deletePerson(person: Person) {
        mainContext.delete(person)
        saveContext()
    }
}

// Define a class to handle the network requests
class NetworkManager {
    static let sharedInstance = NetworkManager()
    
    // Make a GET request to the specified URL
    func get(url: String, parameters: [String: String]?, completion: @escaping (Data?,