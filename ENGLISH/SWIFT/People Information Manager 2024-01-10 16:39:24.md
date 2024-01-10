```swift
import Foundation

// Define a custom struct to represent a person
struct Person {
    var firstName: String
    var lastName: String
    var age: Int
}

// Create an array of people
var people = [
    Person(firstName: "John", lastName: "Doe", age: 25),
    Person(firstName: "Jane", lastName: "Smith", age: 30),
    Person(firstName: "Michael", lastName: "Jones", age: 35)
]

// Define a function to print the names of all the people in the array
func printNames() {
    for person in people {
        print("\(person.firstName) \(person.lastName)")
    }
}

// Define a function to find the oldest person in the array
func oldestPerson() -> Person? {
    var oldestPerson: Person? = nil
    for person in people {
        if oldestPerson == nil || person.age > oldestPerson!.age {
            oldestPerson = person
        }
    }
    return oldestPerson
}

// Define a function to find the youngest person in the array
func youngestPerson() -> Person? {
    var youngestPerson: Person? = nil
    for person in people {
        if youngestPerson == nil || person.age < youngestPerson!.age {
            youngestPerson = person
        }
    }
    return youngestPerson
}

// Print the names of all the people in the array
printNames()

// Print the name of the oldest person in the array
if let oldestPerson = oldestPerson() {
    print("The oldest person is \(oldestPerson.firstName) \(oldestPerson.lastName)")
}

// Print the name of the youngest person in the array
if let youngestPerson = youngestPerson() {
    print("The youngest person is \(youngestPerson.firstName) \(youngestPerson.lastName)")
}
```

This code defines a custom struct called `Person`, which has three properties: `firstName`, `lastName`, and `age`. It then creates an array of `Person` objects and defines three functions: `printNames()`, `oldestPerson()`, and `youngestPerson()`. The `printNames()` function prints the names of all the people in the array, the `oldestPerson()` function finds the oldest person in the array, and the `youngestPerson()` function finds the youngest person in the array. The code then calls the `printNames()`, `oldestPerson()`, and `youngestPerson()` functions to print the names of all the people in the array, the name of the oldest person in the array, and the name of the youngest person in the array.