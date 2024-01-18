// Create a class to represent a car.
class Car {
    // Define properties for the car.
    var make: String
    var model: String
    var year: Int
    var color: String
    var mileage: Double
    
    // Initialize the car with the given values.
    init(make: String, model: String, year: Int, color: String, mileage: Double) {
        self.make = make
        self.model = model
        self.year = year
        self.color = color
        self.mileage = mileage
    }
    
    // Define a method to print the car's information.
    func printInfo() {
        print("Make: \(make)")
        print("Model: \(model)")
        print("Year: \(year)")
        print("Color: \(color)")
        print("Mileage: \(mileage)")
    }
}

// Create an array of cars.
var cars = [
    Car(make: "Honda", model: "Civic", year: 2016, color: "Red", mileage: 25000),
    Car(make: "Toyota", model: "Camry", year: 2018, color: "Blue", mileage: 30000),
    Car(make: "Ford", model: "Mustang", year: 2019, color: "Black", mileage: 15000)
]

// Print the information for each car in the array.
for car in cars {
    car.printInfo()
    print()
}

// Create a class to represent a person.
class Person {
    // Define properties for the person.
    var firstName: String
    var lastName: String
    var age: Int
    var occupation: String
    var hobbies: [String]
    
    // Initialize the person with the given values.
    init(firstName: String, lastName: String, age: Int, occupation: String, hobbies: [String]) {
        self.firstName = firstName
        self.lastName = lastName
        self.age = age
        self.occupation = occupation
        self.hobbies = hobbies
    }
    
    // Define a method to print the person's information.
    func printInfo() {
        print("First Name: \(firstName)")
        print("Last Name: \(lastName)")
        print("Age: \(age)")
        print("Occupation: \(occupation)")
        print("Hobbies:")
        for hobby in hobbies {
            print("- \(hobby)")
        }
    }
}

// Create an array of people.
var people = [
    Person(firstName: "John", lastName: "Doe", age: 25, occupation: "Software Engineer", hobbies: ["Coding", "Hiking", "Reading"]),
    Person(firstName: "Jane", lastName: "Smith", age: 30, occupation: "Doctor", hobbies: ["Running", "Cooking", "Traveling"]),
    Person(firstName: "Michael", lastName: "Jones", age: 35, occupation: "Teacher", hobbies: ["Playing guitar", "Photography", "Gardening"])
]

// Print the information for each person in the array.
for person in people {
    person.printInfo()
    print()
}

// Create a class to represent a company.
class Company {
    // Define properties for the company.
    var name: String
    var address: String
    var phone: String
    var employees: [Person]
    
    // Initialize the company with the given values.
    init(name: String, address: String, phone: String, employees: [Person]) {
        self.name = name
        self.address = address
        self.phone = phone
        self.employees = employees
    }
    
    // Define a method to print the company's information.
    func printInfo() {
        print("Name: \(name)")
        print("Address: \(address)")
        print("Phone: \(phone)")
        print("Employees:")
        for employee in employees {
            print("- \(employee.firstName) \(employee.lastName)")
        }
    }
}

// Create an array of companies.
var companies = [
    Company(name: "Acme Corporation", address: "123 Main Street, Anytown, CA 12345", phone: "1-800-555-1212", employees: people),
    Company(name: "XYZ Company", address: "456 Elm Street, Anytown, CA 12345", phone: "1-800-555-2323", employees: people),
    Company(name: "ABC Company", address: "789 Oak Street, Anytown, CA 12345", phone: "1-800-555-3434", employees: people)
]

// Print the information for each company in the array.
for company in companies {
    company.printInfo()
    print()
}