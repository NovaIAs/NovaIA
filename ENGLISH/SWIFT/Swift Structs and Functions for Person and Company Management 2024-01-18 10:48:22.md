```swift
// MARK: - Structs

struct Person {
    var firstName: String
    var lastName: String
    var age: Int
    var occupation: String

    init(firstName: String, lastName: String, age: Int, occupation: String) {
        self.firstName = firstName
        self.lastName = lastName
        self.age = age
        self.occupation = occupation
    }

    func fullName() -> String {
        return "\(firstName) \(lastName)"
    }
}

struct Company {
    var name: String
    var address: String
    var employees: [Person]

    init(name: String, address: String, employees: [Person]) {
        self.name = name
        self.address = address
        self.employees = employees
    }

    func totalEmployees() -> Int {
        return employees.count
    }
}

// MARK: - Functions

func printPersonInfo(person: Person) {
    print("Name: \(person.fullName())")
    print("Age: \(person.age)")
    print("Occupation: \(person.occupation)")
}

func printCompanyInfo(company: Company) {
    print("Name: \(company.name)")
    print("Address: \(company.address)")
    print("Total Employees: \(company.totalEmployees())")
    print("Employees:")
    for employee in company.employees {
        printPersonInfo(person: employee)
    }
}

// MARK: - Main

// Create a few people.
let john = Person(firstName: "John", lastName: "Smith", age: 30, occupation: "Software Engineer")
let jane = Person(firstName: "Jane", lastName: "Doe", age: 25, occupation: "Accountant")
let bob = Person(firstName: "Bob", lastName: "Jones", age: 40, occupation: "Manager")

// Create a company.
let acmeCorporation = Company(name: "Acme Corporation", address: "123 Main Street", employees: [john, jane, bob])

// Print the company info.
printCompanyInfo(company: acmeCorporation)
```

This code is a complex and differentiated Swift program that defines structs for representing people and companies, creates instances of those structs, and then prints information about the company and its employees. Here's a detailed explanation of the code:

1. **Structs**:
   - The `Person` struct is defined to represent a person with properties like first name, last name, age, and occupation.
   - The `Company` struct is defined to represent a company with properties like name, address, and a list of employees (which are instances of the `Person` struct).

2. **Functions**:
   - The `printPersonInfo` function is defined to print information about a person, including their full name, age, and occupation.
   - The `printCompanyInfo` function is defined to print information about a company, including its name, address, total number of employees, and a list of employees with their information.

3. **Main**:
   - A few instances of the `Person` struct are created, representing different people with different attributes.
   - An instance of the `Company` struct is created, representing a company with a name, address, and a list of employees (the previously created `Person` instances).
   - Finally, the `printCompanyInfo` function is called with the company instance as an argument, which prints information about the company and its employees.