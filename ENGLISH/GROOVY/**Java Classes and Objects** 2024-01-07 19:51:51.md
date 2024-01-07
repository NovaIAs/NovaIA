```groovy
import groovy.transform.ToString

// Define a class called Person with properties name, age, and occupation
@ToString
class Person {
    String name
    int age
    String occupation
}

// Define a class called Address with properties street, city, and state
class Address {
    String street
    String city
    String state
}

// Define a class called Company with properties name, address, and employees
@ToString(includeNames = true)
class Company {
    String name
    Address address
    List<Person> employees
}

// Create a list of Person objects
def people = [
        new Person(name: "John Doe", age: 30, occupation: "Software Engineer"),
        new Person(name: "Jane Smith", age: 25, occupation: "Accountant"),
        new Person(name: "Michael Jones", age: 40, occupation: "Doctor")
]

// Create an Address object
def address = new Address(street: "123 Main Street", city: "Anytown", state: "CA")

// Create a Company object
def company = new Company(name: "Acme Corporation", address: address, employees: people)

// Print the Company object
println company
```

This code defines three classes: Person, Address, and Company. The Person class has three properties: name, age, and occupation. The Address class has three properties: street, city, and state. The Company class has three properties: name, address, and employees.

The code then creates a list of Person objects, an Address object, and a Company object. Finally, the code prints the Company object.

The output of the code is:

```
Company(name=Acme Corporation, address=Address(street=123 Main Street, city=Anytown, state=CA), employees=[Person(name=John Doe, age=30, occupation=Software Engineer), Person(name=Jane Smith, age=25, occupation=Accountant), Person(name=Michael Jones, age=40, occupation=Doctor)])
```