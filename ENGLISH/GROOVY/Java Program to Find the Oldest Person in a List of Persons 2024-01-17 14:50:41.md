```groovy
// Define a class to represent a person
class Person {
    String name
    int age
    String occupation

    // Constructor to initialize the properties
    Person(String name, int age, String occupation) {
        this.name = name
        this.age = age
        this.occupation = occupation
    }

    // Define a method to print the person's details
    void printDetails() {
        println "Name: $name"
        println "Age: $age"
        println "Occupation: $occupation"
    }
}

// Create a list of persons
def persons = [
    new Person("John Doe", 30, "Software Engineer"),
    new Person("Jane Smith", 25, "Teacher"),
    new Person("Michael Jones", 40, "Doctor")
]

// Iterate over the list and print the details of each person
persons.each { person ->
    person.printDetails()
    println()
}

// Define a method to find the oldest person in the list
def findOldestPerson(List<Person> persons) {
    int maxAge = 0
    Person oldestPerson = null

    // Iterate over the list and find the person with the maximum age
    for (Person person : persons) {
        if (person.age > maxAge) {
            maxAge = person.age
            oldestPerson = person
        }
    }

    return oldestPerson
}

// Find the oldest person in the list
Person oldestPerson = findOldestPerson(persons)

// Print the details of the oldest person
println "Oldest Person:"
oldestPerson.printDetails()
```

This code defines a class `Person` to represent a person with properties `name`, `age`, and `occupation`. It also defines a method `printDetails()` to print the details of a person.

The code then creates a list of persons and iterates over the list to print the details of each person.

The code also defines a method `findOldestPerson()` to find the oldest person in a list of persons. This method iterates over the list and finds the person with the maximum age.

The code then calls the `findOldestPerson()` method to find the oldest person in the list of persons and prints the details of the oldest person.