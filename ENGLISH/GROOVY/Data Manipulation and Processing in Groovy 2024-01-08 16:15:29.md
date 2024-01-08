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

    // Method to print the person's details
    void printDetails() {
        println "Name: $name"
        println "Age: $age"
        println "Occupation: $occupation"
    }
}

// Create a list of people
def people = [
    new Person("John Doe", 30, "Software Engineer"),
    new Person("Jane Smith", 25, "Accountant"),
    new Person("Michael Jones", 40, "Doctor"),
    new Person("Mary Johnson", 35, "Teacher"),
    new Person("Robert Brown", 50, "Lawyer")
]

// Print the details of each person
people.each { person ->
    person.printDetails()
    println()
}

// Find the oldest person
def oldestPerson = people.max { it.age }

// Print the oldest person's details
println "Oldest Person:"
oldestPerson.printDetails()

// Find the youngest person
def youngestPerson = people.min { it.age }

// Print the youngest person's details
println "Youngest Person:"
youngestPerson.printDetails()

// Find the average age of all people
def averageAge = people.sum { it.age } / people.size()

// Print the average age
println "Average Age: $averageAge"

// Find all people who are employed in a specific occupation
def specificOccupation = "Doctor"
def peopleWithSpecificOccupation = people.findAll { it.occupation == specificOccupation }

// Print the details of people with the specific occupation
println "People with $specificOccupation occupation:"
peopleWithSpecificOccupation.each { person ->
    person.printDetails()
    println()
}

// Sort the people by their age in ascending order
def sortedPeople = people.sort { it.age }

// Print the details of sorted people
println "Sorted People by Age (Ascending Order):"
sortedPeople.each { person ->
    person.printDetails()
    println()
}

// Sort the people by their age in descending order
sortedPeople = people.sort { -it.age }

// Print the details of sorted people
println "Sorted People by Age (Descending Order):"
sortedPeople.each { person ->
    person.printDetails()
    println()
}

```

This code demonstrates various programming concepts and features in Groovy, including:

1. **Classes and Objects**: The `Person` class represents a person with properties like name, age, and occupation. It defines a constructor to initialize these properties and a `printDetails()` method to print the person's details.

2. **Collections**: The `people` list contains instances of the `Person` class, representing a collection of people.

3. **Iteration**: The `each` method is used to iterate over the `people` list and print the details of each person.

4. **Finding Maximum and Minimum**: The `max` and `min` methods are used to find the oldest and youngest persons based on their ages.

5. **Aggregation**: The `sum` method is used to calculate the sum of ages of all people, and the result is divided by the size of the list to find the average age.

6. **Filtering**: The `findAll` method is used to find all people who are employed in a specific occupation.

7. **Sorting**: The `sort` method is used to sort the list of people by age in ascending and descending orders.

This code showcases the versatility and expressive nature of Groovy, allowing for concise and readable code that can handle complex operations on data structures.