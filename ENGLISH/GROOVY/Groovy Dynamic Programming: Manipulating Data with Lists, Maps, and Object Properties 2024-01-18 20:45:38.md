```groovy

// Script to demonstrate Groovy's dynamic capabilities

// Define a simple class
class Person {
    String name
    int age
}

// Create a list of Person objects
def people = [
    new Person(name: "John Doe", age: 30),
    new Person(name: "Jane Doe", age: 25),
    new Person(name: "Bob Smith", age: 40),
    new Person(name: "Mary Johnson", age: 35)
]

// Iterate over the list of people and print their names and ages
println "People:"
people.each { person ->
    println "Name: ${person.name}, Age: ${person.age}"
}

// Find the person with the oldest age
def oldestPerson = people.max { person -> person.age }
println "Oldest person:"
println "Name: ${oldestPerson.name}, Age: ${oldestPerson.age}"

// Find the sum of all the ages of the people
def totalAge = people.sum { person -> person.age }
println "Total age of all people: ${totalAge}"

// Group the people by their age
def peopleByAge = people.groupBy { person -> person.age }
println "People grouped by age:"
peopleByAge.each { age, people ->
    println "Age: ${age}"
    people.each { person ->
        println "  Name: ${person.name}"
    }
}

// Sort the list of people by their names
def sortedPeople = people.sort { person1, person2 -> person1.name <=> person2.name }
println "Sorted people by name:"
sortedPeople.each { person ->
    println "Name: ${person.name}, Age: ${person.age}"
}

```

Explanation:

* The `Person` class is defined with two properties: `name` and `age`.
* The `people` list is created with four `Person` objects.
* The `println` statement prints the names and ages of the people in the list.
* The `oldestPerson` variable is assigned the result of the `max` method, which finds the person with the oldest age.
* The `totalAge` variable is assigned the result of the `sum` method, which finds the sum of all the ages of the people in the list.
* The `peopleByAge` map is created by grouping the people in the list by their age using the `groupBy` method.
* The `sortedPeople` list is created by sorting the people in the list by their names using the `sort` method.