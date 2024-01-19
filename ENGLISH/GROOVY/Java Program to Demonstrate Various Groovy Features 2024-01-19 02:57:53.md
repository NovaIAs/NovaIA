```groovy
import groovy.transform.CompileDynamic
import groovy.transform.ToString

// Define a class to represent a person
@ToString
@CompileDynamic
class Person {
    String firstName
    String lastName
    int age
}

// Create a list of people
def people = [
    new Person(firstName: "John", lastName: "Doe", age: 25),
    new Person(firstName: "Jane", lastName: "Doe", age: 22),
    new Person(firstName: "Bob", lastName: "Smith", age: 30)
]

// Use Groovy's built-in method `collect` to create a list of the people's full names
def fullNames = people.collect { "$it.firstName $it.lastName" }

// Print the list of full names
println fullNames

// Use Groovy's built-in method `groupBy` to group the people by their age
def peopleByAge = people.groupBy { it.age }

// Print the map of people grouped by age
println peopleByAge

// Use Groovy's built-in method `findAll` to find the people who are over 25 years old
def over25 = people.findAll { it.age > 25 }

// Print the list of people who are over 25 years old
println over25

// Use Groovy's built-in method `eachWithIndex` to iterate over the list of people and print their index and full name
people.eachWithIndex { person, index ->
    println "Person $index: $person"
}

// Use Groovy's built-in method `find` to find the first person who is over 25 years old
def firstOver25 = people.find { it.age > 25 }

// Print the full name of the first person who is over 25 years old
println "First person over 25: $firstOver25.firstName $firstOver25.lastName"

// Use Groovy's built-in method `max` to find the oldest person
def oldestPerson = people.max { it.age }

// Print the full name of the oldest person
println "Oldest person: $oldestPerson.firstName $oldestPerson.lastName"

// Use Groovy's built-in method `min` to find the youngest person
def youngestPerson = people.min { it.age }

// Print the full name of the youngest person
println "Youngest person: $youngestPerson.firstName $youngestPerson.lastName"
```

Explanation:

1. We define a class called `Person` to represent a person with properties like `firstName`, `lastName`, and `age`.

2. We create a list of `Person` objects.

3. We use Groovy's built-in method `collect` to create a list of the people's full names.

4. We use Groovy's built-in method `groupBy` to group the people by their age.

5. We use Groovy's built-in method `findAll` to find the people who are over 25 years old.

6. We use Groovy's built-in method `eachWithIndex` to iterate over the list of people and print their index and full name.

7. We use Groovy's built-in method `find` to find the first person who is over 25 years old.

8. We use Groovy's built-in method `max` to find the oldest person.

9. We use Groovy's built-in method `min` to find the youngest person.