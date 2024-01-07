```groovy
// Import the necessary libraries
import groovy.json.JsonSlurper
import groovy.transform.CompileStatic

// Define a data class to represent a person
@CompileStatic
class Person {
    String name
    int age
}

// Create a list of people
List<Person> people = [
        new Person(name: "John Doe", age: 30),
        new Person(name: "Jane Smith", age: 25),
        new Person(name: "Michael Jones", age: 35)
]

// Create a JSON string representing the list of people
String json = new JsonSlurper().encode(people)

// Print the JSON string
println json

// Parse the JSON string back into a list of people
List<Person> people2 = new JsonSlurper().parseText(json)

// Print the list of people
println people2

// Define a closure to filter the list of people by age
Closure<Boolean> filterClosure = { Person person -> person.age > 30 }

// Filter the list of people using the closure
List<Person> filteredPeople = people.findAll(filterClosure)

// Print the filtered list of people
println filteredPeople

// Define a closure to transform each person in the list into a map
Closure<Map> transformClosure = { Person person -> [name: person.name, age: person.age] }

// Transform the list of people using the closure
List<Map> transformedPeople = people.collect(transformClosure)

// Print the transformed list of people
println transformedPeople

// Define a closure to group the list of people by age
Closure<List> groupClosure = { Person person -> [person.age] }

// Group the list of people using the closure
Map<Integer, List<Person>> groupedPeople = people.groupBy(groupClosure)

// Print the grouped list of people
println groupedPeople

// Define a closure to sum the ages of the people in each group
Closure<Integer> reduceClosure = { Integer sum, Person person -> sum + person.age }

// Reduce the grouped list of people using the closure
Map<Integer, Integer> reducedPeople = groupedPeople.collectEntries { Integer age, List<Person> people -> [age: age, sum: people.sum(reduceClosure)] }

// Print the reduced list of people
println reducedPeople
```

This code demonstrates a variety of Groovy features, including:

* Data classes
* JSON serialization and deserialization
* Closures
* Higher-order functions
* Grouping and reducing collections

The code first defines a data class to represent a person. It then creates a list of people and converts it to a JSON string. The JSON string is then parsed back into a list of people.

The code then defines a closure to filter the list of people by age. The closure is used to filter the list of people, and the filtered list is printed.

The code then defines a closure to transform each person in the list into a map. The closure is used to transform the list of people, and the transformed list is printed.

The code then defines a closure to group the list of people by age. The closure is used to group the list of people, and the grouped list is printed.

Finally, the code defines a closure to sum the ages of the people in each group. The closure is used to reduce the grouped list of people, and the reduced list is printed.