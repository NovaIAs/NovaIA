```groovy
// A complex Groovy program that demonstrates various language features

// Define a class to represent a person
class Person {
    // Properties
    String name
    int age

    // Constructor
    Person(String name, int age) {
        this.name = name
        this.age = age
    }

    // Methods
    String toString() {
        return "Person[name=$name, age=$age]"
    }
}

// Define a list of people
List<Person> people = [
    new Person("John", 25),
    new Person("Mary", 30),
    new Person("Bob", 40),
    new Person("Alice", 50)
]

// Iterate over the list of people using the each method
people.each { person ->
    println person
}

// Filter the list of people by age using the findAll method
List<Person> filteredPeople = people.findAll { person ->
    person.age > 30
}

// Print the filtered list of people
filteredPeople.each { person ->
    println person
}

// Sort the list of people by age using the sort method
List<Person> sortedPeople = people.sort { person1, person2 ->
    person1.age <=> person2.age
}

// Print the sorted list of people
sortedPeople.each { person ->
    println person
}

// Group the list of people by age using the groupBy method
Map<Integer, List<Person>> groupedPeople = people.groupBy { person ->
    person.age
}

// Print the grouped list of people
groupedPeople.each { age, people ->
    println "Age: $age"
    people.each { person ->
        println "\t$person"
    }
}

// Find the sum of all the ages of the people in the list using the sum method
int totalAge = people.sum { person ->
    person.age
}

// Print the total age
println "Total age: $totalAge"

// Find the maximum age of the people in the list using the max method
int maxAge = people.max { person ->
    person.age
}

// Print the maximum age
println "Maximum age: $maxAge"

// Find the minimum age of the people in the list using the min method
int minAge = people.min { person ->
    person.age
}

// Print the minimum age
println "Minimum age: $minAge"

// Find the average age of the people in the list using the average method
double averageAge = people.average { person ->
    person.age
}

// Print the average age
println "Average age: $averageAge"

// Check if all the people in the list are over 30 years old using the every method
boolean allOver30 = people.every { person ->
    person.age > 30
}

// Print the result
println "All over 30: $allOver30"

// Check if any of the people in the list are under 30 years old using the any method
boolean anyUnder30 = people.any { person ->
    person.age < 30
}

// Print the result
println "Any under 30: $anyUnder30"

// Find the first person in the list who is over 30 years old using the find method
Person firstOver30 = people.find { person ->
    person.age > 30
}

// Print the first person over 30
println "First over 30: $firstOver30"

// Find the last person in the list who is under 30 years old using the findLast method
Person lastUnder30 = people.findLast { person ->
    person.age < 30
}

// Print the last person under 30
println "Last under 30: $lastUnder30"

// Find the index of the first person in the list who is over 30 years old using the findIndex method
int indexOfFirstOver30 = people.findIndex { person ->
    person.age > 30
}

// Print the index of the first person over 30
println "Index of first over 30: $indexOfFirstOver30"

// Find the index of the last person in the list who is under 30 years old using the findLastIndex method
int indexOfLastUnder30 = people.findLastIndex { person ->
    person.age < 30
}

// Print the index of the last person under 30
println "Index of last under 30: $indexOfLastUnder30"

// Partition the list of people into two lists: one containing people over 30 years old and one containing people under 30 years old using the partition method
List<List<Person>> partitionedPeople = people.partition { person ->
    person.age > 30
}

// Print the two lists
println "Partitioned people:"
partitionedPeople[0].each { person ->
    println "\tOver 30: $person"
}
partitionedPeople[1].each { person ->
    println "\tUnder 30: $person"
}

// Join the two lists of people into a single list using the flatten method
List<Person> flatPeople = partitionedPeople.flatten()

// Print the flattened list
println "Flattened people:"
flatPeople.each { person ->
    println "\t$person"
}

// Create a map from the list of people, using the name as the key and the age as the value, using the collectEntries method
Map<String, Integer> peopleMap = people.collectEntries { person ->
    [(person.name): person.age]
}

// Print the map
println "People map:"
peopleMap.each { name, age ->
    println "$name: $age"
}

// Create a string containing the names of all the people in the list, separated by commas, using the join method
String peopleNames = people.join(", ")