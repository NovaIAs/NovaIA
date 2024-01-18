```groovy
// Define a class Person with name and age
class Person {
    String name
    int age
}

// Define a list of people
def people = [
    new Person(name: "John Doe", age: 25),
    new Person(name: "Jane Doe", age: 22),
    new Person(name: "Jack Smith", age: 30),
    new Person(name: "Jill Smith", age: 28)
]

// Group people by age using the groupBy method
def peopleByAge = people.groupBy { it.age }

// Print the names of people in each age group
peopleByAge.each { age, people ->
    println "People aged $age:"
    people.each { person ->
        println "\t${person.name}"
    }
}

// Find the oldest and youngest people using the max and min methods
def oldestPerson = people.max { it.age }
def youngestPerson = people.min { it.age }

// Print the names of the oldest and youngest people
println "Oldest person: ${oldestPerson.name}"
println "Youngest person: ${youngestPerson.name}"

// Calculate the total age of all people using the sum method
def totalAge = people.sum { it.age }

// Print the total age
println "Total age of all people: ${totalAge}"

// Calculate the average age of all people using the average method
def averageAge = people.average { it.age }

// Print the average age
println "Average age of all people: ${averageAge}"

// Find the person with the longest name using the maxBy method
def personWithLongestName = people.maxBy { it.name.length() }

// Print the name of the person with the longest name
println "Person with the longest name: ${personWithLongestName.name}"

// Find the person with the shortest name using the minBy method
def personWithShortestName = people.minBy { it.name.length() }

// Print the name of the person with the shortest name
println "Person with the shortest name: ${personWithShortestName.name}"

// Find the people with names that start with the letter "J" using the findAll method
def peopleWithNameStartingWithJ = people.findAll { it.name.startsWith("J") }

// Print the names of the people with names that start with the letter "J"
println "People with names that start with the letter J:"
peopleWithNameStartingWithJ.each { person ->
    println "\t${person.name}"
}

// Find the people who are over 25 years old using the findAll method
def peopleOver25 = people.findAll { it.age > 25 }

// Print the names of the people who are over 25 years old
println "People who are over 25 years old:"
peopleOver25.each { person ->
    println "\t${person.name}"
}

// Sort the people by name using the sort method
def peopleSortedByName = people.sort { it.name }

// Print the names of the people sorted by name
println "People sorted by name:"
peopleSortedByName.each { person ->
    println "\t${person.name}"
}

// Sort the people by age using the sort method
def peopleSortedByAge = people.sort { it.age }

// Print the names of the people sorted by age
println "People sorted by age:"
peopleSortedByAge.each { person ->
    println "\t${person.name}"
}
```

This code demonstrates various operations on a list of people, including grouping by age, finding the oldest and youngest people, calculating the total and average age, finding the person with the longest and shortest name, finding people with names that start with a certain letter, finding people who are over a certain age, sorting people by name and age, and printing the results.

The code uses various Groovy features such as closures, list comprehensions, and method chaining to achieve these operations in a concise and readable way.