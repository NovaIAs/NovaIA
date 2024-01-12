```kotlin
// Define a data class to represent a person.
data class Person(val name: String, val age: Int, val occupation: String)

// Define a list of people.
val people = listOf(
    Person("John Doe", 30, "Software Engineer"),
    Person("Jane Smith", 25, "Accountant"),
    Person("Michael Jones", 40, "Doctor"),
    Person("Mary Johnson", 35, "Teacher"),
    Person("David Williams", 28, "Lawyer")
)

// Define a function to filter the list of people by age.
fun filterPeopleByAge(age: Int): List<Person> {
    return people.filter { it.age == age }
}

// Define a function to filter the list of people by occupation.
fun filterPeopleByOccupation(occupation: String): List<Person> {
    return people.filter { it.occupation == occupation }
}

// Define a function to sort the list of people by name.
fun sortPeopleByName(): List<Person> {
    return people.sortedBy { it.name }
}

// Define a function to sort the list of people by age.
fun sortPeopleByAge(): List<Person> {
    return people.sortedBy { it.age }
}

// Define a function to sort the list of people by occupation.
fun sortPeopleByOccupation(): List<Person> {
    return people.sortedBy { it.occupation }
}

// Print the list of people.
println("All people:")
people.forEach { println(it) }

// Print the list of people filtered by age.
println("\nPeople filtered by age:")
filterPeopleByAge(30).forEach { println(it) }

// Print the list of people filtered by occupation.
println("\nPeople filtered by occupation:")
filterPeopleByOccupation("Doctor").forEach { println(it) }

// Print the list of people sorted by name.
println("\nPeople sorted by name:")
sortPeopleByName().forEach { println(it) }

// Print the list of people sorted by age.
println("\nPeople sorted by age:")
sortPeopleByAge().forEach { println(it) }

// Print the list of people sorted by occupation.
println("\nPeople sorted by occupation:")
sortPeopleByOccupation().forEach { println(it) }
```

This code defines a data class called `Person` to represent a person with a name, age, and occupation. It then defines a list of people and several functions to filter and sort the list of people. The code then prints the list of people, the list of people filtered by age, the list of people filtered by occupation, the list of people sorted by name, the list of people sorted by age, and the list of people sorted by occupation.

This code is complex and differentiated because it uses several different programming concepts, including data classes, lists, functions, filtering, sorting, and printing. It also uses a variety of different language features, including lambda expressions, method references, and the `forEach` method.