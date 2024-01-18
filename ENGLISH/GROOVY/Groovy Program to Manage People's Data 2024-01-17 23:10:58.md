```groovy
// Define a class to represent a person
class Person {
    // Declare the properties of the class
    String name
    int age
    String occupation

    // Define a constructor for the class
    Person(String name, int age, String occupation) {
        this.name = name
        this.age = age
        this.occupation = occupation
    }

    // Define a method to return a string representation of the object
    @Override
    String toString() {
        return "Name: $name, Age: $age, Occupation: $occupation"
    }
}

// Define a list of people
List<Person> people = [
    new Person("John Doe", 25, "Software Engineer"),
    new Person("Jane Smith", 30, "Accountant"),
    new Person("Michael Jones", 35, "Doctor"),
    new Person("Sarah Miller", 40, "Teacher")
]

// Use a closure to find all people who are over 30 years old
List<Person> over30 = people.findAll { it.age > 30 }

// Print the names of the people who are over 30 years old
println "People over 30 years old:"
over30.each { println it.name }

// Use a closure to sort the list of people by age
List<Person> sortedByAge = people.sort { it.age }

// Print the names of the people sorted by age
println "\nPeople sorted by age:"
sortedByAge.each { println it.name }

// Use a closure to group the list of people by occupation
Map<String, List<Person>> groupedByOccupation = people.groupBy { it.occupation }

// Print the occupations and the names of the people in each occupation
println "\nPeople grouped by occupation:"
groupedByOccupation.each { occupation, people ->
    println "Occupation: $occupation"
    people.each { println "  $it.name" }
}
```

This code:

1. Defines a class called `Person` to represent a person.
2. Creates a list of people.
3. Uses a closure to find all people who are over 30 years old.
4. Prints the names of the people who are over 30 years old.
5. Uses a closure to sort the list of people by age.
6. Prints the names of the people sorted by age.
7. Uses a closure to group the list of people by occupation.
8. Prints the occupations and the names of the people in each occupation.