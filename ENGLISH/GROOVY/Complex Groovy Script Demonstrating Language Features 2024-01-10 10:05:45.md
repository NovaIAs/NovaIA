```groovy
// A complex Groovy script that demonstrates various features of the language

// Define a class with multiple constructors
class Person {
    String name
    int age

    // Primary constructor
    Person(String name, int age) {
        this.name = name
        this.age = age
    }

    // Secondary constructor
    Person(String name) {
        this(name, 0)
    }

    // Tertiary constructor
    Person() {
        this("John Doe", 0)
    }

    // Method to greet someone
    String greet(String otherName) {
        "Hello, $otherName! My name is $name."
    }
}

// Create a list of people
def people = [
    new Person("Alice", 25),
    new Person("Bob", 30),
    new Person("Carol", 35)
]

// Print the names of the people
people.each { person ->
    println person.name
}

// Find the oldest person
def oldestPerson = people.max { person -> person.age }

// Print the oldest person's name
println "The oldest person is ${oldestPerson.name}."

// Group the people by their age
def peopleByAge = people.groupBy { person -> person.age }

// Print the groups of people by age
peopleByAge.each { age, people ->
    println "People aged $age:"
    people.each { person ->
        println "\t$person.name"
    }
}

// Create a map of people's names and ages
def peopleMap = people.collectEntries { person ->
    [(person.name): person.age]
}

// Print the map of people's names and ages
peopleMap.each { name, age ->
    println "$name is $age years old."
}

// Create a closure that takes a string and returns a greeting
def greeter = { String otherName ->
    "Hello, $otherName! My name is $name."
}

// Call the closure with a name
println greeter.call("John")

// Define a method that takes a list of people and returns a list of their names
def getNames(List<Person> people) {
    people.collect { person -> person.name }
}

// Call the method with the list of people
def names = getNames(people)

// Print the list of names
println names

// Define a method that takes a list of people and returns a map of their names and ages
def getNamesAndAges(List<Person> people) {
    people.collectEntries { person ->
        [(person.name): person.age]
    }
}

// Call the method with the list of people
def namesAndAges = getNamesAndAges(people)

// Print the map of names and ages
println namesAndAges

// Define a method that takes a list of people and returns a list of their greetings
def getGreetings(List<Person> people) {
    people.collect { person -> person.greet("John") }
}

// Call the method with the list of people
def greetings = getGreetings(people)

// Print the list of greetings
println greetings
```

Explanation:

* The code defines a `Person` class with multiple constructors and a `greet()` method.
* It creates a list of `Person` objects and performs various operations on the list, such as printing the names of the people, finding the oldest person, grouping the people by age, and creating a map of people's names and ages.
* The code also defines a closure that takes a string and returns a greeting, and a method that takes a list of people and returns a list of their names.
* The code calls the closure and the method with the list of people and prints the results.
* The code also defines a method that takes a list of people and returns a map of their names and ages, and a method that takes a list of people and returns a list of their greetings.
* The code calls the methods with the list of people and prints the results.