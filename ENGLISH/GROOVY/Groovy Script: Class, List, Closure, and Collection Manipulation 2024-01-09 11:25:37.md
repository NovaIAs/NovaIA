```groovy
// This is a Groovy script that demonstrates various features of the language.

// Define a class called "Person" with properties "name" and "age".
class Person {
    String name
    int age

    // Define a constructor that takes two parameters, "name" and "age".
    Person(String name, int age) {
        this.name = name
        this.age = age
    }

    // Define a method called "greet" that returns a string.
    String greet() {
        return "Hello, my name is ${name} and I am ${age} years old."
    }

    // Define a method called "isAdult" that returns a boolean.
    boolean isAdult() {
        return age >= 18
    }
}

// Create a list of people.
List<Person> people = [
    new Person("John", 25),
    new Person("Mary", 30),
    new Person("Bob", 16)
]

// Iterate over the list of people and print their names and ages.
println "List of people:"
people.each { person ->
    println "Name: ${person.name}, Age: ${person.age}"
}

// Find the person with the oldest age.
Person oldestPerson = people.max { person -> person.age }

// Print the name and age of the oldest person.
println "Oldest person:"
println "Name: ${oldestPerson.name}, Age: ${oldestPerson.age}"

// Filter the list of people to only include adults.
List<Person> adults = people.findAll { person -> person.isAdult() }

// Print the names and ages of the adults.
println "List of adults:"
adults.each { person ->
    println "Name: ${person.name}, Age: ${person.age}"
}

// Define a closure that takes a person as an argument and returns their name.
Closure getNameClosure = { Person person -> person.name }

// Use the closure to get the names of the people.
List<String> names = people.collect(getNameClosure)

// Print the names of the people.
println "Names of the people:"
names.each { name ->
    println name
}
```

Explanation:

The code starts by defining a class called `Person` with properties `name` and `age`. It also defines a constructor that takes two parameters, `name` and `age`, and two methods, `greet` and `isAdult`.

Next, a list of people is created using the `List` class. Each person is represented by an instance of the `Person` class.

The code then iterates over the list of people and prints their names and ages.

The code then finds the person with the oldest age using the `max` method.

The code then filters the list of people to only include adults using the `findAll` method.

The code then prints the names and ages of the adults.

The code then defines a closure that takes a person as an argument and returns their name.

The code then uses the closure to get the names of the people using the `collect` method.

Finally, the code prints the names of the people.