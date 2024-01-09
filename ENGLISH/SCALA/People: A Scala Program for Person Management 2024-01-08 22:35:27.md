```scala
// Create a class to represent a person
class Person(val name: String, val age: Int) {

    // Define a method to greet someone
    def greet(other: Person): String = {
        s"Hello, ${other.name}, my name is ${this.name} and I am ${this.age} years old."
    }

    // Define a method to calculate the age difference between two people
    def ageDifference(other: Person): Int = {
        Math.abs(this.age - other.age)
    }

    // Create a companion object for the Person class
    object Person {

        // Define a factory method to create a Person object from a name and age
        def apply(name: String, age: Int): Person = {
            new Person(name, age)
        }

        // Define a method to find the oldest person in a list of people
        def findOldest(people: List[Person]): Person = {
            people.maxBy(_.age)
        }

        // Define a method to find the youngest person in a list of people
        def findYoungest(people: List[Person]): Person = {
            people.minBy(_.age)
        }
    }
}

// Create a list of people
val people = List(
    Person("John", 25),
    Person("Mary", 30),
    Person("Bob", 40),
    Person("Alice", 20)
)

// Find the oldest and youngest people in the list
val oldestPerson = Person.findOldest(people)
val youngestPerson = Person.findYoungest(people)

// Print the results
println(s"The oldest person is ${oldestPerson.name} and they are ${oldestPerson.age} years old.")
println(s"The youngest person is ${youngestPerson.name} and they are ${youngestPerson.age} years old.")

// Create two people and greet each other
val person1 = Person("John", 25)
val person2 = Person("Mary", 30)

println(person1.greet(person2))
println(person2.greet(person1))

// Calculate the age difference between two people
val ageDifference = person1.ageDifference(person2)

println(s"The age difference between ${person1.name} and ${person2.name} is ${ageDifference} years.")
```

Explanation:

1. We define a class called `Person` with two properties: `name` and `age`.

2. We define a method called `greet` in the `Person` class to greet another person.

3. We define another method called `ageDifference` in the `Person` class to calculate the age difference between two people.

4. We create a companion object for the `Person` class, which contains factory methods to create `Person` objects from a name and age, and methods to find the oldest and youngest people in a list of people.

5. We create a list of `Person` objects.

6. We use the `Person` companion object's `findOldest` and `findYoungest` methods to find the oldest and youngest people in the list.

7. We print the results.

8. We create two `Person` objects and greet each other.

9. We calculate the age difference between the two people.

10. We print the result.