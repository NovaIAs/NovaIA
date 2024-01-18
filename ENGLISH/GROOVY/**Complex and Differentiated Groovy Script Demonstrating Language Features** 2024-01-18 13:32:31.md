```groovy
// This Groovy script is a complex and differentiated code that will hardly be repeated again.

// Define a class called ComplexCode.
class ComplexCode {

    // Define a method called main, which is the entry point of the script.
    static void main(String[] args) {
        // Create a list of numbers.
        def numbers = [1, 2, 3, 4, 5]

        // Create a map of names and ages.
        def namesAndAges = [
                "John": 25,
                "Mary": 30,
                "Bob": 35
        ]

        // Iterate over the list of numbers and print each number.
        numbers.each { number ->
            println "Number: $number"
        }

        // Iterate over the map of names and ages and print each name and age.
        namesAndAges.each { name, age ->
            println "Name: $name, Age: $age"
        }

        // Create a closure that takes a number and returns its square.
        def square = { number ->
            number * number
        }

        // Use the closure to square each number in the list of numbers.
        def squaredNumbers = numbers.collect(square)

        // Print the squared numbers.
        println "Squared numbers: $squaredNumbers"

        // Create a class called Person.
        class Person {

            // Define the properties of the Person class.
            String name
            int age

            // Define a constructor for the Person class.
            Person(String name, int age) {
                this.name = name
                this.age = age
            }

            // Define a method called toString that returns a string representation of the Person object.
            @Override
            String toString() {
                "Person{name='$name', age=$age}"
            }
        }

        // Create a list of Person objects.
        def people = [
                new Person("John", 25),
                new Person("Mary", 30),
                new Person("Bob", 35)
        ]

        // Iterate over the list of Person objects and print each person.
        people.each { person ->
            println person
        }
    }
}
```

**Explanation:**

This Groovy script is a complex and differentiated code that will hardly be repeated again. It demonstrates various features of the Groovy language, including:

* **Lists:** Groovy lists are a dynamic and versatile data structure that can store elements of any type. In this script, we create a list of numbers and a list of Person objects.
* **Maps:** Groovy maps are a collection of key-value pairs, where the keys can be of any type. In this script, we create a map of names and ages.
* **Closures:** Groovy closures are blocks of code that can be passed around and executed later. In this script, we create a closure that takes a number and returns its square.
* **Classes:** Groovy classes are a powerful way to organize code and data. In this script, we create a Person class that represents a person's name and age.
* **Iteration:** Groovy provides several ways to iterate over collections, including the each method. In this script, we iterate over the list of numbers, the map of names and ages, and the list of Person objects.

This script is a complex and differentiated example of Groovy code, and it demonstrates the power and flexibility of the language.