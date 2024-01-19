```groovy
// This script demonstrates the use of various Groovy features,
// including closures, dynamic dispatch, and metaprogramming.

// Define a closure that takes a number and returns its square.
def square = { num -> num * num }

// Define a list of numbers.
def numbers = [1, 2, 3, 4, 5]

// Use the `collect()` method to apply the `square` closure to each
// element of the `numbers` list, resulting in a new list containing
// the squares of the original numbers.
def squares = numbers.collect(square)

// Define a Person class with a `name` property.
class Person {
    String name
}

// Define a list of people.
def people = [new Person(name: "John"), new Person(name: "Jane")]

// Use the `each()` method to iterate over the `people` list and print
// each person's name.
people.each { person -> println person.name }

// Use metaprogramming to add a `greet()` method to the `Person` class.
Person.metaClass.greet = { println "Hello, ${name}!" }

// Call the `greet()` method on each person in the `people` list.
people.each { person -> person.greet() }
```

Explanation:

1. We define a closure called `square` that takes a number as an argument and returns its square.


2. We define a list of numbers called `numbers`.


3. We use the `collect()` method on the `numbers` list to apply the `square` closure to each element of the list. This results in a new list called `squares` that contains the squares of the original numbers.


4. We define a `Person` class with a `name` property.


5. We define a list of people called `people`.


6. We use the `each()` method on the `people` list to iterate over each person and print their name.


7. We use metaprogramming to add a `greet()` method to the `Person` class. This allows us to call the `greet()` method on each person in the `people` list, which prints a greeting message.