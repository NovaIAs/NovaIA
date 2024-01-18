```groovy
class ComplexGroovyCode {

    static void main(String[] args) {
        // Define a closure that takes a list of numbers and returns the sum of the numbers
        def sumClosure = { list -> list.sum() }

        // Create a list of numbers
        def numbers = [1, 2, 3, 4, 5]

        // Use the closure to calculate the sum of the numbers
        def sum = sumClosure(numbers)

        // Print the sum
        println "The sum of the numbers is $sum"


        // Define a method that takes a list of strings and returns a list of the strings in reverse order
        def reverseStringList = { list -> list.reverse() }

        // Create a list of strings
        def strings = ['a', 'b', 'c', 'd', 'e']

        // Use the method to reverse the order of the strings
        def reversedStrings = reverseStringList(strings)

        // Print the reversed list of strings
        println "The reversed list of strings is $reversedStrings"


        // Define a class that represents a person
        class Person {
            String name
            int age
        }

        // Create a list of people
        def people = [
            new Person(name: 'John', age: 25),
            new Person(name: 'Mary', age: 30),
            new Person(name: 'Bob', age: 35)
        ]

        // Use the find() method to find the first person in the list who is over 30 years old
        def personOver30 = people.find { it.age > 30 }

        // Print the name of the person over 30 years old
        println "The name of the person over 30 years old is ${personOver30.name}"


        // Define a regular expression that matches a valid email address
        def emailPattern = /^[\w-]+(\.[\w-]+)*@[\w-]+(\.[\w-]+)+$/

        // Create a list of email addresses
        def emailAddresses = ['john@example.com', 'mary@example.com', 'bob@example.com', 'invalid@example']

        // Use the findAll() method to find all the valid email addresses in the list
        def validEmailAddresses = emailAddresses.findAll { it =~ emailPattern }

        // Print the list of valid email addresses
        println "The list of valid email addresses is $validEmailAddresses"
    }
}
```

This code demonstrates a variety of complex features of the Groovy programming language, including:

* **Closures:** A closure is a block of code that can be passed as an argument to a method or function. Closures are similar to anonymous functions in other programming languages.

* **Method Definitions:** Groovy allows you to define methods within classes, as well as closures. This allows you to create more concise and readable code.

* **List Comprehension:** List comprehension is a shorthand way of creating a list of values. The syntax for list comprehension is similar to the syntax for a for loop, but it is more concise and readable.

* **Regular Expressions:** Groovy supports regular expressions, which are a powerful way to match and manipulate text.

* **Pattern Matching:** Pattern matching allows you to match a value against a pattern and then extract the matched values. The syntax for pattern matching is similar to the syntax for regular expressions, but it is more concise and readable.