```groovy
// This is a Groovy program that demonstrates the use of various Groovy features, including lambda expressions, closures, and method chaining.

// Define a list of integers
def numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Use a lambda expression to filter the list and return only the even numbers
def evenNumbers = numbers.findAll { it % 2 == 0 }

// Use a closure to calculate the sum of the even numbers
def sumOfEvenNumbers = evenNumbers.sum { it }

// Use method chaining to print the sum of the even numbers
println "Sum of even numbers: ${sumOfEvenNumbers}"

// Define a list of strings
def names = ["John", "Mary", "Bob", "Alice", "Tom"]

// Use a lambda expression to filter the list and return only the names that start with the letter "A"
def namesStartingWithA = names.findAll { it.startsWith("A") }

// Use a closure to concatenate the names starting with "A" into a single string
def concatenatedNames = namesStartingWithA.join(", ")

// Use method chaining to print the concatenated names
println "Concatenated names: ${concatenatedNames}"

// Define a list of maps, where each map represents a person with their name and age
def people = [
  [name: "John", age: 25],
  [name: "Mary", age: 30],
  [name: "Bob", age: 35],
  [name: "Alice", age: 40],
  [name: "Tom", age: 45]
]

// Use a lambda expression to filter the list and return only the people who are older than 30
def peopleOlderThan30 = people.findAll { it.age > 30 }

// Use a closure to calculate the average age of the people who are older than 30
def averageAge = peopleOlderThan30.sum { it.age } / peopleOlderThan30.size()

// Use method chaining to print the average age
println "Average age of people older than 30: ${averageAge}"
```

This code demonstrates the use of various Groovy features, including lambda expressions, closures, and method chaining.

The first part of the code defines a list of integers and uses a lambda expression to filter the list and return only the even numbers. The sum of the even numbers is then calculated using a closure and method chaining.

The second part of the code defines a list of strings and uses a lambda expression to filter the list and return only the names that start with the letter "A". The names starting with "A" are then concatenated into a single string using a closure and method chaining.

The third part of the code defines a list of maps, where each map represents a person with their name and age. A lambda expression is used to filter the list and return only the people who are older than 30. The average age of the people who are older than 30 is then calculated using a closure and method chaining.

This code is an example of how Groovy can be used to write concise and expressive code.