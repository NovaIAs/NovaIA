```groovy
// Create a list of strings
def names = ['Alice', 'Bob', 'Carol', 'Dave', 'Eve']

// Create a map of names to ages
def ages = [Alice: 21, Bob: 25, Carol: 28, Dave: 32, Eve: 35]

// Create a closure that takes a name and age and returns a string
def formatNameAndAge = { String name, Integer age -> "$name is $age years old." }

// Use the closure to create a list of strings
def nameAndAgeStrings = names.collect(formatNameAndAge)

// Print the list of strings
println nameAndAgeStrings

// Create a list of integers
def numbers = [1, 2, 3, 4, 5]

// Create a closure that takes a number and returns its square
def square = { Integer number -> number * number }

// Use the closure to create a list of integers
def squaredNumbers = numbers.collect(square)

// Print the list of integers
println squaredNumbers

// Create a list of strings
def words = ['apple', 'banana', 'cherry', 'durian', 'elderberry']

// Create a closure that takes a word and returns its length
def wordLength = { String word -> word.length() }

// Use the closure to create a list of integers
def wordLengths = words.collect(wordLength)

// Print the list of integers
println wordLengths

// Create a list of maps
def people = [
  [name: 'Alice', age: 21],
  [name: 'Bob', age: 25],
  [name: 'Carol', age: 28],
  [name: 'Dave', age: 32],
  [name: 'Eve', age: 35]
]

// Create a closure that takes a map and returns the person's name
def getName = { Map person -> person.name }

// Use the closure to create a list of strings
def names = people.collect(getName)

// Print the list of strings
println names

// Create a closure that takes a map and returns the person's age
def getAge = { Map person -> person.age }

// Use the closure to create a list of integers
def ages = people.collect(getAge)

// Print the list of integers
println ages

// Create a closure that takes a map and returns a string with the person's name and age
def formatPerson = { Map person -> "${person.name} is ${person.age} years old." }

// Use the closure to create a list of strings
def personStrings = people.collect(formatPerson)

// Print the list of strings
println personStrings
```

This code demonstrates a variety of ways to use closures in Groovy.

The first example creates a list of strings by using a closure to format each name and age.

The second example creates a list of integers by using a closure to square each number.

The third example creates a list of integers by using a closure to find the length of each word.

The fourth example creates a list of strings by using a closure to get the name of each person.

The fifth example creates a list of integers by using a closure to get the age of each person.

The sixth example creates a list of strings by using a closure to format the name and age of each person.

This code demonstrates how closures can be used to make Groovy code more concise and readable.