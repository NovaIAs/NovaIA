```groovy
// Custom groovy script to manipulate and analyze data

// Define a list of numbers
def numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Define a map of names and ages
def namesAndAges = [
    "John"  : 25,
    "Mary"  : 30,
    "Bob"   : 22,
    "Alice" : 28
]

// Use the find() method to find the first even number in the list
def firstEvenNumber = numbers.find { it % 2 == 0 }

// Use the max() method to find the maximum value in the list
def maximumValue = numbers.max()

// Use the sum() method to calculate the sum of all the numbers in the list
def sumOfNumbers = numbers.sum()

// Use the collect() method to create a new list by multiplying each number in the original list by 2
def doubledNumbers = numbers.collect { it * 2 }

// Use the each() method to iterate over the list of numbers and print each number
numbers.each { println it }

// Use the each() method to iterate over the map of names and ages and print each name and age
namesAndAges.each { name, age -> println "$name is $age years old" }

// Use the find() method to find the first person who is older than 25 years old
def personOlderThan25 = namesAndAges.find { name, age -> age > 25 }

// Use the max() method to find the oldest person in the map
def oldestPerson = namesAndAges.max { name, age -> age }

// Use the sum() method to calculate the sum of all the ages in the map
def sumOfAges = namesAndAges.values().sum()

// Use the collect() method to create a new map by converting each name to uppercase
def namesInUpperCase = namesAndAges.collect { name, age -> [name.toUpperCase(), age] }

// Use the each() method to iterate over the new map and print each name and age
namesInUpperCase.each { name, age -> println "$name is $age years old" }
```

This groovy script demonstrates various operations on lists and maps, such as finding specific elements, performing calculations, and manipulating data. It showcases the use of lambda expressions and method chaining for concise and expressive coding.