```groovy
import groovy.time.TimeCategory
import groovy.json.JsonSlurper
import groovy.xml.StreamingMarkupBuilder

// Define a complex object with nested properties
def complexObject = [
    name: 'John Doe',
    age: 30,
    address: [
        street: '123 Main Street',
        city: 'Anytown',
        state: 'CA',
        zip: '12345'
    ],
    phoneNumbers: ['(123) 456-7890', '(456) 789-0123'],
    emailAddresses: ['john.doe@example.com', 'jdoe@example.net'],
    hobbies: ['hiking', 'biking', 'reading']
]

// Use the TimeCategory to format a date
def now = new Date()
def formattedDate = now.format('yyyy-MM-dd HH:mm:ss')

// Parse a JSON string into a Groovy object
def json = '{"name": "Jane Doe", "age": 25}'
def jsonObject = new JsonSlurper().parseText(json)

// Generate XML using the StreamingMarkupBuilder
def xml = new StreamingMarkupBuilder().bind {
    person {
        name('Jane Doe')
        age(25)
    }
}

// Create a closure to calculate the area of a triangle
def calculateTriangleArea = { base, height -> 0.5 * base * height }

// Use the closure to calculate the area of a triangle with a base of 10 and a height of 5
def triangleArea = calculateTriangleArea(10, 5)

// Define a method to greet a person
def greetPerson(name) {
    "Hello, ${name}!"
}

// Call the greetPerson method
def greeting = greetPerson('John Doe')

// Define a regular expression to match a valid email address
def emailRegex = /^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)$/

// Use the regular expression to validate an email address
def isValidEmail = emailRegex.matches('john.doe@example.com')

// Define a list of numbers
def numbers = [1, 2, 3, 4, 5]

// Use the find method to find the first number greater than 3
def firstNumberGreaterThan3 = numbers.find { it > 3 }

// Use the max method to find the maximum value in the list
def maxValue = numbers.max()

// Use the collect method to transform each number in the list into its square
def squaredNumbers = numbers.collect { it * it }

// Use the sum method to calculate the sum of the numbers in the list
def sumOfNumbers = numbers.sum()

// Create a map to store key-value pairs
def capitals = [
    'USA': 'Washington D.C.',
    'Canada': 'Ottawa',
    'Mexico': 'Mexico City'
]

// Use the get method to retrieve the value for a given key
def capitalOfMexico = capitals['Mexico']

// Use the put method to add a new key-value pair to the map
capitals.put('Brazil', 'Brasilia')

// Use the remove method to remove a key-value pair from the map
capitals.remove('Canada')

// Define a class to represent a person
class Person {
    String name
    int age

    String toString() {
        "Name: ${name}, Age: ${age}"
    }
}

// Create a list of Person objects
def people = [
    new Person(name: 'John Doe', age: 30),
    new Person(name: 'Jane Doe', age: 25),
    new Person(name: 'Bob Smith', age: 40)
]

// Use the find method to find the first person with the name "Jane Doe"
def janeDoe = people.find { it.name == 'Jane Doe' }

// Use the max method to find the oldest person in the list
def oldestPerson = people.max { it.age }

// Use the collect method to transform each Person object into a map containing the person's name and age
def personMaps = people.collect { [name: it.name, age: it.age] }

// Use the sum method to calculate the sum of the ages of the people in the list
def sumOfAges = people.sum { it.age }

// Define an enum to represent the days of the week
enum DayOfWeek {
    SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY
}

// Get the current day of the week
def currentDay = DayOfWeek.THURSDAY

// Check if the current day is a weekday
def isWeekday = currentDay != DayOfWeek.SATURDAY && currentDay != DayOfWeek.SUNDAY

// Define a trait to represent a geometric shape
trait Shape {
    double getArea()
}

// Define a class to represent a circle that implements the Shape trait
class Circle implements Shape {
    double radius

    double getArea() {
        Math.PI * radius * radius
    }
}

// Define a class to represent a rectangle that implements the Shape trait
class Rectangle implements Shape {
    double width
    double height

    double getArea() {
        width * height
    }
}

// Create a list of Shape objects
def shapes = [
    new Circle(radius: 5),
    new Rectangle(width: 10, height: 20)
]

// Use the collect method to transform each Shape object into its area
def areas = shapes.collect { it.getArea() }

// Use the sum method to calculate the sum of the areas of the shapes in the list
def sumOfAreas = areas.sum()

// Define a GString to represent a multiline string
def multilineString = '''
This is a multiline
string. It can span
multiple lines.
'''

// Use the =~ operator to perform a regular expression search on a string
def result = 'Hello World!' =~ /World/

// Use the in operator to check if a value is contained in a collection
def isContained = 'World' in ['Hello', 'World', '!']

// Use the == operator to compare two values for equality
def isEqual = 1 == '1'

// Use the != operator to compare two values for inequality
def isNotEqual = 1 != '1'

// Use the > operator to compare two values for greater than
def isGreaterThan = 1 > 0

// Use the < operator to compare two values for less than
def isLessThan = 1 < 2

// Use the >= operator to compare two values for greater than or equal to
def isGreaterThanOrEqualTo = 1 >= 1

// Use the <= operator to compare two values for less than or equal to
def isLessThanOrEqualTo = 1 <= 1

// Use the && operator to perform a logical AND operation
def isTrue = true && true

// Use the || operator to perform a logical OR operation
def isFalse = false || false

// Use the ! operator to perform a logical NOT operation
def isFalse = !true

// Use the ?: operator to perform a conditional assignment
def value = true ? 'True' : 'False'

// Use the switch statement to perform a conditional branching operation
def grade = 'A'
switch (grade) {
    case 'A':
        println('Excellent')
        break
    case 'B':
        println('Good')
        break
    case 'C':
        println('Average')
        break
    default:
        println('Fail')
}

// Use the for loop to iterate over a collection of values
for (i in 1..10) {
    println(i)
}

// Use the while loop to iterate over a collection of values while a condition is true
def i = 1
while (i <= 10) {
    println(i)
    i++
}

// Use the do-while loop to iterate over a collection of values at least once
def j = 1
do {
    println(j)
    j++
} while (j <= 10)

// Use the each method to iterate over a collection of values
['Hello', 'World', '!'].each { println(it) }

// Use the times method to iterate over a specific number of times
5.times { println(it) }

// Use the step method to iterate over a range of values with a specified step size
(1..10).step(2) { println(it) }

// Use the with method to temporarily change the value of a variable
with (complexObject) {
    println(name)
    println(age)
}

// Use the try-catch block to handle exceptions
try {
    def result = 1 / 0
} catch (ArithmeticException e) {
    println('Division by zero')
}

// Use the finally block to always execute a block of code, regardless of whether an exception is thrown
try {
    def result = 1 / 0
} catch (ArithmeticException e) {
    println('Division by zero')
} finally {
    println('Finally block executed')
}
```

This code covers a wide range of Groovy features and demonstrates its versatility and power as a programming language. It includes examples of working with dates, JSON, XML, closures, methods, regular expressions, collections, maps, classes, traits, enums, GStrings, operators, conditional branching, loops, the 'each' method, the 'times' method, the 'step' method, the 'with' method, and exception handling. This code provides a solid foundation for understanding and using Groovy for a variety of programming tasks.