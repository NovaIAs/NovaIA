```groovy
// Define a simple class to represent a person
class Person {
    String name
    int age
}

// Create a list of people
def people = [
    new Person(name: "John", age: 25),
    new Person(name: "Mary", age: 30),
    new Person(name: "Bob", age: 35)
]

// Use the collect() method to transform the list of people into a list of their names
def names = people.collect { it.name }

// Use the find() method to find the first person whose age is greater than 30
def firstPersonOver30 = people.find { it.age > 30 }

// Use the max() method to find the oldest person
def oldestPerson = people.max { it.age }

// Use the min() method to find the youngest person
def youngestPerson = people.min { it.age }

// Use the groupBy() method to group the people by their age
def peopleByAge = people.groupBy { it.age }

// Use the sort() method to sort the list of people by their age
def sortedPeople = people.sort { it.age }

// Use the each() method to iterate over the list of people and print their names
people.each { println it.name }

// Use the count() method to count the number of people in the list
def numberOfPeople = people.count()

// Use the every() method to check if all people are over 18 years old
def allPeopleOver18 = people.every { it.age > 18 }

// Use the any() method to check if any person is over 30 years old
def anyPersonOver30 = people.any { it.age > 30 }

// Use the partition() method to divide the list of people into two lists: one for people over 30 and one for people under 30
def (over30, under30) = people.partition { it.age > 30 }

// Use the takeWhile() method to take the first n elements of the list of people
def firstTwoPeople = people.takeWhile { it.age < 30 }

// Use the dropWhile() method to drop the first n elements of the list of people
def peopleWithoutFirstTwo = people.dropWhile { it.age < 30 }
```

This code demonstrates a variety of Groovy features, including:

* **List comprehensions:** The `[ ... ]` syntax is used to create lists in a concise manner.
* **Method chaining:** Methods can be chained together to perform multiple operations on a collection.
* **Lambda expressions:** Lambda expressions are used to define anonymous functions.
* **Closures:** Closures are blocks of code that can be passed around and executed as needed.
* **Operators:** Groovy provides a rich set of operators, including the `collect`, `find`, `max`, `min`, `groupBy`, `sort`, `each`, `count`, `every`, `any`, `partition`, `takeWhile`, and `dropWhile` operators.

This code can be used to perform a variety of data manipulation tasks, such as filtering, sorting, grouping, and aggregating data.