```scala
// Define a case class to represent a person
case class Person(name: String, age: Int, occupation: String)

// Create a list of persons
val persons = List(
  Person("John", 30, "Software Engineer"),
  Person("Mary", 25, "Doctor"),
  Person("Bob", 40, "Teacher"),
  Person("Alice", 20, "Student")
)

// Define a function to filter persons by their age
def filterPersonsByAge(age: Int): List[Person] = {
  persons.filter(_.age > age)
}

// Define a function to group persons by their occupation
def groupPersonsByOccupation: Map[String, List[Person]] = {
  persons.groupBy(_.occupation)
}

// Define a function to find the oldest person
def findOldestPerson: Person = {
  persons.maxBy(_.age)
}

// Define a function to find the youngest person
def findYoungestPerson: Person = {
  persons.minBy(_.age)
}

// Define a function to calculate the average age of persons
def averageAge: Double = {
  persons.map(_.age).sum / persons.size
}

// Print the results
println(filterPersonsByAge(30))
println(groupPersonsByOccupation)
println(findOldestPerson)
println(findYoungestPerson)
println(averageAge)
```

This code defines a case class called `Person` with three fields: `name`, `age`, and `occupation`.
It then creates a list of `Person` objects and defines several functions to filter, group, and find the oldest, youngest, and average age of the persons in the list.
The code then prints the results of these functions to the console.

The code is explained in more detail below:

* The `Person` case class is defined using the `case class` keyword.
  This creates a new type that can be used to represent a person.
* The `persons` variable is defined using the `val` keyword.
  This creates a list of `Person` objects.
* The `filterPersonsByAge` function is defined using the `def` keyword.
  This function takes an integer as an argument and returns a list of `Person` objects whose age is greater than the specified age.
* The `groupPersonsByOccupation` function is defined using the `def` keyword.
  This function takes no arguments and returns a map of `String` to `List[Person]`.
  The keys of the map are the occupations of the persons in the list, and the values of the map are lists of the persons who have that occupation.
* The `findOldestPerson` function is defined using the `def` keyword.
  This function takes no arguments and returns the oldest person in the list.
* The `findYoungestPerson` function is defined using the `def` keyword.
  This function takes no arguments and returns the youngest person in the list.
* The `averageAge` function is defined using the `def` keyword.
  This function takes no arguments and returns the average age of the persons in the list.
* The `println` function is used to print the results of the functions to the console.