```scala
// Define a case class to represent a person with their name and age
case class Person(name: String, age: Int)

// Create a list of persons
val persons = List(
  Person("John Doe", 25),
  Person("Jane Doe", 30),
  Person("Bob Smith", 40),
  Person("Alice Smith", 35)
)

// Define a function to calculate the sum of the ages of all persons in the list
def sumAges(persons: List[Person]): Int = {
  persons.foldLeft(0)((sum, person) => sum + person.age)
}

// Define a function to calculate the average age of all persons in the list
def averageAge(persons: List[Person]): Double = {
  sumAges(persons).toDouble / persons.length
}

// Define a function to find the oldest person in the list
def oldestPerson(persons: List[Person]): Person = {
  persons.maxBy(_.age)
}

// Define a function to find the youngest person in the list
def youngestPerson(persons: List[Person]): Person = {
  persons.minBy(_.age)
}

// Print the sum of the ages of all persons in the list
println(s"The sum of the ages of all persons in the list is ${sumAges(persons)}")

// Print the average age of all persons in the list
println(s"The average age of all persons in the list is ${averageAge(persons)}")

// Print the oldest person in the list
println(s"The oldest person in the list is ${oldestPerson(persons)}")

// Print the youngest person in the list
println(s"The youngest person in the list is ${youngestPerson(persons)}")
```

This code defines a case class `Person` to represent a person with their name and age. Then, it creates a list of persons and defines four functions: `sumAges` to calculate the sum of the ages of all persons in the list, `averageAge` to calculate the average age of all persons in the list, `oldestPerson` to find the oldest person in the list, and `youngestPerson` to find the youngest person in the list. Finally, the code prints the results of calling these functions on the list of persons.