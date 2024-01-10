```scala
// Define a case class to represent a person with a name and age
case class Person(name: String, age: Int)

// Create a list of people
val people = List(
  Person("Alice", 25),
  Person("Bob", 30),
  Person("Carol", 35),
  Person("Dave", 40),
  Person("Eve", 45)
)

// Define a function to get the average age of a list of people
def averageAge(people: List[Person]): Double = {
  // Calculate the sum of the ages
  val totalAge = people.map(_.age).sum

  // Calculate the average age
  val averageAge = totalAge / people.length

  // Return the average age
  averageAge
}

// Print the average age of the people list
println(averageAge(people))

// Define a function to find the oldest person in a list of people
def oldestPerson(people: List[Person]): Person = {
  // Find the person with the maximum age
  val oldest = people.maxBy(_.age)

  // Return the oldest person
  oldest
}

// Print the oldest person in the people list
println(oldestPerson(people))

// Define a function to group people by their age
def groupByAge(people: List[Person]): Map[Int, List[Person]] = {
  // Group the people by their age
  val groups = people.groupBy(_.age)

  // Return the groups
  groups
}

// Print the groups of people by age
println(groupByAge(people))

// Define a function to find the most common age in a list of people
def mostCommonAge(people: List[Person]): Int = {
  // Group the people by their age
  val groups = people.groupBy(_.age)

  // Find the group with the maximum number of people
  val mostCommonAgeGroup = groups.maxBy(_._2.length)

  // Return the age of the most common age group
  mostCommonAgeGroup._1
}

// Print the most common age in the people list
println(mostCommonAge(people))
```

This code performs a variety of operations on a list of people, including calculating the average age, finding the oldest person, grouping people by their age, and finding the most common age.

The code first defines a case class called `Person` to represent a person with a name and age. It then creates a list of people called `people`.

The code then defines a function called `averageAge` that takes a list of people and returns the average age of the people in the list. The function calculates the sum of the ages of the people in the list and then divides the sum by the number of people in the list to get the average age.

The code then defines a function called `oldestPerson` that takes a list of people and returns the oldest person in the list. The function finds the person with the maximum age in the list and returns that person.

The code then defines a function called `groupByAge` that takes a list of people and returns a map of ages to lists of people. The function groups the people in the list by their age and returns a map where the keys are the ages and the values are lists of people with that age.

The code then defines a function called `mostCommonAge` that takes a list of people and returns the most common age in the list. The function groups the people in the list by their age and then finds the group with the maximum number of people. The age of the group with the maximum number of people is the most common age.

The code then prints the average age, oldest person, groups of people by age, and most common age of the people in the `people` list.