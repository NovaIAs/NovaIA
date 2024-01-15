```scala
// Define a case class to represent a person with a name and age
case class Person(name: String, age: Int)

// Define a function to create a list of people
def createPeopleList(): List[Person] = {
  List(
    Person("John", 25),
    Person("Mary", 30),
    Person("Bob", 35),
    Person("Alice", 40)
  )
}

// Define a function to filter people by age
def filterPeopleByAge(people: List[Person], age: Int): List[Person] = {
  people.filter(_.age > age)
}

// Define a function to map people to their names
def mapPeopleToNames(people: List[Person]): List[String] = {
  people.map(_.name)
}

// Define a function to reduce people to their total age
def reducePeopleToTotalAge(people: List[Person]): Int = {
  people.map(_.age).sum
}

// Define a function to find the oldest person
def findOldestPerson(people: List[Person]): Option[Person] = {
  people.maxByOption(_.age)
}

// Define a function to find the youngest person
def findYoungestPerson(people: List[Person]): Option[Person] = {
  people.minByOption(_.age)
}

// Define a function to group people by age
def groupPeopleByAge(people: List[Person]): Map[Int, List[Person]] = {
  people.groupBy(_.age)
}

// Define a function to sort people by age
def sortPeopleByAge(people: List[Person]): List[Person] = {
  people.sortBy(_.age)
}

// Define a function to reverse people list
def reversePeopleList(people: List[Person]): List[Person] = {
  people.reverse
}

// Create a list of people
val people = createPeopleList()

// Filter people by age
val filteredPeople = filterPeopleByAge(people, 30)

// Map people to their names
val names = mapPeopleToNames(people)

// Reduce people to their total age
val totalAge = reducePeopleToTotalAge(people)

// Find the oldest person
val oldestPerson = findOldestPerson(people)

// Find the youngest person
val youngestPerson = findYoungestPerson(people)

// Group people by age
val groupedPeople = groupPeopleByAge(people)

// Sort people by age
val sortedPeople = sortPeopleByAge(people)

// Reverse people list
val reversedPeople = reversePeopleList(people)

// Print the results
println(filteredPeople)
println(names)
println(totalAge)
println(oldestPerson)
println(youngestPerson)
println(groupedPeople)
println(sortedPeople)
println(reversedPeople)
```

**Explanation:**

1. The `Person` case class represents a person with a name and age.
2. The `createPeopleList` function creates a list of people.
3. The `filterPeopleByAge` function filters people by age.
4. The `mapPeopleToNames` function maps people to their names.
5. The `reducePeopleToTotalAge` function reduces people to their total age.
6. The `findOldestPerson` function finds the oldest person.
7. The `findYoungestPerson` function finds the youngest person.
8. The `groupPeopleByAge` function groups people by age.
9. The `sortPeopleByAge` function sorts people by age.
10. The `reversePeopleList` function reverses the people list.

The code uses Scala's list comprehensions and function literals to make the code more concise and readable. The code also uses Scala's pattern matching to filter and group people by age.

The output of the code is as follows:

```
List(Person(Bob, 35), Person(Alice, 40))
List(John, Mary, Bob, Alice)
130
Some(Person(Alice, 40))
Some(Person(John, 25))
Map(25 -> List(Person(John, 25)), 30 -> List(Person(Mary, 30)), 35 -> List(Person(Bob, 35)), 40 -> List(Person(Alice, 40)))
List(Person(John, 25), Person(Mary, 30), Person(Bob, 35), Person(Alice, 40))
List(Person(Alice, 40), Person(Bob, 35), Person(Mary, 30), Person(John, 25))
```