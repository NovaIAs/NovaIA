```scala
// Create a case class to represent a person.
case class Person(name: String, age: Int)

// Create a list of people.
val people = List(
  Person("Alice", 25),
  Person("Bob", 30),
  Person("Charlie", 35)
)

// Create a function to filter the list of people by age.
def filterPeopleByAge(age: Int): List[Person] = {
  people.filter(_.age > age)
}

// Create a function to map the list of people to their names.
def mapPeopleToNames: List[String] = {
  people.map(_.name)
}

// Create a function to reduce the list of people to their total age.
def reducePeopleToTotalAge: Int = {
  people.reduce((acc, person) => acc + person.age)
}

// Create a function to fold the list of people to their total age.
def foldPeopleToTotalAge: Int = {
  people.fold(0)((acc, person) => acc + person.age)
}

// Create a function to partition the list of people into two lists, one containing people over the age of 30 and the other containing people under the age of 30.
def partitionPeopleByAge: (List[Person], List[Person]) = {
  people.partition(_.age > 30)
}

// Create a function to find the youngest person in the list of people.
def findYoungestPerson: Person = {
  people.minBy(_.age)
}

// Create a function to find the oldest person in the list of people.
def findOldestPerson: Person = {
  people.maxBy(_.age)
}

// Create a function to sort the list of people by age in ascending order.
def sortPeopleByAgeAscending: List[Person] = {
  people.sortBy(_.age)
}

// Create a function to sort the list of people by age in descending order.
def sortPeopleByAgeDescending: List[Person] = {
  people.sortBy(_.age).reverse
}

// Create a function to group the list of people by age.
def groupPeopleByAge: Map[Int, List[Person]] = {
  people.groupBy(_.age)
}

// Create a function to count the number of people in the list of people.
def countPeople: Int = {
  people.count(_ => true)
}

// Create a function to check if the list of people is empty.
def isPeopleEmpty: Boolean = {
  people.isEmpty
}

// Create a function to check if the list of people is not empty.
def isPeopleNotEmpty: Boolean = {
  people.nonEmpty
}

// Create a function to check if the list of people contains a specific person.
def doesPeopleContain(person: Person): Boolean = {
  people.contains(person)
}

// Create a function to find the index of a specific person in the list of people.
def findIndex(person: Person): Int = {
  people.indexOf(person)
}

// Create a function to find the last index of a specific person in the list of people.
def findLastIndex(person: Person): Int = {
  people.lastIndexOf(person)
}

// Create a function to remove a specific person from the list of people.
def removePerson(person: Person): List[Person] = {
  people.filterNot(_ == person)
}

// Create a function to add a specific person to the list of people.
def addPerson(person: Person): List[Person] = {
  people :+ person
}

// Create a function to update a