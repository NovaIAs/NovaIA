```scala
// Define a case class to represent a person
case class Person(name: String, age: Int, city: String)

// Define a companion object for the Person case class
object Person {
  // Define a method to create a Person object from a list of strings
  def fromList(list: List[String]): Person = {
    val name = list(0)
    val age = list(1).toInt
    val city = list(2)
    Person(name, age, city)
  }

  // Define a method to convert a Person object to a list of strings
  def toList(person: Person): List[String] = {
    List(person.name, person.age.toString, person.city)
  }
}

// Define a case class to represent a group of people
case class Group(name: String, members: List[Person])

// Define a companion object for the Group case class
object Group {
  // Define a method to create a Group object from a list of strings
  def fromList(list: List[String]): Group = {
    val name = list(0)
    val members = list.drop(1).map(Person.fromList)
    Group(name, members)
  }

  // Define a method to convert a Group object to a list of strings
  def toList(group: Group): List[String] = {
    group.name :: group.members.map(Person.toList).flatten
  }
}

// Define a main method to test the code
object Main extends App {
  // Create a list of lists of strings representing people
  val peopleList = List(
    List("John", "25", "New York"),
    List("Mary", "30", "Los Angeles"),
    List("Bob", "35", "Chicago")
  )

  // Create a list of people from the list of lists of strings
  val people = peopleList.map(Person.fromList)

  // Create a list of strings representing groups
  val groupsList = List(
    List("Group A", "John", "Mary"),
    List("Group B", "Bob")
  )

  // Create a list of groups from the list of strings
  val groups = groupsList.map(Group.fromList)

  // Print the list of people
  println("People:")
  people.foreach(println)

  // Print the list of groups
  println("Groups:")
  groups.foreach(println)
}
```

This code defines two case classes, `Person` and `Group`, to represent people and groups of people, respectively. The `Person` case class has three fields: `name`, `age`, and `city`. The `Group` case class has two fields: `name` and `members`, where `members` is a list of `Person` objects.

The code also defines companion objects for both `Person` and `Group`. These companion objects provide methods for converting between `Person` and `Group` objects and lists of strings.

The `main` method in the `Main` object is used to test the code. It creates a list of people and groups from a list of lists of strings. It then prints the list of people and groups to the console.