```scala
// Define a case class to represent a person with customizable fields.
case class Person(name: String, age: Int, location: String)

// Define a trait to represent a possible relationship between two people.
trait Relationship {
  def describeRelationship(): String
}

// Define three different types of relationships: friends, family, and acquaintances.
object Friend extends Relationship {
  override def describeRelationship(): String = "are friends"
}
object Family extends Relationship {
  override def describeRelationship(): String = "are family"
}
object Acquaintance extends Relationship {
  override def describeRelationship(): String = "are acquaintances"
}

// Define a function to generate a random relationship between two people.
def generateRelationship(): Relationship = {
  val relationshipType = scala.util.Random.nextInt(3)
  relationshipType match {
    case 0 => Friend
    case 1 => Family
    case 2 => Acquaintance
  }
}

// Define a function to generate a random person.
def generatePerson(): Person = {
  val name = scala.util.Random.alphanumeric.take(10).mkString
  val age = scala.util.Random.nextInt(100)
  val location = scala.util.Random.alphanumeric.take(10).mkString
  Person(name, age, location)
}

// Generate a list of 100 random people.
val people = List.fill(100)(generatePerson())

// Generate a list of 100 random relationships between pairs of people.
val relationships = List.fill(100)(generateRelationship())

// Define a function to create a relationship between two people by adding a new Relationship object to the 'relationships' list.
def addRelationship(person1: Person, person2: Person, relationship: Relationship): Unit = {
  relationships :+ relationship
}

// Add 100 random relationships between the people in the 'people' list.
for (i <- 0 to 99) {
  val person1 = people(i)
  val person2 = people(scala.util.Random.nextInt(100))
  val relationship = generateRelationship()
  addRelationship(person1, person2, relationship)
}

// Print the names of all the people and their relationships.
for (person <- people) {
  println(person.name)
  for (relationship <- relationships) {
    if (relationship.isInstanceOf[Friend]) {
      println(s"${person.name} and ${person.friend} are friends.")
    } else if (relationship.isInstanceOf[Family]) {
      println(s"${person.name} and ${person.family} are family.")
    } else if (relationship.isInstanceOf[Acquaintance]) {
      println(s"${person.name} and ${person.acquaintance} are acquaintances.")
    }
  }
}
```

This code generates a list of 100 random people and 100 random relationships between them. The relationships can be of three types: friends, family, or acquaintances. The code then prints the names of all the people and their relationships.

Here's a step-by-step explanation of the code:

1. We define a case class called `Person` to represent a person with three fields: `name`, `age`, and `location`.
2. We define a trait called `Relationship` to represent a possible relationship between two people. The trait has a method called `describeRelationship()` that returns a string describing the relationship.
3. We define three different types of relationships: `Friend`, `Family`, and `Acquaintance`. Each type of relationship extends the `Relationship` trait and overrides the `describeRelationship()` method to return a different string.
4. We define a function called `generateRelationship()` that randomly generates a relationship between two people. The function uses the `scala.util.Random` class to generate a random number between 0 and 2. It then returns the corresponding relationship type (Friend, Family, or Acquaintance) based on the random number.
5. We define a function called `generatePerson()` that randomly generates a person. The function uses the `scala.util.Random` class to generate a random name, age, and location. It then returns a `Person` object with the generated values.
6. We generate a list of 100 random people by calling the `generatePerson()` function 100 times and storing the results in a list.
7. We generate a list of 100 random relationships by calling the `generateRelationship()` function 100 times and storing the results in a list.
8. We define a function called `addRelationship()` that adds a new relationship between two people to the `relationships` list. The function takes three arguments: `person1` (the first person), `person2` (the second person), and `relationship` (the type of relationship).
9. We add 100 random relationships between the people in the `people` list. We do this by iterating over the `people` list and for each person, we randomly select another person and generate a random relationship between them. We then add the relationship to the `relationships` list using the `addRelationship()` function.
10. We print the names of all the people and their relationships. We do this by iterating over the `people` list and for each person, we iterate over the `relationships` list and print the name of the other person and the type of relationship between them.

This code demonstrates the use of case classes, traits, random number generation, and list manipulation in Scala.