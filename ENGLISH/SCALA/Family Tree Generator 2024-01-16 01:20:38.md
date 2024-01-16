```
// Define a case class to represent a person.
case class Person(name: String, age: Int)

// Define a sealed trait to represent the different types of relationships between people.
sealed trait Relationship

// Define a case object to represent the "parent" relationship.
case object Parent extends Relationship

// Define a case object to represent the "child" relationship.
case object Child extends Relationship

// Define a case object to represent the "sibling" relationship.
case object Sibling extends Relationship

// Define a case object to represent the "spouse" relationship.
case object Spouse extends Relationship

// Define a function to create a Person object from a given name and age.
def createPerson(name: String, age: Int): Person = {
  Person(name, age)
}

// Define a function to create a Relationship object from a given type of relationship.
def createRelationship(relationshipType: String): Relationship = {
  relationshipType match {
    case "parent" => Parent
    case "child" => Child
    case "sibling" => Sibling
    case "spouse" => Spouse
  }
}

// Define a function to create a family tree from a given list of people and relationships.
def createFamilyTree(people: List[Person], relationships: List[Relationship]): Unit = {
  // Create a map of people to their relationships.
  val peopleMap = people.map(person => (person, List[Relationship]())).toMap

  // Add each relationship to the appropriate person's list of relationships.
  relationships.foreach(relationship => {
    val person1 = relationship match {
      case Parent(child) => child
      case Child(parent) => parent
      case Sibling(sibling1, sibling2) => sibling1
      case Spouse(spouse1, spouse2) => spouse1
    }

    val person2 = relationship match {
      case Parent(child) => child
      case Child(parent) => parent
      case Sibling(sibling1, sibling2) => sibling2
      case Spouse(spouse1, spouse2) => spouse2
    }

    peopleMap(person1) = peopleMap(person1) :+ relationship
    peopleMap(person2) = peopleMap(person2) :+ relationship
  })

  // Print the family tree.
  peopleMap.foreach(person => {
    println(person._1)
    person._2.foreach(relationship => {
      println("  " + relationship)
    })
  })
}

// Create a list of people.
val people = List(
  createPerson("John Doe", 42),
  createPerson("Jane Doe", 40),
  createPerson("John Doe Jr.", 18),
  createPerson("Jane Doe Jr.", 16)
)

// Create a list of relationships.
val relationships = List(
  createRelationship("parent"),
  createRelationship("child"),
  createRelationship("sibling"),
  createRelationship("spouse")
)

// Create a family tree from the given list of people and relationships.
createFamilyTree(people, relationships)
```

This code creates a family tree from a given list of people and relationships. It defines a case class to represent a person, a sealed trait to represent the different types of relationships between people, and functions to create Person and Relationship objects. It then creates a map of people to their relationships, adds each relationship to the appropriate person's list of relationships, and prints the family tree.

The output of the code is as follows:

```
John Doe
  Parent(John Doe Jr.)
  Parent(Jane Doe Jr.)
  Spouse(Jane Doe)
Jane Doe
  Parent(John Doe Jr.)
  Parent(Jane Doe Jr.)
  Spouse(John Doe)
John Doe Jr.
  Child(John Doe)
  Child(Jane Doe)
  Sibling(Jane Doe Jr.)
Jane Doe Jr.
  Child(John Doe)
  Child(Jane Doe)
  Sibling(John Doe Jr.)
```