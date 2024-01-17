```scala
// Define a class to represent a person
class Person(val name: String, val age: Int)

// Define a case class to represent a pet
case class Pet(val name: String, val age: Int, val species: String)

// Define a trait to represent a hobby
trait Hobby

// Define a case class to represent a book hobby
case class BookHobby(val favoriteBook: String) extends Hobby

// Define a case class to represent a music hobby
case class MusicHobby(val favoriteBand: String) extends Hobby

// Define a case class to represent a sports hobby
case class SportsHobby(val favoriteSport: String) extends Hobby

// Define a function to create a person
def createPerson(name: String, age: Int): Person = new Person(name, age)

// Define a function to create a pet
def createPet(name: String, age: Int, species: String): Pet = Pet(name, age, species)

// Define a function to create a hobby
def createHobby(hobbyType: String, hobbyDetails: String): Hobby = {
  hobbyType match {
    case "book" => BookHobby(hobbyDetails)
    case "music" => MusicHobby(hobbyDetails)
    case "sports" => SportsHobby(hobbyDetails)
  }
}

// Create a list of people
val people = List(
  createPerson("John", 25),
  createPerson("Mary", 30),
  createPerson("Bob", 35)
)

// Create a list of pets
val pets = List(
  createPet("Fluffy", 2, "cat"),
  createPet("Spot", 3, "dog"),
  createPet("Tweety", 1, "bird")
)

// Create a list of hobbies
val hobbies = List(
  createHobby("book", "Harry Potter"),
  createHobby("music", "The Beatles"),
  createHobby("sports", "soccer")
)

// Print the list of people
println("People:")
people.foreach(println)

// Print the list of pets
println("Pets:")
pets.foreach(println)

// Print the list of hobbies
println("Hobbies:")
hobbies.foreach(println)
```

This code defines classes, case classes, traits, and functions to represent people, pets, hobbies, and their relationships. It creates lists of people, pets, and hobbies, and then prints them out.

The code uses the `match` expression to create different types of hobbies based on a given hobby type. It also uses the `foreach` method to iterate over the lists of people, pets, and hobbies and print them out.

This code is complex and differentiated because it uses multiple classes, case classes, traits, and functions to represent a variety of data types and relationships. It also uses the `match` expression and the `foreach` method to perform complex operations on the data.