```scala
// Scala program to demonstrate the use of companion objects and case classes

// Companion object for the Person case class
object Person {
  // Factory method to create a Person object
  def apply(name: String, age: Int): Person = new Person(name, age)

  // Method to create a Person object from a map
  def fromMap(map: Map[String, Any]): Person = {
    val name = map("name").asInstanceOf[String]
    val age = map("age").asInstanceOf[Int]
    new Person(name, age)
  }
}

// Case class representing a person
case class Person(name: String, age: Int) {

  // Method to get the person's name
  def getName: String = name

  // Method to get the person's age
  def getAge: Int = age

  // Method to create a map of the person's properties
  def toMap: Map[String, Any] = {
    Map("name" -> name, "age" -> age)
  }
}

// Main object to test the Person case class and companion object
object Main {
  def main(args: Array[String]): Unit = {
    // Create a Person object using the companion object's apply method
    val person1 = Person("John Doe", 30)

    // Create a Person object from a map using the companion object's fromMap method
    val person2 = Person.fromMap(Map("name" -> "Jane Doe", "age" -> 25))

    // Print the person's properties
    println(s"Person 1: ${person1.getName}, ${person1.getAge}")
    println(s"Person 2: ${person2.getName}, ${person2.getAge}")

    // Create a map of the person's properties using the toMap method
    val person3Map = person1.toMap

    // Print the map
    println(s"Person 3 Map: ${person3Map}")
  }
}
```

Explanation:

1. **Companion Object:**
   - We define a companion object for the `Person` case class named `Person`.
   - This object contains factory methods and utility methods related to the case class.

2. **Factory Methods:**
   - The `apply` method in the companion object is a factory method that allows us to create a `Person` object more concisely.
   - The `fromMap` method is another factory method that creates a `Person` object from a map containing the person's properties.

3. **Case Class:**
   - We define a case class named `Person` with two properties: `name` (String) and `age` (Int).
   - Case classes are a powerful feature in Scala that allow us to create immutable data structures with minimal boilerplate code.

4. **Getter Methods:**
   - We define getter methods `getName` and `getAge` in the `Person` case class to access the person's name and age, respectively.

5. **`toMap` Method:**
   - We define a `toMap` method in the `Person` case class to convert the person's properties into a map.

6. **Testing the Code:**
   - In the `Main` object, we create two `Person` objects using the factory methods:
     - `person1` is created using the `apply` method.
     - `person2` is created from a map using the `fromMap` method.
   - We print the person's properties and demonstrate the use of the `toMap` method.

This code showcases various features of Scala, including companion objects, case classes, factory methods, getter methods, and the `toMap` method, providing a comprehensive example of data modeling and manipulation in Scala.